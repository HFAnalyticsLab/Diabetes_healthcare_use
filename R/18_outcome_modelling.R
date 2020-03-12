# =======================================================
# Project: Diabetes outpatient care
# Purpose: Cox model to examine effect of CMD and PNC on outcomes 
# for people with T2DM (emergency hospital admission, all-cause mortality)
# Author: Fiona Grimm
# =======================================================

library(tidyverse)
library(lubridate)
library(janitor)
library(tidylog)

library(survival)
library(survminer)


# Source file paths: Rds_path
source('R/file_paths.R')

# Source study parameters 
source('R/study_params.R')

# Source graph parameters 
source('R/plotting_params.R')

# Functions / definitions -----------------------------------------------------

# a function to summarise the coefficients of COX PH models,
# including confidence intervals
summariseCoxModel <- function(model){
  
  table <- broom::tidy(model, exponentiate = TRUE) 
  
  rownames(table) <- NULL
  colnames(table) <- c('Coefficient', 'HR', 'Std.Error', 'z.value', 'p.value', 'CI_lower', 'CI_upper')
  
  table <- table %>% 
    select(Coefficient, HR, CI_lower, CI_upper, everything()) %>% 
    mutate_at(vars(HR, z.value, CI_lower, CI_upper), round, 3)
  
  return(table)
}

# Import data -------------------------------------------------------------

patients <- readRDS(str_c(processed_RDS_path, 'patients_clinical_combined.Rds'))

# Hospital admissions
hesapc_admissions <- readRDS(str_c(processed_RDS_path, 'admissions.rds'))

# Prepare T2D modelling data for all-cause mortality (death within study period) ----

# Censoring time 
# Type of even/censoring
# 1 = censored (study end)
# 2 = died
model_data_mortality <- patients %>% 
  filter(cohort_step2 == 1) %>% 
  group_by(patid) %>% 
  mutate(earliest_eventdate = min(ONS_dod, study_end, na.rm = TRUE),
         time_to_event = as.numeric(earliest_eventdate - study_start, 'days'),
         status = 1,
         status = ifelse(!is.na(ONS_dod) & earliest_eventdate == ONS_dod, 2, status)) %>% 
  ungroup()

model_data_mortality <-  model_data_mortality %>% 
  filter(diabetes_type == 'type2') %>% 
  mutate(diabetes_type = fct_drop(diabetes_type),
         female = ifelse(gender == '1', 0, 1),
         female = factor(female),
         imd_quintile_numeric = as.numeric(gsub(' .*', '', imd_quintile)),
         smoker = ifelse(smoking_status == 'smoker', 1,0),
         newly_diagnosed_1y = ifelse(time_since_diagnosis == 'less than 1 year', 1, 0),
         age_centered = startage_study - round(mean(startage_study)),
         age_5ybin_centered = age_centered / 5,
         mm_count_excl_DEPANXr = rowSums(.[comorbidities[comorbidities != 'DEPANXr']]),
         mm_count_excl_PNC = rowSums(.[comorbidities[comorbidities != 'PNC']]))

# Bin survival time into weeks for kaplan-meier plots
model_data_mortality <-  model_data_mortality %>% 
  mutate(time_to_event_weeks = ceiling(time_to_event/7),
         time_to_event_weeks = ifelse(time_to_event_weeks == 0, 1, time_to_event_weeks),
         time_to_event_4weeks = time_to_event_weeks + 4 - time_to_event_weeks %% 4)

# Cumulative incidence of death, split by DEPANX

ggsurvplot(fit = survfit(Surv(time = time_to_event, event = status) ~ DEPANXr,
                         data = model_data_mortality), 
           xlab = 'Days', ylab = 'Cumulative incidence', 
           censor = FALSE, conf.int = TRUE, fun = 'event')

ggsurvplot(fit = survfit(Surv(time = time_to_event, event = status) ~ PNC,
                         data = model_data_mortality), 
           xlab = 'Days', ylab = 'Cumulative incidence', 
           censor = FALSE, conf.int = TRUE, fun = 'event')

ggsurvplot(fit = survfit(Surv(time = time_to_event, event = status) ~ mm_cat,
                         data = model_data_mortality), 
           xlab = 'Days', ylab = 'Cumulative incidence', 
           censor = FALSE, conf.int = TRUE, fun = 'event')


# T2D Mortality Cox PH ------------------------------------------------------------------

T2D_mortality_coxmodel_base <- coxph(Surv(time = time_to_event, event = status) ~ female + age_5ybin_centered + imd_quintile_numeric  
                        + HbA1C_control + smoker + newly_diagnosed_1y, data = model_data_mortality)

T2D_mortality_coxmodel_base_summary <- summariseCoxModel(model = T2D_mortality_coxmodel_base)
write_csv(T2D_mortality_coxmodel_base_summary, str_c(summary_stats_path, 'outcomes_modelling/Coefficients_T2D_Mortality_basemodel.csv'))

# DEPANXr
T2D_mortality_coxmodel_DEPANXr <- update(T2D_mortality_coxmodel_base, . ~ . + mm_count_excl_DEPANXr + DEPANXr )
summary(T2D_mortality_coxmodel_DEPANXr)
extractAIC(T2D_mortality_coxmodel_DEPANXr)

T2D_mortality_coxmodel_DEPANXr_summary <- summariseCoxModel(model = T2D_mortality_coxmodel_DEPANXr)
write_csv(T2D_mortality_coxmodel_DEPANXr_summary, str_c(summary_stats_path, 'outcomes_modelling/Coefficients_T2D_Mortality_DEPANXr.csv'))

# PNC
T2D_mortality_coxmodel_PNC <- update(T2D_mortality_coxmodel_base, . ~ . + mm_count_excl_PNC + PNC )
summary(T2D_mortality_coxmodel_PNC)
extractAIC(T2D_mortality_coxmodel_PNC)

T2D_mortality_coxmodel_PNC_summary <- summariseCoxModel(model = T2D_mortality_coxmodel_PNC)
write_csv(T2D_mortality_coxmodel_PNC_summary, str_c(summary_stats_path, 'outcomes_modelling/Coefficients_T2D_Mortality_PNC.csv'))


# Model all conditions invididually 
T2D_coxmodel_allmm <- update(T2D_mortality_coxmodel_base, as.formula(str_c('. ~ . + ', str_c(comorbidities, collapse = ' + '))))

T2D_coxmodel_allmm_summary <- summariseCoxModel(model = T2D_coxmodel_allmm)
write_csv(T2D_coxmodel_allmm_summary, str_c(summary_stats_path, 'outcomes_modelling/Coefficients_T2D_Mortality_allmm.csv'))

# Kaplan meier plots

T2DM_mortality_survplot_DEPANXr <- ggsurvplot(fit = survfit(Surv(time = time_to_event_weeks, event = status) ~ DEPANXr,
                         data = model_data_mortality), 
           xlab = 'Weeks', ylab = 'Cumulative incidence',
           censor = FALSE,  fun = 'event', ggtheme = theme_bw(), size = 0.5,
           palette = THF_discrete_colour_order[1:2], legend = c(0.2,0.85),
           legend.labs = c('Without CMD', 'With CMD'), title = 'All-cause mortality') 

T2DM_mortality_survplot_DEPANXr <- T2DM_mortality_survplot_DEPANXr$plot +
  theme(legend.title = element_blank())

ggsave(str_c(summary_stats_path, 'outcomes_modelling/Summary_T2D_Mortality_DEPANXr.png'), 
       T2DM_mortality_survplot_DEPANXr, width = 5, height = 4)


T2DM_mortality_survplot_PNC <- ggsurvplot(fit = survfit(Surv(time = time_to_event_weeks, event = status) ~ PNC,
                                                            data = model_data_mortality), 
                                              xlab = 'Weeks', ylab = 'Cumulative incidence',
                                              censor = FALSE,  fun = 'event', ggtheme = theme_bw(), size = 0.5,
                                              palette = THF_discrete_colour_order[1:2], legend = c(0.2,0.85),
                                              legend.labs = c('Without PNC', 'With PNC'), title = 'All-cause mortality') 

T2DM_mortality_survplot_PNC <- T2DM_mortality_survplot_PNC$plot +
  theme(legend.title = element_blank())

ggsave(str_c(summary_stats_path, 'outcomes_modelling/Summary_T2D_Mortality_PNC.png'), 
       T2DM_mortality_survplot_PNC, width = 5, height = 4)

T2D_mortality_natrisk_DEPANXr <- surv_summary(survfit(Surv(time = time_to_event_weeks, event = status) ~ DEPANXr,
                                              data = model_data_mortality), data = model_data_mortality)

write_csv(T2D_mortality_natrisk_DEPANXr, str_c(summary_stats_path, 'outcomes_modelling/Summary_T2D_Mortality_DEPANXr.csv'))

T2D_mortality_natrisk_PNC <- surv_summary(survfit(Surv(time = time_to_event_weeks, event = status) ~ PNC,
                                              data = model_data_mortality), data = model_data_mortality)

write_csv(T2D_mortality_natrisk_PNC, str_c(summary_stats_path, 'outcomes_modelling/Summary_T2D_Mortality_PNC.csv'))


# Prepare T2D modelling data for emergency hospital admissions ----

em_admissions_study <- hesapc_admissions %>%
  filter(epistart %within% interval(study_start, study_end) & admitype == 'emergency') %>% 
  group_by(patid) %>% 
  arrange(patid, epistart) %>% 
  filter(row_number() == 1) %>% 
  ungroup()


# Censoring time 
# Type of even/censoring
# 1 = censored (study end, or death)
# 2 = admitted

model_data_adm <- patients %>% 
  filter(cohort_step2 == 1 & diabetes_type =='type2') %>% 
  left_join(em_admissions_study[, c('patid', 'epistart')], by = 'patid') %>% 
  group_by(patid) %>% 
  mutate(earliest_eventdate = min(ONS_dod, epistart, study_end, na.rm = TRUE),
         time_to_event = as.numeric(earliest_eventdate - study_start, 'days'),
         status = 1,
         status = ifelse(!is.na(ONS_dod) & earliest_eventdate == ONS_dod, 1, status),
         status = ifelse(!is.na(epistart) & earliest_eventdate == epistart, 2, status)) %>% 
  ungroup()


model_data_adm <-  model_data_adm %>% 
  mutate(time_to_event_weeks = ceiling(time_to_event/7),
         time_to_event_weeks = ifelse(time_to_event_weeks == 0, 1, time_to_event_weeks),
         diabetes_type = fct_drop(diabetes_type),
         female = ifelse(gender == '1', 0, 1),
         female = factor(female),
         imd_quintile_numeric = as.numeric(gsub(' .*', '', imd_quintile)),
         smoker = ifelse(smoking_status == 'smoker', 1,0),
         newly_diagnosed_1y = ifelse(time_since_diagnosis == 'less than 1 year', 1, 0),
         age_centered = startage_study - round(mean(startage_study)),
         age_5ybin_centered = age_centered / 5,
         mm_count_excl_DEPANXr = rowSums(.[comorbidities[comorbidities != 'DEPANXr']]),
         mm_count_excl_PNC = rowSums(.[comorbidities[comorbidities != 'PNC']]))


# Cox models for T2D
T2D_adm_coxmodel_base <- coxph(Surv(time = time_to_event, event = status) ~ female + age_5ybin_centered + imd_quintile_numeric  
                               + HbA1C_control + smoker + newly_diagnosed_1y, data = model_data_adm)
summary(T2D_adm_coxmodel_base)
extractAIC(T2D_adm_coxmodel_base)
T2D_adm_coxmodel_base_summary <- summariseCoxModel(model = T2D_adm_coxmodel_base)
write_csv(T2D_adm_coxmodel_base_summary, str_c(summary_stats_path, 'outcomes_modelling/Coefficients_T2D_EmAdmissions_basemodel.csv'))

# DEPANXr
T2D_adm_coxmodel_DEPANXr <- update(T2D_adm_coxmodel_base, . ~ . + mm_count_excl_DEPANXr + DEPANXr )
summary(T2D_adm_coxmodel_DEPANXr)
extractAIC(T2D_adm_coxmodel_DEPANXr)

T2D_adm_coxmodel_DEPANXr_summary <- summariseCoxModel(model = T2D_adm_coxmodel_DEPANXr)
write_csv(T2D_adm_coxmodel_DEPANXr_summary, str_c(summary_stats_path, 'outcomes_modelling/Coefficients_T2D_EmergencyAdmissions_DEPANXr.csv'))

# PNC
T2D_adm_coxmodel_PNC <- update(T2D_adm_coxmodel_base, . ~ . + mm_count_excl_PNC + PNC )
summary(T2D_adm_coxmodel_PNC)
extractAIC(T2D_adm_coxmodel_PNC)

T2D_adm_coxmodel_PNC_summary <- summariseCoxModel(model = T2D_adm_coxmodel_PNC)
write_csv(T2D_adm_coxmodel_PNC_summary, str_c(summary_stats_path, 'outcomes_modelling/Coefficients_T2D_EmergencyAdmissions_PNC.csv'))

# Model all conditions invididually 

T2D_adm_coxmodel_allmm <-  update(T2D_adm_coxmodel_base, as.formula(str_c('. ~ . + ', str_c(comorbidities, collapse = ' + '))))

summary(T2D_adm_coxmodel_allmm)
extractAIC(T2D_adm_coxmodel_allmm)
T2D_adm_coxmodel_allmm_summary <- summariseCoxModel(model = T2D_adm_coxmodel_allmm)
write_csv(T2D_adm_coxmodel_allmm_summary, str_c(summary_stats_path, 'outcomes_modelling/Coefficients_T2D_EmergencyAdmissions_allmm.csv'))

# Kaplan meier plots

T2DM_adm_survplot_DEPANXr <- ggsurvplot(fit = survfit(Surv(time = time_to_event_weeks, event = status) ~ DEPANXr,
                                                            data = model_data_adm), 
                                              xlab = 'Weeks', ylab = 'Cumulative incidence',
                                              censor = FALSE,  fun = 'event', ggtheme = theme_bw(), size = 0.5,
                                              palette = THF_discrete_colour_order[1:2], legend = c(0.2,0.85),
                                              legend.labs = c('Without CMD', 'With CMD'), title = 'Emergency Hospital Admissions') 

T2DM_adm_survplot_DEPANXr <- T2DM_adm_survplot_DEPANXr$plot +
  theme(legend.title = element_blank())

ggsave(str_c(summary_stats_path, 'outcomes_modelling/Summary_T2D_EmergencyAdmissions_DEPANXr.png'), 
       T2DM_adm_survplot_DEPANXr, width = 5, height = 4)


T2DM_adm_survplot_PNC <- ggsurvplot(fit = survfit(Surv(time = time_to_event_weeks, event = status) ~ PNC,
                                                        data = model_data_adm), 
                                          xlab = 'Weeks', ylab = 'Cumulative incidence',
                                          censor = FALSE,  fun = 'event', ggtheme = theme_bw(), size = 0.5,
                                          palette = THF_discrete_colour_order[1:2], legend = c(0.2,0.85),
                                          legend.labs = c('Without PNC', 'With PNC'), title = 'Emergency Hospital Admissions') 

T2DM_adm_survplot_PNC <- T2DM_adm_survplot_PNC$plot +
  theme(legend.title = element_blank())

ggsave(str_c(summary_stats_path, 'outcomes_modelling/Summary_T2D_EmergencyAdmissions_PNC.png'), 
       T2DM_adm_survplot_PNC, width = 5, height = 4)

T2D_adm_natrisk_DEPANXr <- surv_summary(survfit(Surv(time = time_to_event_weeks, event = status) ~ DEPANXr,
                                                      data = model_data_adm), data = model_data_adm)

write_csv(T2D_mortality_natrisk_DEPANXr, str_c(summary_stats_path, 'outcomes_modelling/Summary_T2D_EmergencyAdmissions_DEPANXr.csv'))

T2D_adm_natrisk_PNC <- surv_summary(survfit(Surv(time = time_to_event_weeks, event = status) ~ PNC,
                                                  data = model_data_adm), data = model_data_adm)

write_csv(T2D_adm_natrisk_PNC, str_c(summary_stats_path, 'outcomes_modelling/Summary_T2D_EmergencyAdmissions_PNC.csv'))
