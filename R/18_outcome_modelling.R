# =======================================================
# Project: Diabetes outpatient care
# Purpose: Cox model to examine effect of long-term conditions on outcomes for patients with T2DM
# (emergeny hospital admission, all-cause mortality)
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

# Prevalence of conditions
mm_prev <- read_csv(str_c(summary_stats_path, 'multimorbidity/MM_prevalence.csv'))

# Keep 20 most prevalent conditions apart from CMD
mm_prev_T2D <- mm_prev %>% 
  filter(diabetes_type ==  'type2' & !is.element(condition, c('DEPANX', 'DEP', 'ANX', 'ANXr'))) %>% 
  arrange(desc(prevalence)) %>% 
  head(20)

# Prepare mortality model data (death within study period) ---------------------------

model_data <- patients %>% 
  filter(cohort_step2 == 1) %>% 
  mutate(mental_disorder = 'None',
         mental_disorder = ifelse(DEPANXr == 1, 'CMD', mental_disorder),
         mental_disorder = ifelse(SCZ == 1, 'SMD', mental_disorder),
         DEPANXr_PNC = case_when(DEPANXr == 0 & PNC == 0 ~ 'none',
                                 DEPANXr == 1 & PNC == 0 ~ 'DEPANXr',
                                 DEPANXr == 0 & PNC == 1 ~ 'PNC',
                                 DEPANXr == 1 & PNC == 1 ~ 'DEPANXr/PNC'),
         DEPANXr_PNC = fct_relevel(DEPANXr_PNC, 'none', 'DEPANXr', 'PNC', 'DEPANXr/PNC'))

model_data %>%  tabyl(DEPANXr_PNC)

# Censoring time 

# Type of even/censoring
# 1 = censored (study end)
# 2 = died
model_data <-  model_data %>% 
  group_by(patid) %>% 
  mutate(earliest_eventdate = min(ONS_dod, study_end, na.rm = TRUE),
         time_to_event = as.numeric(earliest_eventdate - study_start, 'days'),
         status = 1,
         status = ifelse(!is.na(ONS_dod) & earliest_eventdate == ONS_dod, 2, status),
         death = ifelse(!is.na(ONS_dod) & earliest_eventdate == ONS_dod, 1, 0)) %>% 
  ungroup()

model_data <-  model_data %>% 
  filter(diabetes_type == 'type2') %>% 
  mutate(diabetes_type = fct_drop(diabetes_type),
         female = ifelse(gender == '1', 0, 1),
         female = factor(female),
         imd_quintile_numeric = as.numeric(gsub(' .*', '', imd_quintile)),
         smoker = ifelse(smoking_status == 'smoker', 1,0),
         newly_diagnosed_1y = ifelse(time_since_diagnosis == 'less than 1 year', 1, 0),
         HbA1C_control_bad = ifelse(HbA1C_control == 'bad', 1, 0))

# Split by diabetes type
model_data_T2D <- model_data %>% 
  mutate(age_centered = startage_study - round(mean(startage_study)),
         age_5ybin_centered = age_centered / 5)


# mortality Cox PH ------------------------------------------------------------------

ggsurvplot(fit = survfit(Surv(time = time_to_event, event = status) ~ imd_quintile,
                         data = model_data_T2D), 
           xlab = 'Days', ylab = 'Cumulative incidence', facet.by = 'DEPANXr',
           censor = FALSE, conf.int = TRUE, fun = 'event')

T2D_coxmodel_base <- coxph(Surv(time = time_to_event, event = status) ~ female + age_5ybin_centered + imd_quintile_numeric  
                        + HbA1C_control + smoker , data = model_data_T2D)

T2D_coxmodel_base_summary <- summariseCoxModel(model = T2D_coxmodel_base)
write_csv(T2D_coxmodel_base_summary, str_c(summary_stats_path, 'outcomes_modelling/Coefficients_T2D_Mortality_basemodel.csv'))


T2D_coxmodel_1 <- coxph(Surv(time = time_to_event, event = status) ~ female + age_5ybin_centered + imd_quintile_numeric + DEPANXr 
                     + HbA1C_control + physical_mm_count , data = model_data_T2D)
summary(T2D_coxmodel_1)
extractAIC(T2D_coxmodel_1)

T2D_coxmodel_2 <- coxph(Surv(time = time_to_event, event = status) ~ female + age_5ybin_centered + imd_quintile_numeric  
                     + HbA1C_control + smoker + DEPANXr + physical_mm_count , data = model_data_T2D) # slightly better
summary(T2D_coxmodel_2)
extractAIC(T2D_coxmodel_2)

T2D_coxmodel_2_summary <- summariseCoxModel(model = T2D_coxmodel_2)
write_csv(T2D_coxmodel_2_summary, str_c(summary_stats_path, 'outcomes_modelling/Coefficients_T2D_Mortality_DEPANXr.csv'))


T2D_coxmodel_4 <- coxph(Surv(time = time_to_event, event = status) ~ female + age_5ybin_centered + imd_quintile_numeric + DEPANXr_PNC 
                        + HbA1C_control + physical_mm_count + smoker, data = model_data_T2D) # best

T2D_coxmodel_4_summary <- summariseCoxModel(model = T2D_coxmodel_4)
write_csv(T2D_coxmodel_4_summary, str_c(summary_stats_path, 'outcomes_modelling/Coefficients_T2D_Mortality_DEPANXr_PNC.csv'))

extractAIC(T2D_coxmodel_4)


# Outcome: emergency admission --------------------------------------------

em_admissions_study <- hesapc_admissions %>%
  filter(epistart %within% interval(study_start, study_end) & admitype == 'emergency') %>% 
  group_by(patid) %>% 
  arrange(patid, epistart) %>% 
  filter(row_number() == 1) %>% 
  ungroup()


model_data_adm <- patients %>% 
  filter(cohort_step2 == 1) %>% 
  select(patid, gender, pracid, 
         startage_study, age_bins_study, age_bins_study_SDC, 
         imd_quintile, diabetes_type, medication, smoking_status, BMI_categorical, HbA1C_control,
         time_since_diagnosis, 
         mm_cat, mm_count, physical_mm_count, physical_mm_cat, mental_mm_cat_SDC, DEPANX, DEPANXr, SCZ, PNC,
         ONS_dod)  %>% 
  mutate(mental_disorder = 'None',
         mental_disorder = ifelse(DEPANXr == 1, 'CMD', mental_disorder),
         mental_disorder = ifelse(SCZ == 1, 'SMD', mental_disorder),
         DEPANXr_PNC = case_when(DEPANXr == 0 & PNC == 0 ~ 'none',
                                 DEPANXr == 1 & PNC == 0 ~ 'DEPANXr',
                                 DEPANXr == 0 & PNC == 1 ~ 'PNC',
                                 DEPANXr == 1 & PNC == 1 ~ 'DEPANXr/PNC'),
         DEPANXr_PNC = fct_relevel(DEPANXr_PNC, 'none', 'DEPANXr', 'PNC', 'DEPANXr/PNC')) %>% 
  left_join(em_admissions_study[, c('patid', 'epistart')], by = 'patid') 

         
# Censoring time 

# Type of even/censoring
# 1 = censored (study end, or death)
# 2 = admitted

model_data_adm <-  model_data_adm %>% 
  group_by(patid) %>% 
  mutate(earliest_eventdate = min(ONS_dod, epistart, study_end, na.rm = TRUE),
         time_to_event = as.numeric(earliest_eventdate - study_start, 'days'),
         status = 1,
         status = ifelse(!is.na(ONS_dod) & earliest_eventdate == ONS_dod, 1, status),
         status = ifelse(!is.na(epistart) & earliest_eventdate == epistart, 2, status)) %>% 
  ungroup()


View(model_data_adm %>% select(patid, ONS_dod, epistart, earliest_eventdate, time_to_event, status))

model_data_adm <-  model_data_adm %>% 
  mutate(time_to_event_weeks = ceiling(time_to_event/7),
         time_to_event_weeks = ifelse(time_to_event_weeks == 0, 1, time_to_event_weeks))

model_data_adm <-  model_data_adm %>% 
  filter(diabetes_type  == 'type2') %>% 
  mutate(diabetes_type = fct_drop(diabetes_type),
         female = ifelse(gender == '1', 0, 1),
         female = factor(female),
         imd_quintile_numeric = as.numeric(gsub(' .*', '', imd_quintile)),
         smoker = ifelse(smoking_status == 'smoker', 1,0),
         newly_diagnosed_1y = ifelse(time_since_diagnosis == 'less than 1 year', 1, 0),
         HbA1C_control_bad = ifelse(HbA1C_control == 'bad', 1, 0))

model_data_adm_T2D <- model_data_adm %>% 
  mutate(age_centered = startage_study - round(mean(startage_study)),
         age_5ybin_centered = age_centered / 5)


# Cox models for T2D
T2D_adm_coxmodel_1 <- coxph(Surv(time = time_to_event, event = status) ~ female + age_5ybin_centered + imd_quintile_numeric + DEPANXr 
                            + HbA1C_control_bad + physical_mm_count , data = model_data_adm_T2D)
summary(T2D_adm_coxmodel_1)
extractAIC(T2D_adm_coxmodel_1)

T2D_adm_coxmodel_2 <- coxph(Surv(time = time_to_event, event = status) ~ female + age_5ybin_centered + imd_quintile_numeric + DEPANXr 
                            + HbA1C_control + physical_mm_count + smoker, data = model_data_adm_T2D) 
summary(T2D_adm_coxmodel_2)
extractAIC(T2D_adm_coxmodel_2)

T2D_adm_coxmodel_2_summary <- summariseCoxModel(model = T2D_adm_coxmodel_2)
write_csv(T2D_adm_coxmodel_2_summary, str_c(summary_stats_path, 'outcomes_modelling/Coefficients_T2D_EmergencyAdmission_DEPANXr.csv'))


T2D_adm_coxmodel_4<- coxph(Surv(time = time_to_event, event = status) ~ female + age_5ybin_centered + imd_quintile_numeric + DEPANXr_PNC 
                           + HbA1C_control + physical_mm_count + smoker, data = model_data_adm_T2D) # best

summary(T2D_adm_coxmodel_4)
extractAIC(T2D_adm_coxmodel_4)
broom::tidy(T2D_adm_coxmodel_4, exponentiate = TRUE)
T2D_adm_coxmodel_4_summary <- summariseCoxModel(model = T2D_adm_coxmodel_4)
write_csv(T2D_adm_coxmodel_4_summary, str_c(summary_stats_path, 'outcomes_modelling/Coefficients_T2D_EmergencyAdmission_DEPANXr_PNC.csv'))

