# =======================================================
# Project: Diabetes outpatient care
# Purpose: Explanatory statistical models for healthcare utilisation (by type)
# Author: Fiona Grimm
# =======================================================

library(glmmTMB)
library(bbmle) # for AICtab
library(MASS)  # masks dplyr::select -> load tidyverse last
library(DHARMa)

library(tidyverse)
library(lubridate)
library(janitor)
library(tidylog)

# Source file paths: Rds_path
source('R/file_paths.R')

# Source study parameters 
source('R/study_params.R')

# Source graph parameters 
source('R/plotting_params.R')

# Functions ---------------------------------------------------------------

# ggplot theme
utilisation_theme <- theme_classic() +
  theme(legend.position = 'top',
        legend.text = element_text(color = THF_dark_grey),
        legend.title = element_text(color = THF_dark_grey),
        legend.justification = c(1,0),
        legend.key.size = unit(3, 'mm'),
        legend.spacing.x = unit(1 ,'mm'),
        axis.line = element_line(size = 0.25),
        axis.ticks = element_line(size = 0.25)) +
  THF_theme

# fits a negative binomial distribution to the data and produces a diagnosic plot
# showing the fit
plotNBDistributionFit <- function(data, col, max_x){
  
  nb_fit <- MASS::fitdistr(data[[col]], "negative binomial", lower = c(0,0))
  
  col_sym <- sym(col)
  
  plot <- ggplot() +
    geom_histogram(data = data, aes(x = !!col_sym, y = ..density..), binwidth = 1) +
    geom_line(aes(x = c(0:max_x), 
                  y = dnbinom(x = c(0:max_x), 
                              size = nb_fit$estimate["size"], 
                              mu = nb_fit$estimate["mu"]),
                  colour="red"), 
              size = 0.5) +
    scale_color_discrete(labels = c('fitted negative\nbinomial distribution')) +
    coord_cartesian(xlim = c(0, max_x)) +
    labs(title = 'Density plot') +
    theme_bw() +
    theme(legend.position = c(0.75,0.95),
          legend.title = element_blank(),
          legend.background = element_blank())
    
  qqplot <- ggplot(data = data, aes(sample = !!col_sym)) +
      stat_qq(distribution = qnbinom, dparams = nb_fit$estimate, color = THF_dark_grey) +
      stat_qq_line(distribution = qnbinom, dparams = nb_fit$estimate, color = 'red') +
    labs(title = 'Q-Q Plot ') +
    theme_bw()
    
  combined_plot <- gridExtra::grid.arrange(plot, qqplot, nrow = 1)
  return(combined_plot)
}

# a function to summarise the coefficients of regression models,
# including confidence intervals
summariseModel <- function(model){
  
  table <- broom::tidy(model, exponentiate = TRUE) %>% 
    cbind(exp(confint(model))) 
  
  rownames(table) <- NULL
  colnames(table) <- c('Coefficient', 'IRR', 'Std.Error', 'z.value', 'p.value', 'CI_lower', 'CI_upper')
  
  table <- table %>% 
    select(Coefficient, IRR, CI_lower, CI_upper, everything()) %>% 
    mutate_at(vars(IRR, z.value, CI_lower, CI_upper), round, 3)
  
  return(table)
}


# Import data -------------------------------------------------------------

patients <- readRDS(str_c(processed_RDS_path, 'patients_clinical_combined.Rds'))

# GP appointments
consultations_bypat <- readRDS(str_c(processed_RDS_path, 'patients_consultations.rds'))

consultations_bypat <-  consultations_bypat %>% 
  filter(appt_length == 'all') %>% 
  select(patid, consult_type, count) %>% 
  spread(key = consult_type, value = count) %>% 
  rename('consult_primarycare' = consult, "consult_GP" = GP, "consult_nurse" = nurse)

# Outpatient appointments
hesop_count_byPat <- readRDS(str_c(processed_RDS_path, 'patients_OPappointments.rds'))

hesop_count_byPat <- hesop_count_byPat %>% 
  select(patid, type, count) %>% 
  spread(key = type, value = count) %>% 
  select(patid, OP_attended_total) 

# Hospital admissions
admissions_bypat <- readRDS(str_c(processed_RDS_path, 'patients_admissions.rds'))

admissions_bypat <-  admissions_bypat %>% 
  select(patid, emergency_admissions, elective_admissions) 

# Prescriptions
prescripts_bypat <- readRDS(str_c(processed_RDS_path, 'patients_prescriptions.rds'))

prescripts_bypat <-  prescripts_bypat %>% 
  select(patid, n_prescripts) %>% 
  rename('prescriptions' = n_prescripts)

# A&E attendances
hesae_attendances_byPat <- readRDS(str_c(processed_RDS_path, 'patients_AEattendances.rds'))

hesae_attendances_byPat <-  hesae_attendances_byPat %>% 
  select(patid, AE_unplanned) %>% 
  rename('AE_attendances' = AE_unplanned)

# Join them all together
patients_combined <- patients %>% 
  filter(cohort_step2 == 1) %>% 
  select(patid, gender, pracid, 
                years_in_study_cprd, years_in_study_hes, 
                startage_study, age_bins_study, age_bins_study_SDC, 
                imd_quintile, diabetes_type, medication, smoking_status, BMI_categorical, HbA1C_control,
                time_since_diagnosis, 
                mm_cat, mm_count, physical_mm_count, mental_mm_cat_SDC, DEPANX) %>% 
  left_join(consultations_bypat, by = 'patid') %>% 
  left_join(hesop_count_byPat, by = 'patid') %>% 
  left_join(admissions_bypat, by = 'patid') %>% 
  left_join(prescripts_bypat, by = 'patid') %>% 
  left_join(hesae_attendances_byPat, by = 'patid')

patients_combined <- patients_combined %>% 
  mutate(female = ifelse(gender == '1', 0, 1),
         female = factor(female),
         imd_quintile_numeric = as.numeric(gsub(' .*', '', imd_quintile)))


# Prepare modeling data --------------------------------------------------------------

patients_combined <- patients_combined %>% 
  mutate(smoker = ifelse(smoking_status == 'smoker', 1,0),
         newly_diagnosed_1y = ifelse(time_since_diagnosis == 'less than 1 year', 1, 0),
         HbA1C_control_bad = ifelse(HbA1C_control == 'bad', 1, 0))


# Split diabetes types and centre age on the mean age
model_data_T1D <- patients_combined %>% 
  filter(diabetes_type == 'type1') %>% 
  mutate(age_centered = startage_study - round(mean(startage_study)),
         age_5ybin_centered = age_centered / 5)

model_data_T2D <- patients_combined %>% 
  filter(diabetes_type == 'type2') %>% 
  mutate(age_centered = startage_study - round(mean(startage_study)),
         age_5ybin_centered = age_centered / 5)

# Testing the fit of distributions ----------------------------------------

# Primary care consultations
# Check the proportion of 0s
nrow(model_data_T1D[model_data_T1D$consult_primarycare==0,])/nrow(model_data_T1D)

TD1_primarycare_fit <- plotNBDistributionFit(data = model_data_T1D, 
                                             col = 'consult_primarycare', 
                                             max_x = 50)

ggsave(str_c(summary_stats_path, 'utilisation_modelling/diagnostics/T1D_primarycare_nbfit.png'), 
       TD1_primarycare_fit, width = 7, height = 4)

nrow(model_data_T2D[model_data_T2D$consult_primarycare==0,])/nrow(model_data_T2D)

TD2_primarycare_fit <- plotNBDistributionFit(data = model_data_T2D, 
                                             col = 'consult_primarycare', 
                                             max_x = 50)

ggsave(str_c(summary_stats_path, 'utilisation_modelling/diagnostics/T2D_primarycare_nbfit.png'), 
       TD2_primarycare_fit, width = 7, height = 4)

# Outpatient attendances
nrow(model_data_T1D[model_data_T1D$OP_attended_total==0,])/nrow(model_data_T1D)

T1D_OP_fit <- plotNBDistributionFit(data = model_data_T1D, 
                                             col = 'OP_attended_total', 
                                             max_x = 50)

ggsave(str_c(summary_stats_path, 'utilisation_modelling/diagnostics/T1D_OP_nbfit.png'), 
       T1D_OP_fit, width = 7, height = 4)


nrow(model_data_T2D[model_data_T2D$OP_attended_total==0,])/nrow(model_data_T2D)

T2D_OP_fit <- plotNBDistributionFit(data = model_data_T2D, 
                                    col = 'OP_attended_total', 
                                    max_x = 50)

ggsave(str_c(summary_stats_path, 'utilisation_modelling/diagnostics/T2D_OP_nbfit.png'), 
       T2D_OP_fit, width = 7, height = 4)

## Prescriptions: look at overall distribution 
# negative binomial does not seem appropriate
# will need to model differently 
nrow(model_data_T1D[model_data_T1D$prescriptions==0,])/nrow(model_data_T1D)

ggplot(model_data_T1D) +
  geom_histogram(aes(x = prescriptions), binwidth = 1) +
  coord_cartesian(xlim = c(0, 100))


ggplot(model_data_T2D) +
  geom_histogram(aes(x = prescriptions), binwidth = 1) +
  coord_cartesian(xlim = c(0, 100))

# Emergency admissions
ggplot(model_data_T1D) +
  geom_histogram(aes(x = emergency_admissions), binwidth = 1) +
  coord_cartesian(xlim = c(0, 10))

T1D_emadm_fit <- plotNBDistributionFit(data = model_data_T1D, 
                                    col = 'emergency_admissions', 
                                    max_x = 15)

ggsave(str_c(summary_stats_path, 'utilisation_modelling/diagnostics/T1D_emergency_admissions_nbfit.png'), 
       T1D_emadm_fit, width = 7, height = 4)


T2D_emadm_fit <- plotNBDistributionFit(data = model_data_T2D, 
                                       col = 'emergency_admissions', 
                                       max_x = 15)

ggsave(str_c(summary_stats_path, 'utilisation_modelling/diagnostics/T2D_emergency_admissions_nbfit.png'), 
       T2D_emadm_fit, width = 7, height = 4)

# A&E attendances
ggplot(model_data_T1D) +
  geom_histogram(aes(x = AE_attendances), binwidth = 1) +
  coord_cartesian(xlim = c(0, 10))

T1D_AE_attendances_fit <- plotNBDistributionFit(data = model_data_T1D, 
                                       col = 'AE_attendances', 
                                       max_x = 15)

ggsave(str_c(summary_stats_path, 'utilisation_modelling/diagnostics/T1D_AE_attendances_nbfit.png'), 
       T1D_AE_attendances_fit, width = 7, height = 4)


T2D_AE_attendances_fit <- plotNBDistributionFit(data = model_data_T2D, 
                                       col = 'AE_attendances', 
                                       max_x = 15)

ggsave(str_c(summary_stats_path, 'utilisation_modelling/diagnostics/T2D_AE_attendances_nbfit.png'), 
       T2D_AE_attendances_fit, width = 7, height = 4)



# Deciding which package to use  -------------------------------------
# Q: whether to use glmmTmb or MASS package?
# will use T1D cohort, primary care appointments as test case
# glmmTMB has to kinds of negative binomial functions
# Comparing the two (type 1 and type 2) to null model (age only)
model_T1D_PC_glmmTMB0 <- glmmTMB(consult_primarycare ~ female + age_5ybin_centered + offset(log(years_in_study_cprd)), 
                          data = model_data_T1D, 
                          family = nbinom2)

model_T1D_PC_glmmTMB1 <- glmmTMB(consult_primarycare ~ female + age_5ybin_centered + imd_quintile_numeric + HbA1C_control_bad + physical_mm_count
                           + mental_mm_cat_SDC + newly_diagnosed_1y + offset(log(years_in_study_cprd)), 
                           data = model_data_T1D, 
                           family = nbinom1)

model_T1D_PC_glmmTMB2 <- glmmTMB(consult_primarycare ~ female + age_5ybin_centered + imd_quintile_numeric + HbA1C_control_bad + physical_mm_count
                          + mental_mm_cat_SDC + newly_diagnosed_1y + offset(log(years_in_study_cprd)), 
                          data = model_data_T1D, 
                          family = nbinom2)

summary(model_T1D_PC_glmmTMB0)
summary(model_T1D_PC_glmmTMB1)
summary(model_T1D_PC_glmmTMB2)

AICtab(model_T1D_PC_glmmTMB0, model_T1D_PC_glmmTMB1, model_T1D_PC_glmmTMB2) 
## based on AIC nbinom2 fits better (ie model_T1D_PC_glmmTMB2)

plot(simulateResiduals(model_T1D_PC_glmmTMB2))

# comparison to MASS::glm.nb, which only has one kind of negative binomial function
# best model so far
model_T1D_PC <- MASS::glm.nb(consult_primarycare ~ female + age_5ybin_centered + imd_quintile_numeric 
                             + HbA1C_control 
                             + physical_mm_count + smoker
                             + mental_mm_cat_SDC + newly_diagnosed_1y + offset(log(years_in_study_cprd)), 
                             data = model_data_T1D)
summary(model_T1D_PC)
broom::tidy(model_T1D_PC, exponentiate = TRUE)

# MASS::glm.nb gives the same coefficients as glmmTMB with nbinom2 family (model_T1D_PC_glmmTMB2)
# will keep working with the simpler package --> model_T1D_PC
