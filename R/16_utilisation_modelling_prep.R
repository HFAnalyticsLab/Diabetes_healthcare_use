# =======================================================
# Project: Diabetes outpatient care
# Purpose: Prep work for statistical modellling of healthcare utilisation
# Author: Fiona Grimm
# =======================================================

library(glmmTMB)
library(bbmle) # for AICtab
library(MASS)  # masks dplyr::select -> load tidyverse last

library(tidyverse)
library(lubridate)
library(janitor)
library(tidylog)

# Source file paths
source('R/file_paths.R')

# Source study parameters 
source('R/study_params.R')

# Functions ---------------------------------------------------------------

# fits a negative binomial distribution to the data and produces a diagnosic plot
# showing the fit and saves it to file
plotNBDistributionFit <- function(data, col, max_x, path){
  
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
      stat_qq(distribution = qnbinom, dparams = nb_fit$estimate, color = '#524c48') +
      stat_qq_line(distribution = qnbinom, dparams = nb_fit$estimate, color = 'red') +
    labs(title = 'Q-Q Plot ') +
    theme_bw()
    
  combined_plot <- gridExtra::grid.arrange(plot, qqplot, nrow = 1)
  
  ggsave(path, combined_plot, width = 7, height = 4)
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

# Derive and/or simplify variables
patients_combined <- patients_combined %>% 
  mutate(smoker = ifelse(smoking_status == 'smoker', 1,0),
         newly_diagnosed_1y = ifelse(time_since_diagnosis == 'less than 1 year', 1, 0),
         mm_count_excl_DEPANXr = rowSums(.[comorbidities[comorbidities != 'DEPANXr']]),
         mm_count_excl_PNC = rowSums(.[comorbidities[comorbidities != 'PNC']]))


# Split diabetes types and centre age on the mean age
model_data_T1D <- patients_combined %>% 
  filter(diabetes_type == 'type1') %>% 
  mutate(age_centered = startage_study - round(mean(startage_study)),
         age_5ybin_centered = age_centered / 5)

model_data_T2D <- patients_combined %>% 
  filter(diabetes_type == 'type2') %>% 
  mutate(age_centered = startage_study - round(mean(startage_study)),
         age_5ybin_centered = age_centered / 5)

# Save
saveRDS(model_data_T1D, str_c(processed_RDS_path, 'Utilisation_modelling_data_T1D.Rds'))
saveRDS(model_data_T2D, str_c(processed_RDS_path, 'Utilisation_modelling_data_T2D.Rds'))

# Testing the fit of negative binomial distributions ----------------------------------------


utilisation_types <- c('consult_primarycare', 'prescriptions', 'OP_attended_total', 'emergency_admissions', 'AE_attendances')


# For T1DM 
file_paths_T1D <- str_c(summary_stats_path, 'utilisation_modelling/diagnostics/T1D_', utilisation_types, '_nbfit.png')

walk2(.x = utilisation_types, 
      .y = file_paths_T1D, 
      .f = ~plotNBDistributionFit(data = model_data_T1D, col = .x, max_x = 50, path = .y))


# For T2DM 
file_paths_T2D <- str_c(summary_stats_path, 'utilisation_modelling/diagnostics/T2D_', utilisation_types, '_nbfit.png')

walk2(.x = utilisation_types, 
      .y = file_paths_T2D, 
      .f = ~plotNBDistributionFit(data = model_data_T2D, col = .x, max_x = 50, path = .y))




# Deciding which modelling package to use  -------------------------------------

# Question: whether to use glmmTmb or MASS package?
# will use T1D cohort, primary care appointments as test case

# glmmTMB has to kinds of negative binomial functions
# Comparing the two (type 1 and type 2) 

# Base model 
model_T1D_PC_glmmTMB0 <- glmmTMB(consult_primarycare ~ female + age_5ybin_centered + offset(log(years_in_study_cprd)), 
                          data = model_data_T1D, 
                          family = nbinom2)

model_T1D_PC_glmmTMB1 <- glmmTMB(consult_primarycare ~ female + age_5ybin_centered + offset(log(years_in_study_cprd)), 
                           data = model_data_T1D, 
                           family = nbinom1)

summary(model_T1D_PC_glmmTMB0)
summary(model_T1D_PC_glmmTMB1)

AICtab(model_T1D_PC_glmmTMB0, model_T1D_PC_glmmTMB1) 
## based on AIC nbinom2 fits better (ie model_T1D_PC_glmmTMB0)

# comparison to MASS::glm.nb, which only has one kind of negative binomial function
# best model so far
model_T1D_PC <- MASS::glm.nb(consult_primarycare ~ female + age_5ybin_centered + offset(log(years_in_study_cprd)), 
                             data = model_data_T1D)

summary(model_T1D_PC)
broom::tidy(model_T1D_PC, exponentiate = TRUE)

# MASS::glm.nb gives the same coefficients as glmmTMB with nbinom2 family 
# will keep working with the simpler package -->  MASS
