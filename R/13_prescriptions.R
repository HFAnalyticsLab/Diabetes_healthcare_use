# =======================================================
# Project: Diabetes outpatient care
# Purpose: Primary care prescriptions - cleaning, processing and counting
# Author: Fiona Grimm
# =======================================================

library(tidyverse)
library(lubridate)
library(janitor)
library(tidylog)

# Source file paths: Rds_path
source('R/file_paths.R')

# Source study parameters 
source('R/study_params.R')

# Import data -------------------------------------------------------------

patients <- readRDS(str_c(processed_RDS_path, 'patients_clinical_combined.Rds'))

# Diabetes medication code list
medication_codes <- read_csv(str_c(code_list_path, 'Appendix5_antidiabetics.csv'))

# Therapy 
extract_therapy <- readRDS(str_c(raw_RDS_path, 'Extract_therapy.Rds'))


# Clean and process -------------------------------------------------------

prescripts <- extract_therapy %>% 
  filter(eventdate %within% interval(study_start, study_end)) %>% 
  mutate(diabetes_pres = ifelse(prodcode %in% medication_codes$product_code, 1, 0))


# Check: how many prescriptions are for patients in the step 2 and step 3 populations?
prescripts %>% 
  left_join(patients[, c('patid', 'cohort_step2')], by = 'patid') %>% 
  filter(cohort_step2 == 1) %>% 
  nrow()

prescripts %>% 
  left_join(patients[, c('patid', 'cohort_step3')], by = 'patid') %>% 
  filter(cohort_step3 == 1) %>% 
  nrow()

# Counting prescriptions per patient ---------------------------------------

prescripts_bypat <-  prescripts %>% 
  group_by(patid) %>% 
  summarise(n_prescripts = n(),
            n_prescripts_diabetes = sum(diabetes_pres == 1))

# Join in patients 
prescripts_bypat <-  prescripts_bypat %>% 
  right_join(patients[, c('patid')], by = 'patid') 

# Normalising by time spent in study --------------------------------------

prescripts_bypat_norm <- prescripts_bypat %>% 
  right_join(patients[, c('patid', 'tod', 'ONS_dod', 'cohort_step2', 'cohort_step3', 'diabetes_type', 'years_in_study_cprd')], by = 'patid') %>% 
  filter(cohort_step2 == 1 & diabetes_type %in% c('type1', 'type2')) %>% 
  select(-cohort_step2) %>% 
  mutate_if(is.numeric, ~replace_na(.x, 0))

prescripts_bypat_norm <- prescripts_bypat_norm %>% 
  mutate(n_prescripts_per_year = round(n_prescripts / years_in_study_cprd, 1),
         n_prescripts_diabetes_per_year = round(n_prescripts_diabetes / years_in_study_cprd, 1))

# Saving processed files --------------------------------------------------

saveRDS(prescripts_bypat_norm, str_c(processed_RDS_path, 'patients_prescriptions.rds'))


# Create summary tables -------------------------------------------------
# Means

all_prescr_means <- prescripts_bypat_norm %>% 
  group_by(diabetes_type) %>% 
  summarise(n = n(),
            mean_prescr_count = round(mean(n_prescripts), 1),
            mean_prescr_count_per_year = round(mean(n_prescripts_per_year), 1),
            mean_prescr_diabetes_count = round(mean(n_prescripts_diabetes), 1),
            mean_prescr_diabetes_count_per_year = round(mean(n_prescripts_diabetes_per_year), 1)) %>% 
  mutate(subgroup = 'all patients')

not_censored_prescr_means <- prescripts_bypat_norm %>% 
  group_by(diabetes_type) %>% 
  filter(years_in_study_cprd == 2) %>% 
  summarise(n = n(),
            mean_prescr_count = round(mean(n_prescripts), 1),
            mean_prescr_count_per_year = round(mean(n_prescripts_per_year), 1),
            mean_prescr_diabetes_count = round(mean(n_prescripts_diabetes), 1),
            mean_prescr_diabetes_count_per_year = round(mean(n_prescripts_diabetes_per_year), 1)) %>% 
  mutate(subgroup = 'did not transfer out')

prescr_means <- all_prescr_means %>% 
  bind_rows(not_censored_prescr_means)

write_csv(prescr_means, str_c(summary_stats_path, 'table2/Prescriptions_means.csv'))

