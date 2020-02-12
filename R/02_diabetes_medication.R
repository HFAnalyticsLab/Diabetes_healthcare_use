# =======================================================
# Project: Diabetes outpatient care
# Purpose: Determine diabetes medication type during study period 
# Author: Fiona Grimm
# =======================================================

library(tidyverse)
library(lubridate)
library(tidylog)

# Source file paths: Rds_path
source('R/file_paths.R')

# Source study parameters 
source('R/study_params.R')

# Import data -----------------------------------------

# Patiend IDs
patients <- readRDS(str_c(processed_RDS_path, 'patients.Rds'))

# Diabetes medication code list
medication_codes <- read_csv(str_c(code_list_path, 'Appendix5_antidiabetics.csv'))

# Therapy 
extract_therapy <- readRDS(str_c(raw_RDS_path, 'Extract_therapy.Rds'))


# Extract prescriptions for diabetes medication ---------------------------
# and summarise number of prescriptions by patient and drug type

therapy_diabetes <- extract_therapy %>% 
  select(patid, eventdate, prodcode) %>%
  filter(prodcode %in% medication_codes$product_code) %>% 
  left_join(medication_codes[,c('product_code', 'drug_type')], by = c("prodcode" = "product_code")) 

therapy_diabetes_prior <- therapy_diabetes %>% 
  filter(eventdate < study_start)

# Check: how many prescriptions are for patients step 2 cohort?
therapy_diabetes_prior %>% 
  left_join(patients[, c('patid', 'cohort_step2')], by = 'patid') %>% 
  filter(cohort_step2 == 1) %>% 
  nrow()

# Check: how many prescriptions are for patients step 3 cohort?
therapy_diabetes_prior %>% 
  left_join(patients[, c('patid', 'cohort_step3')], by = 'patid') %>% 
  filter(cohort_step3 == 1) %>% 
  nrow()


therapy_diabetes_study <- therapy_diabetes %>% 
  filter(eventdate %within% interval(study_start, study_end))

# Check: how many prescriptions are for patients step 2 cohort?
therapy_diabetes_study %>% 
  left_join(patients[, c('patid', 'cohort_step2')], by = 'patid') %>% 
  filter(cohort_step2 == 1) %>% 
  nrow()

# Check: how many prescriptions are for patients step 3 cohort?
therapy_diabetes_study %>% 
  left_join(patients[, c('patid', 'cohort_step3')], by = 'patid') %>% 
  filter(cohort_step3 == 1) %>% 
  nrow()


# Medication during study period ------------------------------------------

# NB will exclude acarbose as it is technically not an oral hypoglycaemic (OHA)
prescriptions_bypat_study <- therapy_diabetes_study %>% 
  filter(drug_type != 'Acarbose') %>% 
  group_by(patid, drug_type) %>% 
  summarise(count = n()) %>% 
  mutate(insulin = ifelse(drug_type =='Insulin', 1, 0),
         insulin_NIGLD = ifelse(drug_type == 'Insulin/glp-1', 1, 0),
         NIGLD = ifelse(drug_type != 'Insulin' & drug_type != 'Insulin/glp-1', 1, 0),
         NIGLD_combo = ifelse(drug_type %in% c('Metformin/SGLT2-i', 'Metformin/DPP-4i', 'Metformin/Glitazone'), 1, 0))

# categorise into insulin only, non-insulin glucose lowering drug (NIGLD, includes OHA and GLP-1 injectables) only, both insulin and NIGLD

therapy_bypat_study <- prescriptions_bypat_study %>% 
  group_by(patid) %>% 
  summarise(insulin_only = ifelse(sum(insulin) >= 1 & sum(NIGLD) == 0 & sum(insulin_NIGLD) == 0, 1, 0),
            NIGLD_only = ifelse(sum(NIGLD) >= 1 & sum(insulin) == 0 & sum(insulin_NIGLD) == 0, 1, 0),
            insulin_NIGLD = ifelse(sum(insulin_NIGLD) >= 1 | (sum(NIGLD) >= 1 & sum(insulin) >=1), 1, 0)) 

# Join with study population to note where patients had not recorded medication
therapy_bypat_study <- therapy_bypat_study %>% 
  right_join(patients[,'patid'], by = 'patid') %>% 
  mutate(medication = case_when(insulin_only == 1 ~ 'Insulin only',
                                NIGLD_only == 1 ~ 'NIGLD only',
                                insulin_NIGLD == 1 ~ 'Both',
                                is.na(insulin_only) & is.na(NIGLD_only) & is.na(insulin_NIGLD) ~ 'None recorded'))

# Saving processed files
saveRDS(therapy_bypat_study, str_c(processed_RDS_path, 'patients_medication.rds'))

# Medication prior to study period ------------------------------------------

# NB will exclude acarbose as it is technically not an oral hypoglycaemic (OHA)
prescriptions_bypat_prior <- therapy_diabetes_prior %>% 
  filter(drug_type != 'Acarbose') %>% 
  group_by(patid, drug_type) %>% 
  summarise(count = n()) %>% 
  mutate(insulin = ifelse(drug_type =='Insulin', 1, 0),
         insulin_NIGLD = ifelse(drug_type == 'Insulin/glp-1', 1, 0),
         NIGLD = ifelse(drug_type != 'Insulin' & drug_type != 'Insulin/glp-1', 1, 0),
         NIGLD_combo = ifelse(drug_type %in% c('Metformin/SGLT2-i', 'Metformin/DPP-4i', 'Metformin/Glitazone'), 1, 0))

# categorise into insulin only, non-insulin glucose lowering drug (NIGLD, includes OHA and GLP-1 injectables) only, both insulin and NIGLD

therapy_bypat_prior <-  prescriptions_bypat_prior %>% 
  group_by(patid) %>% 
  summarise(insulin_only = ifelse(sum(insulin) >= 1 & sum(NIGLD) == 0 & sum(insulin_NIGLD) == 0, 1, 0),
            NIGLD_only = ifelse(sum(NIGLD) >= 1 & sum(insulin) == 0 & sum(insulin_NIGLD) == 0, 1, 0),
            insulin_NIGLD = ifelse(sum(insulin_NIGLD) >= 1 | (sum(NIGLD) >= 1 & sum(insulin) >=1), 1, 0)) 

# Join with study population to note where patients had not recorded medication
therapy_bypat_prior <- therapy_bypat_prior %>% 
  right_join(patients[,'patid'], by = 'patid') %>% 
  mutate(medication = case_when(insulin_only == 1 ~ 'Insulin only',
                                NIGLD_only == 1 ~ 'NIGLD only',
                                insulin_NIGLD == 1 ~ 'Both',
                                is.na(insulin_only) & is.na(NIGLD_only) & is.na(insulin_NIGLD) ~ 'None recorded'))

# Saving processed files
saveRDS(therapy_bypat_prior, str_c(processed_RDS_path, 'patients_medication_prior.rds'))
