# =======================================================
# Project: Diabetes outpatient care
# Purpose: Determine diabetes medication type during study period 
# Author: Fiona Grimm
# Date: 30/08/2019
# =======================================================

library(tidyverse)
library(lubridate)
library(tidylog)

# Source file paths: Rds_path
source('R_FG/file_paths.R')

# Define study parameters -------------------------------------------------

# Year 1 and 2 to quantify utilisation and other covariates
study_start <- ymd('2015-12-01')
study_end <- ymd('2017-11-30')

# Import data -----------------------------------------

# Patiend IDs
patients <- readRDS('processed_data/patients.Rds')

# Diabetes medication code list
medication_codes <- read_csv(str_c(code_list_path, 'Appendix5_antidiabetics.csv'))

# Therapy 
extract_therapy <- readRDS('raw_data/Extract_therapy.Rds')


# Extract prescriptions for diabetes medication ---------------------------
# and summarise number of prescriptions by patient and drug type

therapy_diabetes <- extract_therapy %>% 
  select(patid, eventdate, prodcode) %>%
  filter(eventdate %within% interval(study_start, study_end) & prodcode %in% medication_codes$product_code) %>% 
  left_join(medication_codes[,c('product_code', 'drug_type')], by = c("prodcode" = "product_code")) 

# NB will exclude acarbose as it is technically not an oral hypoglycaemic (OHA)
prescriptions_bypat <- therapy_diabetes %>% 
  filter(drug_type != 'Acarbose') %>% 
  group_by(patid, drug_type) %>% 
  summarise(count = n()) %>% 
  mutate(insulin = ifelse(drug_type =='Insulin', 1, 0),
         insulin_NIGLD = ifelse(drug_type == 'Insulin/glp-1', 1, 0),
         NIGLD = ifelse(drug_type != 'Insulin' & drug_type != 'Insulin/glp-1', 1, 0),
         NIGLD_combo = ifelse(drug_type %in% c('Metformin/SGLT2-i', 'Metformin/DPP-4i', 'Metformin/Glitazone'), 1, 0))

# categorise into insulin only, non-insulin glucose lowering drug (NIGLD, includes OHA and GLP-1 injectables) only, both insulin and NIGLD

therapy_bypat <-  prescriptions_bypat %>% 
  group_by(patid) %>% 
  summarise(insulin_only = ifelse(sum(insulin) >= 1 & sum(NIGLD) == 0 & sum(insulin_NIGLD) == 0, 1, 0),
            NIGLD_only = ifelse(sum(NIGLD) >= 1 & sum(insulin) == 0 & sum(insulin_NIGLD) == 0, 1, 0),
            insulin_NIGLD = ifelse(sum(insulin_NIGLD) >= 1 | (sum(NIGLD) >= 1 & sum(insulin) >=1), 1, 0)) 

# Join with study population to note where patients had not recorded medication
therapy_bypat <- therapy_bypat %>% 
  right_join(patients[,'patid'], by = 'patid') %>% 
  mutate(medication = case_when(insulin_only == 1 ~ 'Insulin only',
                                NIGLD_only == 1 ~ 'NIGLD only',
                                insulin_NIGLD == 1 ~ 'Both',
                                is.na(insulin_only) & is.na(NIGLD_only) & is.na(insulin_NIGLD) ~ 'None recorded'))

# Saving processed files --------------------------------------------------

saveRDS(therapy_bypat, 'processed_data/patients_medication.rds')
