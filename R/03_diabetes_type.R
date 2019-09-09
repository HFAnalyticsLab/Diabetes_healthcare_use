# =======================================================
# Project: Diabetes outpatient care
# Purpose: Determine diabetes type using Read codes prior to study period 
# Author: Fiona Grimm
# Date: 03/09/2019
# =======================================================

library(tidyverse)
library(lubridate)
library(janitor)
library(tidylog)

# Source file paths: Rds_path
source('R_FG/file_paths.R')

# Define study parameters -------------------------------------------------

# Year 1 and 2 to quantify utilisation and other covariates
study_start <- ymd('2015-12-01')

# Import data -----------------------------------------

# Study population
patients <- readRDS('processed_data/patients.rds')

# Diabetes code list
diabetes_codes <- read_csv(str_c(code_list_path, 'Appendix1_diabetes_diagnosis.csv'))

# Clinical data
extract_clinical <- readRDS('raw_data/Extract_clinical.Rds')

# Diabetes medication code list
medication_codes <- read_csv(str_c(code_list_path, 'Appendix5_antidiabetics.csv'))

# Therapy 
extract_therapy <- readRDS('raw_data/Extract_therapy.Rds')


# Extract diabetes Read codes ---------------------------------------------
# and summarise by patient and diabetes type 

clinical_diabetes <- extract_clinical %>% 
  select(patid, eventdate, medcode) %>%
  filter(eventdate < study_start & medcode %in% diabetes_codes$medcode) %>% 
  left_join(diabetes_codes[,c('medcode' , 'diabetes_type')], by = "medcode")

# Count diagnosis codes by patient and type
diabetes_count_bypat <- clinical_diabetes %>%  
  group_by(patid, diabetes_type) %>% 
  summarise(count = n()) %>% 
  spread(key = 'diabetes_type', value = 'count') %>% 
  rename('other_count' = other, 'type1_count' = `Type 1`, 'type2_count' = `Type 2`, 'unspecified_count' = unspecified)

# List unique diagnosis codes by patient 
diabetes_list_bypat <- clinical_diabetes %>%  
  group_by(patid, diabetes_type) %>% 
  summarise(codes = paste(unique(medcode), collapse = ',')) %>% 
  spread(key = 'diabetes_type', value = 'codes') %>% 
  rename('other_codes' = other, 'type1_codes' = `Type 1`, 'type2_codes' = `Type 2`, 'unspecified_codes' = unspecified)

# Join Read code counts and list
diabetes_bypat <- diabetes_count_bypat %>% 
  left_join(diabetes_list_bypat, by = 'patid')

# Assigning a diabetes type (step 1: unambiguous diagnoses) --------------
# Also create flag for patients who have mixed Read codes (not exclusively type 1, type 2 or other)

diabetes_bypat <- diabetes_bypat %>% 
  mutate(diabetes_type = case_when(is.na(other_count) & !is.na(type1_count) & is.na(type2_codes) ~ 'type1',
                                   is.na(other_count) & is.na(type1_count) & !is.na(type2_codes) ~ 'type2',
                                   !is.na(other_count) & is.na(type1_count) & is.na(type2_codes) ~ 'other'),
         mixed_type_flag = ifelse(is.na(diabetes_type), 1, 0))


# Step 2: Using prescription information from before the study start ----
# Assign type 2 if patients have been on more than one type of OHA in the past
# (not necessarily at the same time)

therapy_diabetes <- extract_therapy %>% 
  select(patid, eventdate, prodcode) %>%
  filter(eventdate < study_start &  prodcode %in% medication_codes$product_code) %>% 
  left_join(medication_codes[,c('product_code', 'drug_type')], by = c("prodcode" = "product_code")) 

# NB will exclude acarbose, as it is technically not an OHA
prescriptions_bypat <- therapy_diabetes %>% 
  filter(drug_type != 'Acarbose' & drug_type != 'Insulin') %>% 
  group_by(patid, drug_type) %>% 
  summarise(count = n()) 

# Combination OHAs need to counted in both individual drug type categories
therapy_bypat <-  prescriptions_bypat %>% 
  mutate(count = ifelse(count >= 1, 1L, 0)) %>%  
  spread(key = 'drug_type', value = 'count') %>% 
  mutate(`GLP-1` = ifelse(!is.na(`Insulin/glp-1`) & `Insulin/glp-1` == 1, 1L, `GLP-1`),
         Metformin = ifelse(!is.na(`Metformin/Glitazone`) & `Metformin/Glitazone` == 1, 1L, Metformin),
         Metformin = ifelse(!is.na(`Metformin/SGLT2-i`) & `Metformin/SGLT2-i` == 1, 1L, Metformin),
         Metformin = ifelse(!is.na(`Metformin/DPP-4i`) & `Metformin/DPP-4i` == 1, 1L, Metformin),
         Glitazone = ifelse(!is.na(`Metformin/Glitazone`) & `Metformin/Glitazone` == 1, 1L, Glitazone),
         `SGLT2-i` = ifelse(!is.na(`Metformin/SGLT2-i`) & `Metformin/SGLT2-i` == 1, 1L, `SGLT2-i`),
         `DPP-4i` = ifelse(!is.na(`Metformin/DPP-4i`) & `Metformin/DPP-4i` == 1, 1L, `DPP-4i`)) %>% 
  select(-`Insulin/glp-1`, -`Metformin/Glitazone`, -`Metformin/SGLT2-i`, -`Metformin/DPP-4i`)

therapy_bypat <- therapy_bypat %>% 
  mutate(OHA_multiple = ifelse(sum(`DPP-4i`, Glitazone, `GLP-1`, Meglitinide, Metformin, `SGLT2-i`, Sulfonylurea, na.rm = TRUE) >= 2, 1, 0))

therapy_bypat %>% 
  tabyl(OHA_multiple)

# Assign type 2 based on prescriptions
diabetes_bypat <- diabetes_bypat %>% 
  left_join(therapy_bypat[, c('patid', 'OHA_multiple')], by = 'patid') %>% 
  mutate(diabetes_type = ifelse(is.na(diabetes_type) & OHA_multiple == 1, 'type2', diabetes_type))


# Step 3: Join with study population, all patients left to be assigned will be diabetes type 'unknown' ----
diabetes_bypat <- diabetes_bypat %>% 
  right_join(patients[,'patid'], by = 'patid') %>% 
  mutate(diabetes_type = ifelse(is.na(diabetes_type), 'unknown', diabetes_type))


# Save processed data -----------------------------------------------------

saveRDS(diabetes_bypat, 'processed_data/patients_diabetes.rds')

