# =======================================================
# Project: Diabetes outpatient care
# Purpose: Define smoking status
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

# Import data -----------------------------------------

# Study population
patients <- readRDS(str_c(processed_RDS_path, 'patients.rds'))

# Smoking code lists 
smoking_pres <- read_csv(str_c(code_list_path, 'Appendix4_smoking_prescriptions.csv'))

smoking_read <- read_csv(str_c(code_list_path, 'Appendix4_smoking_Read.csv')) 


# Therapy
extract_therapy <- readRDS(str_c(raw_RDS_path, 'Extract_therapy.Rds'))

# Additional clinical details 
extract_additional_clinical <- readRDS(str_c(raw_RDS_path, 'Extract_additional_clinical.Rds'))

# Clinical data
extract_clinical <- readRDS(str_c(raw_RDS_path, 'Extract_clinical.Rds'))

# CPRD Variable lookup tables 
YND_lookup <- read_csv('../data_dictionary_CPRD/csv_lookup_tables/YND.csv') %>% 
  rename(code = Code, status = `Smoke/drink Status`)

# 1. Extract prescriptions for smoking cessation therapy ----

therapy_smoking <- extract_therapy %>% 
    filter(prodcode %in% smoking_pres$prodcode & eventdate < study_start)

therapy_smoking_bypat <- therapy_smoking %>% 
    group_by(patid) %>% 
    summarise(cessation_prescr = n())

# 2. Read codes for smoking status from clinical files --------------------

# Extract the relevant entities from clinical details
clin_smoking <- extract_clinical %>% 
  filter(medcode %in% smoking_read$medcode & eventdate <= study_end) %>% 
  left_join(smoking_read[, c('medcode', 'smoking_status')], by = 'medcode') %>% 
  mutate(smoking_status = case_when(smoking_status == 'current smoker' ~ 'smoker',
                                    smoking_status == 'non-smoker' ~ 'nonsmoker',
                                    smoking_status == 'ex smoker' ~ 'exsmoker'))
  semi_join(patients[, c('patid')], by = 'patid')

clin_smoking_bypat <- clin_smoking %>% 
  filter(eventdate < study_start) %>% 
  group_by(patid, smoking_status) %>% 
  summarise(count = n())

clin_smoking_bypat <-  clin_smoking_bypat %>% 
  mutate(smoking_status = case_when(smoking_status == 'smoker' ~ 'clin_smoker',
                                 smoking_status == 'nonsmoker' ~ 'clin_nonsmoker',
                                 smoking_status == 'exsmoker' ~ 'clin_exsmoker')) %>% 
  spread(key = smoking_status, value = count) 

# Latest entry before follow-up period starts
# not older than study start (this is to make sure the record is not too old & missingness might be informative)

clin_smoking_latest <- clin_smoking %>% 
  filter(eventdate %within% interval(study_start, study_end)) %>% 
  arrange(patid, desc(eventdate)) %>% 
  group_by(patid) %>% 
  filter(row_number() == 1) %>% 
  ungroup() %>% 
  select(patid, smoking_status, eventdate) %>% 
  rename(latest_clin_status = smoking_status, latest_clin_eventdate = eventdate) 

# Earliest entry after study period starts
# not older than study end 

clin_smoking_earliest <- clin_smoking %>% 
  filter(eventdate %within% interval(study_start, study_end)) %>% 
  arrange(patid, eventdate) %>% 
  group_by(patid) %>% 
  filter(row_number() == 1) %>% 
  ungroup() %>% 
  select(patid, smoking_status, eventdate) %>% 
  rename(earliest_clin_status = smoking_status, earliest_clin_eventdate = eventdate)

# Latest entry before study period starts
# within 6 months 

clin_smoking_beforestudy <- clin_smoking %>% 
  filter(eventdate %within% interval(study_start - months(6), study_start)) %>% 
  arrange(patid, eventdate) %>% 
  group_by(patid) %>% 
  filter(row_number() == 1) %>% 
  ungroup() %>% 
  select(patid, smoking_status, eventdate) %>% 
  rename(beforestudy_clin_status = smoking_status, beforestudy_clin_eventdate = eventdate)


# 3. Smoking status from additional clinical details ----

# Extract the relevant entities from add clinical details
addclin_smoking <- extract_additional_clinical %>% 
  filter(enttype == 4 & patid %in% patients$patid) %>% 
  rename(status_code =  data1) %>% 
  select(patid, adid, status_code) %>% 
  mutate(smoking_status = case_when(status_code == 1 ~ 'smoker',
                            status_code == 2 ~ 'nonsmoker',
                            status_code == 3 ~ 'exsmoker')) %>% 
  left_join(extract_clinical[, c('patid', 'adid', 'eventdate')], by = c('patid', 'adid')) %>% 
  filter(eventdate <= study_end & status_code != 0) %>% 
  semi_join(patients[, c('patid')], by = 'patid')

# Count entries by patient and smoking status
addclin_smoking_bypat <- addclin_smoking %>%
  filter(eventdate < study_start) %>% 
  group_by(patid, smoking_status) %>% 
  summarise(count = n())

addclin_smoking_bypat <-  addclin_smoking_bypat %>% 
  mutate(smoking_status = case_when(smoking_status == 'smoker' ~ 'addclin_smoker',
                                 smoking_status == 'nonsmoker' ~ 'addclin_nonsmoker',
                                 smoking_status == 'exsmoker' ~ 'addclin_exsmoker')) %>% 
  spread(key = smoking_status, value = count) 

# Latest entry before follow-up period starts
# not older than study start (this is to make sure the record is not too old & missingness might be informative)

addclin_smoking_latest <- addclin_smoking %>% 
  filter(eventdate %within% interval(study_start, study_end)) %>% 
  arrange(patid, desc(eventdate)) %>% 
  group_by(patid) %>% 
  filter(row_number() == 1) %>% 
  ungroup() %>% 
  select(patid, smoking_status, eventdate) %>% 
  rename(latest_addclin_status = smoking_status, latest_addclin_eventdate = eventdate)

# Earliest entry after study period starts
# not older than study end 

addclin_smoking_earliest <- addclin_smoking %>% 
  filter(eventdate %within% interval(study_start, study_end)) %>% 
  arrange(patid, eventdate) %>% 
  group_by(patid) %>% 
  filter(row_number() == 1) %>% 
  ungroup() %>% 
  select(patid, smoking_status, eventdate) %>% 
  rename(earliest_addclin_status = smoking_status, earliest_addclin_eventdate = eventdate)

# Latest entry before study period starts
# within 6 months 

addclin_smoking_beforestudy <- addclin_smoking %>% 
  filter(eventdate %within% interval(study_start - months(6), study_start)) %>% 
  arrange(patid, eventdate) %>% 
  group_by(patid) %>% 
  filter(row_number() == 1) %>% 
  ungroup() %>% 
  select(patid, smoking_status, eventdate) %>% 
  rename(beforestudy_addclin_status = smoking_status, beforestudy_addclin_eventdate = eventdate)

# 4. Combine and decide on status ---------------------------

# latest before study end 
# not older than study start (this is to make sure the record is not too old & missingness might be informative)

smoking_bypat <-  patients[, c('patid')] %>% 
  left_join(clin_smoking_latest, by = 'patid') %>% 
  left_join(addclin_smoking_latest, by = 'patid') %>% 
  left_join(clin_smoking_bypat, by = 'patid') %>% 
  full_join(addclin_smoking_bypat, by = 'patid') %>% 
  left_join(therapy_smoking_bypat, by = 'patid') 

# check agreement between latest clinical and latest additional clinical status
smoking_bypat <-  smoking_bypat %>% 
  mutate(latest_status_agrees = ifelse(latest_clin_status == latest_addclin_status, 1, 0))

# Determine smoking status
# Choose most recent out of clinical and additional clinical details
# or if they are on the same day and agree, also fine

# if they are on the same day and do not agree:
# smoker + exsmoker -> smoker
# non-smoker + exsmoker -> exsmoker
# non-smoker + smoker -> NA

smoking_bypat <- smoking_bypat %>% 
  mutate(smoking_status = case_when(is.na(latest_clin_eventdate) & !is.na(latest_addclin_eventdate) ~ latest_clin_status,
                                   !is.na(latest_clin_eventdate) & is.na(latest_addclin_eventdate) ~ latest_addclin_status,
                                     latest_clin_eventdate > latest_addclin_eventdate  ~ latest_clin_status,
                                     latest_clin_eventdate < latest_addclin_eventdate  ~ latest_addclin_status,
                                     latest_clin_eventdate == latest_addclin_eventdate  & 
                                     latest_status_agrees == 1 ~ latest_clin_status,
                                   latest_clin_eventdate == latest_addclin_eventdate  & 
                                     latest_clin_status %in% c('smoker', 'exsmoker') & latest_addclin_status %in% c('smoker', 'exsmoker') ~ 'smoker',
                                   latest_clin_eventdate == latest_addclin_eventdate  & 
                                     latest_clin_status %in% c('nonsmoker', 'exsmoker') & latest_addclin_status %in% c('nonsmoker', 'exsmoker') ~ 'exsmoker'))

# nonsmoker will be converted to exsmoker if patients ever had a record of smoking, exsmoking or smoking cessation therapy
smoking_bypat <- smoking_bypat %>% 
  mutate(smoking_status = ifelse(smoking_status == 'nonsmoker' & (!is.na(clin_smoker) | !is.na(clin_exsmoker) | 
                                                                  !is.na(addclin_smoker) | !is.na(addclin_exsmoker) |
                                                                  !is.na(cessation_prescr)), 'exsmoker', smoking_status))

# Anything left as NA will be coded as missing
smoking_bypat <- smoking_bypat %>% 
  mutate(smoking_status = fct_explicit_na(smoking_status, 'Missing'))
           

smoking_bypat %>%  tabyl(smoking_status)


# at baseline 
# Define as: latest measurement before study start, if that was within 6 months of study start
# if none available, earliest after study start


smoking_baseline_bypat <- patients[, c('patid')] %>% 
  left_join(clin_smoking_beforestudy, by = 'patid') %>% 
  left_join(addclin_smoking_beforestudy, by = 'patid') %>% 
  left_join(clin_smoking_earliest, by = 'patid') %>% 
  left_join(addclin_smoking_earliest, by = 'patid') %>% 
  left_join(clin_smoking_bypat, by = 'patid') %>% 
  left_join(addclin_smoking_bypat, by = 'patid') %>% 
  left_join(therapy_smoking_bypat, by = 'patid') 
  

# check agreement between latest clinical and latest additional clinical status
smoking_baseline_bypat <-  smoking_baseline_bypat %>% 
  mutate(beforestudy_status_agrees = ifelse(beforestudy_clin_status == beforestudy_addclin_status, 1, 0),
         earliest_status_agrees = ifelse(earliest_clin_status == earliest_addclin_status, 1, 0))

smoking_baseline_bypat <- smoking_baseline_bypat %>% 
  mutate(smoking_status = case_when(is.na(beforestudy_clin_eventdate) & !is.na(beforestudy_addclin_eventdate) ~ beforestudy_clin_status,
                                    !is.na(beforestudy_clin_eventdate) & is.na(beforestudy_addclin_eventdate) ~ beforestudy_addclin_status,
                                    beforestudy_clin_eventdate > beforestudy_addclin_eventdate  ~ beforestudy_clin_status,
                                    beforestudy_clin_eventdate < beforestudy_addclin_eventdate  ~ beforestudy_addclin_status,
                                    beforestudy_clin_eventdate == beforestudy_addclin_eventdate  & 
                                      beforestudy_status_agrees == 1 ~ beforestudy_clin_status,
                                    beforestudy_clin_eventdate == beforestudy_addclin_eventdate  & 
                                      beforestudy_clin_status %in% c('smoker', 'exsmoker') & beforestudy_addclin_status %in% c('smoker', 'exsmoker') ~ 'smoker',
                                    beforestudy_clin_eventdate == beforestudy_addclin_eventdate  & 
                                      beforestudy_clin_status %in% c('nonsmoker', 'exsmoker') & beforestudy_addclin_status %in% c('nonsmoker', 'exsmoker') ~ 'exsmoker'))

# Fill NAs with data from after study start
smoking_baseline_bypat <- smoking_baseline_bypat %>% 
  mutate(smoking_status = ifelse(is.na(smoking_status) & is.na(earliest_clin_eventdate) & !is.na(earliest_addclin_eventdate), earliest_addclin_status, smoking_status),
         smoking_status = ifelse(is.na(smoking_status) & !is.na(earliest_clin_eventdate) & is.na(earliest_addclin_eventdate), earliest_clin_status, smoking_status),
         smoking_status = ifelse(is.na(smoking_status) & !is.na(earliest_clin_eventdate) & !is.na(earliest_addclin_eventdate)
                                & earliest_clin_eventdate > earliest_addclin_eventdate, earliest_addclin_status, smoking_status),
         smoking_status = ifelse(is.na(smoking_status) & !is.na(earliest_clin_eventdate) & !is.na(earliest_addclin_eventdate)
                                & earliest_clin_eventdate < earliest_addclin_eventdate, earliest_clin_status, smoking_status),
         smoking_status = ifelse(is.na(smoking_status) & earliest_clin_eventdate == earliest_addclin_eventdate 
                                & earliest_status_agrees == 1, earliest_addclin_status, smoking_status),
         smoking_status = ifelse(is.na(smoking_status) & earliest_clin_eventdate == earliest_addclin_eventdate  & 
                                   earliest_clin_status %in% c('smoker', 'exsmoker') & earliest_addclin_status %in% c('smoker', 'exsmoker'), 'smoker' , smoking_status),
         smoking_status = ifelse(is.na(smoking_status) & earliest_clin_eventdate == earliest_addclin_eventdate  & 
                                   earliest_clin_status %in% c('nonsmoker', 'exsmoker') & earliest_addclin_status %in% c('nonsmoker', 'exsmoker'), 'exsmoker' , smoking_status))


# nonsmoker will be converted to exsmoker if patients ever had a record of smoking, exsmoking or smoking cessation therapy
smoking_baseline_bypat <- smoking_baseline_bypat %>% 
  mutate(smoking_status = ifelse(smoking_status == 'nonsmoker' & (!is.na(clin_smoker) | !is.na(clin_exsmoker) | 
                                                                    !is.na(addclin_smoker) | !is.na(addclin_exsmoker) |
                                                                    !is.na(cessation_prescr)), 'exsmoker', smoking_status))

# Anything left as NA will be coded as missing
smoking_baseline_bypat <- smoking_baseline_bypat %>% 
  mutate(smoking_status = fct_explicit_na(smoking_status, 'Missing'))

smoking_baseline_bypat %>%  tabyl(smoking_status)

# Saving processed files --------------------------------------------------

# 1. All clinical entries 
saveRDS(clin_smoking, str_c(processed_RDS_path, 'patients_smoking_clinical_all.rds'))

# 2. All additional clinical entries 
saveRDS(addclin_smoking, str_c(processed_RDS_path, 'patients_smoking_add-clinical_al.rds'))

# 3. Combined table with  counts and the latest restults
saveRDS(smoking_bypat, str_c(processed_RDS_path, 'patients_smoking_latest.rds'))

# 4. at baseline (definition above)
saveRDS(smoking_baseline_bypat, str_c(processed_RDS_path, 'patients_smoking_at_baseline.rds'))


