# =======================================================
# Project: Diabetes outpatient care
# Purpose: Apply inclusion and exclusion criteria to the data extract to define the study cohort
# Author: Fiona Grimm
# Date: 29/08/2019
# =======================================================

library(tidyverse)
library(lubridate)
library(tidylog)
library(janitor)

# Source file paths: code_list_path
source('R_FG/file_paths.R')

# Import data -------------------------------------------------------------

# Patient extract
extract_patient <- readRDS('raw_data/Extract_patient.Rds')

# ONS death record
death_patient <- readRDS('raw_data/Linked_death_patient.Rds')

# Patient level IMD
imd_patient <- readRDS('raw_data/Linked_patient_imd.Rds')

# Practice leves rural urban classifier
rururb_patient <- readRDS('raw_data/Linked_rururb_practice.Rds')

# Clinical files 
extract_clinical <- readRDS('raw_data/Extract_clinical.Rds')

# Import diabetes code list
diabetes_codes <- read_csv(str_c(code_list_path,'Appendix1_diabetes_diagnosis.csv'))

# Define study parameters -------------------------------------------------

# Year 1 and 2 to quantify utilisation and other covariates
study_start <- ymd('2015-12-01')
study_end <- ymd('2017-11-30')

# Year 3 for health outcomes
followup_start <- ymd('2017-12-01')
followup_end <-  ymd('2018-11-30')

# Double-check pre-applied inclusion and exclusion criteria ---------------

## Registered in a CPRD practice before study start (based on first registration date)
range(na.omit(extract_patient$frd))[2] < study_start

## Did not deregister before study start (based on transfer out date)
range(na.omit(extract_patient$tod))[1] <= study_start

## Gender criterion (male, female and indeterminate only) 
extract_patient %>% 
  tabyl(gender)

## CPRD generated patient level data quality flag 
extract_patient %>% 
  tabyl(accept)

## Prevalent diabetes: one or more diabetes-related Read codes before 2015-12-01
diabetes_clinical <- extract_clinical %>% 
  filter(medcode %in% diabetes_codes$medcode & eventdate < study_start) %>% 
  group_by(patid) %>% 
  summarise(count = n())   

# Number of patients without diabetes Read codes (to be excluded)
# NB I checked test and referral files, no additional patients found 
sum(!(extract_patient$patid %in% diabetes_clinical$patid))


# Exclusions and linkages -------------------------------------------------

# Exclude patients without diabetes Read codes
patients <- extract_patient %>% 
  filter(patid %in% diabetes_clinical$patid)

# Exclude patients without IMD
patients <- patients %>% 
  left_join(imd_patient[, c('patid', 'imd2015_10')], by = c('patid')) %>% 
  filter(!is.na(imd2015_10))

# Exclude patient who died before 2015-12-01 according to ONS:
# It is a known issue that date of death (dod) is sometimes missing
# Missing dod will be replaced with date of registration (dor)
death_patient <- death_patient %>% 
  mutate(dod_filled = if_else(is.na(dod), dor, dod))

patients <- patients %>% 
  left_join(death_patient[,c('patid', 'dod_filled', 'dor')], by = 'patid') %>% 
  rename(ONS_dod = 'dod_filled', ONS_dor = 'dor')

# Exclude patient who died before study_start according to ONS
patients <- patients %>% 
  filter(is.na(ONS_dod) | ONS_dod >= study_start)

# check range of death dates
range(na.omit(patients$ONS_dod))[1] <= study_start

# Urban-Rural indicator
# first construct practice ID to join on, which is equivalent of the last 3 digits of the patient ID
patients <- patients %>% 
  mutate(pracid = patid %% 1000) %>% 
  left_join(rururb_patient[,c('pracid', 'e2011_urban_rural')], by ='pracid') %>% 
  filter(!is.na(e2011_urban_rural)) 


# Rename variables ------------------------------------------------------

# CPRD date of death, as we are using ONS date of death
patients <- patients %>% 
  rename(CPRD_deathdate = deathdate)

# Deriving variables ------------------------------------------------------

# Mortality flags and follow-up times
patients <- patients %>% 
         # died during study period or follow up 
  mutate(died_study = ifelse(!is.na(ONS_dod) & ONS_dod %within% interval(study_start, study_end), 1, 0),
         died_followup = ifelse(!is.na(ONS_dod) & ONS_dod %within% interval(followup_start, followup_end) , 1, 0),
         # transferred during study period or follow up (for reasons other than death)
         transfer_out_study = ifelse(!is.na(tod) & tod %within% interval(study_start, study_end) & toreason != 1, 1, 0),
         transfer_out_followup = ifelse(!is.na(tod) & tod %within% interval(followup_start, followup_end) & toreason != 1, 1, 0),
         # Resolve cases where patients both die and transfer out - which is first?
         transfer_out_study = ifelse(died_study == 1 & transfer_out_study == 1 & ONS_dod <= tod, 0, transfer_out_study),
         died_study = ifelse(died_study == 1 & transfer_out_study == 1 & ONS_dod > tod, 0, died_study),
         transfer_out_followup = ifelse(died_followup == 1 & transfer_out_followup == 1 & ONS_dod <= tod, 0, transfer_out_followup),
         died_followup = ifelse(died_followup == 1 & transfer_out_followup == 1 & ONS_dod > tod, 0, died_followup))

# Check that this worked
patients %>%  tabyl(died_followup, transfer_out_followup) %>%  adorn_title()
patients %>%  tabyl(died_study, transfer_out_study) %>%  adorn_title()


# Categorical age variable: 5-year bins
age_labels <- c(str_c(seq(0, 80, by = 5), '-', seq(4, 84, by = 5)), '85+')
age_breaks <- c(seq(0, 85, by = 5), Inf)

patients <- patients %>% 
  mutate(startage = 2015 - yob,
         age_bins = cut(startage, breaks = age_breaks, labels = age_labels, include.lowest = TRUE))

# IMD quintiles
imd_labels <- c('1 least deprived', '2', '3', '4', '5 most deprived')

patients <- patients %>% 
  mutate(imd_quintile = cut(imd2015_10, breaks = seq(0, 10, by = 2), labels = imd_labels))

# Saving processed files --------------------------------------------------

saveRDS(patients, 'processed_data/patients.rds')

