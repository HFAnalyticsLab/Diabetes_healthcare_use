# =======================================================
# Project: Diabetes outpatient care
# Purpose: Apply inclusion and exclusion criteria to the data extract to define the study cohort
# Author: Fiona Grimm
# =======================================================

library(tidyverse)
library(lubridate)
library(tidylog)
library(janitor)

# Source file paths: code_list_path
source('R/file_paths.R')

# Source study parameters 
source('R/study_params.R')

# Import data -------------------------------------------------------------

# Patient extract
extract_patient <- readRDS(str_c(raw_RDS_path, 'Extract_patient.Rds'))

# ONS death record
death_patient <- readRDS(str_c(raw_RDS_path, 'Linked_death_patient.Rds'))

# Patient level IMD
imd_patient <- readRDS(str_c(raw_RDS_path, 'Linked_patient_imd.Rds'))

# Practice leves rural urban classifier
rururb_patient <- readRDS(str_c(raw_RDS_path, 'Linked_rururb_practice.Rds'))

# Practice
extract_practice <- readRDS(str_c(raw_RDS_path, 'Extract_practice.Rds'))

# Clinical files 
extract_clinical <- readRDS(str_c(raw_RDS_path, 'Extract_clinical.Rds'))

# Import diabetes code list
diabetes_codes <- read_csv(str_c(code_list_path,'Appendix1_diabetes_diagnosis.csv'))

# Mecode-readcode lookup table
medical_dic <- read_tsv(str_c(code_list_path, "medical.txt"))

# Ethnicity code list
# need to join in medcodes
ethnicity_codes <- read_csv(str_c(code_list_path, "res56-ethnicity.csv")) %>% 
  select(readcode, ethnic5) %>% 
  left_join(medical_dic, by = 'readcode')
  
# HES Patient
hes_patient <- readRDS(str_c(raw_RDS_path, 'HES_patient.Rds'))



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

# Research quality data flag
patients <- patients %>% 
  left_join(extract_practice, by = 'pracid') %>% 
  mutate(resquality = ifelse(uts <= study_start & lcd > study_end, 1, 0)) 

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
         died_followup = ifelse(died_followup == 1 & transfer_out_followup == 1 & ONS_dod > tod, 0, died_followup),
         followup_pop = ifelse(resquality == 1 & died_study == 0 & transfer_out_study == 0, 1, 0))

# Check that this worked
patients %>%  tabyl(died_followup, transfer_out_followup) %>%  adorn_title()
patients %>%  tabyl(died_study, transfer_out_study) %>%  adorn_title()


# Categorical age variable: 5-year bins
age_labels <- c(str_c(seq(0, 80, by = 5), '-', seq(4, 84, by = 5)), '85+')
age_breaks <- c(seq(0, 85, by = 5), Inf)

patients <- patients %>% 
  mutate(startage_study = 2015 - yob,
         age_bins_study = cut(startage_study, breaks = age_breaks, 
                              labels = age_labels, include.lowest = TRUE, right = FALSE),
         startage_followup = 2017 - yob,
         age_bins_followup = cut(startage_followup, breaks = age_breaks, 
                                 labels = age_labels, include.lowest = TRUE, right = FALSE))

# IMD quintiles
imd_labels <- c('1 least deprived', '2', '3', '4', '5 most deprived')

patients <- patients %>% 
  mutate(imd_quintile = cut(imd2015_10, breaks = seq(0, 10, by = 2), labels = imd_labels))


# Ethnicity ---------------------------------------------------------------

# 1. Try to determine from clinical codes in CPRD
ethnicity_clinical <- extract_clinical %>% 
  filter(eventdate < study_start) %>% 
  semi_join(ethnicity_codes, by = 'medcode') %>% 
  left_join(ethnicity_codes, by = 'medcode') %>% 
  group_by(patid, ethnic5) %>% 
  tally()

# Keep most frequent ethnicity by patient (and if there is a tie keep all)
ethnicity_clinical_mostfreq <- ethnicity_clinical %>% 
  group_by(patid) %>% 
  filter(ethnic5 == max(ethnic5))

# Check for ties
ethnicity_clinical_mostfreq %>% 
  select(patid) %>% 
  group_by(patid) %>% 
  tally() %>% 
  filter(n>1)

# Remove patients with ties
ethnicity_clinical_mostfreq <- ethnicity_clinical_mostfreq %>% 
  group_by(patid) %>% 
  mutate(patient_count = n()) %>% 
  filter(patient_count == 1 ) %>% 
  select(-patient_count)

# Fill in the gaps (including patients that had tie in CPRD) using HES patient information
ethnicity_clinical_mostfreq <- ethnicity_clinical_mostfreq %>%  
  right_join(patients[, 'patid'], by = 'patid') %>% 
  left_join(hes_patient[, c('patid', 'gen_ethnicity')])

ethnicity_clinical_mostfreq <- ethnicity_clinical_mostfreq  %>% 
  mutate(ethnicity = ethnic5,
         ethnicity = ifelse(is.na(ethnicity) & gen_ethnicity == 'White', 'White', ethnicity),
         ethnicity = ifelse(is.na(ethnicity) & 
                             (gen_ethnicity=="Other" | gen_ethnicity=="Chinese" | gen_ethnicity=="Oth_Asian"), 'Other', ethnicity),
         ethnicity = ifelse(is.na(ethnicity) & 
                              (gen_ethnicity=="Bl_Afric" | gen_ethnicity=="Bl_Carib" | gen_ethnicity=="Bl_Other"), 'Black/Black British', ethnicity),
         ethnicity = ifelse(is.na(ethnicity) & 
                              (gen_ethnicity=="Bangladesi" | gen_ethnicity=="Indian" | gen_ethnicity=="Pakistani"), 'Asian/British Asian', ethnicity),
         ethnicity = fct_explicit_na(ethnicity, 'Unknown'))

ethnicity_clinical_mostfreq %>%  tabyl(ethnicity)

# Add this back into patients table
patients <- patients %>% 
  left_join(ethnicity_clinical_mostfreq[, c('patid', 'ethnicity')], by = 'patid')

# Saving processed files --------------------------------------------------

saveRDS(patients, str_c(processed_RDS_path, 'patients.rds'))

