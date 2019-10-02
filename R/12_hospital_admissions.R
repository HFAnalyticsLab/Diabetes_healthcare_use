# =======================================================
# Project: Diabetes outpatient care
# Purpose: Hospital admissions - cleaning, processing and counting
# Author: Fiona Grimm
# Date: 17/09/2019
# =======================================================

library(tidyverse)
library(lubridate)
library(janitor)
library(tidylog)

# Source file paths: Rds_path
source('R_FG/file_paths.R')

# Source study parameters 
source('R_FG/study_params.R')

# Import data -------------------------------------------------------------

# Patiend IDs
patients <- readRDS('processed_data/patients_clinical_combined.Rds')

# Inpatient episodes
hesapc_episodes <- readRDS('raw_data/HES_APC_episodes.Rds')

# Cleaning ----------------------------------------------------------------

## Recode missing values as NA
hesapc_episodes <- hesapc_episodes %>% 
  mutate(admisorc = ifelse(admisorc %in% c(98, 99), NA, admisorc),
         classpat = ifelse(classpat %in% c(8, 9), NA, classpat),
         disdest = ifelse(disdest %in% c(98, 99), NA, disdest),
         dismeth = na_if(dismeth, 9),
         intmanig = ifelse(intmanig %in% c(8, 9), NA, intmanig), # there are characters, shouldn't be acc to data dictionary
         admimeth = ifelse(admimeth == '2A', '66', admimeth),
         admimeth = ifelse(admimeth == '2B', '67', admimeth),
         admimeth = ifelse(admimeth == '2C', '68', admimeth),
         admimeth = ifelse(admimeth == '2D', '69', admimeth),
         admimeth = as.integer(admimeth),
         tretspef = na_if(tretspef, '&'),
         tretspef = as.integer(tretspef),
         mainspef = na_if(mainspef, '&'),
         mainspef = as.integer(mainspef),
         firstreg = as.integer(firstreg))

# Check epidur variable
hesapc_episodes <- hesapc_episodes %>% 
  mutate(epidur_calc = epiend - epistart)

hesapc_episodes[hesapc_episodes$epidur!= hesapc_episodes$epidur_calc,]
# seems ok

hesapc_episodes <- hesapc_episodes %>% 
  select(-epidur_calc)

# Exclude episodes that end before they start
hesapc_episodes <- hesapc_episodes %>% 
  filter(epiend >= epistart & discharged >= admidate)

# Filter for study period
hesapc_episodes <- hesapc_episodes %>% 
  filter(epistart %within% interval(study_start, followup_end)) 


# Filtering for admissions ------------------------------------------------
# ie only admission episodes

hesapc_episodes <- hesapc_episodes %>% 
  arrange(patid, epistart)

hesapc_admissions <- hesapc_episodes %>% 
  filter(eorder == 1 & (admisorc <51 | admisorc >53) & admimeth != 81) %>% 
  select(patid, spno, epikey, admidate, eorder, epistart, epiend, discharged, epidur, admimeth, admisorc, disdest, dismeth, tretspef) %>% 
  mutate(emergency = ifelse(admimeth %in% c(21:25), 1, 0),
         during_study = ifelse(admidate %within% interval(study_start, study_end), 1, 0),
         during_followup = ifelse(admidate %within% interval(followup_start, followup_end), 1, 0),
         TTE_study = ifelse(during_study == 1, epistart - study_start, NA),
         TTE_outcome = ifelse(during_followup == 1, epistart - followup_start, NA)) %>% 
  filter(during_study == 1 | during_followup == 1)

# is admidate always the same as epistart?
check <- hesapc_admissions[hesapc_admissions$admidate!= hesapc_admissions$epistart,]

hesapc_admissions %>% 
  tabyl(during_study, during_followup) %>% 
  adorn_title()

# Duplicates?
duplicates <- hesapc_admissions %>% 
  group_by(patid, emergency) %>% 
  get_dupes(epistart)

# if there are several admissions on the same day with the same emergency flag 
# then keep the longer one (as the other one will be a 0-day admission)

hesapc_admissions <- hesapc_admissions %>% 
  arrange(patid, epistart, desc(epidur)) %>% 
  distinct(patid, epistart, emergency, .keep_all = TRUE)

# Saving processed files --------------------------------------------------

saveRDS(hesapc_admissions, 'processed_data/patients_admissions_all.rds')


# Summary per patient -----------------------------------------------------


admissions_bypat <-  hesapc_admissions %>% 
  group_by(patid) %>% 
  summarise(n_adm_study = sum(during_study == 1),
            n_emadm_study = sum(during_study == 1 & emergency == 1),
            n_adm_followup = sum(during_followup == 1),
            n_emadm_followup = sum(during_followup == 1 & emergency == 1)) %>% 
  right_join(patients[, c('patid')], by = 'patid') 



# Saving processed files --------------------------------------------------

saveRDS(admissions_bypat, 'processed_data/patients_admissions_count.rds')

