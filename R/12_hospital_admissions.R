# =======================================================
# Project: Diabetes outpatient care
# Purpose: Hospital Admissions  cleaning, processing and counting
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

# Patiend IDs
patients <- readRDS(str_c(processed_RDS_path, 'patients_clinical_combined.Rds'))

# Inpatient episodes
hesapc_episodes <- readRDS(str_c(raw_RDS_path, 'HES_APC_episodes.Rds'))

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
  filter(eorder == 1 & (admisorc <51 | admisorc >53) & admimeth != 81 & admimeth != 67) 

hesapc_admissions <- hesapc_admissions %>% 
  select(patid, spno, epikey, admidate, eorder, epistart, epiend, discharged, epidur, admimeth, classpat, admisorc, disdest, dismeth, tretspef) %>% 
  mutate(admitype = case_when(admimeth %in% c(21:25, 66, 67, 69) ~ 'emergency',
                              admimeth %in% c(11, 12, 13) & classpat %in% c(1, 4) ~ 'elective',
                              admimeth %in% c(31, 32, 68, 82, 83) ~ 'maternity',
                              admimeth %in% c(11, 12, 13) & classpat %in% c(2, 3) ~ 'daycase',
                              admimeth %in% c(98, 99, 28) ~ 'unknown'),
         during_study = ifelse(epistart %within% interval(study_start, study_end), 1, 0),
         during_followup = ifelse(epistart %within% interval(followup_start, followup_end), 1, 0),
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
  get_dupes(patid, admitype, epistart)

# if there are several admissions on the same day with the admittions type 
# then keep the longer one (as the other one will be a 0-day admission)

hesapc_admissions <- hesapc_admissions %>% 
  arrange(patid, epistart, desc(epidur)) %>% 
  distinct(patid, epistart, admitype, .keep_all = TRUE)

# Check: how many admissions are for patients in the final study population?
hesapc_admissions %>% 
  left_join(patients[, c('patid', 'resquality')], by = 'patid') %>% 
  filter(resquality == 1) %>% 
  nrow()

# Saving processed files --------------------------------------------------

saveRDS(hesapc_admissions, str_c(processed_RDS_path, 'patients_admissions.rds'))


# Summary per patient -----------------------------------------------------

admissions_bypat <-  hesapc_admissions %>% 
  group_by(patid) %>% 
  summarise(elective_admissions = sum(admitype == 'elective'),
            emergency_admissions= sum(admitype == 'emergency')) 

# Normalising by time spent in study
admissions_bypat <- admissions_bypat %>% 
  right_join(patients[, c('patid', 'resquality', 'diabetes_type', 'years_in_study')], by = 'patid') %>% 
  filter(resquality == 1 & diabetes_type %in% c('type1', 'type2')) %>% 
  select(-resquality) %>% 
  mutate_if(is.numeric, ~replace_na(.x, 0))

admissions_bypat <- admissions_bypat %>% 
  ungroup() %>% 
  mutate(elective_admissions_per_year = round(elective_admissions / years_in_study, 1),
         emergency_admissions_per_year = round(emergency_admissions / years_in_study, 1))

# Create categorical variables (binned appointment counts)
admissions_bypat <-  admissions_bypat %>% 
  mutate(elective_admissions_cat = cut(elective_admissions, 
                                       breaks = c(0, 1, 2, 3, 4, 5, Inf), 
                                       labels = c('None', '1', '2', '3', '4', '5+'),
                                       ordered_result = TRUE, include.lowest = TRUE, right = FALSE),
         emergency_admissions_cat = cut(emergency_admissions, 
                                        breaks = c(0, 1, 2, 3, 4, 5, Inf), 
                                        labels = c('None', '1', '2', '3', '4', '5+'),
                                        ordered_result = TRUE, include.lowest = TRUE, right = FALSE),
         elective_admissions_per_year_cat = cut(elective_admissions_per_year, 
                                                breaks = c(0, 1, 2, 3, 4, 5, Inf), 
                                                labels = c('None', '1', '2', '3', '4', '5+'),
                                       ordered_result = TRUE, include.lowest = TRUE, right = FALSE),
         emergency_admission_per_years_cat = cut(emergency_admissions_per_year, 
                                                 breaks = c(0, 1, 2, 3, 4, 5, Inf), 
                                                 labels = c('None', '1', '2', '3', '4', '5+'),
                                        ordered_result = TRUE, include.lowest = TRUE, right = FALSE))

# Saving processed files 
saveRDS(admissions_bypat, str_c(processed_RDS_path, 'patients_admissions.rds'))

# Create summary tables -------------------------------------------------
# Means

all_admissions_means <- admissions_bypat %>% 
  group_by(diabetes_type) %>% 
  summarise(n = n(),
            mean_elective_admissions = round(mean(elective_admissions), 1),
            mean_elective_admissions_per_year = round(mean(elective_admissions_per_year), 1),
            mean_emergency_admissions = round(mean(emergency_admissions), 1),
            mean_emergency_admissions_per_year = round(mean(emergency_admissions_per_year), 1)) %>% 
  mutate(subgroup = 'all patients')

not_censored_admissions_means <- admissions_bypat %>% 
  group_by(diabetes_type) %>% 
  filter(years_in_study == 2) %>% 
  summarise(n = n(),
            mean_elective_admissions = round(mean(elective_admissions), 1),
            mean_elective_admissions_per_year = round(mean(elective_admissions_per_year), 1),
            mean_emergency_admissions = round(mean(emergency_admissions), 1),
            mean_emergency_admissions_per_year = round(mean(emergency_admissions_per_year), 1)) %>% 
  mutate(subgroup = 'did not transfer out')

admissions_means <- all_admissions_means %>% 
  bind_rows(not_censored_admissions_means)

write_csv(admissions_means, str_c(summary_stats_path, 'table2/Admissions_means.csv'))

