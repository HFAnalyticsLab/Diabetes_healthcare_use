# =======================================================
# Project: Diabetes outpatient care
# Purpose: Primary care/GP appointments - cleaning, processing, defining and counting
# Author: Fiona Grimm
# =======================================================

library(tidyverse)
library(lubridate)
library(janitor)
library(tidylog)
library(tableone)

# Source file paths: Rds_path
source('R/file_paths.R')

# Source study parameters 
source('R/study_params.R')

# Source graph parameters 
source('R/plotting_params.R')

# Import data -------------------------------------------------------------

patients <- readRDS(str_c(processed_RDS_path, 'patients_clinical_combined.Rds'))

# Diabetes type
diabetes_bypat <- readRDS(str_c(processed_RDS_path, 'patients_diabetes.rds'))

# Consultations
extract_consultation <- readRDS(str_c(raw_RDS_path, 'Extract_consultation.Rds'))

# Clinical
extract_clinical <- readRDS(str_c(raw_RDS_path, 'Extract_clinical.Rds'))

# Staff
extract_staff <- readRDS(str_c(raw_RDS_path, 'Extract_staff.Rds'))

# Join consulations and staff role
consultations <-  extract_consultation %>% 
  left_join(extract_staff, by = 'staffid') 


# Cleaning ----------------------------------------------------------------

# Only appts during study period
consultations <-  consultations %>% 
  filter(eventdate %within% interval(study_start, study_end)) 

#   Face to face consultation codes:	Home consultation codes:		Telephone consultation codes:
#   --------------------------------	------------------------		-----------------------------
#   Clinic,1,							            Home Visit,27,					    Telephone call from a patient,10,
#   Follow-up/routine visit,3,			  Hotel Visit,28,					    Telephone call to a patient,21,
#   Night visit , practice,6,			    Nursing Home Visit,30,			Triage,33,
#   Out of hours, Practice,7,			    Residential Home Visit,31,  Telephone Consultation,55,
#   Surgery consultation,9,				    Twilight Visit,32,
#   Acute visit,11,						        Night Visit,50,
#   Emergency Consultation,18,
#   Initial Post Discharge Review,48,
# 
#   GP codes from rol:				Nurse codes from rol:				Other clinician codes from rol:
#   ------------------				---------------------				-------------------------------
#   Senior Partner,1,				Practice Nurse,11,					  Physiotherapist,26,
#   Partner,2,						 Other Nursing & Midwifery,54		Other Health Care Professional,33
#   Assistant,3,
#   Associate,4, 
#   Locum,7,
#   GP Registrar,8,
#   Sole Practitioner,10,
#   Salaried Partner,47,
#   GP Retainer,50,
#   Other Students,53

# Coding up consultation type, duration and staff type
# only keep valid consultations
consultations <- consultations %>%   
  mutate(pracid = patid %% 1000,
         unique_id = str_c(pracid, consid, sep = '-'),
         # Consultation type
         f2f = ifelse(constype %in% c(1, 3, 6, 7, 9, 11, 18, 48), 1, 0),
         homevis = ifelse(constype %in% c(27, 28, 30, 31, 32, 50), 1, 0),
         telecons = ifelse(constype %in% c(10, 21, 33, 55), 1, 0),
         consult = ifelse(f2f == 1 | homevis == 1 | telecons == 1 , 1, 0),
         # Duration
         dur0 = ifelse(duration == 0, 1, 0),
         duration = ifelse(duration == 0 , 0.5, duration),
         duration = ifelse(duration >= 60 , 60, duration),
         duratcons = ifelse(consult == 1 & homevis == 0, duration, NA),
         duratf2f = ifelse(f2f == 1 & homevis == 0, duration, NA),
         # Staff type
         gp = ifelse(consult == 1 & role %in% c(1, 2, 3, 4, 7, 8, 10, 47, 50, 53), 1, 0),
         nurse = ifelse(consult == 1 & role %in% c(11, 54), 1, 0),
         otherstaff = ifelse(consult == 1 & role %in% c(26, 33), 1, 0),
         staff_valid = ifelse( gp == 1 | nurse == 1 | otherstaff == 1, 1, 0)) %>% 
  filter(consult == 1)

# Remove appointments scheduled after patient died??
consultations<- consultations %>% 
  left_join(patients[, c('patid', 'ONS_dod')], by = 'patid') %>% 
  filter(is.na(ONS_dod) | eventdate < ONS_dod)

# Counting appointments per patient ---------------------------------------

consultations_bypat <-  consultations %>% 
  group_by(patid) %>% 
  summarise(n_consult = n(),
            n_consult_GP = sum(gp == 1),
            n_consult_nurse = sum(nurse == 1),
            n_consult_otherstaff = sum(otherstaff == 1),
            n_consult_f2f = sum(f2f == 1),
            n_consult_telecons = sum(telecons == 1))


consultations_bypat %>%
  gather(-patid, key = 'type', value = 'count') %>% 
  ggplot(aes(x = count, group = type, fill = type)) +
  geom_histogram(binwidth = 1) +
  coord_cartesian(xlim = c(0,10)) +
  facet_grid(. ~ type)


# Create categorical variables (binned appointment counts)
consultations_bypat <-  consultations_bypat %>% 
  mutate(consult_cat = cut(n_consult, breaks = c(0, 1, 3, 5, 9, 13, 25, 49, Inf), labels = c('None', '1-2', '3-4', '5-8', '9-12', '13-24', '25-48', 'Over 48'),
                           ordered_result = TRUE, include.lowest = TRUE, right = FALSE),
         GP_cat = cut(n_consult_GP, breaks = c(0, 1, 3, 5, 9, 13, 25, 49, Inf), labels = c('None', '1-2', '3-4', '5-8', '9-12', '13-24', '25-48', 'Over 48'),
                      ordered_result = TRUE, include.lowest = TRUE, right = FALSE),
         nurse_cat = cut(n_consult_nurse, breaks = c(0, 1, 3, 5, 9, 13, 25, 49, Inf), labels = c('None', '1-2', '3-4', '5-8', '9-12', '13-24', '25-48', 'Over 48'),
                         ordered_result = TRUE, include.lowest = TRUE, right = FALSE))
         
# Join in patients 
consultations_bypat <-  consultations_bypat %>% 
  right_join(patients[, c('patid')], by = 'patid') 

# Convert missing to none
consultations_bypat <- consultations_bypat %>% 
  mutate_at(vars(consult_cat, GP_cat, nurse_cat), funs(fct_explicit_na(., 'None')))

# Saving processed files --------------------------------------------------

saveRDS(consultations_bypat, 'processed_data/patients_consultations.rds')

# Diabetes annual check ---------------------------------------------------

# Additional clinical details 
extract_additional_clinical <- readRDS('raw_data/Extract_additional_clinical.Rds')

diab_annual_check <- extract_additional_clinical %>% 
  filter(enttype == 22) %>% 
  rename(prog_type = data1, checkup_type = data2) %>% 
  select(patid, adid, prog_type, checkup_type) %>% 
  left_join(extract_clinical[, c('patid', 'adid', 'eventdate')], by = c('patid', 'adid')) 


diab_annual_check <-  diab_annual_check %>% 
  filter(eventdate %within% interval(study_start, study_end)) 

# counting the number of checks by patient
diab_annual_check_bypat  <- diab_annual_check %>% 
  group_by(patid) %>% 
  summarise(n_checks = n()) 

# Create categorical variables (binned appointment counts)
diab_annual_check_bypat <-  diab_annual_check_bypat %>% 
  mutate(diab_annual_check_cat = cut(n_checks, breaks = c(0, 1, 2, Inf), labels = c('None', '1', '2 or more'),
                                     ordered_result = TRUE, include.lowest = TRUE, right = FALSE))

# Join in patients 
diab_annual_check_bypat <-  diab_annual_check_bypat %>% 
  right_join(patients[, c('patid')], by = 'patid')

# Convert missing to none
diab_annual_check_bypat <- diab_annual_check_bypat %>% 
  mutate(diab_annual_check_cat = fct_explicit_na(diab_annual_check_cat, 'None'))

# Saving processed files --------------------------------------------------

saveRDS(diab_annual_check_bypat, 'processed_data/patients_annual_diabetes_checks.rds')



