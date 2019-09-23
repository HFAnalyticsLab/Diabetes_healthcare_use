# =======================================================
# Project: Diabetes outpatient care
# Purpose: Determine wether first diagnosis was within last x years
# Author: Fiona Grimm
# Date: 12/09/2019
# =======================================================
# TO DO: figure out whether/how practice UTS date needs to be taken into account

library(tidyverse)
library(lubridate)
library(tidylog)
library(janitor)

# Source file paths: Rds_path
source('R_FG/file_paths.R')

# Define study parameters -------------------------------------------------

# Year 1 and 2 to quantify utilisation and other covariates
study_start <- ymd('2015-12-01')
study_end <- ymd('2017-11-30')

# Import data -----------------------------------------

# Patiend IDs
patients <- readRDS('processed_data/patients.Rds')

# Diabetes code list
diabetes_codes <- read_csv(str_c(code_list_path, 'Appendix1_diabetes_diagnosis.csv'))

# Clinical data
extract_clinical <- readRDS('raw_data/Extract_clinical.Rds')

# Diabetes type
diabetes_bypat <- readRDS('processed_data/patients_diabetes.rds')


# Extract diabetes Read codes ---------------------------------------------
# filter for codes before study start
# the episode field is mostly not filled in - can't be used to identify first diagnoses
  
clinical_diabetes <- extract_clinical %>% 
  filter(medcode %in% diabetes_codes$medcode & eventdate < study_start) 


clinical_diabetes_first <-  clinical_diabetes %>% 
    arrange(patid, eventdate) %>%
  select(patid, eventdate, medcode, episode) %>%
  group_by(patid) %>% 
  filter(row_number() == 1)

clinical_diabetes_first <-  clinical_diabetes_first %>% 
  right_join(patients[, c('patid', 'frd', 'yob')]) %>% 
  left_join(diabetes_bypat[, c('patid', 'diabetes_type')], by = 'patid')

# Sense checking age distributions at first diagnosis by diabetes type
clinical_diabetes_first <- clinical_diabetes_first %>% 
  mutate(age_at_diagnosis = year(eventdate) - yob)

clinical_diabetes_first %>% 
  ggplot(aes(x = age_at_diagnosis, group = diabetes_type, color = diabetes_type)) +
  geom_density()

# Constructing binary variables for 
# First diagnosis within six months before study start
# First diagnosis within two years before study start
# First diagnosis within five years before study start

clinical_diabetes_first <-  clinical_diabetes_first %>% 
  select(-age_at_diagnosis) %>% 
  mutate(diag_6m = ifelse(eventdate %within% interval(study_start - months(6), study_start-1), 1, 0),
         diag_2y = ifelse(eventdate %within% interval(study_start - years(2), study_start-1), 1, 0),
         diag_5y = ifelse(eventdate %within% interval(study_start - years(5), study_start-1), 1, 0))

clinical_diabetes_first %>% 
  tabyl(diag_6m, diag_2y) %>% 
  adorn_title()

# Saving processed files --------------------------------------------------

saveRDS(clinical_diabetes_first, 'processed_data/patients_diabetes_firstdiagnosis.rds')
