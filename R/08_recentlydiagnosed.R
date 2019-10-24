# =======================================================
# Project: Diabetes outpatient care
# Purpose: Determine wether first diagnosis was within last x years
# Author: Fiona Grimm
# =======================================================
# TO DO: figure out whether/how practice UTS date needs to be taken into account

library(tidyverse)
library(lubridate)
library(tidylog)
library(janitor)

# Source file paths: Rds_path
source('R/file_paths.R')

# Source study parameters 
source('R/study_params.R')

# Import data -----------------------------------------

# Patiend IDs
patients <- readRDS(str_c(processed_RDS_path, 'patients.Rds'))

# Diabetes code list
diabetes_codes <- read_csv(str_c(code_list_path, 'Appendix1_diabetes_diagnosis.csv'))

# Clinical data
extract_clinical <- readRDS(str_c(raw_RDS_path, 'Extract_clinical.Rds'))

# Diabetes type
diabetes_bypat <- readRDS(str_c(processed_RDS_path, 'patients_diabetes.rds'))


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

# Similar to what is reported in the National Diabetes Audit, will construct binary variables for 
# First diagnosis within less than 1 year before study start
# First diagnosis within 1-4 years before study start
# First diagnosis within 5-9 years before study start

# NB this is incredibly slow - think about finding faster method
clinical_diabetes_first <-  clinical_diabetes_first %>% 
  select(-age_at_diagnosis) %>% 
  mutate(diag_less_than_1y = ifelse(eventdate %within% interval(study_start - years(1) , study_start - 1), 1, 0),
         diag_1_to_4y = ifelse(eventdate %within% interval(study_start - years(4), study_start - years(1) - 1), 1, 0),
         diag_5_to_9y = ifelse(eventdate %within% interval(study_start - years(9), study_start - years(4) - 1), 1, 0))

clinical_diabetes_first %>% 
  tabyl(diag_less_than_1y, diag_1_to_4y) %>% 
  adorn_title()

clinical_diabetes_first %>% 
  tabyl(diag_less_than_1y, diag_5_to_9y) %>% 
  adorn_title()

clinical_diabetes_first %>% 
  tabyl(diag_1_to_4y, diag_5_to_9y) %>% 
  adorn_title()

clinical_diabetes_first <-  clinical_diabetes_first %>% 
  mutate(time_since_diagnosis = case_when(diag_less_than_1y == 1 ~ 'less than 1 year',
                                          diag_1_to_4y == 1 ~ '1 to 4 years',
                                          diag_5_to_9y == 1 ~ '5 to 9 years',
                                          diag_less_than_1y ==  0 & diag_1_to_4y == 0 & diag_5_to_9y == 0 ~ 'more than 9 years'))

# Saving processed files --------------------------------------------------

saveRDS(clinical_diabetes_first, str_c(processed_RDS_path, 'patients_diabetes_firstdiagnosis.rds'))
