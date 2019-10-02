# =======================================================
# Project: Diabetes outpatient care
# Purpose: Primary care prescriptions - cleaning, processing and counting
# Author: Fiona Grimm
# Date: 05/09/2019
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

patients <- readRDS('processed_data/patients_clinical_combined.Rds')


# Diabetes medication code list
medication_codes <- read_csv(str_c(code_list_path, 'Appendix5_antidiabetics.csv'))

# Therapy 
extract_therapy <- readRDS('raw_data/Extract_therapy.Rds')


# Clean and process -------------------------------------------------------

prescripts <- extract_therapy %>% 
  filter(eventdate %within% interval(study_start, study_end)) %>% 
  mutate(diabetes_pres = ifelse(prodcode %in% medication_codes$product_code, 1, 0))

# Counting prescriptions per patient ---------------------------------------

prescripts_bypat <-  prescripts %>% 
  group_by(patid) %>% 
  summarise(n_prescripts = n(),
            n_prescripts_diabetes = sum(diabetes_pres == 1))

# Join in patients 
prescripts_bypat <-  prescripts_bypat %>% 
  right_join(patients[, c('patid')], by = 'patid') 

# Saving processed files --------------------------------------------------

saveRDS(prescripts_bypat, 'processed_data/patients_prescriptions.rds')


