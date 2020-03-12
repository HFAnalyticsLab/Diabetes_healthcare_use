# =======================================================
# Project: Diabetes outpatient care
# Purpose: Multimorbidity at study start 
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
# Multiple conditions were counted using SAS code in folder SAS_multimorbidity

patient_mm <- read_csv(str_c(SAS_path, 'results/cprdcamlts.csv'))

patients <- readRDS(str_c(processed_RDS_path, 'patients.rds'))

# Diabetes type
diabetes_bypat <- readRDS(str_c(processed_RDS_path, 'patients_diabetes.rds'))

# Derive counts --------------------------------------------------------------

# Note: for diabetes we are using a slightly more comprehensive list of medcodes (29 more than CPRD)
# Will exclude diabetes from co-morbidities anyway, as it is part of the inclusion criteria
# study population

# Note 2: ANXr represents a new definition of anxiety, developed by KH based on advice from GPs
# The difference to ANX is that Z drugs have been excluded from the product code list (45 product codes in total)
# This includes Zopiclone, Zolpidem, Stilnoct, Zimovane, Zileze, Sonata, Zaleplon
# as these are mainly prescribed for insomnia

mm_bypat <- patient_mm %>% 
  select(patid, HYP:MIG) 

# What difference is there between the two definitions of anxiety?

# Will combine depression and axiety into DEPANX
mm_bypat <- mm_bypat %>% 
  mutate(DEPANX = ifelse(DEP == 1 | ANX == 1, 1, 0),
         DEPANXr = ifelse(DEP == 1 | ANXr == 1, 1, 0))

mm_bypat <- mm_bypat %>% 
  mutate(mm_count = rowSums(.[comorbidities]),
         mm_count_excl_DEPANXr = rowSums(.[comorbidities[comorbidities != 'DEPANXr']]),
         mm_count_excl_PNC = rowSums(.[comorbidities[comorbidities != 'PNC']]),
         mm_cat = cut(mm_count, breaks = c(0, 1, 2, 3, 4, Inf), 
                             labels = c('0', '1', '2', '3', '4+'), include.lowest = TRUE, right = FALSE))

# Saving processed files --------------------------------------------------

saveRDS(mm_bypat, str_c(processed_RDS_path, 'patients_multimorbidity.rds'))
