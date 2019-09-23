# =======================================================
# Project: Diabetes outpatient care
# Purpose: Multimorbidity, current cutoff is study start (covariate)
# Author: Fiona Grimm
# Date: 05/09/2019
# =======================================================

library(tidyverse)
library(lubridate)
library(janitor)
library(tidylog)


# Source file paths: Rds_path
source('R_FG/file_paths.R')

# Define study parameters -------------------------------------------------

# Year 1 and 2 to quantify utilisation and other covariates
study_start <- ymd('2015-12-01')
study_end <- ymd('2017-11-30')


# Import data -------------------------------------------------------------
# Multiple conditions were counted using SAS code in folder SAS_multimorbidity
# Based on the way it was done in project 36

patient_mm <- read_csv(str_c(SAS_path, 'results/cprdcamlts.csv'))

patients <- readRDS('processed_data/patients.rds')

# Derive counts --------------------------------------------------------------

# Note: for diabetes we are using a slightly more comprehensive list of medcodes (29 more than CPRD)
# Will exclude diabetes from co-morbidities anyway, as it is part of the inclusion criteria

mm_bypat <- patient_mm %>% 
  select(patid, HYP:MIG) %>% 
  right_join(patients[, c('patid')], by = 'patid')

# Will combine depression and axiety
mm_bypat <- mm_bypat %>% 
  mutate(DEPANX = ifelse(DEP == 1 | ANX == 1, 1, 0),
         mental_mm_count = ALC + ANO + DEPANX + DEM + LEA + OPS + SCZ,
         physical_mm_count = AST + ATR + BLI + BRO + CAN + CHD + CKD + CLD + CON + COP + DIV + EPI + HEF + HEL + 
                             HYP + IBD + IBS + MIG + MSC + PNC + PRK + PRO + PSO + PVD + RHE + SIN + STR + THY,
         mm_count = mental_mm_count + physical_mm_count)
         

mm_bypat <- mm_bypat %>% 
  mutate(mental_mm_cat = cut(mental_mm_count, breaks = c(0, 1, 2, 3, 4, Inf), 
                             labels = c('0', '1', '2', '3', '4+'), include.lowest = TRUE, right = FALSE),
         physical_mm_cat = cut(physical_mm_count, breaks = c(0, 1, 2, 3, 4, Inf), 
                             labels = c('0', '1', '2', '3', '4+'), include.lowest = TRUE, right = FALSE),
         mm_cat = cut(mm_count, breaks = c(0, 1, 2, 3, 4, Inf), 
                             labels = c('0', '1', '2', '3', '4+'), include.lowest = TRUE, right = FALSE))



# Saving processed files --------------------------------------------------

saveRDS(mm_bypat, 'processed_data/patients_multimorbidity.rds')

