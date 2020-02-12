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
         DEPANXr = ifelse(DEP == 1 | ANXr == 1, 1, 0),
         # this is still using the older definition of ANX,
         mental_mm_count = ALC + ANO + DEPANX + DEM + LEA + OPS + SCZ,
         physical_mm_count = AST + ATR + BLI + BRO + CAN + CHD + CKD + CLD + CON + COP + DIV + EPI + HEF + HEL + 
                             HYP + IBD + IBS + MIG + MSC + PNC + PRK + PRO + PSO + PVD + RHE + SIN + STR + THY,
         mm_count = mental_mm_count + physical_mm_count)
         

# this is still using the older definition of ANX
mm_bypat <- mm_bypat %>% 
  mutate(mental_mm_cat = cut(mental_mm_count, breaks = c(0, 1, 2, 3, 4, Inf), 
                             labels = c('0', '1', '2', '3', '4+'), include.lowest = TRUE, right = FALSE),
         physical_mm_cat = cut(physical_mm_count, breaks = c(0, 1, 2, 3, 4, Inf), 
                             labels = c('0', '1', '2', '3', '4+'), include.lowest = TRUE, right = FALSE),
         mm_cat = cut(mm_count, breaks = c(0, 1, 2, 3, 4, Inf), 
                             labels = c('0', '1', '2', '3', '4+'), include.lowest = TRUE, right = FALSE))

# Saving processed files --------------------------------------------------

saveRDS(mm_bypat, str_c(processed_RDS_path, 'patients_multimorbidity.rds'))



# Summary tables for prevalence -------------------------------------------

mm_bypat2 <- mm_bypat %>% 
  right_join(patients[, c('patid', 'cohort_step2', 'cohort_step3')], by = 'patid') %>% 
  left_join(diabetes_bypat[, c('patid',  'diabetes_type')], by = 'patid')

mm_bypat_study  <-  mm_bypat2 %>% 
  filter(cohort_step2 == 1 & diabetes_type %in% c('type1', 'type2')) %>% 
  select(-cohort_step2) 

mm_prev <- mm_bypat_study %>% 
  select(patid, diabetes_type, HYP:DEPANXr) %>% 
  gather(-patid, - diabetes_type, key = 'condition', value = 'present') %>% 
  group_by(diabetes_type, condition) %>% 
  summarise(n = n(),
            n_with_condition = sum(present == 1)) %>% 
  mutate(proportion_with_condition = n_with_condition / n ,
         prevalence = round(100 * n_with_condition / n, 1),prevalence = round(100 * n_with_condition / n, 2),
         prevalence_CI_lower =  round(100*proportion_with_condition - 1.96*sqrt((proportion_with_condition* (1 - proportion_with_condition)) / n), 2),
         prevalence_CI_upper =  round(100*proportion_with_condition + 1.96*sqrt((proportion_with_condition * (1 - proportion_with_condition)) / n), 2))

write_csv(mm_prev, str_c(summary_stats_path, 'multimorbidity/MM_prevalence.csv'))

# by CMD

mm_prev_CMD <- mm_bypat_study %>% 
  filter(diabetes_type == 'type2') %>% 
  select(patid, HYP:DEPANXr) %>% 
  gather(-patid, - DEPANXr, key = 'condition', value = 'present') %>% 
  group_by(DEPANXr, condition) %>% 
  summarise(n = n(),
            n_with_condition = sum(present == 1)) %>% 
  mutate(proportion_with_condition = n_with_condition / n ,
         prevalence = round(100 * n_with_condition / n, 1),prevalence = round(100 * n_with_condition / n, 2),
         prevalence_CI_lower =  round(100*proportion_with_condition - 1.96*sqrt((proportion_with_condition* (1 - proportion_with_condition)) / n), 2),
         prevalence_CI_upper =  round(100*proportion_with_condition + 1.96*sqrt((proportion_with_condition * (1 - proportion_with_condition)) / n), 2))

write_csv(mm_prev_CMD, str_c(summary_stats_path, 'multimorbidity/MM_prevalence_T2DM_byDEPANXr.csv'))


# Summary tables for mean number of conditions -------------------------------------------

mm_means <- mm_bypat2 %>%  
  filter(cohort_step2 == 1 & diabetes_type %in% c('type1', 'type2')) %>% 
  group_by(diabetes_type) %>% 
  summarise(n = n(),
            mean_mm_count = round(mean(mm_count), 1),
            mean_physical_mm_count = round(mean(physical_mm_count), 1),
            mean_mental_mm_count = round(mean(mental_mm_count), 1))
            

write_csv(mm_means, str_c(summary_stats_path, 'multimorbidity/MM_count_means.csv'))


# Summary table for condition count ---------------------------------------

mm_count <- mm_bypat2 %>%  
  filter(cohort_step2 == 1 & diabetes_type == 'type2') %>% 
  group_by(physical_mm_cat) %>% 
  summarise(n_with_count = n()) %>% 
  ungroup() %>% 
  mutate(n = sum(n_with_count),
         proportion_with_count = n_with_count / n ,
         prevalence = round(100 * n_with_count / n, 1),prevalence = round(100 * n_with_count / n, 2),
         prevalence_CI_lower =  round(100*proportion_with_count - 1.96*sqrt((proportion_with_count* (1 - proportion_with_count)) / n), 2),
         prevalence_CI_upper =  round(100*proportion_with_count + 1.96*sqrt((proportion_with_count * (1 - proportion_with_count)) / n), 2))
             
write_csv(mm_count, str_c(summary_stats_path, 'multimorbidity/MM_physical_cat_prevalence.csv'))

mm_count_CMD <- mm_bypat2 %>%  
  filter(cohort_step2 == 1 & diabetes_type == 'type2') %>% 
  group_by(physical_mm_cat) %>% 
  summarise(n_with_count = n()) %>% 
  ungroup() %>% 
  mutate(n = sum(n_with_count),
         proportion_with_count = n_with_count / n ,
         prevalence = round(100 * n_with_count / n, 1),prevalence = round(100 * n_with_count / n, 2),
         prevalence_CI_lower =  round(100*proportion_with_count - 1.96*sqrt((proportion_with_count* (1 - proportion_with_count)) / n), 2),
         prevalence_CI_upper =  round(100*proportion_with_count + 1.96*sqrt((proportion_with_count * (1 - proportion_with_count)) / n), 2))

write_csv(mm_count_CMD, str_c(summary_stats_path, 'multimorbidity/MM_physical_cat_T2DM_byCMD_prevalence.csv'))
