# =======================================================
# Project: Diabetes outpatient care
# Purpose: Descriptive analysis of clinical characteristics from scripts 01-08
# To generate 'Table 1'
# Author: Fiona Grimm
# =======================================================

library(tidyverse)
library(lubridate)
library(tidylog)
library(tableone)

# Source file paths: Rds_path
source('R/file_paths.R')

# Source study parameters 
source('R/study_params.R')

# Import data -----------------------------------------

# Study population
patients <- readRDS(str_c(processed_RDS_path, 'patients.rds'))

# Diabetes type
diabetes_bypat <- readRDS(str_c(processed_RDS_path, 'patients_diabetes.rds'))

# Diabetes medication during study
therapy_bypat <- readRDS(str_c(processed_RDS_path, 'patients_medication.rds'))

# Diabetes medication prior to study
therapy_bypat_prior <- readRDS(str_c(processed_RDS_path, 'patients_medication_prior.rds'))
therapy_bypat_prior <- therapy_bypat_prior %>% 
  rename(medication_prior = 'medication')

# Smoking (latest within study period)
smoking_bypat <- readRDS(str_c(processed_RDS_path, 'patients_smoking_at_baseline.rds'))

# BMI (to check % of patients without record)
BMI_bypat <- readRDS(str_c(processed_RDS_path, 'patients_BMI_at_baseline.rds'))

# HbA1C (to check % of patients without record)
HbA1C_bypat <- readRDS(str_c(processed_RDS_path, 'patients_HbA1C_at_baseline.rds'))

# Long-term conditions
mm_bypat <- readRDS(str_c(processed_RDS_path, 'patients_multimorbidity.rds'))

# Binary variables on 'first diagnosis within last x years'
clinical_diabetes_first <- readRDS(str_c(processed_RDS_path, 'patients_diabetes_firstdiagnosis.rds'))

# Join them all together
patients_combined <- patients %>% 
  select(-vmid, -mob, -marital, -famnum, -CHSreg, -CHSdate, -prescr, -capsup, -regstat, -reggap, -internal, -accept, -ONS_dor) %>% 
  left_join(diabetes_bypat[, c('patid', 'diabetes_type')], by = 'patid') %>% 
  left_join(therapy_bypat[, c('patid', 'medication')], by = 'patid') %>% 
  left_join(therapy_bypat_prior[, c('patid', 'medication_prior')], by = 'patid') %>% 
  left_join(smoking_bypat[, c('patid', 'smoking_status')], by = 'patid') %>% 
  left_join(BMI_bypat[, c('patid', 'BMI_categorical')], by = 'patid') %>% 
  left_join(HbA1C_bypat[, c('patid', 'HbA1C_control')], by = 'patid') %>% 
  left_join(clinical_diabetes_first[, c('patid', 'time_since_diagnosis')], by = 'patid') %>% 
   mutate(all_clinical_missing = ifelse(medication == 'None recorded' & smoking_status == 'Missing'
                             & BMI_categorical == 'Missing' & HbA1C_control == 'Missing', 1, 0)) %>% 
  left_join(mm_bypat, by = 'patid') 

# Create factor for censoring type 
patients_combined <- patients_combined %>% 
  mutate(censoring_type_cprd = case_when(died_study == 1 ~ 'death',
                                         transfer_out_study == 1 ~ 'transfer out',
                                         died_study == 0 & transfer_out_study == 0 & years_in_study_cprd < 2 ~ 'practice last data collection',
                                         years_in_study_cprd == 2 ~ 'study end'),
         censoring_type_hes = case_when(died_study == 1 ~ 'death',
                                        died_study == 0 & years_in_study_hes < 2 ~ 'death', # died_study is based on what happens first in CPRD
                                        # but some patients transfer out and then die before the end of the study
                                        years_in_study_hes == 2 ~ 'study end'))
  
# Reorder some factor levels 
patients_combined <- patients_combined %>% 
  mutate(diabetes_type = fct_relevel(diabetes_type, 'type1', 'type2', 'unknown', 'other'),
         medication = fct_relevel(medication, 'NIGLD only', 'Insulin only', 'Both', 'None recorded'),
         medication_prior = fct_relevel(medication_prior, 'NIGLD only', 'Insulin only', 'Both', 'None recorded'),
         smoking_status = fct_relevel(smoking_status, 'nonsmoker', 'exsmoker', 'smoker', 'Missing'),
         BMI_categorical = fct_relevel(BMI_categorical, 'Missing', after = Inf),
         HbA1C_control = fct_relevel(HbA1C_control, 'good', 'borderline', 'bad', 'Missing'),
         ethnicity = fct_relevel(ethnicity, 'White', 'Asian/British Asian', 'Black/Black British', 'Other', 'Mixed', 'Unknown'),
         time_since_diagnosis = fct_relevel(time_since_diagnosis, 'less than 1 year'))

# Change/combine some factor level to avoid having small groups
patients_combined <- patients_combined %>% 
  mutate(female = ifelse(gender == 2, 1, 0),
         female = factor(female),
         age_bins_study_SDC = fct_collapse(age_bins_study, '0-24' = c('0-4', '5-9', '10-14', '15-19', '20-24'),
                                     '80+' = c('80-84', '85+')),
         age_bins_followup_SDC = fct_collapse(age_bins_followup, '0-19' = c('0-4', '5-9', '10-14', '15-19'),
                                           '80+' = c('80-84', '85+')))


saveRDS(patients_combined, str_c(processed_RDS_path, 'patients_clinical_combined.Rds'))
        

# Baseline descriptives ---------------------------------------------------

# For study population (step 2 cohort)
patients_study <-  patients_combined %>% 
  filter(cohort_step2 == 1)

# Variables to summarise
vars_tosummarise <- c("female", "ethnicity", "startage_study", "age_bins_study_SDC", 
                      'censoring_type_cprd', 'censoring_type_hes',
                      'years_in_study_cprd', 'years_in_study_hes',
                      'time_since_diagnosis', 
                      'e2011_urban_rural', 'imd_quintile',
                      "medication", "medication_prior", "smoking_status", "BMI_categorical", "HbA1C_control",
                      'mm_cat', 'mm_count', 'mm_count_excl_DEPANXr', 'mm_count_excl_PNC', comorbidities)

cat_vars_tosummarise <- c('censoring_type_hes', 'e2011_urban_rural', comorbidities)

table_patients_study <- CreateTableOne(vars = vars_tosummarise, 
                            data = patients_study, 
                            strata = 'diabetes_type', 
                            factorVars = cat_vars_tosummarise,
                            test = FALSE)

table_patients_study_csv <- print(table_patients_study, 
                                  noSpaces = TRUE,
                                  showAllLevels = TRUE) 

write.csv(table_patients_study_csv, str_c(summary_stats_path, 'table1/Table1_cohort2.csv'))

# Study population split by mental health comorbidity ------------------

patients_study_red <- patients_study %>% 
  filter(diabetes_type %in% c('type1', 'type2')) %>% 
  mutate(diabetes_type = fct_drop(diabetes_type))

# DEPANXr
table_study_DEPANX <- CreateTableOne(vars = vars_tosummarise, 
                                        strata = c('DEPANXr','diabetes_type'), 
                                        data = patients_study_red, 
                                        factorVars = cat_vars_tosummarise,
                                        test = FALSE)

table_study_DEPANX_csv <- print(table_study_DEPANX, noSpaces = TRUE)
write.csv(table_study_DEPANX_csv, str_c(summary_stats_path, 'table1/Table1_cohort2_DEPANX.csv'))

# PNC
table_study_PNC <- CreateTableOne(vars = vars_tosummarise, 
                                     strata = c('PNC','diabetes_type'), 
                                     data = patients_study_red, 
                                     factorVars = cat_vars_tosummarise,
                                     test = FALSE)

table_study_PNC_csv <- print(table_study_PNC, noSpaces = TRUE)
write.csv(table_study_PNC_csv, str_c(summary_stats_path, 'table1/Table1_cohort2_PNC.csv'))
