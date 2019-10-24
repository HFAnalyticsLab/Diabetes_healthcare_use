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

# Source graph parameters 
source('R/plotting_params.R')

# Import data -----------------------------------------

# Study population
patients <- readRDS(str_c(processed_RDS_path, 'patients.rds'))

# Diabetes type
diabetes_bypat <- readRDS(str_c(processed_RDS_path, 'patients_diabetes.rds'))

# Diabetes medication
therapy_bypat <- readRDS(str_c(processed_RDS_path, 'patients_medication.rds'))

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
  left_join(smoking_bypat[, c('patid', 'smoking_status')], by = 'patid') %>% 
  left_join(BMI_bypat[, c('patid', 'BMI_categorical')], by = 'patid') %>% 
  left_join(HbA1C_bypat[, c('patid', 'HbA1C_control')], by = 'patid') %>% 
  left_join(clinical_diabetes_first[, c('patid', 'time_since_diagnosis')], by = 'patid') %>% 
   mutate(all_clinical_missing = ifelse(medication == 'None recorded' & smoking_status == 'Missing'
                             & BMI_categorical == 'Missing' & HbA1C_control == 'Missing', 1, 0)) %>% 
  left_join(mm_bypat, by = 'patid') 


# Reorder some factor levels 
patients_combined <- patients_combined %>% 
  mutate(diabetes_type = fct_relevel(diabetes_type, 'type1', 'type2', 'unknown', 'other'),
         medication = fct_relevel(medication, 'NIGLD only', 'Insulin only', 'Both', 'None recorded'),
         smoking_status = fct_relevel(smoking_status, 'nonsmoker', 'exsmoker', 'smoker', 'Missing'),
         BMI_categorical = fct_relevel(BMI_categorical, 'Missing', after = Inf),
         HbA1C_control = fct_relevel(HbA1C_control, 'good', 'borderline', 'bad', 'Missing'),
         ethnicity = fct_relevel(ethnicity, 'White', 'Asian/British Asian', 'Black/Black British', 'Other', 'Mixed', 'Unknown'),
         time_since_diagnosis = fct_relevel(time_since_diagnosis, 'less than 1 year'))

# Change/combine some factor level to avoid having small groups
patients_combined <- patients_combined %>% 
  mutate(female = ifelse(gender == 2, 1, 0),
         age_bins_study_SDC = fct_collapse(age_bins_study, '0-19' = c('0-4', '5-9', '10-14', '15-19'),
                                     '80+' = c('80-84', '85+')),
         age_bins_followup_SDC = fct_collapse(age_bins_followup, '0-19' = c('0-4', '5-9', '10-14', '15-19'),
                                           '80+' = c('80-84', '85+')),
         mental_mm_cat_SDC = fct_collapse(mental_mm_cat, '1+' = c('1', '2', '3', '4+')))


saveRDS(patients_combined, str_c(processed_RDS_path, 'patients_clinical_combined.Rds'))


# Variables to summarise
vars_tosummarise <- c('resquality', 'diabetes_type', "female", "ethnicity", "startage_study", "age_bins_study_SDC", 
                      'died_study', 'died_followup', 'transfer_out_study', 'transfer_out_followup',
                      'time_since_diagnosis', 
                      'e2011_urban_rural', 'imd_quintile',
                      "medication", "smoking_status", "BMI_categorical", "HbA1C_control", 'all_clinical_missing',
                      'mental_mm_cat_SDC', 'physical_mm_cat', 'mm_cat',
                      'HYP', 'PNC', 'HEL','CHD', 'DEPANX', 'CKD', 'THY', 'DIV', 'IBS', 'ATR')

cat_vars_tosummarise <- vars_tosummarise[vars_tosummarise != 'startage_study']

# 1. All patients ---------------------------------------------------------
# Acceptable patients with diabetes diagnosis and valid IMD

table_all <- CreateTableOne(vars = vars_tosummarise, 
                            data = patients_combined, 
                            factorVars = cat_vars_tosummarise,
                            test = FALSE)

table_all_csv <- print(table_all, noSpaces = TRUE) 
write.csv(table_all_csv, str_c(summary_stats_path, 'table1/191023_Table1_allpatients.csv'))

# 2. Comparing patients that are research quality with the rest -----------
# Research quality being defined at being in an UTS practice at study start
# and for the last data collection date of the practice to be after follow up end

vars_resqual <- vars_tosummarise[vars_tosummarise != 'resquality']
cat_vars_resqual <- cat_vars_tosummarise[cat_vars_tosummarise != 'resquality']



table_resqual <- CreateTableOne(vars = vars_resqual, strata = 'resquality', 
                                data = patients_combined, factorVars = cat_vars_resqual,
                                test = FALSE)

table_resqual_csv <- print(table_resqual, noSpaces = TRUE) 
write.csv(table_resqual_csv, str_c(summary_stats_path, 'table1/191023_Table1_researchquality.csv'))

# 3. Population at study start  -------------------------------------------
# By diabetes type, only research quality patients

patients_study <-  patients_combined %>% 
  filter(resquality == 1)

vars_study <- vars_resqual[vars_resqual != 'diabetes_type']
cat_vars_study <- cat_vars_resqual[cat_vars_resqual != 'diabetes_type']

table_study <- CreateTableOne(vars = vars_study, strata = 'diabetes_type', 
                              data = patients_study, factorVars = cat_vars_study,
                              test = FALSE)

table_study_csv <- print(table_study, noSpaces = TRUE)
write.csv(table_study_csv, str_c(summary_stats_path, 'table1/191023_Table1_studystart.csv'))

# 4. Population at follow-up start ---------------------------------------
# By diabetes type
# Only patients that did not die or transfer out during study period

patients_followup <-  patients_combined %>% 
  filter(followup_pop == 1)

vars_followup <- vars_study[!is.element(vars_study, c('died_study', 'transfer_out_study', "startage_study", "age_bins_study_SDC"))]
vars_followup <- c(vars_followup, "startage_followup", "age_bins_followup_SDC")
cat_vars_followup <- cat_vars_study[!is.element(cat_vars_study, c('died_study', 'transfer_out_study', "startage_study", "age_bins_study_SDC"))]
cat_vars_followup <-  c(cat_vars_followup, "age_bins_followup_SDC")


table_followup <- CreateTableOne(vars = vars_followup, strata = 'diabetes_type', 
                                 data = patients_followup, factorVars = cat_vars_followup,
                                 test = FALSE)


table_followup_csv <- print(table_followup, noSpaces = TRUE)
write.csv(table_followup_csv, 'summary_stats/table1/190917_Table1_followupstart.csv')

