# =======================================================
# Project: Diabetes outpatient care
# Purpose: Descriptive analysis of health care utilisation from scripts 10-13
# Author: Fiona Grimm
# Date: 05/09/2019
# =======================================================

library(tidyverse)
library(lubridate)
library(janitor)
library(tidylog)
library(tableone)


# Source file paths: Rds_path
source('R_FG/file_paths.R')

# Define study parameters -------------------------------------------------

# Source study parameters 
source('R_FG/study_params.R')


# Function definition -----------------------------------------------------

# Count the number of patients that have the value corresponding to 25th percentile, median and 75th percentile
# this is needed for statistical disclosure control

patn_by_percentile <- function(values){
  
  n_25 <- sum(values == quantile(values, 0.25))
  n_50 <- sum(values == quantile(values, 0.5))
  n_75 <- sum(values == quantile(values, 0.75))
  
  result <- str_c(n_50, ' [', n_25, ', ', n_75, ']')
  result
  
}

# Where medians or quantiles are calculated as averages (when there is an even number of observations)
# Need to make sure that this average was calculated over a big enough group of patients
# this is needed for statistical disclosure control

median_tiebreak_npat <- function(data, variable, type, quantile){
  
  sum(pull(data[data$diabetes_type == type,], variable) == floor(quantile(pull(data[data$diabetes_type == type,], variable), quantile))) +
    sum(pull(data[data$diabetes_type == type,],variable) == ceiling(quantile(pull(data[data$diabetes_type == type, ], variable), quantile)))
}

# Import data -------------------------------------------------------------

patients <- readRDS('processed_data/patients_clinical_combined.Rds') %>% 
  select(patid, resquality, followup_pop, diabetes_type)

# GP appointments
consultations_bypat <- readRDS('processed_data/patients_consultations.rds')

# Annual diabetes checks
diab_annual_check_bypat <- readRDS('processed_data/patients_annual_diabetes_checks.rds')

# Outpatient appointments
hesop_count_byPat <- readRDS('processed_data/patients_OPappointments.rds')

# Hospital admissions
admissions_bypat <- readRDS('processed_data/patients_admissions.rds')

# Prescriptions
prescripts_bypat <- readRDS('processed_data/patients_prescriptions.rds')



# Join them all together
patients_combined <- patients %>% 
  left_join(consultations_bypat, by = 'patid') %>% 
  left_join(diab_annual_check_bypat[, c('patid', 'n_checks', 'diab_annual_check_cat')], by = 'patid') %>% 
  left_join(hesop_count_byPat, by = 'patid') %>% 
  left_join(admissions_bypat, by = 'patid') %>% 
  left_join(prescripts_bypat, by = 'patid') 


# Reorder some factor levels 
patients_combined <- patients_combined %>% 
  mutate(diabetes_type = fct_relevel(diabetes_type, 'type1', 'type2', 'unknown', 'other'))

# Change/combine some factor level to avoid having small groups
patients_combined <- patients_combined %>% 
  mutate(GP_cat = fct_collapse(GP_cat, 'Over 25' = c('25-48', 'Over 48')),
         nurse_cat = fct_collapse(nurse_cat, 'Over 25' = c('25-48', 'Over 48')))

# Replace NAs

patients_combined <- patients_combined %>% 
  mutate_if(is.numeric, funs(replace_na(.,0)))

saveRDS(patients_combined, 'processed_data/patients_utilisation_combined.Rds')

         
# Variables to summarise
vars_tosummarise <- c('n_consult', "n_consult_GP", "n_consult_nurse", "n_consult_f2f", "n_consult_telecons", "n_checks",
                      "n_adm_study", "n_emadm_study", "n_prescripts", "n_prescripts_diabetes", "OP_attended_total", "OP_attended_total_diab",
                      'consult_cat', 'GP_cat', 'nurse_cat', 'diab_annual_check_cat', 'OP_attended_cat', 'OP_attended_diab_cat')
# cat_vars_tosummarise <- c('consult_cat', 'GP_cat', 'nurse_cat', 'diab_annual_check_cat', 'OP_attended_cat', 'OP_attended_diab_cat')
vars_nonnormal <- c('n_consult', "n_consult_GP", "n_consult_nurse", "n_consult_f2f", "n_consult_telecons", "n_checks",
                    "n_adm_study", "n_emadm_study", "n_prescripts", "n_prescripts_diabetes", "OP_attended_total", "OP_attended_total_diab")

# 1. Population at study start  -------------------------------------------
# By diabetes type, only research quality patients

patients_study <-  patients_combined %>% 
  filter(resquality == 1)

table_study <- CreateTableOne(vars = vars_tosummarise, strata = 'diabetes_type', 
                              data = patients_study, 
                              test = FALSE)

table_study_csv <- print(table_study, noSpaces = TRUE, nonnormal = TRUE)
write.csv(table_study_csv, 'summary_stats/table2/190917_Table2_studystart.csv')

# for SDC, check the number of patients with the associated 25th, 50th and 75th percentile value

table_study_patn <- patients_study %>% 
  group_by(diabetes_type) %>% 
  summarise_at(vars(vars_nonnormal), patn_by_percentile) %>% 
  gather(-diabetes_type, key = 'type', value = 'value') %>% 
  spread(key = 'diabetes_type', value = 'value')

write_csv(table_study_patn, 'summary_stats/table2/190917_Table2_studystart_patientN.csv')




# 2. Population at follow-up stsart ---------------------------------------
# By diabetes type
# Only patients that did not die or transfer out during study period

patients_followup <-  patients_combined %>% 
  filter(resquality == 1 & followup_pop == 1)

table_followup <- CreateTableOne(vars = vars_tosummarise, strata = 'diabetes_type', 
                                 data = patients_followup, 
                                 test = FALSE)


table_followup_csv <- print(table_followup, noSpaces = TRUE, nonnormal = TRUE)
write.csv(table_followup_csv, 'summary_stats/table2/190917_Table2_followupstart.csv')


# for SDC, check the number of patients with the associated 25th, 50th and 75th percentile value
table_followup_patn <- patients_followup %>% 
  group_by(diabetes_type) %>% 
  summarise_at(vars(vars_nonnormal), patn_by_percentile) %>% 
  gather(-diabetes_type, key = 'type', value = 'value') %>% 
  spread(key = 'diabetes_type', value = 'value')

write_csv(table_followup_patn, 'summary_stats/table2/190917_Table2_followup_patientN.csv')

# Where the mean corresponds to an average of two values, need to make sure that 
# this was the average of 5 or more patients:

median_tiebreak_npat(patients_followup, 'n_prescripts', 'type1', 0.5)
median_tiebreak_npat(patients_followup, 'n_prescripts', 'type1', 0.75)

median_tiebreak_npat(patients_followup, 'n_prescripts', 'unknown', 0.75)

median_tiebreak_npat(patients_followup, 'n_consult_telecons', 'unknown', 0.75)
median_tiebreak_npat(patients_followup, 'n_checks', 'unknown', 0.75)

