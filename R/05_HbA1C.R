# =======================================================
# Project: Diabetes outpatient care
# Purpose: Clean HbA1C data and create categorical HbA1C variable
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

# Import data -----------------------------------------

# Study population
patients <- readRDS(str_c(processed_RDS_path, 'patients.rds'))

# Additional clinical details 
extract_additional_clinical <- readRDS(str_c(raw_RDS_path, 'Extract_additional_clinical.Rds'))

# HbA1C Read code list
HbA1C_codelist <- read_csv(str_c(code_list_path, 'Appendix2_HbA1c_diabetic_control.csv'))

# Clinical data
extract_clinical <- readRDS(str_c(raw_RDS_path, 'Extract_clinical.Rds'))

# Test files
extract_tests <- readRDS(str_c(raw_RDS_path, 'Extract_test.Rds'))

# CPRD Variable lookup tables 
OPR_lookup <- read_csv('../data_dictionary_CPRD/csv_lookup_tables/OPR.csv') %>% 
  rename(operator = Code, operator_name = Operator)

SUM_lookup <- read_csv('../data_dictionary_CPRD/csv_lookup_tables/SUM.csv') %>% 
  rename(unit = Code, unit_name = `Specimen Unit Of Measure`)

TQU_lookup <- read_csv('../data_dictionary_CPRD/csv_lookup_tables/TQU.csv')%>% 
  rename(qualifier = Code, qualifier_name = `Test Qualifier`)

# Three ways of recording HBA1C in CPRD
# 1. Additional clinical details type 275 'HbA1C - diabetic control' --------

extract_additional_clinical %>% 
  filter(enttype == 275)
# No records!

# 2. As Read code in the clinical and referral files that contain the HbA1C result ----------------------------

# Define categories according to medical read term description (also see Appendix2_HbA1c_diabetic_control.csv)
HbA1C_good <- c(42360, 13597)
HbA1C_borderline <- c(40463, 29218)
HbA1C_bad <- c(13604, 46079)

# Filter read codes relating to HbA1C control before study end
clinical_HbA1C <- extract_clinical %>% 
  filter(medcode %in% c(HbA1C_good, HbA1C_borderline, HbA1C_bad) & eventdate <= study_end)

# Create categorical variable
clinical_HbA1C <- clinical_HbA1C %>% 
  mutate(control = case_when(medcode %in% HbA1C_good ~ 'good',
                             medcode %in% HbA1C_borderline ~ 'borderline',
                             medcode %in% HbA1C_bad ~ 'bad')) %>% 
  semi_join(patients[, c('patid')], by = 'patid')

clinical_HbA1C %>% 
  tabyl(control)


# Latest entry before follow-up period starts
# not older than study start (this is to make sure the record is not too old & missingness might be informative)

clinical_HbA1C_latest <- clinical_HbA1C %>% 
  filter(eventdate %within% interval(study_start, study_end)) %>% 
  arrange(patid, desc(eventdate)) %>% 
  group_by(patid) %>% 
  filter(row_number() == 1) %>% 
  ungroup() %>% 
  select(patid, control, eventdate) %>% 
  rename(latest_clinical_HbA1C = control, latest_clin_eventdate = eventdate)

# Earliest entry after study period starts
# not older than study end 

clinical_HbA1C_earliest <- clinical_HbA1C %>% 
  filter(eventdate %within% interval(study_start, study_end)) %>% 
  arrange(patid, eventdate) %>% 
  group_by(patid) %>% 
  filter(row_number() == 1) %>% 
  ungroup() %>% 
  select(patid, control, eventdate) %>% 
  rename(earliest_clinical_HbA1C = control, earliest_clin_eventdate = eventdate)

# Latest entry before study period starts
# within 6 months 

clinical_HbA1C_beforestudy <- clinical_HbA1C %>% 
  filter(eventdate %within% interval(study_start - months(6), study_start)) %>% 
  arrange(patid, eventdate) %>% 
  group_by(patid) %>% 
  filter(row_number() == 1) %>% 
  ungroup() %>% 
  select(patid, control, eventdate) %>% 
  rename(beforestudy_clinical_HbA1C = control, beforestudy_clin_eventdate = eventdate)

# 3. As a  test result in the test files ----------------------------------

# 275 HbA1C - diabetic control
HbA1C_tests <-  extract_tests %>% 
  filter(enttype == 275)

# convert to correct variable types
HbA1C_tests <- HbA1C_tests %>% 
  rename(operator =  data1,  value = data2, unit = data3, qualifier = data4, normal_from = data5, normal_to = data6, normal_basis = data7) %>% 
  select(-data8) %>% 
  mutate(operator = as.integer(operator),
         value  = as.numeric(value),
         unit = as.integer(unit),
         qualifier = as.integer(qualifier),
         normal_from  = as.numeric(normal_from),
         normal_to  = as.numeric(normal_to),
         normal_basis = as.integer(normal_basis))

# add factor levels to make it easier to read
HbA1C_tests <- HbA1C_tests %>% 
  left_join(OPR_lookup, by = 'operator') %>% 
  left_join(SUM_lookup, by = 'unit') %>% 
  left_join(TQU_lookup, by = 'qualifier') 

HbA1C_tests <- HbA1C_tests %>% 
  select(patid, eventdate, operator, operator_name, value, unit, unit_name, 
         normal_from, normal_to, everything()) %>% 
  semi_join(patients[, c('patid')], by = 'patid')

# Cleaning
# Exlude tests where value is 0 or below, or invalid
HbA1C_tests <- HbA1C_tests %>% 
  filter(value > 0 & eventdate <= study_end & !is.na(value))

# Determining reference systems of infividual test results 
# DCCT if: medcode says so, or unit is % or %Hb, or value is <=7, or normal range <=7
# ICCT if: medcode says so, or unit is mmol/mol or mmol/mol Hb, or normal range between 42 and 53
HbA1C_tests <-  HbA1C_tests %>% 
  mutate(reference = case_when(medcode %in% c(14053, 19807) | unit_name %in% c('%', '%Hb') ~ 'DCCT',
                               medcode == 96968 | unit_name %in% c('mmol/mol', 'mmol/mol Hb') ~ 'IFCC'),
         reference = ifelse(is.na(reference) & unit_name == 'No Data Entered' & normal_to <= 7, 'DCCT', reference),
         reference = ifelse(is.na(reference) & unit_name == 'No Data Entered' & value <= 7, 'DCCT', reference),
         reference = ifelse(is.na(reference) & unit_name == 'No Data Entered' & between(normal_to, 42, 53), 'IFCC', reference))
    
# Create categorical variable for HbA1C control
# NB if operator has not been entered, will assume it is '='
HbA1C_tests <-  HbA1C_tests %>% 
  filter(!is.na(reference) & eventdate <= study_end) %>% 
  select(patid, eventdate, operator, operator_name, value, reference, medcode, unit) %>% 
  mutate(control = case_when(reference == 'IFCC' & operator_name %in% c('=', '<=', 'Data Not Entered') & value < 53 ~ 'good',
                             reference == 'IFCC' & operator_name == '<' & value <= 53 ~ 'good',
                             reference == 'IFCC' & operator_name %in% c('=', '<=') & value >= 53 & value <= 86 ~ 'borderline',
                             reference == 'IFCC' & operator_name %in% c('=', '>=') & value > 86 ~ 'bad',
                             reference == 'IFCC' & operator_name %in% c('>', '>=', 'Data Not Entered') & value >= 86 ~ 'bad',
                             reference == 'DCCT' & operator_name %in% c('=', '<=', 'Data Not Entered') & value < 7 ~ 'good',
                             reference == 'DCCT' & operator_name == '<' & value <= 7 ~ 'good',
                             reference == 'DCCT' & operator_name %in% c('=', '<=') & value >= 7 & value <= 10 ~ 'borderline',
                             reference == 'DCCT' & operator_name %in% c('=', '>=') & value > 10 ~ 'bad',
                             reference == 'DCCT' & operator_name %in% c('>', '>=', 'Data Not Entered') & value >= 10 ~ 'bad'))


HbA1C_tests %>% 
  tabyl(control)

# Latest entry before follow-up period starts
# not older than study start (this is to make sure the record is not too old & missingness might be informative)

HbA1C_tests_latest <- HbA1C_tests %>% 
  filter(eventdate %within% interval(study_start, study_end)) %>% 
  arrange(patid, desc(eventdate)) %>% 
  group_by(patid) %>% 
  filter(row_number() == 1) %>% 
  ungroup() %>% 
  select(patid, control, eventdate) %>% 
  rename(latest_test = control, latest_test_eventdate = eventdate)

# Earliest entry after study period starts
# not older than study end 

HbA1C_tests_earliest <- HbA1C_tests %>% 
  filter(eventdate %within% interval(study_start, study_end)) %>% 
  arrange(patid, eventdate) %>% 
  group_by(patid) %>% 
  filter(row_number() == 1) %>% 
  ungroup() %>% 
  select(patid, control, eventdate) %>% 
  rename(earliest_test = control, earliest_test_eventdate = eventdate)

# Latest entry before study period starts
# within 6 months 

HbA1C_tests_beforestudy <- HbA1C_tests %>% 
  filter(eventdate %within% interval(study_start - months(6), study_start)) %>% 
  arrange(patid, eventdate) %>% 
  group_by(patid) %>% 
  filter(row_number() == 1) %>% 
  ungroup() %>% 
  select(patid, control, eventdate) %>% 
  rename(beforestudy_test = control, beforestudy_test_eventdate = eventdate)


# 4. Combine and decide on status ---------------------------
# latest before study end 
# not older than study start (this is to make sure the record is not too old & missingness might be informative)

hba1c_bypat <- patients[, c('patid')] %>% 
  left_join(clinical_HbA1C_latest, by = 'patid') %>% 
  left_join(HbA1C_tests_latest, by = 'patid') 

# latest of clinical or test results decides
# if both recorded on the same day, will believe clinical result, as there is less room for error

hba1c_bypat <- hba1c_bypat %>% 
  mutate(HbA1C_control = case_when(is.na(latest_clin_eventdate) & !is.na(latest_test_eventdate) ~ latest_test,
                                   !is.na(latest_clin_eventdate) & is.na(latest_test_eventdate) ~ latest_clinical_HbA1C,
                                   !is.na(latest_clin_eventdate) & !is.na(latest_test_eventdate) & 
                                     latest_clin_eventdate > latest_test_eventdate  ~ latest_clinical_HbA1C,
                                   !is.na(latest_clin_eventdate) & !is.na(latest_test_eventdate) & 
                                     latest_clin_eventdate < latest_test_eventdate  ~ latest_test,
                                   !is.na(latest_clin_eventdate) & !is.na(latest_test_eventdate) & 
                                     latest_clin_eventdate == latest_test_eventdate  ~ latest_clinical_HbA1C))


hba1c_bypat <- hba1c_bypat %>% 
  mutate(HbA1C_control = fct_explicit_na(HbA1C_control, 'Missing'))


hba1c_bypat %>%  tabyl(HbA1C_control)

# at baseline 
# Define as: latest measurement before study start, if that was within 6 months of study start
# if none available, earliest after study start

hba1c_baseline_bypat <- patients[, c('patid')] %>% 
  left_join(clinical_HbA1C_beforestudy, by = 'patid') %>% 
  left_join(HbA1C_tests_beforestudy, by = 'patid')  %>% 
  left_join(clinical_HbA1C_earliest, by = 'patid') %>% 
  left_join(HbA1C_tests_earliest, by = 'patid')  

# latest of clinical or test results decides
# if both recorded on the same day, will believe clinical result, as there is less room for error
hba1c_baseline_bypat <- hba1c_baseline_bypat %>% 
  mutate(HbA1C_control = case_when(is.na(beforestudy_clin_eventdate) & !is.na(beforestudy_test_eventdate) ~ beforestudy_test,
                                 !is.na(beforestudy_clin_eventdate) & is.na(beforestudy_test_eventdate) ~ beforestudy_clinical_HbA1C,
                                 !is.na(beforestudy_clin_eventdate) & !is.na(beforestudy_test_eventdate) & 
                                   beforestudy_clin_eventdate > beforestudy_test_eventdate  ~ beforestudy_clinical_HbA1C,
                                 !is.na(beforestudy_clin_eventdate) & !is.na(beforestudy_test_eventdate) & 
                                   beforestudy_clin_eventdate < beforestudy_test_eventdate  ~ beforestudy_test,
                                 !is.na(beforestudy_clin_eventdate) & !is.na(beforestudy_test_eventdate) & 
                                   beforestudy_clin_eventdate == beforestudy_test_eventdate  ~ beforestudy_clinical_HbA1C))

# Fill NAs with data from after study start
hba1c_baseline_bypat <- hba1c_baseline_bypat %>% 
  mutate(HbA1C_control = ifelse(is.na(HbA1C_control) & is.na(earliest_clin_eventdate) & !is.na(earliest_test_eventdate), earliest_test, HbA1C_control),
         HbA1C_control = ifelse(is.na(HbA1C_control) & !is.na(earliest_clin_eventdate) & is.na(earliest_test_eventdate), earliest_clinical_HbA1C, HbA1C_control),
         HbA1C_control = ifelse(is.na(HbA1C_control) & !is.na(earliest_clin_eventdate) & !is.na(earliest_test_eventdate)
                                & earliest_clin_eventdate > earliest_test_eventdate, earliest_test, HbA1C_control),
         HbA1C_control = ifelse(is.na(HbA1C_control) & !is.na(earliest_clin_eventdate) & !is.na(earliest_test_eventdate)
                                & earliest_clin_eventdate < earliest_test_eventdate, earliest_clinical_HbA1C, HbA1C_control),
         HbA1C_control = ifelse(is.na(HbA1C_control) & !is.na(earliest_clin_eventdate) & !is.na(earliest_test_eventdate)
                                & earliest_clin_eventdate == earliest_test_eventdate, earliest_clinical_HbA1C, HbA1C_control))


hba1c_baseline_bypat <- hba1c_baseline_bypat %>% 
  mutate(HbA1C_control = fct_explicit_na(HbA1C_control, 'Missing'))


hba1c_baseline_bypat %>%  tabyl(HbA1C_control)


# Saving processed files --------------------------------------------------

# 1. All clinical entries 
saveRDS(clinical_HbA1C, str_c(processed_RDS_path, 'patients_HbA1C_clinical_all.rds'))

# 2. All test entries
saveRDS(HbA1C_tests, str_c(processed_RDS_path, 'patients_HbA1C_test_all.rds'))

# 3. latest before study end 
saveRDS(hba1c_bypat, str_c(processed_RDS_path, 'patients_HbA1C_latest.rds'))

# 4. at baseline (definition above)
saveRDS(hba1c_baseline_bypat, str_c(processed_RDS_path, 'patients_HbA1C_at_baseline.rds'))


# Experimenting with cutoff dates -----------------------------------------
# ie how many years of data to take into account before study end date
# and what does this mean for the percentage of patients who have HbA1C recorded

HbA1C_within_x_years <- function(years_before_cutoff){
  
  
  clinical_HbA1C_latest <- clinical_HbA1C %>% 
    filter(eventdate %within% interval(study_end - years(years_before_cutoff) + 1, study_end)) %>% 
    arrange(patid, desc(eventdate)) %>% 
    group_by(patid) %>% 
    filter(row_number() == 1) %>% 
    ungroup() %>% 
    select(patid, control, eventdate) %>% 
    rename(latest_clinical_HbA1C = control, latest_clin_eventdate = eventdate)
  
  HbA1C_tests_latest <- HbA1C_tests %>% 
    filter(eventdate %within% interval(study_end - years(years_before_cutoff) + 1, study_end)) %>% 
    arrange(patid, desc(eventdate)) %>% 
    group_by(patid) %>% 
    filter(row_number() == 1) %>% 
    ungroup() %>% 
    select(patid, control, eventdate) %>% 
    rename(latest_test = control, latest_test_eventdate = eventdate)
  
  hba1c_bypat <- clinical_HbA1C_latest %>% 
    full_join(HbA1C_tests_latest, by = 'patid') %>% 
    right_join(patients[, c('patid')], by = 'patid') 
  
  hba1c_bypat <- hba1c_bypat %>% 
    mutate(HbA1C_control = case_when(is.na(latest_clin_eventdate) & !is.na(latest_test_eventdate) ~ latest_test,
                                     !is.na(latest_clin_eventdate) & is.na(latest_test_eventdate) ~ latest_clinical_HbA1C,
                                     !is.na(latest_clin_eventdate) & !is.na(latest_test_eventdate) & 
                                       latest_clin_eventdate > latest_test_eventdate  ~ latest_clinical_HbA1C,
                                     !is.na(latest_clin_eventdate) & !is.na(latest_test_eventdate) & 
                                       latest_clin_eventdate < latest_test_eventdate  ~ latest_test,
                                     !is.na(latest_clin_eventdate) & !is.na(latest_test_eventdate) & 
                                       latest_clin_eventdate == latest_test_eventdate  ~ latest_clinical_HbA1C))
  
  hba1c_bypat <- hba1c_bypat %>% 
    mutate(HbA1C_control = fct_explicit_na(HbA1C_control, 'Missing'))
  
  hba1c_bypat %>%  tabyl(HbA1C_control)
  
}

HbA1C_within_x_years(1)
HbA1C_within_x_years(2) # current cutoff
HbA1C_within_x_years(3)
HbA1C_within_x_years(5)
HbA1C_within_x_years(10)
