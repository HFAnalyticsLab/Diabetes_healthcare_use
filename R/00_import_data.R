# =======================================================
# Project: Diabetes outpatient care
# Purpose: Read in raw CPRD and linked data sets, combine files and save as Rds files
# Author: Fiona Grimm
# Date: 29/08/2019
# =======================================================

library(tidyverse)

# Source file paths: raw_data_path, raw_data_path_linked 
source('R_FG/file_paths.R')

# Patient -----------------------------------------------------------------
extract_patient <- read_tsv(str_c(raw_data_path, '2019_07_24_diabetes_Extract_Patient_001.txt'),
                            col_types = cols(frd = col_date(format = "%d/%m/%Y"),
                                             crd = col_date(format = "%d/%m/%Y"),
                                             CHSdate = col_date(format = "%d/%m/%Y"),
                                             deathdate = col_date(format = "%d/%m/%Y"),
                                             tod = col_date(format = "%d/%m/%Y")))

saveRDS(extract_patient,'raw_data/Extract_patient.Rds')


# ONS mortality -----------------------------------------------------------
death_patient <- read_tsv(str_c(raw_data_path_linked, 'death_patient_19_138.txt'),
                          col_types = cols(dor = col_date(format = "%d/%m/%Y"),
                                           dod = col_date(format = "%d/%m/%Y")))

saveRDS(death_patient, 'raw_data/Linked_death_patient.Rds')


# IMD ---------------------------------------------------------------------
imd_patient <- read_tsv(str_c(raw_data_path_linked, 'patient_imd2015_19_138.txt'))

saveRDS(imd_patient, 'raw_data/Linked_patient_imd.Rds')


# Rural-urban indicator ---------------------------------------------------
rururb_practice <- read_tsv(str_c(raw_data_path_linked,'practice_urban_rural_19_138.txt'))

saveRDS(rururb_practice, 'raw_data/Linked_rururb_practice.Rds')


# Clinical ----------------------------------------------------------------
clinical_files <- str_c(raw_data_path, '2019_07_24_diabetes_Extract_Clinical_00', seq(1:5),'.txt')

extract_clinical <- map(clinical_files, read_tsv, col_types = cols(eventdate = col_date(format = "%d/%m/%Y"),
                                                                   sysdate = col_date(format = "%d/%m/%Y"))) %>% 
  reduce(rbind)

saveRDS(extract_clinical, 'raw_data/Extract_clinical.Rds')


# Therapy -----------------------------------------------------------------
therapy_files <- c(str_c(raw_data_path, '2019_07_24_diabetes_Extract_Therapy_00',
                         seq(1:9), '.txt'),
                   str_c(raw_data_path, '2019_07_24_diabetes_Extract_Therapy_01',
                         c(0, seq(1:5)), '.txt'))

extract_therapy <- map(therapy_files, read_tsv, col_types = cols(eventdate = col_date(format = "%d/%m/%Y"),
                                                                 sysdate = col_date(format = "%d/%m/%Y"),
                                                                 qty = col_double(),
                                                                 numpacks = col_double())) %>% 
  reduce(rbind)

saveRDS(extract_therapy, 'raw_data/Extract_therapy.Rds')


# Test --------------------------------------------------------------------
test_files <- str_c(raw_data_path, '2019_07_24_diabetes_Extract_Test_00', seq(1:6), '.txt')

extract_tests <- map(test_files, read_tsv, col_types = cols(eventdate = col_date(format = "%d/%m/%Y"),
                                                   data1 = col_character(),
                                                   data2 = col_character(),
                                                   data3 = col_character(),
                                                   data4 = col_character(),
                                                   data5 = col_character(),
                                                   data6 = col_character(),
                                                   data7 = col_character(),
                                                   data8 = col_character())) %>% 
  reduce(rbind) 

saveRDS(extract_tests, 'raw_data/Extract_test.Rds')

# Additional clinical details ---------------------------------------------
extract_additional_clinical <- read_tsv(str_c(raw_data_path, '2019_07_24_diabetes_Extract_Additional_001.txt'),
                                col_types = cols(data1 = col_character(),
                                                 data2 = col_character(),
                                                 data3 = col_character(),
                                                 data4 = col_character(),
                                                 data5 = col_character(),
                                                 data6 = col_character(),
                                                 data7 = col_character())) 

saveRDS(extract_additional_clinical, 'raw_data/Extract_additional_clinical.Rds')


# Referral ----------------------------------------------------------------
extract_referral <- read_tsv(str_c(raw_data_path, '2019_07_24_diabetes_Extract_Referral_001.txt'),
                                        col_types = cols(eventdate = col_date(format = "%d/%m/%Y"),
                                                         sysdate = col_date(format = "%d/%m/%Y"))) 

saveRDS(extract_referral, 'raw_data/Extract_additional_referral.Rds')


# Consultation ------------------------------------------------------------
consultation_files <- str_c(raw_data_path, '2019_07_24_diabetes_Extract_Consultation_00', seq(1:5),'.txt')

extract_consultation <- map(consultation_files, read_tsv, col_types = cols(eventdate = col_date(format = "%d/%m/%Y"),
                                                                           sysdate = col_date(format = "%d/%m/%Y"))) %>% 
  reduce(rbind)

saveRDS(extract_consultation, 'raw_data/Extract_consultation.Rds')


# Staff -------------------------------------------------------------------
extract_staff <- read_tsv(str_c(raw_data_path, '2019_07_24_diabetes_Extract_Staff_001.txt')) 

saveRDS(extract_staff, 'raw_data/Extract_staff.Rds')

# Practice ----------------------------------------------------------------
extract_spractice <- read_tsv(str_c(raw_data_path, '2019_07_24_diabetes_Extract_Practice_001.txt'),
                              col_types = cols(lcd = col_date(format = "%d/%m/%Y"),
                                               uts = col_date(format = "%d/%m/%Y")))


saveRDS(extract_spractice, 'raw_data/Extract_practice.Rds')

# HES Patient ------------------------------------------------------------

hes_patient <- read_tsv(str_c(raw_data_path_linked, 'hes_patient_19_138.txt')) 

saveRDS(hes_patient, 'raw_data/HES_patient.Rds')


# HES Outpatients ------------------------------------------------------------
hesop_appts_raw <- read_tsv(str_c(raw_data_path_linked, 'hesop_appointment_19_138.txt'),
                         col_types = cols(patid = col_integer(),
                                          apptdate = col_date(format = "%d/%m/%Y"),
                                          dnadate = col_date(format = "%d/%m/%Y"),
                                          reqdate = col_date(format = "%d/%m/%Y"),
                                          apptage = col_integer()))

hesop_clinical_raw <- read_tsv(str_c(raw_data_path_linked, 'hesop_clinical_19_138.txt'),
                            col_types = cols(patid = col_integer(),
                                             tretspef = col_integer(),
                                             mainspef = col_integer()))


# Join treatment speciality and main speciality columns from hesop_clinical file
hesop_appts <- hesop_appts_raw %>% 
  left_join(hesop_clinical_raw[, c('patid', 'attendkey', 'tretspef','mainspef')], by = c('patid', 'attendkey'))

saveRDS(hesop_appts, 'raw_data/HES_outpatients.Rds')


# HES Admitted Patient Care ------------------------------------------------------------
# Episodes 

hesapc_episodes <- read_tsv(str_c(raw_data_path_linked, 'hes_episodes_19_138.txt'),
                         col_types = cols(admidate = col_date(format = "%d/%m/%Y"),
                                          epistart = col_date(format = "%d/%m/%Y"),
                                          epiend = col_date(format = "%d/%m/%Y"),
                                          discharged = col_date(format = "%d/%m/%Y")))

saveRDS(hesapc_episodes, 'raw_data/HES_APC_episodes.Rds')

hesapc_episodes_diagnoses <- read_tsv(str_c(raw_data_path_linked, 'hes_diagnosis_epi_19_138.txt'),
                            col_types = cols(epistart = col_date(format = "%d/%m/%Y"),
                                             epiend = col_date(format = "%d/%m/%Y")))

hesapc_episodes_diagnoses <- hesapc_episodes_diagnoses %>% 
  select(-ICDx) %>% 
  mutate(d_order = ifelse(d_order %in% c(1:9), str_c('Diag_0', d_order), str_c('Diag_', d_order))) %>% 
  spread(key = 'd_order', value = 'ICD')

saveRDS(hesapc_episodes_diagnoses, 'raw_data/HES_APC_episodes_diagnoses.Rds')

# Spells
hesapc_spells <- read_tsv(str_c(raw_data_path_linked, 'hes_hospital_19_138.txt'),
                            col_types = cols(admidate = col_date(format = "%d/%m/%Y"),
                                             epistart = col_date(format = "%d/%m/%Y"),
                                             epiend = col_date(format = "%d/%m/%Y"),
                                             discharged = col_date(format = "%d/%m/%Y")))

saveRDS(hesapc_spells, 'raw_data/HES_APC_spells.Rds')
