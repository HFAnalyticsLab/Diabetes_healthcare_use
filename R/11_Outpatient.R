# =======================================================
# Project: Diabetes outpatient care
# Purpose: Ouptatient appointments - - cleaning, processing and counting
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

# HES outpatient appointments
hesop_appts <- readRDS('raw_data/HES_outpatients.Rds')

# Cleaning ----------------------------------------------------------------

## Recode missing values as NA
# Clean

hesop_appts <- hesop_appts %>% 
  mutate(admincat = na_if(admincat, 99),
         apptage = ifelse(apptage %in% c('7001', '7002', '7003', '7004', '7005', '7006', '7007'), 0, apptage),
         atentype = na_if(atentype, 13),
         attended = na_if(attended, 9),
         firstatt = ifelse(firstatt %in% c('X', '9'), NA, firstatt),
         firstatt = as.integer(firstatt),
         outcome = na_if(outcome, 9),
         priority = na_if(priority, 9),
         refsourc = na_if(refsourc, 99),
         servtype = na_if(servtype, 9),
         stafftyp = ifelse(stafftyp %in% c(9, 99), NA, stafftyp)) 

# Only keep appts for study pop
hesop_appts <- hesop_appts %>% 
  semi_join(patients, by = 'patid')

# Remove duplicates
hesop_appts <- hesop_appts %>% 
  distinct(patid,             admincat,          apptdate,         atentype,         
           attended,          dnadate,           firstatt,         outcome,           
           priority,          refsourc,          reqdate,          servtype,          
           stafftyp,          wait_ind,          waiting,          HES_yr,            
           tretspef,          mainspef, .keep_all = TRUE)

# Exclude appointment without tretspef, firstatt, attended
hesop_appts<- hesop_appts %>% 
  filter(!is.na(tretspef) & !is.na(firstatt) & !is.na(attended))

# Only keep appointments during study 
hesop_appts<- hesop_appts %>% 
  filter(apptdate %within% interval(study_start, study_end))

# Drop columns we don't need
hesop_appts<- hesop_appts %>% 
  select(-apptage, -atentype)

# Appointments after date of death
hesop_appts<- hesop_appts %>% 
  left_join(patients[, c('patid', 'ONS_dod')], by = 'patid') %>% 
  filter(is.na(ONS_dod) | apptdate < ONS_dod)

# Deriving variables ------------------------------------------------------

# Flag if appointment was in diabetic medicine
# tretspef == 307, 263 (diabetic medicine, paediatric diabetic medicine)
hesop_appts<- hesop_appts %>% 
  mutate(diabetic_medicine = ifelse(tretspef %in% c('307','263'), 1, 0))
  
# Saving processed files --------------------------------------------------
saveRDS(hesop_appts, 'processed_data/hesop_appts.Rds')


# Descriptives ------------------------------------------------------------

hesop_count_byPat <- hesop_appts %>% 
  group_by(patid) %>% 
  summarise(OP_attended_first = sum(attended == 5 & firstatt %in% c(1,3)),
            OP_attended_subs = sum(attended == 5 & firstatt %in% c(2,4)),
            OP_attended_total = sum(attended == 5),
            OP_sched_first = sum(firstatt %in% c(1,3)),
            OP_sched_subs = sum(firstatt %in% c(2,4)),
            OP_sched_total = n(),
            OP_attended_first_diab = sum(attended == 5 & firstatt %in% c(1,3) & diabetic_medicine == 1),
            OP_attended_subs_diab = sum(attended == 5 & firstatt %in% c(2,4) & diabetic_medicine == 1),
            OP_attended_total_diab = sum(attended == 5 & diabetic_medicine == 1))

# Create categorical variables (binned appointment counts)
hesop_count_byPat <-  hesop_count_byPat %>% 
  mutate(OP_attended_cat = cut(OP_attended_total, breaks = c(0, 1, 3, 5, 9, 13, 25, Inf), labels = c('None', '1-2', '3-4', '5-8', '9-12', '13-24', 'Over 25'),
                           ordered_result = TRUE, include.lowest = TRUE, right = FALSE),
         OP_attended_diab_cat = cut(OP_attended_total_diab, breaks = c(0, 1, 3, 5, 9, 13, 25, Inf), labels = c('None', '1-2', '3-4', '5-8', '9-12', '13-24',  'Over 25'),
                      ordered_result = TRUE, include.lowest = TRUE, right = FALSE))


hesop_count_byPat <- hesop_count_byPat %>% 
  right_join(patients[, c('patid')], by = 'patid') 

# Convert missing to none
hesop_count_byPat <- hesop_count_byPat %>% 
  mutate_at(vars(OP_attended_cat, OP_attended_diab_cat), funs(fct_explicit_na(., 'None')))

  # Saving processed files --------------------------------------------------

saveRDS(hesop_count_byPat, 'processed_data/patients_OPappointments.rds')




