# =======================================================
# Project: Diabetes outpatient care
# Purpose: Ouptatient appointments - cleaning, processing and counting
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

# Source graph parameters 
source('R/plotting_params.R')

# Import data -------------------------------------------------------------

patients <- readRDS(str_c(processed_RDS_path, 'patients_clinical_combined.Rds'))

# HES outpatient appointments
hesop_appts <- readRDS(str_c(raw_RDS_path, 'HES_outpatients.Rds'))

# Lookup table for treatment specialty names
tretspef_lookup <- read_csv('../data_dictionaries_HES/lookup_tables/tretspef_lookup.csv')


# Clean and derive variables ----------------------------------------------

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

# Exclude appointment without tretspef, firstatt, attended
hesop_appts<- hesop_appts %>% 
  filter(!is.na(tretspef) & !is.na(firstatt) & !is.na(attended))

# Remove appointments scheduled after date of death
hesop_appts<- hesop_appts %>% 
  left_join(patients[, c('patid', 'ONS_dod')], by = 'patid') %>% 
  filter(is.na(ONS_dod) | apptdate < ONS_dod)

# Remove duplicates
hesop_appts <- hesop_appts %>% 
  distinct(patid,             admincat,          apptdate,         atentype,         
           attended,          dnadate,           firstatt,         outcome,           
           priority,          refsourc,          reqdate,          servtype,          
           stafftyp,          wait_ind,          waiting,          HES_yr,            
           tretspef,          mainspef, .keep_all = TRUE)


# Only keep appts for study pop
hesop_appts <- hesop_appts %>% 
  semi_join(patients, by = 'patid')


# Drop columns we don't need
hesop_appts<- hesop_appts %>% 
  select(-apptage, -atentype)


# Create flag indicating whether appointment was in diabetic medicine
# tretspef == 307, 263 (diabetic medicine, paediatric diabetic medicine)
hesop_appts<- hesop_appts %>% 
  mutate(diabetic_medicine = ifelse(tretspef %in% c('307','263'), 1, 0))
  
# Separate table with appointments during study period only
hesop_appts_study <- hesop_appts %>% 
  filter(apptdate %within% interval(study_start, study_end))

# Check: how many appointments are for patients in the final study population?
hesop_appts_study %>% 
  left_join(patients[, c('patid', 'resquality')], by = 'patid') %>% 
  filter(resquality == 1) %>% 
  nrow()

# Saving processed files 
saveRDS(hesop_appts, str_c(processed_RDS_path, 'hesop_appts_all.Rds'))

saveRDS(hesop_appts_study, str_c(processed_RDS_path, 'hesop_appts_study.Rds'))


# Descriptive: number of appointments by patient ---------------------------
# for OP appointments within study period 

hesop_count_byPat <- hesop_appts_study %>% 
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


hesop_count_byPat %>%
  gather(-patid, key = 'type', value = 'count') %>% 
  ggplot(aes(x = count, group = type, fill = type)) +
  geom_histogram(binwidth = 1) +
  coord_cartesian(xlim = c(0,10)) +
  facet_grid(. ~ type)

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

# Saving processed files 
saveRDS(hesop_count_byPat, 'processed_data/patients_OPappointments.rds')


# Descriptives: number of appointments by specialty -----------------------------------------------
# here it's important to specify which patient population we are looking at 

# Study population (including those who died or transferred out during study period)
specialty_count <- hesop_appts_study %>% 
  semi_join(patients[patients$resquality == 1, ], by ='patid') %>% 
  left_join(patients[, c('patid', 'diabetes_type')], by ='patid') %>% 
  filter(diabetes_type %in% c('type1', 'type2')) %>% 
  left_join(tretspef_lookup,  by = 'tretspef') %>%
  mutate(tretspef_name = gsub(" \\(.*", "", tretspef_name), 
         tretspef_name_fct = fct_collapse(factor(tretspef_name), 
                                          'Adult or Paediatric Diabetic Medicine' = c('Paediatric Diabetic Medicine',
                                                                               'Diabetic Medicine'))) %>% 
  group_by(tretspef_name_fct, diabetes_type) %>% 
  summarise(n_appts = n(),
            n_attends = sum(attended  %in% c(5, 6)),
            n_patients = length(unique(patid))) %>% 
  group_by(diabetes_type) %>% 
  mutate(pct_attends = round(100*n_attends/sum(n_attends), 1))

# Top 5 tretspefs for type 1 and type 2 patients separately

top5_type1  <- specialty_count %>% 
  filter(diabetes_type == 'type1') %>%
  arrange(desc(pct_attends)) %>% 
  head(5) %>% 
  mutate(tretspef_name_fct = gsub(" ", "\n", tretspef_name_fct)) %>% 
  ggplot(aes(x = fct_reorder(tretspef_name_fct, desc(pct_attends)), y = pct_attends)) +
  geom_bar(stat='identity', fill = THF_red, width = 0.8) +
  theme_classic() +
  ylab('Percent') +
  labs(title = 'Outpatient attendances', subtitle = 'Patients with type 1 diabetes') +
  theme(axis.title.x = element_blank(),
        plot.margin =margin(t = 5, l = 5, r = 15, b= 5, unit = "mm")) +
  THF_theme 

ggsave('graphs/OP_attendances/OP_tretspefTop5_type1.pdf', top5_type1, device = 'pdf', width = 5.5, height = 5.5)


specialty_count %>% 
  filter(diabetes_type == 'type1') %>%
  arrange(desc(pct_attends)) %>% 
  head(5) %>% 
  write_csv('graphs/OP_attendances/OP_tretspefTop5_type1.csv')


top5_type2  <- specialty_count %>% 
  filter(diabetes_type == 'type2') %>%
  arrange(desc(pct_attends)) %>% 
  head(5) %>% 
  mutate(tretspef_name_fct = gsub(" ", "\n", tretspef_name_fct)) %>% 
  ggplot(aes(x = fct_reorder(tretspef_name_fct, desc(pct_attends)), y = pct_attends)) +
  geom_bar(stat='identity', fill = THF_red, width = 0.8) +
  theme_classic() +
  ylab('Percent') +
  labs(title = 'Outpatient attendances', subtitle = 'Patients with type 1 diabetes') +
  theme(axis.title.x = element_blank(),
        plot.margin =margin(t = 5, l = 5, r = 15, b = 5, unit = "mm")) +
  THF_theme 

ggsave('graphs/OP_attendances/OP_tretspefTop5_type2.pdf', top5_type2, device = 'pdf', width = 5.5, height = 5.5)


specialty_count %>% 
  filter(diabetes_type == 'type2') %>%
  arrange(desc(pct_attends)) %>% 
  head(5) %>% 
  write_csv('graphs/OP_attendances/OP_tretspefTop5_type2.csv')

