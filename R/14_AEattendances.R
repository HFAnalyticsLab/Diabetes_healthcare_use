# =======================================================
# Project: Diabetes outpatient care
# Purpose: A&E attendances - cleaning, processing and counting
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
patients <- readRDS(str_c(processed_RDS_path, 'patients_clinical_combined.Rds'))

# A&E attendances
hesae_attendances <- readRDS(str_c(raw_RDS_path, 'HES_AE_attendances.Rds'))


# Clean and derive variables ----------------------------------------------

# Recode missing values as NA
# and drop columns we don't need

hesae_attendances <- hesae_attendances %>% 
  mutate(aepatgroup = na_if(aepatgroup, 99),
         aeattendcat = na_if(aeattendcat, 9),
         aearrivalmode = na_if(aearrivalmode, 9),
         aedepttype = na_if(aedepttype, 99),
         aerefsource = na_if(aerefsource, 99),
         aeincloctype = na_if(aeincloctype, 99),
         aeattenddisp = na_if(aeattenddisp, 99)) %>% 
  select(-ethnos)
         
# Cannot define duplicate as we have not provider code or exact arrival time
# CHeck how many patients show up several times on the same day 
hesae_attendances %>% 
  get_dupes(patid, arrivaldate) %>% 
  nrow() / nrow(hesae_attendances)


# Remove A&E attendances after date of death
hesae_attendances <- hesae_attendances %>% 
  left_join(patients[, c('patid', 'ONS_dod')], by = 'patid') %>% 
  filter(is.na(ONS_dod) | arrivaldate < ONS_dod)

# Separate table with appointments during study period only
hesae_attendances_study <- hesae_attendances %>% 
  filter(arrivaldate %within% interval(study_start, study_end))

# Check: how many appointments are for patients in the final study population?
hesae_attendances_study %>% 
  left_join(patients[, c('patid', 'resquality')], by = 'patid') %>% 
  filter(resquality == 1) %>% 
  nrow()


# Derive variables --------------------------------------------------------

hesae_attendances_study <- hesae_attendances_study %>% 
  mutate(seen = ifelse(aeattenddisp %in% c(12,13) | is.na(aeattenddisp), 0, 1),
         category = case_when(aeattendcat == 2 ~ 'planned',
                              aeattendcat %in% c(1, 3) ~ 'unplanned',
                              is.na(aeattendcat) ~ 'unknown'))


# Saving processed files  -------------------------------------------------

saveRDS(hesae_attendances, str_c(processed_RDS_path, 'hesae_attendances_all.Rds'))

saveRDS(hesae_attendances_study, str_c(processed_RDS_path, 'hesae_attendances_study.Rds'))

# Descriptive: number of appointments by patient ---------------------------
# for OP appointments within study period 

hesae_attendances_byPat <- hesae_attendances_study %>% 
  group_by(patid) %>% 
  summarise(AE_planned = sum(seen == 1 & category == 'planned'),
            AE_unplanned = sum(seen == 1 & category == 'unplanned'))


hesae_attendances_byPat %>%
  gather(-patid, key = 'type', value = 'count') %>% 
  ggplot(aes(x = count, group = type, fill = type)) +
  geom_histogram(binwidth = 1) +
  coord_cartesian(xlim = c(0,10)) +
  facet_grid(. ~ type)

# Filter for study population
hesae_attendances_byPat <- hesae_attendances_byPat %>% 
  right_join(patients[, c('patid', 'resquality', 'diabetes_type', 'years_in_study')], by = 'patid') %>% 
  filter(resquality == 1 & diabetes_type %in% c('type1', 'type2')) %>% 
  select(-resquality) %>% 
  mutate_if(is.numeric, ~replace_na(.x, 0))   
  
# Normalise for time spent in study
hesae_attendances_byPat <- hesae_attendances_byPat %>% 
  ungroup() %>% 
  mutate(AE_unplanned_per_year = round(AE_unplanned / years_in_study, 1))

# Create categorical variables (binned unplanned attendance counts)
hesae_attendances_byPat <-  hesae_attendances_byPat %>% 
  mutate(AE_unplanned_cat = cut(AE_unplanned, 
                                breaks = c(0, 1, 2, 3, 4, 5, Inf), 
                                labels = c('None', '1', '2', '3', '4', '5+'),
                                ordered_result = TRUE, include.lowest = TRUE, right = FALSE),
         AE_unplanned_per_year_cat = cut(AE_unplanned_per_year, 
                                         breaks = c(0, 1, 2, 3, 4, 5, Inf), 
                                         labels = c('None', '1', '2', '3', '4', '5+'),
                                ordered_result = TRUE, include.lowest = TRUE, right = FALSE))
# Convert missing to none
hesae_attendances_byPat <- hesae_attendances_byPat %>% 
  mutate_at(vars(AE_unplanned_cat, AE_unplanned_per_year_cat), funs(fct_explicit_na(., 'None')))

# Saving processed files 
saveRDS(hesae_attendances_byPat, str_c(processed_RDS_path, 'patients_AEattendances.rds'))


# Create summary tables -------------------------------------------------
# Means

all_AE_unplanned_means <- hesae_attendances_byPat %>% 
  group_by(diabetes_type) %>% 
  summarise(n = n(),
            mean_count = round(mean(AE_unplanned), 1),
            mean_count_per_year = round(mean(AE_unplanned_per_year), 1)) %>% 
  mutate(subgroup = 'all patients')

not_censored_AE_unplanned_means <-hesae_attendances_byPat %>% 
  group_by(diabetes_type) %>% 
  filter(years_in_study == 2) %>% 
  summarise(n = n(),
            mean_count = round(mean(AE_unplanned), 1),
            mean_count_per_year = round(mean(AE_unplanned_per_year), 1)) %>% 
  mutate(subgroup = 'did not transfer out')

AE_unplanned_means <- all_AE_unplanned_means %>% 
  bind_rows(not_censored_AE_unplanned_means)

write_csv(AE_unplanned_means, str_c(summary_stats_path, 'table2/AEattendances_unplanned_means.csv'))


