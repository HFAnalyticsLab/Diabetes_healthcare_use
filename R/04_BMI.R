# =======================================================
# Project: Diabetes outpatient care
# Purpose: Clean weight and height data to calculate BMI and create categorical BMI variable
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

# Import diabetes code list
obesity_codes <- read_csv(str_c(code_list_path, 'Appendix3_obesity.csv'))

# Additional clinical details 
extract_additional_clinical <- readRDS(str_c(raw_RDS_path, 'Extract_additional_clinical.Rds'))

# Clinical data
extract_clinical <- readRDS(str_c(raw_RDS_path, 'Extract_clinical.Rds'))

# Calculating BMI ---------------------------------------------------------

# Extract all relevant entities for height and weight from additional clinical details

# Note: weight entity contains a column for BMI, but will aim to re-derive this 
# from weight and height

# Add year of birth as cleaning will be done separately for <18s and >18s
extract_weight <- extract_additional_clinical %>% 
  filter(enttype == 13) %>% 
  rename(weight_kg =  data1,  weight_centile = data2, BMI = data3) %>% 
  select(patid, adid, weight_kg, BMI) %>% 
  mutate(weight_kg = as.numeric(weight_kg),
         BMI = as.numeric(BMI)) %>% 
  left_join(extract_clinical[, c('patid', 'adid', 'eventdate')], by = c('patid', 'adid')) %>% 
  left_join(patients[,c('patid', 'yob')], by = 'patid')

extract_height <- extract_additional_clinical %>% 
  filter(enttype == 14)  %>% 
  rename(height_m =  data1,  height_centile = data2) %>% 
  select(patid, adid, height_m) %>% 
  mutate(height_m = as.numeric(height_m)) %>% 
  left_join(extract_clinical[, c('patid', 'adid', 'eventdate')], by = c('patid', 'adid')) %>% 
  left_join(patients[,c('patid', 'yob')], by = 'patid')


# Cleaning ----------------------------------------------------------------

# Valid values for weight
# under 18s: between 10 and 250 kg
# over 18s: between 25 and 250kg
# Valid value for BMI: not 0, not greater than 100
extract_weight <-  extract_weight %>% 
  mutate(age = year(eventdate) - yob) %>% 
  filter(!(weight_kg == 0 & is.na(BMI)) & !(weight_kg == 0 & BMI == 0)) %>% 
  filter(!(weight_kg > 250)) %>%
  filter(!(age >= 18 & weight_kg < 25) & !(age < 18 & weight_kg < 10)) %>% 
  mutate(BMI = ifelse(BMI == 0, NA, BMI),
         BMI = ifelse(BMI > 100, NA, BMI))

# visualise distributions to sense check
extract_weight %>% 
  ggplot(aes(x = weight_kg)) +
  geom_density()

extract_weight %>% 
  ggplot(aes(x = BMI)) +
  geom_density()

# Convert all values above 100 from cm to m
# under 18s: above 60 cm
# over 18s: above 1.30m
# Valid values for height between 0.6 and 2.20 m
extract_height <- extract_height %>% 
  mutate(age = year(eventdate) - yob) %>% 
  mutate(height_m = ifelse(height_m >= 100, height_m/100, height_m)) %>% 
  filter(height_m != 0 & height_m <= 2.2) %>% 
  filter(!(age >= 18 & height_m < 1.3) & !(age < 18 & height_m < 0.6)) 

# visualise distributions to sense check
extract_height %>% 
  ggplot(aes(x = height_m)) +
  geom_density()

# Join weight and height tables 
extract_weight_height <- extract_weight %>% 
  left_join(extract_height[,c('patid', 'eventdate', 'height_m')], by = c('patid', 'eventdate')) 
  

# 1. Fill in missing height values with the height from the measurement before
# NB this is slow!
extract_weight_height <-  extract_weight_height %>% 
  mutate(height_filled = ifelse(is.na(height_m), 1, 0)) %>% 
  arrange(patid, eventdate) %>% 
  group_by(patid) %>% 
  fill(height_m, .direction = 'down')

# 2. If there was no height measurement before, fill it with the next valid height after
# NB this is slow!
extract_weight_height <-  extract_weight_height %>% 
  arrange(patid, eventdate) %>% 
  group_by(patid) %>% 
  fill(height_m, .direction = 'up')

# 3. Calculate BMI from height and weight and create categories
extract_weight_height <- extract_weight_height %>% 
  ungroup() %>% 
  mutate(BMI_calc =  round(weight_kg / (height_m * height_m), 1))

# 4. For the entries where no heigh was available before or after, we will take 
# the pre-calculated BMI from the additional clinical details table 
extract_weight_height <- extract_weight_height %>% 
  mutate(BMI_filled = ifelse(is.na(height_m), 1, 0),
         BMI_calc =  ifelse(is.na(BMI_calc), round(BMI, 1), BMI_calc),
         BMI_categorical = cut(BMI_calc, breaks = c(0, 18.5, 25, 30, 35, 100), 
                               labels = c('<18.5', '18.5-24.9', '25-29.9', '30-34.9', '>35'),
                               ordered_result = TRUE, right = FALSE)) 

BMI_all_records <- extract_weight_height %>% 
  semi_join(patients[, c('patid')], by = 'patid')


# BMI per patient ---------------------------------------------------------
# latest before study end 
# not older than study start (this is to make sure the record is not too old & missingness might be informative)

BMI_bypat_latest <-  BMI_all_records %>% 
  filter(eventdate %within% interval(study_start, study_end)) %>% 
  arrange(patid, desc(eventdate)) %>% 
  group_by(patid) %>% 
  filter(row_number() == 1) %>% 
  ungroup() %>% 
  select(patid, BMI_calc, BMI_categorical, BMI_filled, eventdate) %>% 
  right_join(patients[, c('patid')], by = 'patid')


BMI_bypat_latest <- BMI_bypat_latest %>% 
  mutate(BMI_categorical = fct_explicit_na(BMI_categorical, 'Missing'))

BMI_bypat_latest %>% tabyl(BMI_categorical)

# earliest after study start
# not not later than study end

BMI_bypat_earliest <-  BMI_all_records %>% 
  filter(eventdate %within% interval(study_start, study_end)) %>% 
  arrange(patid, eventdate) %>% 
  group_by(patid) %>% 
  filter(row_number() == 1) %>% 
  ungroup() %>% 
  select(patid, BMI_calc, BMI_filled, BMI_categorical, eventdate) %>% 
  right_join(patients[, c('patid')], by = 'patid')


BMI_bypat_earliest <- BMI_bypat_earliest %>% 
  mutate(BMI_categorical = fct_explicit_na(BMI_categorical, 'Missing')) %>% 
  rename(BMI_calc_study = BMI_calc)

BMI_bypat_earliest %>% tabyl(BMI_categorical)

# BMI at baseline ---------------------------------------------------------
# Define as: latest measurement before study start, if that was within 6 months of study start
# if none available, earliest after study start

BMI_before_study <- BMI_all_records %>% 
  filter(eventdate %within% interval(study_start - months(6), study_start)) %>% 
  arrange(patid, desc(eventdate)) %>% 
  group_by(patid) %>% 
  filter(row_number() == 1) %>% 
  ungroup() %>%  
  select(patid, BMI_calc, BMI_categorical, BMI_filled, eventdate) %>% 
  right_join(patients[, c('patid')], by = 'patid')

BMI_at_baseline <-  BMI_before_study %>% 
  left_join(BMI_bypat_earliest[, c('patid', 'BMI_calc_study')], by = 'patid')

BMI_at_baseline <- BMI_at_baseline %>% 
  mutate(BMI_calc = ifelse(is.na(BMI_calc), BMI_calc_study, BMI_calc),
         BMI_categorical = cut(BMI_calc, breaks = c(0, 18.5, 25, 30, 35, 100), 
                               labels = c('<18.5', '18.5-24.9', '25-29.9', '30-34.9', '>35'),
                               ordered_result = TRUE, right = FALSE))


BMI_at_baseline <- BMI_at_baseline %>% 
  mutate(BMI_categorical = fct_explicit_na(BMI_categorical, 'Missing')) 

BMI_at_baseline %>% tabyl(BMI_categorical)


# Saving processed files --------------------------------------------------

# 1. all BMI records
saveRDS(BMI_all_records, str_c(processed_RDS_path, 'patients_BMI_all.rds'))

# 2. latest before study end 
saveRDS(BMI_bypat_latest, str_c(processed_RDS_path, 'patients_BMI_latest.rds'))

# 3. earliest after study start
saveRDS(BMI_bypat_earliest, str_c(processed_RDS_path, 'patients_BMI_earliest.rds'))

# 4. at baseline (definition above)
saveRDS(BMI_at_baseline, str_c(processed_RDS_path, 'patients_BMI_at_baseline.rds'))


# Experimenting with cutoff dates -----------------------------------------
# depending on number of years of data before study end taken into account
# what percentage of patients has a BMI recorded?

BMI_within_x_years <- function(years_before_cutoff){
    
  BMI_bypat_latest <-  BMI_all_records %>% 
    filter(eventdate %within% interval(study_end - years(years_before_cutoff) + 1, study_end)) %>% 
    arrange(patid, desc(eventdate)) %>% 
    group_by(patid) %>% 
    filter(row_number() == 1) %>% 
    ungroup() %>% 
    select(patid, BMI_calc, BMI_categorical, eventdate) %>% 
    right_join(patients[, c('patid')], by = 'patid')
  
  BMI_bypat <- BMI_bypat_latest %>% 
    mutate(BMI_categorical = fct_explicit_na(BMI_categorical, 'Missing'))
  
  BMI_bypat %>% tabyl(BMI_categorical)
}

BMI_within_x_years(1)
BMI_within_x_years(2) # current cutoff
BMI_within_x_years(3)
BMI_within_x_years(5)
BMI_within_x_years(10)
