# =======================================================
# Project: Diabetes outpatient care
# Purpose: Primary care/GP appointments - cleaning, processing, defining and counting
# Author: Fiona Grimm
# =======================================================

library(tidyverse)
library(lubridate)
library(janitor)
library(tidylog)
library(tableone)

# Source file paths: Rds_path
source('R/file_paths.R')

# Source study parameters 
source('R/study_params.R')

# Source graph parameters 
source('R/plotting_params.R')

# Import data -------------------------------------------------------------

patients <- readRDS(str_c(processed_RDS_path, 'patients_clinical_combined.Rds'))

# Diabetes type
diabetes_bypat <- readRDS(str_c(processed_RDS_path, 'patients_diabetes.rds'))

# Consultations
extract_consultation <- readRDS(str_c(raw_RDS_path, 'Extract_consultation.Rds'))

# Clinical
extract_clinical <- readRDS(str_c(raw_RDS_path, 'Extract_clinical.Rds'))

# Staff
extract_staff <- readRDS(str_c(raw_RDS_path, 'Extract_staff.Rds'))

# Join consulations and staff role
consultations <-  extract_consultation %>% 
  left_join(extract_staff, by = 'staffid') 


# Cleaning ----------------------------------------------------------------

# Only appts during study period
consultations <-  consultations %>% 
  filter(eventdate %within% interval(study_start, study_end)) 

# Remove appointments scheduled after patient died
consultations<- consultations %>% 
  left_join(patients[, c('patid', 'ONS_dod')], by = 'patid') %>% 
  filter(is.na(ONS_dod) | eventdate < ONS_dod)

# Check: how many consultations are for patients in step 2 and 3 cohorts
consultations %>% 
  left_join(patients[, c('patid', 'cohort_step2')], by = 'patid') %>% 
  filter(cohort_step2 == 1) %>% 
  nrow()

consultations %>% 
  left_join(patients[, c('patid', 'cohort_step3')], by = 'patid') %>% 
  filter(cohort_step3 == 1) %>% 
  nrow()

# Defining primary care consultations -------------------------------------

#   1st definition was from previous multimorbidity work 
# 
#   Face to face consultation codes:	Home consultation codes:		Telephone consultation codes:
#   --------------------------------	------------------------		-----------------------------
#   Clinic,1,							            Home Visit,27,					    Telephone call from a patient,10,
#   Follow-up/routine visit,3,			  Hotel Visit,28,					    Telephone call to a patient,21,
#   Night visit , practice,6,			    Nursing Home Visit,30,			Triage,33,
#   Out of hours, Practice,7,			    Residential Home Visit,31,  Telephone Consultation,55,
#   Surgery consultation,9,				    Twilight Visit,32,
#   Acute visit,11,						        Night Visit,50,
#   Emergency Consultation,18,
#   Initial Post Discharge Review,48,
# 
#   GP codes from rol:				Nurse codes from rol:				    Other clinician codes from rol:
#   ------------------				---------------------				    -------------------------------
#   Senior Partner,1,				  Practice Nurse, 11,					    Physiotherapist,26,
#   Partner,2,						    Other Nursing & Midwifery, 54		Other Health Care Professional, 33
#   Assistant,3,
#   Associate,4, 
#   Locum,7,
#   GP Registrar,8,
#   Sole Practitioner,10,
#   Salaried Partner,47,
#   GP Retainer,50,
#   Other Students,53

# Coding up consultation type, duration and staff type
# according to THF definitions
consultations <- consultations %>%   
  mutate(pracid = patid %% 1000,
         unique_id = str_c(pracid, consid, sep = '-'),
         mode_old = case_when(constype %in% c(1, 3, 6, 7, 9, 11, 18, 48) ~ 'face-to-face',
                              constype %in% c(27, 28, 30, 31, 32, 50) ~ 'homevisit',
                              constype %in% c(10, 21, 33, 55) ~ 'teleconsultation'),
         staff_old = case_when(role %in% c(1, 2, 3, 4, 7, 8, 10, 47, 50, 53) ~ 'GP',
                               role %in% c(11, 54) ~ 'Nurse',
                               role %in% c(26, 33) ~ 'Other staff'),
         consult_old = ifelse(!is.na(mode_old) & !is.na(staff_old), 1, 0)) 

# Second definition is from the report 'General Practice WOrkload in England 2007 to 2014'
# by Chris Salisbury et al 
consultations <- consultations %>%   
  mutate(mode = case_when(constype %in% c(1, 9, 18) ~ 'face-to-face',
                          constype %in% c(3, 6, 11, 24, 27, 28, 30, 31, 32) ~ 'visit',
                          constype %in% c(10, 21, 33, 55) ~ 'teleconsultation'),
         staff = case_when(role %in% c(1, 2, 3, 4, 5, 6, 7, 8, 10, 47, 50) ~ 'GP',
                           role %in% c(11, 13) ~ 'Nurse'),
         consult = ifelse(!is.na(mode) & !is.na(staff), 1, 0))

consultations <- consultations %>%   
  mutate(short_appt = ifelse(duration < 1, 1, 0))

# Check: how many consultations are for patients in the final study population, depending on
# the definition of what a consultation is?
consultations %>% 
  left_join(patients[, c('patid', 'cohort_step2')], by = 'patid') %>% 
  filter(cohort_step2 == 1) %>% 
  filter(consult_old == 1) %>% 
  nrow()

consultations %>% 
  left_join(patients[, c('patid', 'cohort_step2')], by = 'patid') %>% 
  filter(cohort_step2 == 1) %>% 
  filter(consult == 1) %>% 
  nrow()

# How many when short appointments are excluded?

consultations %>% 
  left_join(patients[, c('patid', 'cohort_step2')], by = 'patid') %>% 
  filter(cohort_step2 == 1) %>% 
  filter(consult_old == 1 & short_appt == 0) %>% 
  nrow()

consultations %>% 
  left_join(patients[, c('patid', 'cohort_step2')], by = 'patid') %>% 
  filter(cohort_step2 == 1) %>% 
  filter(consult == 1 & short_appt == 0) %>% 
  nrow()

# Check: how many of each type, depending on definition?
consultations %>% 
  left_join(patients[, c('patid', 'cohort_step2')], by = 'patid') %>% 
  filter(cohort_step2 == 1) %>% 
  filter(consult_old == 1) %>% 
  tabyl(mode_old, staff_old) %>% 
  adorn_title() %>% 
  write_csv(str_c(summary_stats_path, 'table2/PrimaryCare_old_definition.csv'))

consultations %>% 
  left_join(patients[, c('patid', 'cohort_step2')], by = 'patid') %>% 
  filter(cohort_step2 == 1) %>% 
  filter(consult == 1) %>% 
  tabyl(mode, staff) %>% 
  adorn_title() %>% 
  write_csv(str_c(summary_stats_path, 'table2/PrimaryCare_new_definition.csv'))


# Counting appointments per patient ---------------------------------------
# will only do this for Salisbury definition

consultations_bypat <-  consultations %>% 
  group_by(patid) %>% 
  summarise(n_consult_all = sum(consult == 1),
            n_GP_all = sum(consult == 1 & staff == 'GP'),
            n_nurse_all = sum(consult == 1 & staff == 'Nurse'),
            # appts 1min or over
            n_consult_long = sum(consult == 1 & short_appt == 0),
            n_GP_long = sum(consult == 1 & staff == 'GP' & short_appt == 0),
            n_nurse_long = sum(consult == 1 & staff == 'Nurse'& short_appt == 0))


consultations_bypat %>%
  gather(-patid, key = 'type', value = 'count') %>% 
  ggplot(aes(x = count, group = type, fill = type)) +
  geom_histogram(binwidth = 1) +
  coord_cartesian(xlim = c(0,40)) +
  facet_wrap('type', ncol = 3)


# Normalising appt count for time spent in study ---------------------------
# the above currently does not take into account whether a patients dies or transfers out during the study period
# in this step we normalise the number of appointments by patient years in the study
# only do this for patients in the study population

consultations_bypat_study <- consultations_bypat %>% 
  right_join(patients[, c('patid', 'tod', 'ONS_dod', 'resquality', 'diabetes_type')], by = 'patid') %>% 
  filter(resquality == 1 & diabetes_type %in% c('type1', 'type2')) %>% 
  select(-resquality) %>% 
  mutate_if(is.numeric, ~replace_na(.x, 0))

consultations_bypat_study <-  consultations_bypat_study %>% 
  group_by(patid) %>% 
  mutate(censoring_date = min(tod, ONS_dod, study_end, na.rm = TRUE),
         years_in_study = round(as.numeric(censoring_date - study_start) / 365, 2),
         years_in_study = ifelse(years_in_study == 0, 0.01, years_in_study))

consultations_bypat_study_norm <- consultations_bypat_study %>% 
  ungroup() %>% 
  select(-tod, -ONS_dod, -censoring_date) %>% 
  gather(-years_in_study, -patid, -diabetes_type, key = 'type', value = 'count') %>% 
  mutate(count_per_year = round(count / years_in_study, 1))


plot_appts_peryear <- consultations_bypat_study_norm %>%
  ggplot(aes(x = count_per_year, fill = diabetes_type)) +
  geom_histogram(binwidth = 1) +
  coord_cartesian(xlim = c(0,40)) +
  facet_grid(diabetes_type ~ type)


# Create categorical variable for script 14 (descriptives) ----------------


# Create categorical variables (binned appointment counts)
consultations_bypat_study_norm <-  consultations_bypat_study_norm %>% 
  mutate(count_cat = cut(count, breaks = c(0, 2, 4, 8, 12, 24, Inf), labels = c(' 0-1', ' 2-3', ' 4-7', ' 8-11', ' 12-23', ' 24+'),
                         ordered_result = TRUE, include.lowest = TRUE, right = FALSE),
         count_norm_cat = cut(count_per_year, breaks = c(0, 2, 4, 6, 12, 9999), labels = c(' 0-1', ' 2-3', ' 4-5', ' 6-11', ' 12+'),
                         ordered_result = TRUE, include.lowest = TRUE, right = FALSE))
         
consultations_bypat_study_norm <- consultations_bypat_study_norm %>% 
  separate(type, into = c('temp', 'consult_type', 'appt_length')) %>% 
  select(-temp)

# Saving processed files --------------------------------------------------

saveRDS(consultations_bypat_study_norm, str_c(processed_RDS_path, 'patients_consultations.rds'))


# Create summary tables -------------------------------------------------

# Means

all_consult_means <- consultations_bypat_study_norm %>% 
  group_by(diabetes_type, consult_type, appt_length) %>% 
  summarise(n = n(),
            mean_count = round(mean(count), 1),
            mean_count_per_year = round(mean(count_per_patient_year), 1)) %>% 
  mutate(subgroup = 'all patients')

not_censored_consult_means <-consultations_bypat_study_norm %>% 
  group_by(diabetes_type, consult_type, appt_length) %>% 
  filter(years_in_study == 2) %>% 
  summarise(n = n(),
            mean_count = round(mean(count), 1),
            mean_count_per_year = round(mean(count_per_patient_year), 1)) %>% 
  mutate(subgroup = 'did not transfer out')

consult_means <- all_consult_means %>% 
  bind_rows(not_censored_consult_means)

write_csv(consult_means, str_c(summary_stats_path, 'table2/PrimaryCare_means.csv'))


# Binned count over two years (not normalised)
all_count_cat  <-  consultations_bypat_study_norm %>% 
  group_by(diabetes_type, consult_type, appt_length, count_cat) %>% 
  count() %>% 
  group_by(diabetes_type, consult_type, appt_length) %>% 
  mutate(denominator = sum(n),
         percent = round(100* n / sum(n), 1)) %>% 
  mutate(subgroup = 'all patients')

not_censored_count_cat  <-  consultations_bypat_study_norm %>% 
  filter(years_in_study == 2) %>% 
  group_by(diabetes_type, consult_type, appt_length, count_cat) %>% 
  count() %>% 
  group_by(diabetes_type, consult_type, appt_length) %>% 
  mutate(denominator = sum(n),
         percent = round(100* n / sum(n), 1)) %>% 
  mutate(subgroup = 'did not transfer out')

count_cat <- all_count_cat %>% 
  bind_rows(not_censored_count_cat)

write_csv(count_cat, str_c(summary_stats_path, 'table2/PrimaryCare_appt_count.csv'))

# Binned count per person year
all_count_norm_cat  <-  consultations_bypat_study_norm %>% 
  group_by(diabetes_type, consult_type, appt_length, count_norm_cat) %>% 
  count() %>% 
  group_by(diabetes_type, consult_type, appt_length) %>% 
  mutate(denominator = sum(n),
         percent = round(100* n / sum(n), 1)) %>% 
  mutate(subgroup = 'all patients')

not_censored_count_norm_cat  <-  consultations_bypat_study_norm %>% 
  filter(years_in_study == 2) %>% 
  group_by(diabetes_type, consult_type, appt_length, count_norm_cat) %>% 
  count() %>% 
  group_by(diabetes_type, consult_type, appt_length) %>% 
  mutate(denominator = sum(n),
         percent = round(100* n / sum(n), 1)) %>% 
  mutate(subgroup = 'did not transfer out')

count_norm_cat <- all_count_norm_cat %>% 
  bind_rows(not_censored_count_norm_cat)

write_csv(count_norm_cat, str_c(summary_stats_path, 'table2/PrimaryCare_appt_per_year.csv'))


# Visualising binned counts -----------------------------------------------

# 1. All appointments, all patients 
count_plot <-  count_cat %>% 
  filter(appt_length == 'all' & subgroup == 'all patients') %>% 
  ggplot(aes(x = count_cat, y = percent, fill = diabetes_type)) +
  geom_bar(stat = 'identity', width = 0.8, position = position_dodge()) +
  scale_fill_manual(values = c(THF_red, THF_50pct_light_blue),
                    labels = c('Type 1 Diabetes','Type 2 Diabetes')) +
  ylab('Patients [%]') +
  facet_grid(. ~ consult_type, labeller = as_labeller(c('consult' = 'Consultations overall',
                                                      'GP' = 'GP consultations',
                                                      'nurse' = 'Nurse consultations'))) +
  xlab('Number of consultations') +
  labs(title = 'Primary care consulations (over  2 years)') +
  theme_classic() +
  theme(legend.title = element_blank(),
        legend.position = 'top',
        legend.text = element_text(color = THF_dark_grey),
        legend.justification = c(1,0),
        legend.key.size = unit(3, 'mm'),
        legend.spacing.x = unit(1 ,'mm'),
        axis.line = element_line(size = 0.25),
        axis.ticks = element_line(size = 0.25)) +
  THF_theme

ggsave(str_c(summary_stats_path, 'table2/PrimaryCare_appt_count.pdf'), count_plot,  device = 'pdf', width = 8, height =5)

norm_count_plot <-  count_norm_cat %>% 
  filter(appt_length == 'all' & subgroup == 'all patients') %>% 
  ggplot(aes(x = count_norm_cat, y = percent, fill = diabetes_type)) +
  geom_bar(stat = 'identity', width = 0.8, position = position_dodge()) +
  scale_fill_manual(values = c(THF_red, THF_50pct_light_blue),
                    labels = c('Type 1 Diabetes','Type 2 Diabetes')) +
  ylab('Patients [%]') +
  facet_grid(. ~ consult_type, labeller = as_labeller(c('consult' = 'Consultations overall',
                                                        'GP' = 'GP consultations',
                                                        'nurse' = 'Nurse consultations'))) +
  xlab('Number of consultations per year') +
  labs(title = 'Primary care consulations (per year)') +
  theme_classic() +
  theme(legend.title = element_blank(),
        legend.position = 'top',
        legend.text = element_text(color = THF_dark_grey),
        legend.justification = c(1,0),
        legend.key.size = unit(3, 'mm'),
        legend.spacing.x = unit(1 ,'mm'),
        axis.line = element_line(size = 0.25),
        axis.ticks = element_line(size = 0.25)) +
  THF_theme

ggsave(str_c(summary_stats_path, 'table2/PrimaryCare_appt_count_per_year.pdf'), norm_count_plot,  device = 'pdf', width = 8, height =5)


# 2. Sensitivity analysis: all appointments vs. short appointments excluded 
count_plot_apptlength <-  count_cat %>% 
  filter(subgroup == 'all patients') %>% 
  ggplot(aes(x = count_cat, y = percent, fill = appt_length)) +
  geom_bar(stat = 'identity', width = 0.8, position = position_dodge()) +
  scale_fill_manual(values = c(THF_red, THF_50pct_light_blue),
                    labels = c('All','>= 1 min')) +
  ylab('Patients [%]') +
  facet_grid(diabetes_type ~ consult_type, labeller = labeller(diabetes_type = c('type1' = 'Type 1 Diabetes',
                                                                                 'type2' = 'Type 2 Diabetes'),
                                                               consult_type = c('consult' = 'Consultations overall',
                                                                                'GP' = 'GP consultations',
                                                                                'nurse' = 'Nurse consultations'))) +
  xlab('Number of consultations') +
  labs(title = 'Primary care consulations (over  2 years)',
       fill = 'Consultations included') +
  theme_classic() +
  theme(legend.position = 'top',
        legend.text = element_text(color = THF_dark_grey),
        legend.justification = c(1,0),
        legend.key.size = unit(3, 'mm'),
        legend.spacing.x = unit(1 ,'mm'),
        axis.line = element_line(size = 0.25),
        axis.ticks = element_line(size = 0.25)) +
  THF_theme

ggsave(str_c(summary_stats_path, 'table2/PrimaryCare_appt_count_sensitivity_apptlength.pdf'), count_plot_apptlength,  device = 'pdf', width = 8, height =8)

norm_count_plot_apptlength <-  count_norm_cat %>% 
  filter(subgroup == 'all patients') %>% 
  ggplot(aes(x = count_norm_cat, y = percent, fill = appt_length)) +
  geom_bar(stat = 'identity', width = 0.8, position = position_dodge()) +
  scale_fill_manual(values = c(THF_red, THF_50pct_light_blue),
                    labels = c('All','>= 1 min')) +
  ylab('Patients [%]') +
  facet_grid(diabetes_type ~ consult_type, labeller = labeller(diabetes_type = c('type1' = 'Type 1 Diabetes',
                                                                     'type2' = 'Type 2 Diabetes'),
                                                   consult_type = c('consult' = 'Consultations overall',
                                                                    'GP' = 'GP consultations',
                                                                    'nurse' = 'Nurse consultations'))) +
  xlab('Number of consultations per year') +
  labs(title = 'Primary care consulations (per year)',
       fill = 'Consultations included') +
  theme_classic() +
  theme(legend.position = 'top',
        legend.text = element_text(color = THF_dark_grey),
        legend.justification = c(1,0),
        legend.key.size = unit(3, 'mm'),
        legend.spacing.x = unit(1 ,'mm'),
        axis.line = element_line(size = 0.25),
        axis.ticks = element_line(size = 0.25)) +
  THF_theme

ggsave(str_c(summary_stats_path, 'table2/PrimaryCare_appt_count_per_year_sensitivity_apptlength.pdf'), 
       norm_count_plot_apptlength,  device = 'pdf', width = 8, height = 6)


# 3. Sensitivity analysis: all patients vs. patients who transfer out or die excluded 
count_plot_censoring <-  count_cat %>% 
  filter(appt_length == 'all') %>% 
  ggplot(aes(x = count_cat, y = percent, fill = subgroup)) +
  geom_bar(stat = 'identity', width = 0.8, position = position_dodge()) +
  scale_fill_manual(values = c(THF_red, THF_50pct_light_blue),
                    labels = c('All','In the study for 2 years')) +
  ylab('Patients [%]') +
  facet_grid(diabetes_type ~ consult_type, labeller = labeller(diabetes_type = c('type1' = 'Type 1 Diabetes',
                                                                                 'type2' = 'Type 2 Diabetes'),
                                                               consult_type = c('consult' = 'Consultations overall',
                                                                                'GP' = 'GP consultations',
                                                                                'nurse' = 'Nurse consultations'))) +
  xlab('Number of consultations') +
  labs(title = 'Primary care consulations (over  2 years)',
       fill = 'Patients included') +
  theme_classic() +
  theme(legend.position = 'top',
        legend.text = element_text(color = THF_dark_grey),
        legend.justification = c(1,0),
        legend.key.size = unit(3, 'mm'),
        legend.spacing.x = unit(1 ,'mm'),
        axis.line = element_line(size = 0.25),
        axis.ticks = element_line(size = 0.25)) +
  THF_theme

ggsave(str_c(summary_stats_path, 'table2/PrimaryCare_appt_count_sensitivity_censoring.pdf'), 
       count_plot_censoring,  device = 'pdf', width = 8, height =8)

norm_count_plot_censoring <-  count_norm_cat %>% 
  filter(appt_length == 'all') %>% 
  ggplot(aes(x = count_norm_cat, y = percent, fill = subgroup)) +
  geom_bar(stat = 'identity', width = 0.8, position = position_dodge()) +
  scale_fill_manual(values = c(THF_red, THF_50pct_light_blue),
                    labels = c('All','In the study for 2 years')) +
  ylab('Patients [%]') +
  facet_grid(diabetes_type ~ consult_type, labeller = labeller(diabetes_type = c('type1' = 'Type 1 Diabetes',
                                                                                 'type2' = 'Type 2 Diabetes'),
                                                               consult_type = c('consult' = 'Consultations overall',
                                                                                'GP' = 'GP consultations',
                                                                                'nurse' = 'Nurse consultations'))) +
  xlab('Number of consultations per year') +
  labs(title = 'Primary care consulations (per year)',
       fill = 'Patients included') +
  theme_classic() +
  theme(legend.position = 'top',
        legend.text = element_text(color = THF_dark_grey),
        legend.justification = c(1,0),
        legend.key.size = unit(3, 'mm'),
        legend.spacing.x = unit(1 ,'mm'),
        axis.line = element_line(size = 0.25),
        axis.ticks = element_line(size = 0.25)) +
  THF_theme

ggsave(str_c(summary_stats_path, 'table2/PrimaryCare_appt_count_per_year_sensitivity_censoring.pdf'), 
       norm_count_plot_censoring,  device = 'pdf', width = 8, height = 7)





# Diabetes annual check ---------------------------------------------------

# Additional clinical details 
extract_additional_clinical <- readRDS(str_c(raw_RDS_path, 'Extract_additional_clinical.Rds'))

diab_annual_check <- extract_additional_clinical %>% 
  filter(enttype == 22) %>% 
  rename(prog_type = data1, checkup_type = data2) %>% 
  select(patid, adid, prog_type, checkup_type) %>% 
  left_join(extract_clinical[, c('patid', 'adid', 'eventdate')], by = c('patid', 'adid')) 


diab_annual_check <-  diab_annual_check %>% 
  filter(eventdate %within% interval(study_start, study_end)) 

# counting the number of checks by patient
diab_annual_check_bypat  <- diab_annual_check %>% 
  group_by(patid) %>% 
  summarise(n_checks = n()) 

# Join in patients 
diab_annual_check_bypat <-  diab_annual_check_bypat %>% 
  right_join(patients[, c('patid', 'tod', 'ONS_dod', 'cohort_step2', 'diabetes_type', 'years_in_study')], by = 'patid') %>% 
  filter(cohort_step2 == 1 & diabetes_type %in% c('type1', 'type2')) %>% 
  select(-cohort_step2) %>%  
  mutate(n_checks = replace_na(n_checks, 0))

# correct for years in study
diab_annual_check_bypat <- diab_annual_check_bypat %>% 

  mutate(n_checks_per_year = round(n_checks / years_in_study, 1))


# Create categorical variables (binned appointment counts)
diab_annual_check_bypat <-  diab_annual_check_bypat %>% 
  mutate(diab_annual_check_cat = cut(n_checks, breaks = c(0, 1, 2, Inf), labels = c('None', '1', '2 or more'),
                                     ordered_result = TRUE, include.lowest = TRUE, right = FALSE))

# Create summary tables -------------------------------------------------

# Means

all_checkup_means <- diab_annual_check_bypat %>% 
  group_by(diabetes_type) %>% 
  summarise(n = n(),
            mean_checkup_count = round(mean(n_checks), 1),
            mean_checkup_count_per_year = round(mean(n_checks_per_year), 1)) %>% 
  mutate(subgroup = 'all patients')

not_censored_checkup_means <-diab_annual_check_bypat %>% 
  group_by(diabetes_type) %>% 
  filter(years_in_study == 2) %>% 
  summarise(n = n(),
            mean_checkup_count = round(mean(n_checks), 1),
            mean_checkup_count_per_year = round(mean(n_checks_per_year), 1)) %>% 
  mutate(subgroup = 'did not transfer out')

checkup_means <- all_checkup_means %>% 
  bind_rows(not_censored_checkup_means)

write_csv(checkup__means, str_c(summary_stats_path, 'table2/DiabetesCheckup_means.csv'))


# Saving processed files --------------------------------------------------

saveRDS(diab_annual_check_bypat, str_c(processed_RDS_path, 'patients_annual_diabetes_checks.rds'))



