# =======================================================
# Project: Diabetes outpatient care
# Purpose: Descriptive analysis of health care utilisation from scripts 10-14
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


# Functions ---------------------------------------------------------------

summariseUtilisation <-  function(data){
  data <-  data %>% 
    summarise(n = n(), 
              mean_count_per_year = round(mean(count_per_year), 1),
              sd_count_per_year = round(sd(count_per_year), 1))
  return(data)
}

quantifyUtilisation <- function(data, grouping_var, path, plot_width){
  
  data_summary <- data %>% 
    group_by_('type', grouping_var) %>% 
    summariseUtilisation()
  
  write_csv(data_summary, paste0(path, '_', grouping_var, '.csv'))
  
  plot <- data_summary %>% 
    ggplot(aes_string(x = 'type', y = 'mean_count_per_year', group = grouping_var, fill = grouping_var)) +
    geom_bar(stat = 'identity', position = position_dodge(), width = 0.8) +
    geom_text(aes(label = mean_count_per_year, y = mean_count_per_year + 2),
              color = THF_dark_grey, size = 3, position = position_dodge(width = 0.8)) +
    labs(title = paste0('Health care utilisation - by ', grouping_var)) +
    scale_x_discrete(limits = axis_order, labels = axis_labels) +
    scale_fill_manual(values = THF_discrete_colour_order[1:length(unique(data[[grouping_var]]))]) +
    ylab('Mean number per year') +
    utilisation_theme 
  
  ggsave(paste0(path,'_', grouping_var, '.png'), plot, width = plot_width, height = 5)
  
}

# Import data -------------------------------------------------------------
# using counts normalised to time spent in study 

patients <- readRDS(str_c(processed_RDS_path, 'patients_clinical_combined.Rds'))

# GP appointments
consultations_bypat <- readRDS(str_c(processed_RDS_path, 'patients_consultations.rds'))

consultations_bypat <-  consultations_bypat %>% 
  filter(appt_length == 'all') %>% 
  select(patid, consult_type, count_per_year) %>% 
  spread(key = consult_type, value = count_per_year) %>% 
  rename('consult_primarycare' = consult, "consult_GP" = GP, "consult_nurse" = nurse)

# Outpatient appointments
hesop_count_byPat <- readRDS(str_c(processed_RDS_path, 'patients_OPappointments.rds'))

hesop_count_byPat <- hesop_count_byPat %>% 
  select(patid, type, count_per_year) %>% 
  spread(key = type, value = count_per_year) %>% 
  select(patid, OP_attended_total) 

# Hospital admissions
admissions_bypat <- readRDS(str_c(processed_RDS_path, 'patients_admissions.rds'))

admissions_bypat <-  admissions_bypat %>% 
  select(patid, emergency_admissions_per_year, elective_admissions_per_year) %>% 
  rename('emergency_admissions' = emergency_admissions_per_year)
  
# Prescriptions
prescripts_bypat <- readRDS(str_c(processed_RDS_path, 'patients_prescriptions.rds'))

prescripts_bypat <-  prescripts_bypat %>% 
  select(patid, n_prescripts_per_year) %>% 
  rename('prescriptions' = n_prescripts_per_year)

# A&E attendances
hesae_attendances_byPat <- readRDS(str_c(processed_RDS_path, 'patients_AEattendances.rds'))

hesae_attendances_byPat <-  hesae_attendances_byPat %>% 
  select(patid, AE_unplanned_per_year) %>% 
  rename('AE_attendances' = AE_unplanned_per_year)


# Join them all together
patients_combined <- patients %>% 
  select(patid, gender, pracid, region, e2011_urban_rural, uts, lcd, cohort_step2, cohort_step3, followup_pop, 
         years_in_study_cprd, years_in_study_hes, startage_study, died_study, transfer_out_study, ONS_dod,
         age_bins_study, age_bins_study_SDC, imd_quintile, ethnicity, diabetes_type, medication, smoking_status, BMI_categorical, HbA1C_control,
         time_since_diagnosis, mm_cat, mm_count, physical_mm_count, physical_mm_cat, mental_mm_cat_SDC, DEPANX) %>% 
  left_join(consultations_bypat, by = 'patid') %>% 
  left_join(hesop_count_byPat, by = 'patid') %>% 
  left_join(admissions_bypat, by = 'patid') %>% 
  left_join(prescripts_bypat, by = 'patid') %>% 
  left_join(hesae_attendances_byPat, by = 'patid')

patients_combined <- patients_combined %>% 
  mutate(female = ifelse(gender == '1', 0, 1),
         imd_quintile_numeric = as.numeric(gsub(' .*', '', imd_quintile)))


patients_combined %>% 
  tabyl(cohort_step2, cohort_step3) %>% 
  adorn_title()

patients_combined %>% 
  filter(diabetes_type == 'type1') %>% 
  tabyl(cohort_step2, cohort_step3) %>% 
  adorn_title()

patients_combined %>% 
  filter(diabetes_type == 'type2') %>% 
  tabyl(cohort_step2, cohort_step3) %>% 
  adorn_title()


# Cleaning ----------------------------------------------------------------

# Replace NAs

patients_combined <- patients_combined %>% 
  mutate_if(is.numeric, funs(replace_na(.,0)))

# Create factor to indicate type of censoring
patients_combined <- patients_combined %>% 
  mutate(censoring_type_cprd = case_when(died_study == 1 ~ 'death',
                                         transfer_out_study == 1 ~ 'transfer out',
                                         died_study == 0 & transfer_out_study == 0 & years_in_study_cprd < 2 ~ 'practice last data collection',
                                         years_in_study_cprd == 2 ~ 'study end'),
         censoring_type_hes = case_when(died_study == 1 ~ 'death',
                                        died_study == 0 & years_in_study_hes < 2 ~ 'death', # died_study is based on what happens first in CPRD
                                        # but some patients transfer out and then die before the end of the study
                                        years_in_study_hes == 2 ~ 'study end'))

patients_combined <- patients_combined %>% 
  mutate(DEPANX = factor(DEPANX),
         female = ifelse(gender == 2, 1, 0),
         female = factor(female))



# Summarising crude utilisation -------------------------------------------

# Variables to summarise
vars_tosummarise <- c('consult_primarycare', "consult_GP", "consult_nurse", "OP_attended_total",
                       "emergency_admissions",  "prescriptions", "AE_attendances")

# Compare utilisation in subgroups
# Cohort step 2: practice is UTS at study start 
# Time in study is between study start and earliest of transfer out, patient death and practice lcd

utilisation_T1D <- patients_combined %>% 
  filter(diabetes_type == 'type1' & cohort_step2 == 1) %>% 
  gather(vars_tosummarise, key = 'type', value = 'count_per_year') 

utilisation_T1D %>% 
  group_by(patid) %>% 
  tabyl(cohort_step3)

utilisation_T2D <- patients_combined %>% 
  filter(diabetes_type == 'type2' & cohort_step2 == 1) %>% 
  gather(vars_tosummarise, key = 'type', value = 'count_per_year') 


utilisation_T2D %>% 
  group_by(patid) %>% 
  tabyl(cohort_step3)

# Step 2 cohort --
# practice UTS at study start

axis_order <- c("consult_primarycare", "consult_GP", "consult_nurse", "prescriptions",
                "OP_attended_total", "AE_attendances",
                "emergency_admissions" )

axis_labels <- c("primary care\nconsultations", "GP\ncontacts", "nurse\ncontacts", "prescriptions",
                "outpatient\nattendances", "A&E\nattendances\n(unplanned)",
                "emergency\nhospital\nadmissions" )

utilisation_theme <- theme_classic() +
  theme(legend.position = 'top',
        legend.title = element_blank(),
        legend.text = element_text(color = THF_dark_grey),
        legend.justification = c(1,0),
        legend.key.size = unit(3, 'mm'),
        legend.spacing.x = unit(1 ,'mm'),
        axis.line = element_line(size = 0.25),
        axis.title.x = element_blank(),
        axis.ticks = element_line(size = 0.25)) +
  THF_theme


utilisation_S2Cohort_T1D <- utilisation_T1D %>% 
  group_by(type) %>% 
  summariseUtilisation()

write_csv(utilisation_S2Cohort_T1D, str_c(summary_stats_path, 'utilisation_crude/Utilisation_T1D_cohort2.csv'))

utilisation_S2Cohort_T1D_plot <- utilisation_S2Cohort_T1D %>% 
  ggplot(aes(x = type, y = mean_count_per_year)) +
  geom_bar(stat = 'identity', position = position_dodge(), 
           fill = THF_red, width = 0.8) +
  geom_text(aes(label = mean_count_per_year, y = mean_count_per_year + 2),
            color = THF_dark_grey, size = 3) +
  labs(title = 'Health care utilisation - T1D (step 2 cohort)') +
  scale_x_discrete(limits = axis_order, labels = axis_labels) +
  ylab('Mean number per year') +
  utilisation_theme 
  

ggsave(str_c(summary_stats_path, 'utilisation_crude/Utilisation_T1D_cohort2.png'), 
       utilisation_S2Cohort_T1D_plot, width = 6, height = 5)

utilisation_S2Cohort_T2D <- utilisation_T2D %>% 
  group_by(type) %>% 
  summariseUtilisation()

write_csv(utilisation_S2Cohort_T2D, str_c(summary_stats_path, 'utilisation_crude/Utilisation_T2D_cohort2.csv'))


utilisation_S2Cohort_T2D_plot <- utilisation_S2Cohort_T2D %>% 
  ggplot(aes(x = type, y = mean_count_per_year)) +
  geom_bar(stat = 'identity', position = position_dodge(), 
           fill = THF_red, width = 0.8) +
  geom_text(aes(label = mean_count_per_year, y = mean_count_per_year + 2),
            color = THF_dark_grey, size = 3) +
  labs(title = 'Health care utilisation - T2D (step 2 cohort)') +
  scale_x_discrete(limits = axis_order, labels = axis_labels) +
  ylab('Mean number per year') +
  utilisation_theme 

ggsave(str_c(summary_stats_path, 'utilisation_crude/Utilisation_T2D_cohort2.png'), utilisation_S2Cohort_T2D_plot, width = 6, height = 5)


# Utilisation by censoring mechanism --

# CPRD data
utilisation_S2Cohort_T1D_cprdcensor <- utilisation_T1D %>% 
  filter(type %in% c('consult_primarycare', 'consult_GP', 'consult_nurse', 'prescriptions')) %>% 
  group_by(type, censoring_type_cprd) %>% 
  summariseUtilisation()

write_csv(utilisation_S2Cohort_T1D_cprdcensor, str_c(summary_stats_path, 'utilisation_crude/Utilisation_T1D_censoring_cprd.csv'))


utilisation_S2Cohort_T1D_cprdcensor_plot <- utilisation_S2Cohort_T1D_cprdcensor %>% 
  ggplot(aes(x = type, y = mean_count_per_year, group = censoring_type_cprd, fill = censoring_type_cprd)) +
  geom_bar(stat = 'identity', position = position_dodge(), width = 0.8) +
  geom_text(aes(label = mean_count_per_year, y = mean_count_per_year + 2),
            color = THF_dark_grey, size = 3, position = position_dodge(width = 0.8)) +
  labs(title = 'Health care utilisation - T1D by type of censoring') +
  scale_x_discrete(limits = axis_order[1:4], labels = axis_labels[1:4]) +
  scale_fill_manual(values = THF_discrete_colour_order[1:4]) +
  ylab('Mean number per year') +
  utilisation_theme 

ggsave(str_c(summary_stats_path, 'utilisation_crude/Utilisation_T1D_censoring_cprd.png'), 
       utilisation_S2Cohort_T1D_cprdcensor_plot, width = 6, height = 5)



utilisation_S2Cohort_T2D_cprdcensor <- utilisation_T2D %>% 
  filter(type %in% c('consult_primarycare', 'consult_GP', 'consult_nurse', 'prescriptions')) %>% 
  group_by(type, censoring_type_cprd) %>% 
  summariseUtilisation()

write_csv(utilisation_S2Cohort_T2D_cprdcensor, str_c(summary_stats_path, 'utilisation_crude/Utilisation_T2D_censoring_cprd.csv'))


utilisation_S2Cohort_T2D_cprdcensor_plot <- utilisation_S2Cohort_T2D_cprdcensor %>% 
  ggplot(aes(x = type, y = mean_count_per_year, group = censoring_type_cprd, fill = censoring_type_cprd)) +
  geom_bar(stat = 'identity', position = position_dodge(), width = 0.8) +
  geom_text(aes(label = mean_count_per_year, y = mean_count_per_year + 3),
            color = THF_dark_grey, size = 3, position = position_dodge(width = 0.8)) +
  labs(title = 'Health care utilisation - T2D by type of censoring') +
  scale_x_discrete(limits = axis_order[1:4], labels = axis_labels[1:4]) +
  scale_fill_manual(values = THF_discrete_colour_order[1:4]) +
  ylab('Mean number per year') +
  utilisation_theme 

ggsave(str_c(summary_stats_path, 'utilisation_crude/Utilisation_T2D_censoring_cprd.png'), 
       utilisation_S2Cohort_T2D_cprdcensor_plot, width = 6, height = 5)


# HES data
utilisation_S2Cohort_T1D_hescensor <- utilisation_T1D %>% 
  filter(type %in% c('OP_attended_total', 'AE_attendances', 'emergency_admissions')) %>% 
  group_by(type, censoring_type_hes) %>% 
  summariseUtilisation()

write_csv(utilisation_S2Cohort_T1D_hescensor, str_c(summary_stats_path, 'utilisation_crude/Utilisation_T1D_censoring_hes.csv'))


utilisation_S2Cohort_T1D_hescensor_plot <- utilisation_S2Cohort_T1D_hescensor %>% 
  ggplot(aes(x = type, y = mean_count_per_year, group = censoring_type_hes, fill = censoring_type_hes)) +
  geom_bar(stat = 'identity', position = position_dodge(), width = 0.8) +
  geom_text(aes(label = mean_count_per_year, y = mean_count_per_year + 0.5),
            color = THF_dark_grey, size = 3, position = position_dodge(width = 0.8)) +
  labs(title = 'Health care utilisation - T1D by type of censoring') +
  scale_x_discrete(limits = axis_order[5:7], labels = axis_labels[5:7]) +
  scale_fill_manual(values = THF_discrete_colour_order[1:2]) +
  ylab('Mean number per year') +
  utilisation_theme 

ggsave(str_c(summary_stats_path, 'utilisation_crude/Utilisation_T1D_censoring_hes.png'), 
       utilisation_S2Cohort_T1D_hescensor_plot, width = 6, height = 5)



utilisation_S2Cohort_T2D_hescensor <- utilisation_T2D %>% 
  filter(type %in% c('OP_attended_total', 'AE_attendances', 'emergency_admissions')) %>% 
  group_by(type, censoring_type_hes) %>% 
  summariseUtilisation()

write_csv(utilisation_S2Cohort_T2D_hescensor, str_c(summary_stats_path, 'utilisation_crude/Utilisation_T2D_censoring_hes.csv'))


utilisation_S2Cohort_T2D_hescensor_plot <- utilisation_S2Cohort_T2D_hescensor %>% 
  ggplot(aes(x = type, y = mean_count_per_year, group = censoring_type_hes, fill = censoring_type_hes)) +
  geom_bar(stat = 'identity', position = position_dodge(), width = 0.8) +
  geom_text(aes(label = mean_count_per_year, y = mean_count_per_year + 0.5),
            color = THF_dark_grey, size = 3, position = position_dodge(width = 0.8)) +
  labs(title = 'Health care utilisation - T2D by type of censoring') +
  scale_x_discrete(limits = axis_order[5:7], labels = axis_labels[5:7]) +
  scale_fill_manual(values = THF_discrete_colour_order[1:2]) +
  ylab('Mean number per year') +
  utilisation_theme 

ggsave(str_c(summary_stats_path, 'utilisation_crude/Utilisation_T2D_censoring_hes.png'), 
       utilisation_S2Cohort_T2D_hescensor_plot, width = 6, height = 5)



# Utilisation by multimorbidity --

# Number of long-term conditions
quantifyUtilisation(data = utilisation_T1D, grouping_var = 'mm_cat', 
                    path = str_c(summary_stats_path, 'utilisation_crude/utilisation_T1D'), plot_width = 8)

quantifyUtilisation(data = utilisation_T2D, grouping_var = 'mm_cat', 
                    path = str_c(summary_stats_path, 'utilisation_crude/utilisation_T2D'), plot_width = 8)

# Number of physical long-term conditions
quantifyUtilisation(data = utilisation_T1D, grouping_var = 'physical_mm_cat', 
                    path = str_c(summary_stats_path, 'utilisation_crude/utilisation_T1D'), plot_width = 8)

quantifyUtilisation(data = utilisation_T2D, grouping_var = 'physical_mm_cat', 
                    path = str_c(summary_stats_path, 'utilisation_crude/utilisation_T2D'), plot_width = 8)

# DEPANX
quantifyUtilisation(data = utilisation_T1D, grouping_var = 'DEPANX', 
                    path = str_c(summary_stats_path, 'utilisation_crude/utilisation_T1D'), plot_width = 6)

quantifyUtilisation(data = utilisation_T2D, grouping_var = 'DEPANX', 
                    path = str_c(summary_stats_path, 'utilisation_crude/utilisation_T2D'), plot_width = 6)

# mental comorbidity
quantifyUtilisation(data = utilisation_T1D, grouping_var = 'mental_mm_cat_SDC', 
                    path = str_c(summary_stats_path, 'utilisation_crude/utilisation_T1D'), plot_width = 6)


quantifyUtilisation(utilisation_T2D, grouping_var = 'mental_mm_cat_SDC', 
                    path = str_c(summary_stats_path, 'utilisation_crude/utilisation_T2D'), plot_width = 6)


# gender
quantifyUtilisation(data = utilisation_T1D, grouping_var = 'female', 
                    path = str_c(summary_stats_path, 'utilisation_crude/utilisation_T1D'), plot_width = 6)


quantifyUtilisation(utilisation_T2D, grouping_var = 'female', 
                    path = str_c(summary_stats_path, 'utilisation_crude/utilisation_T2D'), plot_width = 6)

# hba1c control
quantifyUtilisation(data = utilisation_T1D, grouping_var = 'HbA1C_control', 
                    path = str_c(summary_stats_path, 'utilisation_crude/utilisation_T1D'), plot_width = 8)


quantifyUtilisation(utilisation_T2D, grouping_var = 'HbA1C_control', 
                    path = str_c(summary_stats_path, 'utilisation_crude/utilisation_T2D'), plot_width = 8)


