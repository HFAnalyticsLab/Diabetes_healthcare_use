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

# Functions ---------------------------------------------------------------

utilisation_theme <- theme_classic() +
  theme(legend.position = 'top',
        legend.text = element_text(color = THF_dark_grey),
        legend.title = element_text(color = THF_dark_grey),
        legend.justification = c(1,0),
        legend.key.size = unit(3, 'mm'),
        legend.spacing.x = unit(1 ,'mm'),
        axis.line = element_line(size = 0.25),
        axis.ticks = element_line(size = 0.25)) +
  THF_theme


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

# Create factor for censoring type 
patients_combined <- patients_combined %>% 
  mutate(censoring_type_cprd = case_when(died_study == 1 ~ 'death',
                                         transfer_out_study == 1 ~ 'transfer out',
                                         died_study == 0 & transfer_out_study == 0 & years_in_study_cprd < 2 ~ 'practice last data collection',
                                         years_in_study_cprd == 2 ~ 'study end'),
         censoring_type_hes = case_when(died_study == 1 ~ 'death',
                                        died_study == 0 & years_in_study_hes < 2 ~ 'death', # died_study is based on what happens first in CPRD
                                        # but some patients transfer out and then die before the end of the study
                                        years_in_study_hes == 2 ~ 'study end'))
  
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
         female = factor(female),
         age_bins_study_SDC = fct_collapse(age_bins_study, '0-19' = c('0-4', '5-9', '10-14', '15-19'),
                                     '80+' = c('80-84', '85+')),
         age_bins_followup_SDC = fct_collapse(age_bins_followup, '0-19' = c('0-4', '5-9', '10-14', '15-19'),
                                           '80+' = c('80-84', '85+')),
         mental_mm_cat_SDC = fct_collapse(mental_mm_cat, '1+' = c('1', '2', '3', '4+')))


saveRDS(patients_combined, str_c(processed_RDS_path, 'patients_clinical_combined.Rds'))
        

# Baseline descriptives ---------------------------------------------------

# For study population (step 2 cohort)
patients_study <-  patients_combined %>% 
  filter(cohort_step2 == 1)

# Variables to summarise
vars_tosummarise <- c("female", "ethnicity", "startage_study", "age_bins_study_SDC", 
                      'censoring_type_cprd', 'censoring_type_hes',
                      'years_in_study_cprd', 'years_in_study_hes',
                      'time_since_diagnosis', 
                      'e2011_urban_rural', 'imd_quintile',
                      "medication", "smoking_status", "BMI_categorical", "HbA1C_control",
                      'mental_mm_cat_SDC', 'physical_mm_cat', 'mm_cat',
                      'HYP', 'PNC', 'HEL','CHD', 'DEPANX', 'CKD', 'THY', 'DIV', 'IBS', 'ATR')

cat_vars_tosummarise <- c('censoring_type_hes', 'e2011_urban_rural',
                          'HYP', 'PNC', 'HEL','CHD', 'DEPANX', 'CKD', 'THY', 'DIV', 'IBS', 'ATR')

table_patients_study <- CreateTableOne(vars = vars_tosummarise, 
                            data = patients_study, 
                            strata = 'diabetes_type', 
                            factorVars = cat_vars_tosummarise,
                            test = FALSE)

table_patients_study_csv <- print(table_patients_study, 
                                  noSpaces = TRUE,
                                  showAllLevels = TRUE) 

write.csv(table_patients_study_csv, str_c(summary_stats_path, 'table1/Table1_cohort2.csv'))



# Study population split by mental health comorbidity ------------------

patients_study_red <- patients_study %>% 
  filter(diabetes_type %in% c('type1', 'type2')) %>% 
  mutate(diabetes_type = fct_drop(diabetes_type))

vars_tosummarise2 <- vars_tosummarise[vars_tosummarise != 'mental_mm_cat_SDC']

table_study_mental_mm <- CreateTableOne(vars = vars_tosummarise2, 
                                        strata = c('mental_mm_cat_SDC','diabetes_type'), 
                                        data = patients_study_red, 
                                        factorVars = cat_vars_tosummarise,
                                        test = FALSE)

table_study_mental_mm_csv <- print(table_study_mental_mm, noSpaces = TRUE)
write.csv(table_study_mental_mm_csv, str_c(summary_stats_path, 'table1/Table1_cohort2_mental_mm.csv'))

table_study_DEPANX <- CreateTableOne(vars = vars_tosummarise2, 
                                        strata = c('DEPANX','diabetes_type'), 
                                        data = patients_study_red, 
                                        factorVars = cat_vars_tosummarise,
                                        test = FALSE)

table_study_DEPANX_csv <- print(table_study_DEPANX, noSpaces = TRUE)
write.csv(table_study_DEPANX_csv, str_c(summary_stats_path, 'table1/Table1_cohort2_DEPANX.csv'))




vars_followup <- vars_study[!is.element(vars_study, c('died_study', 'transfer_out_study', "startage_study", "age_bins_study_SDC"))]
vars_followup <- c(vars_followup, "startage_followup", "age_bins_followup_SDC")
cat_vars_followup <- cat_vars_study[!is.element(cat_vars_study, c('died_study', 'transfer_out_study', "startage_study", "age_bins_study_SDC"))]
cat_vars_followup <-  c(cat_vars_followup, "age_bins_followup_SDC")


table_followup <- CreateTableOne(vars = vars_followup, strata = 'diabetes_type', 
                                 data = patients_followup, factorVars = cat_vars_followup,
                                 test = FALSE)


table_followup_csv <- print(table_followup, noSpaces = TRUE)
write.csv(table_followup_csv, str_c(summary_stats_path, 'table1/191023_Table1_followupstart.csv'))


# 5. Study population split by mental health comorbidity ------------------

patients_study_red <- patients_study %>% 
  filter(diabetes_type %in% c('type1', 'type2')) %>% 
  mutate(diabetes_type = fct_drop(diabetes_type))

table_study_mental_mm <- CreateTableOne(vars = vars_study, strata = c('mental_mm_cat_SDC','diabetes_type'), 
                              data = patients_study_red, factorVars = cat_vars_study,
                              test = FALSE)

table_study_mental_mm_csv <- print(table_study_mental_mm, noSpaces = TRUE)
write.csv(table_study_mental_mm_csv, str_c(summary_stats_path, 'table1/191023_Table1_studystart_mental_mm.csv'))

# 6. Study population spliy by time since diagnosis --------------------------

table_study_time_since_diagnosis <- CreateTableOne(vars = vars_study, strata = c('time_since_diagnosis','diabetes_type'), 
                                        data = patients_study_red, factorVars = cat_vars_study,
                                        test = FALSE)

table_study_time_since_diagnosis_csv <- print(table_study_time_since_diagnosis, noSpaces = TRUE)
write.csv(table_study_time_since_diagnosis_csv, str_c(summary_stats_path, 'table1/191023_Table1_studystart_time_since_diagnosis.csv'))




# Exploring glycaemic control ---------------------------------------------

patients_study_red2 <- patients_study_red %>%
  filter(gender %in% c(1,2)) %>% 
  mutate(gender = factor(gender),
         gender = fct_recode(gender, male = '1', female = '2'),
         HbA1C_control_good = ifelse(HbA1C_control == 'good', 1, 0),
         female = ifelse(gender == 'female', 1, 0),
         imd_quintile_numeric = as.numeric(gsub(' .*', '', imd_quintile)))

patients_study_red2  %>% 
  group_by(gender, diabetes_type, imd_quintile, HbA1C_control) %>% 
  summarise(n = n()) %>% 
  group_by(gender, diabetes_type, imd_quintile) %>% 
  mutate(percent = round(100 * n / sum(n), 1)) %>% 
  write_csv(str_c(summary_stats_path, 'table1/HbA1C_control_variation.csv'))

hba1c_plot <- patients_study_red2  %>% 
  group_by(gender, diabetes_type, imd_quintile_numeric, HbA1C_control) %>% 
  summarise(n = n()) %>% 
  group_by(gender, diabetes_type, imd_quintile_numeric) %>% 
  mutate(percent = round(100 * n / sum(n), 1)) %>% 
           ggplot(aes(x = imd_quintile_numeric, y = percent, fill = gender)) +
  geom_bar(stat = 'identity', position = position_dodge()) +
  facet_grid(diabetes_type ~ HbA1C_control) +
  scale_fill_manual(values = c(THF_red, THF_50pct_light_blue)) +
  ylab('Patients [%]') +
  xlab('IMD quintile') +
  labs(title = 'HbA1C control by gender and IMD') +
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

ggsave(str_c(summary_stats_path, 'table1/HbA1C_control_variation.pdf'), hba1c_plot,  device = 'pdf', width = 8, height = 5)


type1_model_good_control <-  glm(HbA1C_control_good ~ startage_study + female + imd_quintile_numeric + female*imd_quintile_numeric,
                                 data = patients_study_red2[patients_study_red2$diabetes_type == 'type1',],
                                 family = 'binomial')

summary(type1_model_good_control)
exp(cbind(OR = coef(type1_model_good_control), confint(type1_model_good_control)))

type2_model_good_control <-  glm(HbA1C_control_good ~ startage_study + female + imd_quintile_numeric + female*imd_quintile_numeric,
                                 data = patients_study_red2[patients_study_red2$diabetes_type == 'type2',],
                                 family = 'binomial')

summary(type1_model_good_control)
exp(cbind(OR = coef(type1_model_good_control), confint(type1_model_good_control)))

