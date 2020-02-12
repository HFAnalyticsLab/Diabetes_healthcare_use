# =======================================================
# Project: Diabetes outpatient care
# Purpose: Explanatory statistical models for healthcare utilisation for patients with T2DM
# Author: Fiona Grimm
# =======================================================

library(bbmle) # for AICtab
library(MASS)  # masks dplyr::select -> load tidyverse last

library(tidyverse)
library(lubridate)
library(janitor)
library(tidylog)
library(ggrepel)

# Source file paths: Rds_path
source('R/file_paths.R')

# Source study parameters 
#source('R/study_params.R')

# Source graph parameters 
source('R/plotting_params.R')

# Functions ---------------------------------------------------------------

# ggplot theme
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

# a function to summarise the coefficients of negative binomial regression models,
# including confidence intervals
summariseNbModel <- function(model, type = c('nb', 'cox', 'binomial')){
  
  if(type == 'nb'){
    table <- broom::tidy(model, exponentiate = TRUE) %>% 
      cbind(exp(confint(model))) 
    
    rownames(table) <- NULL
    colnames(table) <- c('Coefficient', 'IRR', 'Std.Error', 'z.value', 'p.value', 'CI_lower', 'CI_upper')
    
    table <- table %>% 
      select(Coefficient, IRR, CI_lower, CI_upper, everything()) %>% 
      mutate_at(vars(IRR, z.value, CI_lower, CI_upper), round, 3)
    
    return(table)} else if(type = 'cox'){
      table <- broom::tidy(model, exponentiate = TRUE) 
      
      rownames(table) <- NULL
      colnames(table) <- c('Coefficient', 'HR', 'Std.Error', 'z.value', 'p.value', 'CI_lower', 'CI_upper')
      
      table <- table %>% 
        select(Coefficient, HR, CI_lower, CI_upper, everything()) %>% 
        mutate_at(vars(HR, z.value, CI_lower, CI_upper), round, 3)
      
      return(table)
    }
}


# Import data -------------------------------------------------------------

# Modelling data was created in script 16
model_data_T1D <- readRDS(str_c(processed_RDS_path, 'Utilisation_modelling_data_T1D.Rds'))
model_data_T2D <- readRDS(str_c(processed_RDS_path, 'Utilisation_modelling_data_T2D.Rds'))

# Prevalence of conditions
mm_prev <- read_csv(str_c(summary_stats_path, 'multimorbidity/MM_prevalence.csv'))

# Keep 20 most prevalent conditions
mm_prev_T1D <- mm_prev %>% 
  filter(diabetes_type ==  'type1' & !is.element(condition, c('DEPANX', 'DEP', 'ANX', 'ANXr'))) %>% 
  arrange(desc(prevalence)) %>% 
  head(20)

mm_prev_T2D <- mm_prev %>% 
  filter(diabetes_type ==  'type2' & !is.element(condition, c('DEPANX', 'DEP', 'ANX', 'ANXr'))) %>% 
  arrange(desc(prevalence)) %>% 
  head(20)

# Modelling utilisation of people with T2DM: negative binomial models -------------------------------------

## PRIMARY CARE CONSULTATIONS
# Base model
m_T2D_primcare_base <- glm.nb(consult_primarycare ~ female + age_5ybin_centered + offset(log(years_in_study_cprd)), 
                                    data = model_data_T2D)
summary(m_T2D_primcare_base)

# Add IMD quintile as factor
m_T2D_primcare_1a <- update(m_T2D_primcare_base, . ~ . + imd_quintile)
summary(m_T2D_primcare_1a) # estimates are similar for every quintile, will treat as continuous

# Add IMD quintile as continuous variable
m_T2D_primcare_1b <- update(m_T2D_primcare_base, . ~ . + imd_quintile_numeric )
summary(m_T2D_primcare_1b)

AICtab(m_T2D_primcare_1a, m_T2D_primcare_1b) 

# Add other clinical co-variates
m_T2D_primcare_2 <- update(m_T2D_primcare_1b, . ~ . + HbA1C_control + smoker + newly_diagnosed_1y )
summary(m_T2D_primcare_2)

# Add multimorbidity: count of physical conditions and depression/anxiety as binary
m_T2D_primcare_3 <- update(m_T2D_primcare_2, . ~ . + physical_mm_count + DEPANXr )
summary(m_T2D_primcare_3)

AICtab(m_T2D_primcare_base, m_T2D_primcare_1b, m_T2D_primcare_2, m_T2D_primcare_3) # full model is the best model
stepAIC(m_T2D_primcare_3, direction = 'backward') # none eliminated

summariseNbModel(model = m_T2D_primcare_3) %>% 
  write_csv(str_c(summary_stats_path, 'utilisation_modelling/Coefficients_T2D_Primarycare.csv'))

# Model physical conditions invididually (top 20 according to prevalence)
m_T2D_primcare_4 <- update(m_T2D_primcare_2, as.formula(str_c('. ~ . + ', str_c(mm_prev_T2D$condition, collapse = ' + '))))
summary(m_T2D_primcare_4)

AICtab(m_T2D_primcare_3, m_T2D_primcare_4) # model with individual conditions is better
stepAIC(m_T2D_primcare_4, direction = 'backward') # eliminating smoking - minimal improvement

summariseNbModel(model = m_T2D_primcare_4) %>% 
  write_csv(str_c(summary_stats_path, 'utilisation_modelling/Coefficients_T2D_Primarycare_20mm.csv'))

## Outpatient attendances
# Base model
m_T2D_OPattends_base <- glm.nb(OP_attended_total ~ female + age_5ybin_centered + offset(log(years_in_study_cprd)), 
                              data = model_data_T2D)
summary(m_T2D_OPattends_base)

# Add IMD quintile as factor
m_T2D_OPattends_1a <- update(m_T2D_OPattends_base, . ~ . + imd_quintile)
summary(m_T2D_OPattends_1a) # estimates are similar for every quintile, will treat as continuous

# Add IMD quintile as continuous variable
m_T2D_OPattends_1b <- update(m_T2D_OPattends_base, . ~ . + imd_quintile_numeric )
summary(m_T2D_OPattends_1b)

AICtab(m_T2D_OPattends_1a, m_T2D_OPattends_1b) 

# Add other clinical co-variates
m_T2D_OPattends_2 <- update(m_T2D_OPattends_1b, . ~ . + HbA1C_control + smoker + newly_diagnosed_1y )
summary(m_T2D_OPattends_2)

# Add multimorbidity: count of physical conditions and depression/anxiety as binary
m_T2D_OPattends_3 <- update(m_T2D_OPattends_2, . ~ . + physical_mm_count + DEPANXr )
summary(m_T2D_OPattends_3)

AICtab(m_T2D_OPattends_base, m_T2D_OPattends_1b, m_T2D_OPattends_2, m_T2D_OPattends_3) # full model is the best model
stepAIC(m_T2D_OPattends_3, direction = 'backward') # none eliminated

summariseNbModel(model = m_T2D_OPattends_3) %>% 
  write_csv(str_c(summary_stats_path, 'utilisation_modelling/Coefficients_T2D_Outpatients.csv'))

# Model physical conditions invididually (top 20 according to prevalence)
m_T2D_OPattends_4 <- update(m_T2D_OPattends_2, as.formula(str_c('. ~ . + ', str_c(mm_prev_T2D$condition, collapse = ' + '))))
summary(m_T2D_OPattends_4)

AICtab(m_T2D_OPattends_3, m_T2D_OPattends_4) # model with individual conditions is better
stepAIC(m_T2D_OPattends_4, direction = 'backward') # eliminating smoking - minimal improvement

summariseNbModel(model = m_T2D_OPattends_4) %>% 
  write_csv(str_c(summary_stats_path, 'utilisation_modelling/Coefficients_T2D_Outpatients_20mm.csv'))

# A&E attendances
m_T2D_AE_1 <- glm.nb(AE_attendances ~ female + age_5ybin_centered + offset(log(years_in_study_cprd))
                     + imd_quintile_numeric + HbA1C_control + smoker + newly_diagnosed_1y + physical_mm_count + DEPANXr, 
                     data = model_data_T2D)

summary(m_T2D_AE_1)
summariseNbModel(model = m_T2D_AE_1) %>% 
  write_csv(str_c(summary_stats_path, 'utilisation_modelling/Coefficients_T2D_A&E.csv'))

m_T2D_AE_2 <- glm.nb(as.formula(str_c('AE_attendances ~ female + age_5ybin_centered + offset(log(years_in_study_cprd)) + imd_quintile_numeric + HbA1C_control + smoker + newly_diagnosed_1y + ', 
                                      str_c(mm_prev_T2D$condition, collapse = ' + '))), 
                     data = model_data_T2D)

summary(m_T2D_AE_2)
summariseNbModel(model = m_T2D_AE_2) %>% 
  write_csv(str_c(summary_stats_path, 'utilisation_modelling/Coefficients_T2D_A&E_20mm.csv'))

AICtab(m_T2D_AE_1, m_T2D_AE_2) # model with individual conditions is better
