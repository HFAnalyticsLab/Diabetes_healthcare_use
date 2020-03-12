# =======================================================
# Project: Diabetes outpatient care
# Purpose: Explanatory statistical models for healthcare utilisation (by type)
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

# Source graph parameters 
source('R/study_params.R')

# Functions ---------------------------------------------------------------

# a function to summarise the coefficients of negative binomial regression models,
# including confidence intervals
summariseNbModel <- function(model){
  
    table <- broom::tidy(model, exponentiate = TRUE) %>% 
      cbind(exp(confint(model))) 
    
    rownames(table) <- NULL
    colnames(table) <- c('Coefficient', 'IRR', 'Std.Error', 'z.value', 'p.value', 'CI_lower', 'CI_upper')
    
    table <- table %>% 
      select(Coefficient, IRR, CI_lower, CI_upper, everything()) %>% 
      mutate_at(vars(IRR, z.value, CI_lower, CI_upper), round, 3)
    
    return(table)
}

# Import data -------------------------------------------------------------

# Modelling data was created in script 16
model_data_T2D <- readRDS(str_c(processed_RDS_path, 'Utilisation_modelling_data_T2D.Rds'))

#####
# Modelling utilisation of people with T2DM: negative binomial models -------------------------------------
#####

# PRIMARY CARE CONSULTATIONS ----------------------------------------------
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

# CMD: Add multimorbidity (count of physical conditions) and depression/anxiety as binary
m_T2D_primcare_3_DEPANXr <- update(m_T2D_primcare_2, . ~ . + mm_count_excl_DEPANXr + DEPANXr )
summary(m_T2D_primcare_3_DEPANXr)

AICtab(m_T2D_primcare_base, m_T2D_primcare_1b, m_T2D_primcare_2, m_T2D_primcare_3_DEPANXr) # full model is the best model
stepAIC(m_T2D_primcare_3_DEPANXr, direction = 'backward') # none eliminated

summariseNbModel(model = m_T2D_primcare_3_DEPANXr) %>% 
  write_csv(str_c(summary_stats_path, 'utilisation_modelling/Coefficients_T2D_Primarycare_DEPANXr.csv'))

# PNC: Add multimorbidity (count of physical conditions) and painful condition as binary
m_T2D_primcare_3_PNC <- update(m_T2D_primcare_2, . ~ . + mm_count_excl_PNC + PNC )
summary(m_T2D_primcare_3_PNC)

AICtab(m_T2D_primcare_base, m_T2D_primcare_1b, m_T2D_primcare_2, m_T2D_primcare_3_PNC) # full model is the best model
stepAIC(m_T2D_primcare_3_PNC, direction = 'backward') # none eliminated

summariseNbModel(model = m_T2D_primcare_3_PNC) %>% 
  write_csv(str_c(summary_stats_path, 'utilisation_modelling/Coefficients_T2D_Primarycare_PNC.csv'))

# Model physical conditions invididually 
m_T2D_primcare_4 <- update(m_T2D_primcare_2, as.formula(str_c('. ~ . + ', str_c(comorbidities, collapse = ' + '))))
summary(m_T2D_primcare_4)

AICtab(m_T2D_primcare_3_DEPANXr, m_T2D_primcare_3_PNC , m_T2D_primcare_4) # model with individual conditions is better
stepAIC(m_T2D_primcare_4, direction = 'backward') # eliminating smoking - minimal improvement

summariseNbModel(model = m_T2D_primcare_4) %>% 
  write_csv(str_c(summary_stats_path, 'utilisation_modelling/Coefficients_T2D_Primarycare_allmm.csv'))


# Outpatient attendances --------------------------------------------------

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

# CMD: Add multimorbidity (count of physical conditions) and depression/anxiety as binary
m_T2D_OPattends_3_DEPANXr  <- update(m_T2D_OPattends_2, . ~ . + mm_count_excl_DEPANXr + DEPANXr )
summary(m_T2D_OPattends_3_DEPANXr)

AICtab(m_T2D_OPattends_base, m_T2D_OPattends_1b, m_T2D_OPattends_2, m_T2D_OPattends_3_DEPANXr) # full model is the best model
stepAIC(m_T2D_OPattends_3_DEPANXr, direction = 'backward') # none eliminated

summariseNbModel(model = m_T2D_OPattends_3_DEPANXr) %>% 
  write_csv(str_c(summary_stats_path, 'utilisation_modelling/Coefficients_T2D_Outpatients_DEPANXr.csv'))


# PNC: Add multimorbidity (count of physical conditions) and painful condition as binary
m_T2D_OPattends_3_PNC <- update(m_T2D_OPattends_2, . ~ . + mm_count_excl_PNC + PNC )
summary(m_T2D_OPattends_3_PNC)

AICtab(m_T2D_OPattends_base, m_T2D_OPattends_1b, m_T2D_OPattends_2, m_T2D_OPattends_3_PNC) # full model is the best model
stepAIC(m_T2D_OPattends_3_PNC, direction = 'backward') # none eliminated

summariseNbModel(model = m_T2D_OPattends_3_PNC) %>% 
  write_csv(str_c(summary_stats_path, 'utilisation_modelling/Coefficients_T2D_Outpatients_PNC.csv'))


# Model physical conditions invididually 
m_T2D_OPattends_4 <- update(m_T2D_OPattends_2, as.formula(str_c('. ~ . + ', str_c(comorbidities, collapse = ' + '))))
summary(m_T2D_OPattends_4)

AICtab(m_T2D_OPattends_3_DEPANXr, m_T2D_OPattends_3_PNC, m_T2D_OPattends_4) # model with individual conditions is better
stepAIC(m_T2D_OPattends_4, direction = 'backward') # eliminating smoking - minimal improvement

summariseNbModel(model = m_T2D_OPattends_4) %>% 
  write_csv(str_c(summary_stats_path, 'utilisation_modelling/Coefficients_T2D_Outpatients_allmm.csv'))


# A&E attendances ---------------------------------------------------------

# CMD: Add multimorbidity (count of physical conditions) and depression/anxiety as binary
m_T2D_AE_1_DEPANXr <- glm.nb(AE_attendances ~ female + age_5ybin_centered + offset(log(years_in_study_cprd))
                     + imd_quintile_numeric + HbA1C_control + smoker + newly_diagnosed_1y + mm_count_excl_DEPANXr + DEPANXr, 
                     data = model_data_T2D)

summary(m_T2D_AE_1_DEPANXr)
summariseNbModel(model = m_T2D_AE_1_DEPANXr) %>% 
  write_csv(str_c(summary_stats_path, 'utilisation_modelling/Coefficients_T2D_A&E_DEPANXr.csv'))

# PNC: Add multimorbidity (count of physical conditions) and painful condition as binary
m_T2D_AE_1_PNC <- glm.nb(AE_attendances ~ female + age_5ybin_centered + offset(log(years_in_study_cprd))
                         + imd_quintile_numeric + HbA1C_control + smoker + newly_diagnosed_1y + mm_count_excl_PNC + PNC, 
                         data = model_data_T2D)

summary(m_T2D_AE_1_PNC)
summariseNbModel(model = m_T2D_AE_1_PNC) %>% 
  write_csv(str_c(summary_stats_path, 'utilisation_modelling/Coefficients_T2D_A&E_PNC.csv'))


m_T2D_AE_2 <- glm.nb(as.formula(str_c('AE_attendances ~ female + age_5ybin_centered + offset(log(years_in_study_cprd)) + imd_quintile_numeric + HbA1C_control + smoker + newly_diagnosed_1y + ', 
                                      str_c(comorbidities, collapse = ' + '))), 
                     data = model_data_T2D)

summary(m_T2D_AE_2)
summariseNbModel(model = m_T2D_AE_2) %>% 
  write_csv(str_c(summary_stats_path, 'utilisation_modelling/Coefficients_T2D_A&E_allmm.csv'))

AICtab(m_T2D_AE_1_DEPANXr, m_T2D_AE_1_PNC, m_T2D_AE_2) # model with individual conditions is better
