# Define study parameters -------------------------------------------------

# Year 1 and 2 to quantify utilisation and other covariates
study_start <- ymd('2015-12-01')
study_end <- ymd('2017-11-30')

# Year 3 for health outcomes
followup_start <- ymd('2017-12-01')
followup_end <-  ymd('2018-11-30')

# Comorbidities
comorbidities <- c("ALC", "ANO", "AST", "ATR", "BLI", 
                   "BRO", "CAN", "CHD", "CKD", "CLD", 
                   "CON", "COP", "DEPANXr", "DEM", "DIV", 
                   "EPI", "HEF", "HEL", "HYP", "IBD", 
                   "IBS", "LEA", "MIG", "MSC", "OPS", 
                   "PNC", "PRK", "PRO", "PSO", "PVD", 
                   "RHE", "SCZ", "SIN", "STR", "THY")