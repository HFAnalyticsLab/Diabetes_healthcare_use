# Healthcare utilisation by patients with type 1 and type 2 diabetes

#### Project Status: In progress

## Project Description

Diabetes is a common chronic health condition. Care for patients with diabetes accounts for almost 9% of the NHS budget: £8.8 billion a year. The number of cases of diabetes is rising, due an ageing population. Other risk factors, such as diet and physical activity, also contribute to increased demand for diabetes-related primary and secondary care. 

There is little evidence on the variation of health care utilisation by patients with diabetes. It is also unclear what  effect this variation might have on health outcomes. This study aims to describe health care utilisation patterns of diabetes patients in England, across primary care and secondary care.  

The code in this repository is used for data cleaning, to define and describe the study cohort (eg demographic details, clinical characteristics) and to quantify health care utilisation and health outcomes.   

## Data source
We used data from the Clinical Practice Research Datalink (CPRD) linked to Hospital Episode Statistics (HES). ISAC protocol number 19_138(link to study description to be added once available). 

Data used for this analysis were anonymised in line with the ICO's Anonymisation Code of Practice. The data were accessed in The Health Foundation's Secure Data Environment, which is a secure data analysis facility (accredited for the ISO27001 information security standard, and recognised for the NHS Digital Data Security and Protection Toolkit). No information that could directly identify a patient or other individual was used. Variables labelled 'patid' do not refer to NHS IDs or other identifiable patient data.

Read code lists used in this analyis can be found in the 'code_lists' folder.

## How does it work?
As the data used for this analysis is not publically available, the code cannot be used to replicate the analysis on this dataset. However, with modifications the code will be able to be used on other patient-level CPRD extracts. 

### Requirements

These scripts were written in R version (to be added) and RStudio Version 1.1.383. 
The following R packages (available on CRAN) are needed: 

* [**tidyverse**](https://www.tidyverse.org/)
* [**tidylog**](https://cran.r-project.org/web/packages/tidylog/index.html)
* [**janitor**](https://cran.r-project.org/web/packages/janitor/index.html)
* [**lubridate**](https://cran.r-project.org/web/packages/lubridate/vignettes/lubridate.html)

### Getting started
The 'R' folder contains the following scripts:
* '00_import_data.R' - imports and combines data, needs to be run first
* '01_define_study_population.R' - defines study cohort based on inclusion and exclusion criteria, all other scripts depend on the outputs of it
* '02_diabetes_medication.R' - extracts data on diabetes medication and defines drug types
* '03_diabetes_type.R' - extracts diabetes diagnoses and defines diabetes type, additionally depends on script 02
* '04_BMI.R' - extracts height and weight data, calculates numerical and categorical BMI
* '05_HbA1C.R' - extracts HbA1C measurements and defines HbA1C control
* '06_smoking_status.R' - extracts data on smoking status and defines smoking status

## Useful references
* Khalid JM, Raluy-Callado M, Curtis BH, Boye KS, Maguire A, Reaney M. Rates and risk of hospitalisation among patients with type 2 diabetes: Retrospective cohort study using the UK General Practice Research Database linked to English Hospital Episode Statistics. Int J Clin Pract. 2014;68(1):40–8.  
* Kontopantelis E, Springate DA, Reeves D, Ashcroft DM, Rutter M, Buchan I, et al. Glucose, blood pressure and cholesterol levels and their relationships to clinical outcomes in type 2 diabetes: a retrospective cohort study. Diabetologia. 2015;58(3):505–18. 
* Wright AK, Kontopantelis E, Emsley R, Buchan IE, Mamas MA, Sattar N, et al. Cardiovascular Risk and Risk Factor Management in Type 2 Diabetes: A Population-Based Cohort Study Assessing Sex Disparities. Circulation. 2019;44(0):CIRCULATIONAHA.118.039100. 


## Authors
* **Fiona Grimm** - [@fiona_grimm](https://twitter.com/fiona_grimm) - [fiona-grimm](https://github.com/fiona-grimm)
* **Meetali Kakad**  

## License
This project is licensed under the [MIT License](https://github.com/HFAnalyticsLab/Diabetes_outpatients/blob/master/LICENSE).

