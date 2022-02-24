# Healthcare utilisation by patients with type 1 and type 2 diabetes

#### Project Status: copmlete

## Project Description

Diabetes is a common chronic health condition. Care for patients with diabetes accounts for almost 9% of the NHS budget: £8.8 billion a year. The number of cases of diabetes is rising, due an ageing population. Other risk factors, such as diet and physical activity, also contribute to increased demand for diabetes-related primary and secondary care. 

There is little evidence on the variation of health care utilisation by patients with diabetes. It is also unclear what  effect this variation might have on health outcomes. This study aims to describe health care utilisation patterns of diabetes patients in England, across primary care and secondary care.  

The code in this repository is used for data cleaning, to define and describe the study cohort (eg demographic details, clinical characteristics) and to quantify health care utilisation and health outcomes.   

## Data source
We used data from the Clinical Practice Research Datalink (CPRD) linked to Hospital Episode Statistics (HES), [ISAC protocol number 19_138](https://www.cprd.com/protocol/variation-healthcare-utilisation-across-primary-and-secondary-care-patients-type-1-and-type). 

Data used for this analysis were anonymised in line with the ICO's Anonymisation Code of Practice. The data were accessed in The Health Foundation's Secure Data Environment, which is a secure data analysis facility (accredited for the ISO27001 information security standard, and recognised for the NHS Digital Data Security and Protection Toolkit). No information that could directly identify a patient or other individual was used. Variables labelled 'patid' do not refer to NHS IDs or other identifiable patient data.

Read code lists used in this analyis can be found in the 'code_lists' folder. The code list used for ethnicity was from Wright *et al.*, 2017, and can be [downloaded from the Clinical Codes repository](https://clinicalcodes.rss.mhs.man.ac.uk/medcodes/article/56/codelist/res56-ethnicity/).

Definitions and code lists to generate the  [Cambridge multimorbidity score v1.0](https://www.phpc.cam.ac.uk/pcu/cprd_cam/codelists/) can be found in the folder ['SAS/CPRDCAM'](https://github.com/HFAnalyticsLab/Diabetes_healthcare_use/tree/master/SAS/CPRDCAM).

## How does it work?
As the data used for this analysis is not publically available, the code cannot be used to replicate the analysis on this dataset. However, with modifications the code will be able to be used on other patient-level CPRD extracts. 

### Requirements

These scripts were written in R version (to be added) and RStudio Version 1.1.383. 
The following R packages (available on CRAN) are needed: 

* [**tidyverse**](https://www.tidyverse.org/)
* [**tidylog**](https://cran.r-project.org/web/packages/tidylog/index.html)
* [**janitor**](https://cran.r-project.org/web/packages/janitor/index.html)
* [**lubridate**](https://cran.r-project.org/web/packages/lubridate/vignettes/lubridate.html)
* [**tableone**](https://cran.r-project.org/web/packages/tableone/vignettes/introduction.html)
* [**ggrepel**](https://cran.r-project.org/web/packages/ggrepel/vignettes/ggrepel.html)
* [**MASS**](https://cran.r-project.org/web/packages/MASS/index.html)
* [**bbmle**](https://cran.r-project.org/web/packages/bbmle/index.html)
* [**survival**](https://cran.r-project.org/web/packages/survival/index.html)
* [**survminer**](https://cran.r-project.org/web/packages/survminer/index.html)


### Getting started

The 'R' folder contains:

1. sourced files: 'study_params.R' contains study parameters used by all scripts

2. file import scripts:
* '00_import_data.R' - imports and combines CPRD and linked data, needs to be run first

3. analysis scripts (study population, **demographic and clinical charateristics**, to be used in this order):
* '01_define_study_population.R' - defines study cohort based on inclusion and exclusion criteria, all other scripts depend on the outputs of it
* '02_diabetes_medication.R' - extracts data on diabetes medication and defines drug types
* '03_diabetes_type.R' - extracts diabetes diagnoses and defines diabetes type, additionally depends on script 02
* '04_BMI.R' - extracts height and weight data, calculates numerical and categorical BMI
* '05_HbA1C.R' - extracts HbA1C measurements and defines HbA1C control
* '06_smoking_status.R' - extracts data on smoking status and defines smoking status
* '07_multimorbidity.R' - calculated Cambridge multimorbidity score, based on the output of the SAS scripts
* '08_recentlydiagnosed.R' - creates binary flag if first diagnosis was within last x years before study start
* '09_descriptives_clinical.R' - creates table with baseline study population descriptives (table 1), based on 
scripts 2-8

4. analysis and modelling scripts (**healthcare utilisation**, to be used in this order):
* '10_GPappointments.R' - cleans and counts GP appointments per patient
* '11_Outpatient.R' - cleans and counts outpatient appointments
* '12_hospital_admissions.R' - cleans and counts emergency hospital admissions
* '13_prescriptions.R'- cleans and counts prescriptions
* '14_AEattendances.R' - cleans and counts unplanned A&E attendances
* '15_descriptives_utilisation.R' -  creates descriptives analyses of healthcare utilisation, based on scripts 10-14
* '16_utilisation_modelling_prep.R' - tests fit of negative binomial distribution, prepares data for negative binomial models, tests different modelling packages
* '17_utilisation_modelling.R' - creates models for primary care consultations, outpatient attendances and A&E attendances for people with T2DM and saves the output

4. analysis and modelling scripts (**health outcomes**):
* '18_outcome_modelling.R' -creates Cox proportional hazard models for the risk of emergency hospital admissions and all-cause mortality for people with T2DM and saves the output 


The 'SAS' folder contains scripts that generates flags for each of the conditions in the [Cambridge multimorbidity
score v1.0](https://www.phpc.cam.ac.uk/pcu/cprd_cam/codelists/) (to be converted to R in the future):
* '01-libnames.sas'- set libnames, file paths and variables 
* '02_import.sas' - import raw C{RD data and save as sas files
* '03_CPRDCAM_prep.sas' - import reference data (to be added to repo)
* '04_CPRDCAM_processing.sas' - identify each of the 36 conditions 
* '05_CPRDCAM_save.sas' - save output as csv file


## Useful references
* Khalid JM, Raluy-Callado M, Curtis BH, Boye KS, Maguire A, Reaney M. Rates and risk of hospitalisation among patients with type 2 diabetes: Retrospective cohort study using the UK General Practice Research Database linked to English Hospital Episode Statistics. Int J Clin Pract. 2014;68(1):40–8.  
* Kontopantelis E, Springate DA, Reeves D, Ashcroft DM, Rutter M, Buchan I, et al. Glucose, blood pressure and cholesterol levels and their relationships to clinical outcomes in type 2 diabetes: a retrospective cohort study. Diabetologia. 2015;58(3):505–18. 
* Wright AK, Kontopantelis E, Emsley R, Buchan IE, Mamas MA, Sattar N, et al. Cardiovascular Risk and Risk Factor Management in Type 2 Diabetes: A Population-Based Cohort Study Assessing Sex Disparities. Circulation. 2019;44(0):CIRCULATIONAHA.118.039100.
* Cassell A, Edwards D, Harshfield A, et al. The epidemiology of multimorbidity in primary care: a retrospective cohort study. Br J Gen Pract. 2018;68(669):e245-e251. doi:10.3399/bjgp18X695465




## Authors
* **Fiona Grimm** - [@fiona_grimm](https://twitter.com/fiona_grimm) - [fiona-grimm](https://github.com/fiona-grimm)
* **Meetali Kakad**  -[@tali_md](https://twitter.com/tali_md?lang=en)
* **Will Parry** - [@DrWillParry](https://twitter.com/DrWillParry) - [Dr Will Parry](https://willparry.net/)
* **Kathryn Dreyer** - [@kathrynadreyer](https://twitter.com/kathrynadreyer) - [kathdreyer](https://github.com/kathdreyer)

## License
This project is licensed under the [MIT License](https://github.com/HFAnalyticsLab/Diabetes_outpatients/blob/master/LICENSE).

