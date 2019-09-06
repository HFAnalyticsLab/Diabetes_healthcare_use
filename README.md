# Healthcare utilisation by patients with type 1 and type 2 diabetes

#### Project Status: In progress

## Project Description

Diabetes is a common chronic health condition. Care for patients with diabetes accounts for almost 9% of the NHS budget: £8.8 billion a year. The number of cases of diabetes is rising, due an ageing population. Other risk factors, such as diet and physical activity, also contribute to increased demand for diabetes-related primary and secondary care. 

There is little evidence on the variation of health care utilisation by patients with diabetes. It is also unclear what  effect this variation might have on health outcomes. This study aims to describe health care utilisation patterns of diabetes patients in England, across primary care and secondary care.  

The code in this repository is used for data cleaning, to define and describe the study cohort (eg demographic details, clinical characteristics) and to quantify health care utilisation and health outcomes.   

## Data source
We used data from the Clinical Practice Research Datalink (CPRD) linked to Hospital Episode Statistics (HES). ISAC protocol number 19_138(link to study description to be added once available). 

Data used for this analysis were anonymised in line with the ICO's Anonymisation Code of Practice. The data were accessed in The Health Foundation's Secure Data Environment, which is a secure data analysis facility (accredited for the ISO27001 information security standard, and recognised for the NHS Digital Data Security and Protection Toolkit). No information that could directly identify a patient or other individual was used. Variables labelled 'patid' do not refer to NHS IDs or other identifiable patient data.

## How does it work?
As the data used for this analysis is not publically available, the code cannot be used to replicate the analysis on this dataset. However, with modifications the code will be able to be used on other patient-level CPRD extracts. 

### Requirements

These scripts were written in R version (to be added) and RStudio Version 1.1.383. 
The following R packages (available on CRAN) are needed: 

* **tidyverse** - [https://www.tidyverse.org/](https://www.tidyverse.org/)
* **tidylog** - [https://cran.r-project.org/web/packages/tidylog/index.html](https://cran.r-project.org/web/packages/tidylog/index.html)
* **janitor** - [https://cran.r-project.org/web/packages/janitor/index.html](https://cran.r-project.org/web/packages/janitor/index.html)
* **lubridate** - [https://cran.r-project.org/web/packages/lubridate/vignettes/lubridate.html](https://cran.r-project.org/web/packages/lubridate/vignettes/lubridate.html)


### Getting started
To be added.

## Useful references

## Authors
* **Fiona Grimm** - [@fiona_grimm](https://twitter.com/fiona_grimm) - [fiona-grimm](https://github.com/fiona-grimm)
* **Meetali Kakad** - 

## License
This project is licensed under the [MIT License](LICENSE.md).

