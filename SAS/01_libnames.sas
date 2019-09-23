*Allocate libnames;
libname raw "SAS_multimorbidity\rawsas"; *raw imported data;
libname results "SAS_multimorbidity\results"; *csv results

*Set path files for raw SAS data;
data _null_;
	%let datasets = Clinical Consultation Patient Practice Staff Therapy ; *actual names of original data sets (exluding any common parts);
	%let numfileslist = 5 5 1 1 1 15; *number of original data files for each data set;
	%let path_cprd = 2019_07_24 CPRD extract\; *Set file path for raw data;
run;

*Set path files for reference data and to save results;
data _null_;
	%let cprdcam_path = SAS_multimorbidity\CPRDCAM\;
	%let csv_path = SAS_multimorbidity\results\;
run;