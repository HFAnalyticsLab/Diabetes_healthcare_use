*Multimorbidty using the Cambridge Multimorbidity Index (37 chronic conditions from Cassell et al. (2017));

*This consists of three code sections;
	*1) Data prep;
	*2) Processing;
	*3) Save as csv file;

*1 - Import CPRDCAM data 

*Medcodes;
proc import datafile="&cprdcam_path.medcodes.csv" dbms=csv 
	out=medcodes replace;
	guessingrows=100000;
run;

*Prodcodes;
proc import datafile="&cprdcam_path.prodcodes.csv" dbms=csv 
	out=prodcodes replace;
	guessingrows=100000;
run;

*The following columns were added in to the 'descriptions' table (..\CPRDCAM\descriptions.csv),
by hand in Excel, based on the 'Usage Definition' field, and saved as 'edited_descriptions.csv';

/*
Field		Value	Description
-----		-----	-----------
read		1		specified read codes ever
read		2		specified read codes first recorded in last 5 years
read		3		specified read codes in the last year
prod		1		4 prescriptions in the last year
prod		2		any prescriptions in the last year
prod		3		any prescriptions ever
logic		AND		whether conditions need to be combined using AND logic
logic		OR		whether conditions need to be combined using OR logic
warning		yes		identifies morbidity codings that are particularly challenging
*/

*NB: The long text fields (Provenance, Links, Citations) were also deleted from the 'edited_descriptions.csv' table so that it would read into SAS without any problems;


*Import edited descriptions table;
proc import datafile="&cprdcam_path.edited_descriptions.csv" dbms=csv
	out=desc_edit(rename=('CONDITION.CODE'n=Condition)) replace;
	guessingrows=100000;
run;

*Get one logic rule per condition from desc_edit (so that joining logic on in processing does not create duplicate records);
proc sql;
	create table logicrules as 
		select Condition, logic, count(*) as count from desc_edit
			group by Condition, logic;
quit;


*2 - Sort and import clinical data, which contains patients' medcodes;

*Sort by eventdate;
proc sort data=raw.extract_clinical; 
	by eventdate;
run;

*Pull in data and add rownum to order dataset and deal with any eventdate ties later on;
data pre_clinical1 (keep=patid eventdate medcode rownum);
	set raw.extract_clinical;
	rownum=_n_;
	where eventdate ne .;
run;


*3 - Import therapy data, which contains patients' prodcodes;

*Pull in data;
data pre_therapy1 (keep=patid eventdate prodcode);
	set raw.extract_therapy;
	where eventdate ne .;
run;


*4 - Import test data, which contains patients' eGFR test values - used in the chronic kidney disease (CKD) condition;
*(Read Code ever recorded OR if the best (highest value) of the last 2 eGFR readings is < 60 mL/min);

*Pull in only the eGFR test values  (466 is the enttype code for eGFR - see 'P:\P036 Costing model CPRD\CPRDCAM\eGFR entity lookup.csv');
data eGFR (keep=patid eventdate enttype data1-data7);
	set raw.extract_test;
	where enttype=466 AND data2 ne . AND eventdate ne . ;
run;

