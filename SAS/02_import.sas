*Code to import raw text files and save as single SAS datasets;
*Be warned - it can take a long time to run! - See note on Test files in the proc import below;

*Import original CPRD datasets;
%macro readin(datasets, numfileslist);

%do i = 1 %to %sysfunc(countw(&datasets.)); *for each data set...;
	%let dataset = %scan(&datasets., &i.); *extract current data set;
	%let numfiles = %scan(&numfileslist., &i.); *extract number of files for current data set from list;

		%do j=1 %to &numfiles; *for each file... import the data;
			
			%if &j < 10 %then %let file = &path_cprd.2019_07_24_diabetes_Extract_&dataset._00&j..txt;
			%else %if &j >= 10 %then %let file = &path_cprd.2019_07_24_diabetes_Extract_&dataset._0&j..txt; 

			proc import datafile = "&file" dbms=tab out=data&j replace ; 
			guessingrows=100000; 
			
			run;
		%end;

	data raw.extract_&dataset.; *save the data to the raw folder;
		set data1-data&numfiles.;
	run;

%end;


%mend readin;
%readin(&datasets, &numfileslist);

*Test files need to be imported separately;

%macro readin_test;
	%let dataset = Test; 
	%let numfiles = 6; *number of files in the test data set;

		%do j=1 %to &numfiles; *for each file... import the data;

			data data&j;
             infile "&path_cprd.2019_07_24_diabetes_Extract_&dataset._00&j..txt" delimiter='09'x MISSOVER
        		DSD lrecl=32767 firstobs=2 ;
                informat patid best32. ;
                informat eventdate ddmmyy10. ;
                informat sysdate ddmmyy10. ;
                informat constype best32. ;
                informat consid best32. ;
                informat medcode best32. ;
                informat staffid best32. ;
                informat enttype best32. ;
                informat data1 best32. ;
                informat data2 best32. ;
                informat data3 best32. ;
                informat data4 best32. ;
                informat data5 best32. ;
                informat data6 best32. ;
                informat data7 best32. ;
                informat data8 best32. ;
                format patid best32. ;
                format eventdate ddmmyy10. ;
                format sysdate ddmmyy10. ;
                format constype best32. ;
                format consid best32. ;
                format medcode best32. ;
                format staffid best32. ;
                format enttype best32. ;
                format data1 best32. ;
                format data2 best32. ;
                format data3 best32. ;
                format data4 best32. ;
                format data5 best32. ;
                format data6 best32. ;
                format data7 best32. ;
                format data8 best32. ;
             input
                         patid
                         eventdate
                         sysdate
                         constype
                         consid
                         medcode
                         staffid
                         enttype
                         data1
                         data2
                         data3
                         data4
                         data5
                         data6
                         data7
                         data8 
             ;
             run;
		%end;

	data raw.extract_&dataset.; *save the data to the raw folder;
		set data1-data&numfiles.;
	run;


%mend readin_test;
%readin_test;
