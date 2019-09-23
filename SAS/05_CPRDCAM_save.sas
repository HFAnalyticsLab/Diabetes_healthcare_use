* Save output as csv;

%macro save_csv;
	proc export data = results.cprdcamlts dbms=csv
		outfile="&csv_path.cprdcamlts.csv"
		replace;
		run;

%mend;
%save_csv;