		
data cohort (keep=patid gender yob tod startdate);; 
set raw.extract_patient; 
startdate='01dec2015'd;
run;


			*Collate all fields required for sorting out medcodes (inner joins ensure only data for cohort group is returned);
			proc sql;
				create table pre_clinical2 as
					select pre_clinical1.patid, eventdate, rownum, startdate-eventdate as days, condition, medcodes.source, read
						from cohort inner join pre_clinical1 on cohort.patid=pre_clinical1.patid
							inner join medcodes on pre_clinical1.medcode=medcodes.medcode
								inner join desc_edit on medcodes.source=desc_edit.source
									where desc_edit.TYPE="MEDCODES" and startdate-eventdate > 0 
										order by pre_clinical1.patid, source, eventdate, rownum;
			quit;
			
			*Save condition into 'cond' if medcode criteria are met and retain most recent record of condition for each patient;
			proc sql;
				create table medcond as
					select pre_clinical2.* , 
						case when read=1 then condition 
							when read=2 and 0<days<=1826 then condition /*Note: only condition 'CAN' (cancer) has read code criteria=2*/
								when read=3 and 0<days<=365 then condition end as cond
									from pre_clinical2 group by patid, cond having rownum=max(rownum) AND cond<>"";
			quit;


			*Collate all fields required for sorting out prodcodes (inner joins ensure only data for cohort group is returned);
			proc sql;
				create table pre_therapy2 as
					select pre_therapy1.patid, eventdate, startdate-eventdate as days, condition, prodcodes.source, prod
						from cohort inner join pre_therapy1 on cohort.patid=pre_therapy1.patid
							inner join prodcodes on pre_therapy1.prodcode=prodcodes.prodcode
								inner join desc_edit on prodcodes.source=desc_edit.source
									where desc_edit.TYPE="PRODCODES" and startdate-eventdate > 0
										order by pre_therapy1.patid, source, eventdate;
			quit;
			
			*Save condition into 'cond' if prodcode criteria are met ('count' ensures only one row for each existing condition for each patient);
			proc sql;
				create table prodcond as
					select patid, prod, source,
						case when prod=1 and 0<days<=365 then condition 
							when prod=2 and 0<days<=365 then condition 
								when prod=3 then condition end as cond, count(*) as count
									from pre_therapy2 group by patid, prod, source, cond having ((prod=1 and count>=4) or prod=2 or prod=3) and  cond<>"";
			quit;

			*Combine medcond and prodcond tables;
			proc sql;
				create table allconds as 
					select patid, "MEDCODES" as data, cond from medcond 
						union
					select patid, "PRODCODES" as data, cond from prodcond;
			quit;

			*Join on logicrules and count the conditions meeting the rules;
			proc sql;
				create table countconds as
					select patid, cond, logic, count(*) as count
						from allconds inner join logicrules on Condition=cond 
							group by patid, cond, logic having logic<>"AND" or (logic="AND" and count=2);
			quit;


			*Dealing with Painful conditions (PNC), which are treated slightly differently to the rest...;
			*4 or more POM analgesics in last 12 months OR (4 or more specified anti-epileptics in last 12 months in the absence of an epilepsy Read code ever recorded);
			*(Analgesics are in source file PNC004, Anti-epileptics are in source file PNC079, Epilepsy read codes are in sourcefile EPI069);
			proc sql;
				create table pnc as
					select cohort.patid, "PNC" as PNC, count(*) as count
						from cohort left join medcond on cohort.patid=medcond.patid
							left join prodcond on cohort.patid=prodcond.patid
								where prodcond.source="PNC004" or (prodcond.source="PNC079" and medcond.cond<>"EPI")
									group by cohort.patid, pnc;
			quit;


			*Dealing with Chronic kidney disease (CKD), which is also treated slightly differently to the rest...;
			*Read Code ever recorded OR if the best (highest value) of the last 2 eGFR readings is < 60 mL/min;

			*Join eGFR data to cohort data and calculate days between eventdates and cohort start dates;
			proc sql;
				create table eGFR1 as
					select eGFR.patid, eventdate, startdate-eventdate as days, data2
						from eGFR inner join cohort on cohort.patid=eGFR.patid
							where startdate-eventdate>0 order by patid, eventdate desc;
			quit;

			*Add record number to identify last two tests;
			data eGFR2;
				set eGFR1;
				recnum + 1;
				by patid;
				if first.patid then recnum=1;
			run;
			
			*Identify whether both values of last two tests are below the normal range threshold;
			*DN: what about when they only have one test in their history? Currently, these people are not included in the CKD condition;
			*Does it matter that some test values may be very old? Currently, this issue is being ignored;
			proc sql;
				create table ckdtest as
					select patid, "CKD" as cond, count(*) as count 
						from eGFR2 where recnum<=2 and data2<60
							group by patid having count=2;
			quit;
		
			*Join CKD medcodes to test data;
			proc sql;
				create table ckd as
					select cohort.patid, "CKD" as CKD, count(*) as count
						from cohort left join ckdtest on cohort.patid=ckdtest.patid
							left join countconds on cohort.patid=countconds.patid
								where ckdtest.cond="CKD" or countconds.cond ="CKD" 
									group by cohort.patid;
			quit;


			*Join all the results together;
			proc sql;
				create table reslong as
					select patid, cond, logic, count, 1 as flag from countconds where cond<>"PNC" and cond<>"CKD"
						union select patid, "PNC" as cond, "OR" as logic, 1 as count, 1 as flag from pnc
							union select patid, "CKD" as cond, "OR" as logic, 1 as count, 1 as flag from ckd;
			quit;

			*Sort and transpose results;
			proc sort data=reslong;
				by patid;
			run;
			proc transpose data=reslong out=reswide(drop=_NAME_ rename=(patid=patidx)); *rename patid so that there is no warning in proc sql, below;
				var flag;
				id cond;
				by patid;
			run;

			*Ensure all cohort members are included in the output (even those with no identified conditions), and include death indicators;
			proc sql;
				create table reswidefull(drop=patidx) as select patid, reswide.* 
					from cohort left join reswide on cohort.patid=reswide.patidx;
			quit;



			*Fill-in blanks, add counts and save to folder;
			data results.cprdcamlts;
				set reswidefull;
				array vals _numeric_;
				do over vals;
					if vals = . then vals = 0;	
				end;
				format mentalb 2.; format physicalb 2.; format total 2.;
				* MAI S: This code uses Barnett et al defn of mental vs physical LTC;
				mentalb = sum(ALC, ANO, ANX, DEM, DEP, LEA, OPS, SCZ);
				* MAI S: Cambridge group combine depression and anxiety;
				depanx=DEP+ANX;
				if depanx=2 then depanx=1;
				mentalbc= sum(ALC, ANO, DEM, LEA, OPS, SCZ, depanx);
				physicalb = sum(AST, ATR, BLI, BRO, CAN, CHD, CKD, CLD, CON, COP, DIB, DIV, EPI, HEF, HEL, HYP, IBD, IBS, MIG, MSC, PNC, PRK, PRO, PSO, PVD, RHE, SIN, STR, THY);
				total = sum(mentalbc, physicalb);
				label ALC="Alcohol problems" ANO="Anorexia/bulimia" ANX="Anxiety" AST="Asthma" BLI="Blindness/low vision" BRO="Bronhiectasis" CAN="Cancer";
				label CHD="Coronary heart dis" CKD="Chronic kidney dis" CLD="Chronic liver dis" CON="Constipation" COP="COPD";
				label DEM="Dementia" DEP="Depression" DIB="Diabetes" DIV="Divertic" EPI="Epilepsy" HEF="Heart failure" HEL="Hearing loss" HYP="Hypertension";
				label IBD="Inflamm bowel dis" IBS="Irritable bowel syndrome" LEA="Learning disab" MIG="Migraine" MSC="Multiple sclerosis" OPS="Substance misuse";
				label PNC="Painful condition" PRK="Parkinsons" PRO="Prostate disorders" PSO="Psoriasis/eczema" PVD="Peripheral vasc dis" RHE="Rheum arth" SCZ="Schizophrenia";
				label SIN="Sinusitis" STR="Stroke/TIA" THY="Thyroid disorders";
				label mentalb="Mental LTCs" mentalbc="Mental LTCS depanx combined" physicalb="Physical LTCs";
			run;
			

			
