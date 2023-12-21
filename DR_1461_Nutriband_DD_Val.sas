
/******************************************************************************
PROGRAM NAME:	DR 1461 Nutriband DD Val 

SAS Version:	9.4

PURPOSE: 		

Analysis Time Period: 
	
INPUT FILES:    
OUTPUT FILES: 
				
AUTHOR:	NS

DATE CREATED: 12/20/2023

Validated By:
	
NOTES: 	
*surveillance period starts in 2018Q1; 

*******************************************************************************/

options noformdlim='_' helpbrowser=sas  mprint mlogic msglevel=i validvarname=UPCASE label ;
ods escapechar='^'; 
options nofmterr;
options fmtsearch=(nmufmt);

%include '\\rvffls01\biostatistics\RADARS\Biostatistics Resources\SAS Programs\RADARS Pathnames.sas';
libname iqvia 'R:\RADARS\Ad Hoc Data Requests\Nutriband\DR_1461_Nutriband_transdermal_Patch_report_2022_12_04\Task_1_2022\analysis datasets';


*define yearquarter variables;
%let yyyyq = 20224;
%let yyqq = 22q4;
%let qqyy = 4q22;

Proc Format;
/*Drug Codes*/
Value $dgf
	"_01" = "Buprenorphine Transdermal Patches"
	"_02" = "Fentanyl Transdermal Patches"
	"_03" = "Methylphenidate Transdermal Patches"
	"_04" = "Buprenorphine Sublingual Tablets and Buccal Films"
	;
Value $outcomef
"abmis"     = "Abuse or Misuse"	
"abuse"     = "Abuse"
	"misuse"    = "Misuse"
	"diversion" = "Diversion"
	"unint"     = "Unintentional Exposures"
	;
Value threef
	0		= [comma20.3]
	0<-<0.001	= '<0.001'
	0.001-high 	= [comma20.3]
	;
Value onef
	0		= [comma20.1]
	0<-<0.1	= '<0.1'
	0.1-high 	= [comma20.1]
	;
Value zerof
	0		= [comma20.0]
	0<-<1		= '<1'
	1-high 	= [comma20.0]
	;
Run;


*build rate table;
*dg, yearquarter, cases, denominators of population and prescriptions dispensed; 

data dd_sum_&yyqq.;
	set sum.ddsummaryfile&qqyy.;
	where yearquarter >= 20181 and drug in (840 210 1110 810 820 830);
	outcome = "Diversion";
	if drug = 840 then dg = 1;
	if drug = 210 then dg = 2;
	if drug = 1110 then dg = 3;
	if drug in (810 820 830) then dg = 4;
	keep dg yearquarter zip cases outcome;
run;

proc print data=dd_sum_&yyqq.;
	where zip in (039 997);
run;

*sum cases by dg, yearquarter, zip;
proc sql;
	create table dd_sum_yq_zip
	as select dg, yearquarter, zip, sum(cases) as cases, outcome
	from dd_sum_&yyqq.
	group by dg,yearquarter, zip, outcome;
quit;


*read in IQVIA denominators; 
data iqvia;
	set iqvia.denominators;
run;

*get population denominators;
data pop;
	set dpop.rmpds_popest_zcta3_acs5;
	where yearquarter >= 20181 and yearquarter <= &yyyyq.;
	keep yearquarter zip pop;
run;

*merge pop and iqvia onto main dataset by zip and yearquarter;
*if row is missing from IQVIA, there is 0 dispensing for that drug group in that zipcode;
proc sql;
	create table dd_sum_&yyqq._full 
	as select x.*, 
			  case when iqvia.yearquarter is null and iqvia.zip is null and iqvia.dg is null then 0
			  else iqvia.rx
		      end as rx, 
			  iqvia.dd_weight, 
			  pop.pop
	from dd_sum_yq_zip as x
	left join iqvia 
	on x.yearquarter = iqvia.yearquarter and x.zip = iqvia.zip and x.dg = iqvia.dg
	left join pop
	on x.yearquarter=pop.yearquarter and x.zip =pop.zip;
quit;

*sum by yearquarter for rates table;

proc sql;
	create table rates_base as select 
		dg, 
		yearquarter,
		sum(cases) as cases,
		outcome, 
		sum(rx) as rx format 12.7,
		sum(pop) as pop format 12.7
	from dd_sum_22q4_full
	group by dg, yearquarter, outcome;
quit;

*calculate rates;

data rates;
	set rates_base;

	if cases=0 then do;
		poprate = 0;
		rxrate = 0;

		up = -log(1-(.975));
		low = 0;
	end;

	else do;
		poprate = (cases/pop)*100000;
		rxrate = (cases/rx)*10000;
		up = gaminv((1 + (.95))/2, (cases) + 1);
		low = gaminv((1 - (.95))/2, (cases));
	end;

	pop_low = (low/pop)*100000;
	pop_up = (up/pop)*100000;

	rx_low = (low/rx)*10000;
	rx_up = (up/rx)*10000;

	poprate_f= strip(put(poprate, threef.))||"^n(" || strip(put(pop_low, threef.))|| ",^{unicode 00A0}" || strip(put(pop_up, threef.)) || ")";

	rxrate_bi = strip(put(rxrate, threef.))||"^n(" || strip(put(rx_low, threef.))|| ",^{unicode 00A0}" || strip(put(rx_up, threef.)) || ")";
		
run;

*create dataset for modeling;

data data_model;
	set rates;
	log_rx = log(rx);

	keep dg yearquarter cases log_rx ;
run;

proc sort data=data_model; by dg; run;
















