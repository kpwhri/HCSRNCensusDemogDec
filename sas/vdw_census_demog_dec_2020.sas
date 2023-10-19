************************************************************************************
INVENTORY OVERVIEW
************************************************************************************

Program Name:   vdw_census_demog_2020.sas      
Contacts: 		Alphonse.Derus@kp.org


VDW Version: V5                                                

Purpose: This is an ETL used to extract Decennial Census data directly from the US Census Bureau's API. The data is then transformed into a SAS dataset. The data is then available to be loaded into a database.
;
************************************************************************************
PROGRAM DETAILS
************************************************************************************

Dependencies :
 
VDW tables:

Other Files:  
	census_key

-------------------------------------------------------------------------------------- 
input:
	1	custom_macros.sas

-------------------------------------------------------------------------------------- 
local_only: 
SAS List file to remain at your site - DO NOT SEND
Number of files created in this folder = [Varies]

-------------------------------------------------------------------------------------- 
share:    
Number of shared SAS Datasets created: 3
Output SAS data sets to be shared outside your site: 
;

ods listing close;

*--------------------------------------------
SITE EDITS
---------------------------------------------;

*Where did you unpack the zip file?;
* %let root = \\fsproj\aaa...\PACKAGE_LOCATION;

* Where does your census key live?;
* check out document/sample_census_key.txt for an example;
/* %include "\\path\to\census_key.txt"; */


*---NO EDITS SHOULD BE NEEDED BEYOND THIS POINT---;

*--------------------------------------------
SETUP
---------------------------------------------;


*Set sub-directories;
libname root "&root";
%let outlocal = &root/local_only;
libname outlocal "&outlocal.";
%let input = &root/input;
libname input "&input.";
%let outshare = &root/share;
libname outshare "&outshare.";
libname QA_ds "&outshare";
%let QAoutlib=QA_ds;

%let workplan = vdw_census_demog_2020;

*Define content area;
%let content_area = CENSUS;

*Set the year/month of the program distribution YYYYMM;
%let era = 202310;
*Set version of this workplan;
%let wp_v = 1;
*Set VDW specification version;
%let version = 5;

data _null_;
	*For trend cutoffs;
	call symput('start_year', 2000);
	call symput('end_year', strip(year(today())));
	call symput('currentMonth', strip(put(today(), monname.)));

	*For runtime file;
	st=datetime();
	call symput("session_date",put(today(),mmddyy10.));
	call symput("st",st);
	call symput("start_time",put(st,datetime.));
run;

*Call input files;
%include "&input./custom_macros.sas";
%include "&input./qa_macros.sas";

* Global titles and footnotes;
title1 "VDW Census: Decennial 2020 ETL"; 
footnote1 "&_sitename : &workplan. (&sysdate, &systime)"; 

*Establish log in share folder and list in local;
proc printto 	
    log="&root./share/&workplan._&era..log"
    print="&root./local_only/&workplan._&era..lst"
    ;
run; */

 *--------------------------------------------
 ---------------------------------------------
 ---------------------------------------------
 START MAIN PROGRAM
 ---------------------------------------------
 ---------------------------------------------
 ---------------------------------------------;

 
filename pdfmain "&outshare./VDW Census Demog Decennial 2020 ETL &currentMonth. &end_year. &_siteabbr..pdf"; 

%macro decennial_pipeline(outds,year=2020, geog=tract, key=&census_key., new_basetable=true);
    %get_states; * get a listing of states, then iterate through them fro get_dec_from_api and decennial base;
    %put NOTE: STATE_LIST = &state_list.;
    %local i next_state;
    %let base_setup = &new_basetable.;
    %do i=1 %to %sysfunc(countw(&state_list));
       %let next_state = %scan(&state_list, &i);
       %put INFO: Retrieving state=&next_state..;
       %** Fetch the &next_state;
       %get_dec_from_api(year=&year, geog=&geog., state=&next_state., census_key=&key., outds=outfile);
       %put INFO: base_setup = &base_setup..;
       %base_append(outfile, basetable=&outds., new_basetable=&base_setup.);
       %let base_setup = false;
    %end;   

%mend decennial_pipeline;

%decennial_pipeline(outlocal.census_raw, year=2020, geog=tract, key=&census_key., new_basetable=true);

%let vdw_cen_demog_dec_vars = GEOCODE STATE COUNTY TRACT RA_NHS_WH RA_NHS_BL RA_NHS_AM RA_NHS_AS RA_NHS_HA RA_NHS_OT RA_NHS_ML RA_HIS_WH RA_HIS_BL RA_HIS_AM RA_HIS_AS RA_HIS_HA RA_HIS_OT RA_HIS_ML HOUSES_N HOUSES_OCCUPIED HOUSES_OWN HOUSES_RENT HOUSES_UNOCC_FORRENT HOUSES_UNOCC_FORSALE HOUSES_UNOCC_RENTSOLD HOUSES_UNOCC_SEASONAL HOUSES_UNOCC_MIGRANT HOUSES_UNOCC_OTHER ;
%let uni_vars = RA_NHS_WH RA_NHS_BL RA_NHS_AM RA_NHS_AS RA_NHS_HA RA_NHS_OT RA_NHS_ML RA_HIS_WH RA_HIS_BL RA_HIS_AM RA_HIS_AS RA_HIS_HA RA_HIS_OT RA_HIS_ML HOUSES_N HOUSES_OCCUPIED HOUSES_OWN HOUSES_RENT HOUSES_UNOCC_FORRENT HOUSES_UNOCC_FORSALE HOUSES_UNOCC_RENTSOLD HOUSES_UNOCC_SEASONAL HOUSES_UNOCC_MIGRANT HOUSES_UNOCC_OTHER ;
data outlocal.decennial_2020;
    length 
        geocode     $11
        state       $2
        county      $3
        tract       $6
        &uni_vars.  8.
    ;
    set outlocal.census_raw;
    keep &vdw_cen_demog_dec_vars.;
    geocode = catx(state, county, tract);
    RA_NHS_WH = divide(P5_003N,P5_001N);
    RA_NHS_BL = divide(P5_004N,P5_001N);
    RA_NHS_AM = divide(P5_005N,P5_001N);
    RA_NHS_AS = divide(P5_006N,P5_001N);
    RA_NHS_HA = divide(P5_007N,P5_001N);
    RA_NHS_OT = divide(P5_008N,P5_001N);
    RA_NHS_ML = divide(P5_009N,P5_001N);
    RA_HIS_WH = divide(P5_011N,P5_001N);
    RA_HIS_BL = divide(P5_012N,P5_001N);
    RA_HIS_AM = divide(P5_013N,P5_001N);
    RA_HIS_AS = divide(P5_014N,P5_001N);
    RA_HIS_HA = divide(P5_015N,P5_001N);
    RA_HIS_OT = divide(P5_016N,P5_001N);
    RA_HIS_ML = divide(P5_017N,P5_001N);  
    HOUSES_N = H1_001N;
    HOUSES_OCCUPIED = divide((H10_002N + H10_010N),H1_001N);
    HOUSES_OWN = divide(H10_002N,H1_001N);
    HOUSES_RENT = divide(H10_010N,H1_001N);
    HOUSES_UNOCC_FORRENT = divide(H5_002N,(H1_001N-(H10_002N+H10_010N)));
    HOUSES_UNOCC_FORSALE = divide(H5_004N,(H1_001N-(H10_002N+H10_010N)));
    HOUSES_UNOCC_RENTSOLD = divide((H5_003N+H5_005N),(H1_001N-(H10_002N+H10_010N)));
    HOUSES_UNOCC_SEASONAL = divide(H5_006N,(H1_001N-(H10_002N+H10_010N)));
    HOUSES_UNOCC_MIGRANT = divide(H5_007N,(H1_001N-(H10_002N+H10_010N)));
    HOUSES_UNOCC_OTHER = divide(H5_008N,(H1_001N-(H10_002N+H10_010N)));
    ;
run;


ods listing gpath="&outlocal.";
ods PDF file=pdfmain uniform style=analysis pdftoc=1;
ods graphics / reset width=90pct height=90pct;


* create final dataset;
proc contents data= outlocal.census_raw;
run;

proc contents data=outlocal.decennial_2020;
run;

proc print data=outlocal.decennial_2020(obs=10);
run;

*-------------------------------------
CENSUS_DEMOG_DEC: META CHECKS
--------------------------------------;
%let content_area = cendemogdec;

* Variable type:  1=Numeric   2=Character;
ods proclabel="Check Variable Existence: CENSUS_DEMOG_DEC";
%CESR_VLC_TYPE_STMV( indataset=outlocal.decennial_2020, 
					vars_and_types= &uni_vars. 1 geocode state county tract 2,
					outdataset= &qaoutlib..&content_area._vartype); 

ods proclabel="Check Variable Lengths: CENSUS_DEMOG_DEC";
%CESR_VLC_Length_STMV(indataset= outlocal.decennial_2020,
					vars_and_lengths= 	geocode 11 state 2 county 3 tract 6 
										,
					outdataset=&qaoutlib..&content_area._length   ); 

ods proclabel="Examine variable distributions: CENSUS_DEMOG_DEC";
proc univariate data=outlocal.decennial_2020 round=.0001;
    var &uni_vars.;
    histogram &uni_vars. / normal ;
run;

ods pdf close;