* Macro to fetch states;
%macro get_states();
    %global state_list statename_list;
        * AL  01  ALABAMA;
        * AK  02  ALASKA;
        * AZ  04  ARIZONA;
        * AR  05  ARKANSAS;        
        * CA  06  CALIFORNIA;
        * CO  08  COLORADO;
        * CT  09  CONNECTICUT;
        * DC  11  DISTRICT OF COLUMBIA;
        * DE  10  DELAWARE;
        * FL  12  FLORIDA;
        * GA  13  GEORGIA;
        * HI  15  HAWAII;
        * ID  16  IDAHO;
        * IL  17  ILLINOIS;
        * IN  18  INDIANA;
        * IA  19  IOWA;
        * KS  20  KANSAS;
        * KY  21  KENTUCKY;
        * LA  22  LOUISIANA;
        * ME  23  MAINE;
        * MD  24  MARYLAND;
        * MA  25  MASSACHUSETTS;
        * MI  26  MICHIGAN;
        * MN  27  MINNESOTA;
        * MS  28  MISSISSIPPI;
        * MO  29  MISSOURI;
        * MT  30  MONTANA;
        * NE  31  NEBRASKA;
        * NV  32  NEVADA;
        * NH  33  NEW HAMPSHIRE;
        * NJ  34  NEW JERSEY;
        * NM  35  NEW MEXICO;
        * NY  36  NEW YORK;
        * NC  37  NORTH CAROLINA;
        * ND  38  NORTH DAKOTA;
        * OH  39  OHIO;
        * OK  40  OKLAHOMA;
        * OR  41  OREGON;
        * PA  42  PENNSYLVANIA;
        * RI  44  RHODE ISLAND;
        * SC  45  SOUTH CAROLINA;
        * SD  46  SOUTH DAKOTA;
        * TN  47  TENNESSEE;
        * TX  48  TEXAS;
        * UT  49  UTAH;
        * VA  51  VIRGINIA;
        * VT  50  VERMONT;
        * WA  53  WASHINGTON;
        * WI  55  WISCONSIN;
        * WV  54  WEST VIRGINIA;
        * WY  56  WYOMING;
    * Puerto Rico sometimes has data, but the other territories not so much.;
    * 64 - Federated States of Micronesia;
    * 66 - Guam;
    * 68 - Marshall Islands;
    * 69 - Northern Mariana Islands;
    * 70 - Palau;
    * 72 - Puerto Rico;
    * 78 - Virgin Islands of the US;
    %let state_list = 01 02 04 05 06 08 09 10 11 12 13 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 44 45 46 47 48 49 50 51 53 54 55 56;
%mend get_states;

* Macro to get the decennial data from the api;
%macro get_dec_from_api(year=, geog=tract, state=, census_key=, outds=outfile, debug=true);
    * This only works for 2020 for now. Need to elaborate for 2010 and 2000;
    * create a response file;
    * 2020 API Documentation - https://api.census.gov/data/2020/dec/dhc/variables.html;
    * 2010 API Documentation - https://api.census.gov/data/2010/dec/sf1/variables.html;
    filename resp temp;
    * if the year chosen is 2020, then use the 2020 url;
    * if the year chosen is not supported, raise an error and exit out of the macro - this comment has to be outside the macro if/then block.;
    %if &year = 2020 %then %do;
        %let varlist = P5_001N,P5_003N,P5_004N,P5_005N,P5_006N,P5_007N,P5_008N,P5_009N,P5_011N,P5_012N,P5_013N,P5_014N,P5_015N,P5_016N,P5_017N,H1_001N,H10_002N,H10_010N,H5_002N,H5_003N,H5_004N,H5_005N,H5_006N,H5_007N,H5_008N;
        %let varlist_ar = %sysfunc(compbl(%sysfunc(tranwrd(%quote(&varlist),%str(,),%str( )))));
        %let url = https://api.census.gov/data/2020/dec/dhc?get=NAME,&varlist.%str(&)for=&geog.:*%str(&)in=state:&state.;
    %end;
    %else %do;
            %put ERROR: Year &year. is not supported.;
            %return;
    %end;
    %* if there is a key in census_key, then append to the URL;
    %IF &CENSUS_KEY. ne FALSE %THEN %DO;
        %put INFO: Appending CENSUS_KEY.; 
        %LET url = &URL.%STR(&)key=&CENSUS_KEY.;
    %END;
    %ELSE %DO;
        %put WARNING: Not appending CENSUS_KEY - Too many requests will cause the Census API to throttle you.;
    %END;
    
    proc http
        url="&url."
        method="get"
        out=resp;
    run;

    * API returns data in JSON format.;
    libname decjson JSON fileref=resp;

    * Read column names from first row of JSON data.;
    data var_names;
        set decjson.root;
        if _n_ = 1;
        array varlist {*} element:;
        do v = 1 to dim(varlist);
            origname = compress('element' || v);
            kp = varlist{v};
            rn = compress(origname || "=" || varlist{v});
            if varlist{v} = 'NAME' then do;
                ln = varlist{v} || ' $65';
            end;
            else if varlist{v} = 'state' then do;
                ln = varlist{v} || ' $2';
            end;
            else if varlist{v} = 'county' then do;
                ln = varlist{v} || ' $3';
            end;
            else if varlist{v} = 'tract' then do;
                ln = varlist{v} || ' $6';
            end;
            else do;
                ln = varlist{v} || ' $10';
            end;
            output;
        end;
        keep rn kp ln;
    run;

    proc sql noprint;
        select rn into :rn_stmt separated by ' '
            from var_names;
        select kp into :kp_stmt separated by ' '
            from var_names;
        select ln into :ln_stmt separated by ' '
            from var_names;
        quit;
        
        data _tmp_ds;
        length &ln_stmt. ;
        set decjson.root(firstobs=2 rename=(&rn_stmt.));
        keep &KP_STMT.;
        run;

        
        * pull out metadata for some type changing;
        proc contents data=_tmp_ds out=cont noprint;
        run;

        * produce dynamic type changing b/c all census variables are counts;
        proc sql noprint;
            select cat('_',NAME,'=input(',NAME,',8.);drop ',NAME,'; rename _',NAME,'=',NAME) 
            into :code separated by ';'
            from cont
            where 
                upcase(NAME) not in('NAME','STATE','COUNTY','TRACT') and type=2
            ;
        quit;
        %put INFO: Outputting dataset for state=&state. to &outds.. ;
        data &outds.;
            set _tmp_ds;
            &code.;
        run;

%mend get_dec_from_api;

* macro to create a base table and append future data to it;

%macro base_append( inds                    /*The dataset we will append to the dataset. */
                    ,basetable=basetable    /*Just the name of the base table to house the raw counts. It could be named anything */
                    ,new_basetable=true     /*Set to false if you have a structure and just need to append data. */
                    );
    %if &new_basetable. = true %then %do;
    %put INFO: creating a new base table: &basetable..;
    proc sql;
    create table &basetable. like &inds.;
    quit;
    %end;
    %else %do;
    %put INFO: not creating a new base table.;
    %end;
    %put INFO: Appending to &basetable..;
    proc datasets library=work nolist;
        append base=&basetable. data=&inds.;
    run;
%mend base_append;