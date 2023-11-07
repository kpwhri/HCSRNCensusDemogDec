# VDW Census Demographics Decennial ETL
This is an ETL used to extract data directly from the US Census Bureau's API. The data is then transformed into a SAS dataset. The data is then available to be loaded into a database.

Primary contact: Al Derus
Institution: KPWHRI
Email: Alphonse.Derus@kp.org

## Project Stage
Please run the ETL at your site and incorporate the data in your VDW.

## Workplan Timeline
Please complete the workplan by December 20, 2023.

## Workplan Package
  vdw_census_demog_dec_2020.zip

Files Included in Zip file:
* sas/vdw_census_demog_dec_2020.sas
* input/qa_macros.sas 
* input/custom_macros.sas 
* input/qa_macros.sas 
* local_only/info.md
* document/sample_census_key.txt

Number and Type of Files to be Returned: 
1 zip file containing:
1. 1 log file
1. 1 PDF file
1. 1 share_info.md
1. 4 sas datasets

# Running this workplan
* Prior to running the plan, you'll need to get an API key from the Census Bureau.  You can do this in about two minutes by: 
1. Go to https://api.census.gov/data/key_signup.html and request a key (you'll need to provide an email address, share your organization, and agree to the terms of service). 
1. Check your email and copy the key from the email you receive to a text file (e.g., census_key.txt). You can see an example in the documents folder of this workplan "census_key.txt". Example [Census Key](/document/sample_census_key.txt).
1. Once you have your key, you'll %include the  to the sas/vdw_census_demog_dec_2020.sas file in the following line:
```sas
* Where does your census key live?;
%include "\\path\to\census_key.txt";
```
1. The workplan program has a clearly marked edit section near the top of the program.  Please complete the edits as directed by comments 
1. Review the log for ERRORS or WARNINGS.  If there are problems, please send a full log to the workgroup leads (contact info at top of workplan), after first making sure the log is redacted of PHI and any site-specific information that your site does not want released. You can refer to the review pdf in the local_only folder for a quick summary of the datasets being returned.
1. Append the 2020 Decennial Census Data to your existing Decennial Data.

> **NOTE**
> The code produces warnings about the length of the variables.  These warnings can be ignored.

Directions to transfer data:
* Zip files in the ‘share’ folder into one compressed file named:  vdw_census_demog_dec_yyyymmdd_&_siteabbr..zip.
* Upload the zip file to the HCSRN folder on the [KP Washington SFT](https://projects.kpwashingtonresearch.org/SFT/main/login.aspx)
