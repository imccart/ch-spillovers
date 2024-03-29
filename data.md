# Data

Our main data for this project come from the Health Care Cost Institute (HCCI, version 1.0), and we supplement these data with information about hospitals and hospital systems from the American Hospital Association (AHA) annual surveys, the Hospital Cost Report Information System (HCRIS), and market-level data from the American Community Survey (ACS). Our main data and relevant code files are discussed in more detail in the subsequent sections. While we provide almost all of our code files below, note that the data extraction, wrangling, and analyses were all performed via a secure remote desktop. Exporting the files required some manual code adjustments, and we are therefore unable to provide a complete set of replication files that one could directly run against the raw data. 

## Local Data Merged to HCCI
Due to restrictions on identifying hospital information, we first created a hospital-level dataset which we provided to HCCI for encryption. The encrypted version of these data was then made available to use in the secure remote desktop environment, which we could merge to the HCCI data based on the encrypted ID. 

The [Hospital Data](data-code/local-aha-merge/_HospitalData.do) Stata file calls all subsequent code files and creates our final hospital-level dataset. Individual code files called from this main file include:

1. [AHA Data](data-code/local-aha-merge/1_AHA_Data.do). Hospital information from the AHA Annual Surveys.

2. [HCRIS Data](data-code/local-aha-merge/2_HCRIS_Data.do). Hospital information from the hospital cost reports. For more details on the creation of the underlying cost report data, please see the GitHub repo [here](https://github.com/imccart/HCRIS).

3. [Inpatient PPS Data](data-code/local-aha-merge/3_Inpatient_PPS_Data.do). Hospital information from the inpatient prospective payment system final rule files. This information is ultimately not used in the analysis.

4. [ACS Data](data-code/local-aha-merge/4_ACS_Data.do). County-level information (education, income, employment) from the American Community Survey.

5. [CH Tiers](data-code/local-aha-merge/5_CH_Tiers.do). This calls a prior analysis, "AHA_tiers_merge.csv," in which we first delineated tiers of Children's Hospitals. This paper can be found [here](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6986900/).

6. [AMA Data](data-code/local-aha-merge/6_AMA_Data.do). This file incorporates purchased data on commercial insurance market shares at the MSA level. This information was never used in our analysis.


## Extracting HCCI Claims
The [HCCI data](https://healthcostinstitute.org/) include millions of commercial insurance claims and have become the gold-standard for hospital pricing analysis. HCCI is transitioning to Version 2.0 of the data, which consists of a different set of private insurance contributors than we had available to use in Version 1.0. Unfortunately, access to Version 1.0 expires for all researchers on September 30, 2022, but the code used for our analysis would also apply to Version 2.0. 

1. [HCCI Data Part 1](data-code/sas-build/_HCCI_01.sas). This code file pulls all of the relevant pediatric HCCI claims from Vertica, which is the data management software used at the time. This file exports "ipclaims" and "opclaims" datasets for each year.

2. [HCCI Data Part 2](data-code/sas-build/_HCCI_02.sas). This code file creates inpatient admission data that we subsequently use to calculate the share of pediatric procedures per hospital. The data created from this file include all admissions, regardless of procedure. This file exports "ipadmits" datasets for each year.



## Build analytic data
We follow several steps to clean the resulting HCCI claims data and merge additional variables, as described below:


1. [Total Admissions](data-code/stata-build/_AdmitsIP.do). Create a dataset of total inpatient admissions for each hospital, "TotalHospitalAdmits.dta". The file calls the following individual code files.

    a. [Admits NPI](data-code/stata-build/A1_AdmitsNPI.do) Drops observations with missing encrypted NPIs from [HCCI Data Part 2](data-code/sas-build/_HCCI_02.sas). This file exports a temporary "ipadmits" file for each year.
  
    b. **AHA Match.** Find matching hospitals between AHA and HCCI data. This file requires some manual adjustments that may allow individual hospitals to be identified from the code. As such, the underlying code file cannot be posted or shared. Please feel free to contact me for more details. This file uses the "ipadmits" temporary data from the prior step, merges encrypted hospital IDs from HCCI, and exports a set of temporary datasets, "IPAdmits_AHA_2010"-"IPAdmits_AHA_2015".
  
    c. [Unique Admits](data-code/stata-build/A3_UniqueAdmits.do). Final data wrangling of the "IPAdmits_AHA" files, dropping duplicate admits and other admissions with missing or questionable diagnosis codes. Exports a set of temporary files for each year, "CleanAdmits10" - "CleanAdmits15".
  

2. [Inpatient Claims](data-code/stata-build/_ClaimsIP.do). Creates a dataset of all inpatient claims and identifies relevant pediatric procedures. Exports the claims data file, "FinalInpatientData.dta". The file calls the following individual code files.

    a. [IP Claims Step 1](data-code/stata-build/ClaimsIP1_AdmitsNPI.do). Initial cleanup of the claims files created from [HCCI Data Part 1](data-code/sas-build/_HCCI_01.sas), where we drop observations with missing NPIs and irrelevant bill types. Exports a set of temporary "ipclaims" files for each year.
  
    b. **IP Claims Step 2.** Find matching hospitals between AHA and HCCI data. This file requires manual adjustments that may facilitate the identification of individual hospitals and is therefore omitted. The file uses "ipclaims" from step a and exports a set of temporary datasets, "IPClaims_AHA_2010" - "IPClaims_AHA_2015". The file also exports a crosswalk file used to better match outpatient facilities to inpatient (encrypted) NPIs.
  
    c. [IP Claims Step 3](data-code/stata-build/ClaimsIP3_UniqueAdmits.do). Clean "IPClaims_AHA" files from step b and collapse to admit level. Exports "CleanClaims10" - "CleanClaims15" data files.
  
    d. [IP Claims Step 4](data-code/stata-build/ClaimsIP4_DRG_LatLong.do). Incorporate drg weight information and the latitude and longitude based on centroid of patient and hospital zip codes. Uses the "CleanClaims" files from step c and exports "Inpatient_Analysis10" - "Inpatient_Analysis15" data files.
  
    e. [IP Claims Step 5](data-code/stata-build/ClaimsIP5_Distance.do). Calculate the distance between patient zip code centroid and hospital zip code centroid. Exports a temporary "HospDistance" file for each year.
  
    f. [IP Claims Step 6](data-code/stata-build/ClaimsIP6_Prices.do). Uses the "Inpatient_Analysis" files from step d, merges the distance from step e, drops outliers and transfers, and creates final negotiated price variables. Exports "FinalInpatientData_2010" - "FinalInpatientData_2015".

3. [Outpatient Claims](data-code/stata-build/_ClaimsOP.do). Mirrors the analysis of inpatient claims but for outpatient data. As with the inpatient claims, there are 6 individual code files to process the outpatient claims data. Exports the claims data file, "FinalOutpatientData.dta". 

4. [Hospital Data](data-code/stata-build/_HospitalData.do). Takes as inputs the IPClaims_AHA and OPClaims_AHA files, merges external hospital data from the Local Data section above, and exports a final hospital-level dataset, "AHA_ConsolidatedNPI_Data.dta."

5. **Quality Data.** We calculate complication and readmission rates, separately for outpatient and inpatient visits. The [Inpatient Quality](data-code/stata-build/_QualityIP.do) calls the [Inpatient Complications](data-code/stata-build/QIP1_Complications.do) and [Inpatient Readmissions](data-code/stata-build/QIP2_Readmissions.do) files, and the analogous [Outpatient Quality](data-code/stata-build/_QualityOP.do) calls the [Outpatient Complications](data-code/stata-build/QOP1_Complications.do) and [Outpatient Readmissions](data-code/stata-build/QOP2_Readmissions.do) files. Exports "QualityData_IP.dta" and "QualityData_OP.dta" datasets. Readmissions capture any inpatient readmission within 90 days of a given procedure, and complications capture a large set of clinicaly relevent complications for our selected procedures. See the individual complications code files for the list of codes to identify each complication. These codes are also listed in a more reader-friendly way in the [supplemental appendix](papers/specialization/jhe-appendix-202207.pdf).

6. [Final Claims Data](data-code/stata-build/_FinalData.do). Create our final inpatient and outpatient claims files, which we subsequently import into R for analysis. This file incorporates the "FinalInpatientData.dta," "FinalOutpatientData.dta," "TotalHospitalAdmits.dta," "QualityData_IP.dta," and "QualityData_OP.dta" datasets, and calls the [Pediatric CCC Code](data-code/stata-build/Pediatric_Index.do) to calculate the pediatric comorbidities index. The file exports two datasets: "FinalData.txt" and "FinalData_All.txt". The first is a subset of the second, consisting only of the 13 routine pediatric procedures of interest. The final version of the paper only uses the "FinalData.txt" data.

7. [Choice Sets](data-code/stata-build/_ChoiceSets.do). This file constructs the set of all hospitals for discrete choice modeling. Like the final claims data, this file creates two choice sets, one based on all hospitals treating any pediatric patients and one based only on hospitals treating pediatric patients for one of the selected routine procedures. The narrower choice set is employed in the final version of the paper.
