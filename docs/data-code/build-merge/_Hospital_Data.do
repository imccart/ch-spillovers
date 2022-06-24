capture log close
set logtype text
local logdate = string( d(`c(current_date)'), "%dCYND" )
log using "D:\CloudStation\Professional\Research Projects\Children's Hospitals\Logs\DataLog`logdate'.log", replace

***********************************************************************************
** Title:	      Build Final Dataset for Children's Hospital Analysis
** Author:        Ian McCarthy
** Date created:  2/5/18
** Date edited:   8/15/2018
***********************************************************************************
set more off
global DATA_IPPS "D:\CloudStation\Professional\Research Data\Hospital Inpatient PPS\PPS Payment Impact Files\"
global DATA_HCRIS "D:\CloudStation\Professional\Research Data\Hospital Cost Reports\Final HCRIS Data\"
global DATA_ACS "D:\CloudStation\Professional\Research Data\ACS Data\"
global DATA_AHA "D:\CloudStation\Professional\Research Data\AHA Data\"
global DATA_CROSSWALK "D:\CloudStation\Professional\Research Data\"
global DATA_FINAL "D:\CloudStation\Professional\Research Projects\Children's Hospitals\Data\"
global CODE_FILES "D:\CloudStation\Professional\Research Projects\Children's Hospitals\Analysis\"


***********************************************************************************
/* Run do files */
do "${CODE_FILES}1_AHA_Data.do"
do "${CODE_FILES}2_HCRIS_Data.do"
do "${CODE_FILES}3_Inpatient_PPS_Data.do"
do "${CODE_FILES}4_ACS_Data.do"
do "${CODE_FILES}5_CH_Tiers.do"
do "${CODE_FILES}6_AMA_Data.do"


** Identify MSA
import delimited using "${DATA_CROSSWALK}Zip and MSA\Zip_to_msa.csv", clear
rename zipcode zip
save temp_zip_msa, replace

***********************************************************************************
/* Merge Data */

** Begin with AHA data at Hospital FYear
use "${DATA_FINAL}AHA_Data.dta", clear

** Merge with HCRIS data
merge 1:1 provider_number FYear using "${DATA_FINAL}HCRIS_Hospital_Level.dta", keep(master match) generate(HCRIS_merge)

** Merge with PPS (case mix index data, at federal FY level)
merge 1:1 provider_number FYear using "${DATA_FINAL}Hospital_PPS.dta", keep(master match) generate(PPS_merge)

** replace missing zip data
replace zip=0 if zip==.
bys provider_number: egen max=max(zip)
replace zip=max if zip==0 & max!=0
drop max 

** replace missing fips
gen byte nonmiss=!mi(fips)
sort provider_number fips nonmiss
bys provider_number (nonmiss): replace fips=fips[_N] if nonmiss==0
drop nonmiss

** fill in missing state data
gen byte nonmiss=!mi(state)
sort provider_number state nonmiss
bys provider_number (nonmiss): replace state=state[_N] if nonmiss==0
drop nonmiss

** Merge with ACS data
merge m:1 fips CYear using "${DATA_FINAL}ACS_Data.dta", keep(master match) generate(ACS_merge)

** clean fips codes
split fips, parse("_")
destring fips2, replace
gen str3 fips3 = string(fips2, "%03.0f")
gen fips_clean=fips1+fips3
gen byte notnumeric=real(fips_clean)==.
replace fips_clean="" if notnumeric==1
drop notnumeric

destring fips_clean, replace
sort fips_clean
drop fips fips1 fips2 fips3
rename fips_clean fips
sort fips FYear

** fill in missing msa data
merge m:1 zip using temp_zip_msa.dta, keep(master match) nogenerate
gen fips_state=string(fips) + "_" + state
replace fips_state="" if state==""
gen byte nonmiss=!mi(msaname)
sort fips_state msaname nonmiss
bys fips_state (msaname): replace msaname=msaname[_N] if nonmiss==0
replace msaname="" if fips_state=="" & nonmiss==0
drop nonmiss
replace msaname=upper(msaname)

** clean some MSA names
replace msaname="ATHENS-CLARKE COUNTY, GA MSA" if msaname=="ATHENS-CLARK COUNTY, GA MSA"
replace msaname="CORVALLIS, OR MSA" if msaname=="CORVALIS, OR MSA"
replace msaname="DALLAS-PLANO-IRVING, TX MSA" if msaname=="DALLAS-FORT WORTH-ARLINGTON, TX MSA"
replace msaname="DES MOINES, IA MSA" if msaname=="DES MOINES-WEST DES MOINES, IA MSA"
replace msaname="CHICAGO-NAPERVILLE-JOLIET, IL MSA" if msaname=="CHICAGO-NAPERVILLE-JOLIET, IL-IN-WI MSA"
replace msaname="FLORENCE, AL MSA" if msaname=="FLORENCE-MUSCLE SHOALS, AL MSA"
replace msaname="GREENVILLE, SC MSA" if msaname=="GREENVILLE-MAULDIN-EASLEY, SC MSA"
replace msaname="BOSTON-CAMBRIDGE-QUINCY, MA MSA" if msaname=="BOSTON-CAMBRIDGE-QUINCY, MA-NH MSA"
replace msaname="BUFFALO-CHEEKTOWAGA-TONAWANDA, NY MSA" if msaname=="BUFFALO-NIAGARA FALLS, NY MSA"
replace msaname="HICKORY-MORGANTON-LENOIR, NC MSA" if msaname=="HICKORY-LENOIR-MORGANTON, NC MSA"
replace msaname="INDIANAPOLIS, IN MSA" if msaname=="INDIANAPOLIS-CARMEL, IN MSA"
replace msaname="KINGSPORT-BRISTOL, TN-VA MSA" if msaname=="KINGSPORT-BRISTOL-BRISTOL, TN-VA MSA"
replace msaname="LAKELAND-WINTER HAVEN, FL MSA" if msaname=="LAKELAND, FL MSA"
replace msaname="LITTLE ROCK-NORTH LITTLE ROCK, AR MSA" if msaname=="LITTLE ROCK-NORTH LITTLE ROCK-CONWAY, AR MSA"
replace msaname="LONGVIEW-KELSO, WA MSA" if msaname=="LONGVIEW, WA MSA"
replace msaname="LOS ANGELES-LONG BEACH-GLENDALE, CA MSA" if msaname=="LOS ANGELES-LONG BEACH-SANTA ANA, CA MSA"
replace msaname="LOUISVILLE, KY-IN MSA" if msaname=="LOUISVILLE/JEFFERSON COUNTY, KY-IN MSA"
replace msaname="MANCHESTER, NH MSA" if msaname=="MANCHESTER-NASHUA, NH MSA"
replace msaname="MEMPHIS, TN-MS-AR MSA" if msaname=="MEMPHIS, TN-AR-MS MSA"
replace msaname="MIAMI-MIAMI BEACH-KENDALL, FL MSA" if msaname=="MIAMI-FORT LAUDERDALE-POMPANO BEACH, FL MSA"
replace msaname="NASHVILLE-DAVIDSON-MURFREESBORO, TN MSA" if msaname=="NASHVILLE-DAVIDSON-MURFREESBORO-FRANKLIN, TN MSA"
replace msaname="NEW YORK-WHITE PLAINS-WAYNE, NY-NJ MSA" if msaname=="NEW YORK-NORTHERN NEW JERSEY-LONG ISLAND, NY-NJ-PA MSA"
replace msaname="NORWICH-NEW LONDON, CT-RI MSA" if msaname=="NORWICH-NEW LONDON, CT MSA"
replace msaname="DELTONA-DAYTONA BEACH-ORMOND BEACH, FL MSA" if msaname=="PALM COAST, FL MSA"
replace msaname="PORT ST. LUCIE-FORT PIERCE, FL MSA" if msaname=="PORT ST. LUCIE, FL MSA"
replace msaname="PORTLAND-SOUTH PORTLAND, ME MSA" if msaname=="PORTLAND-SOUTH PORTLAND-BIDDEFORD, ME MSA"
replace msaname="PROVIDENCE-FALL RIVER-WARWICK, RI-MA MSA" if msaname=="PROVIDENCE-NEW BEDFORD-FALL RIVER, RI-MA MSA"
replace msaname="SANTA BARBARA-SANTA MARIA, CA MSA" if msaname=="SANTA BARBARA-SANTA MARIA-GOLETA, CA MSA"
replace msaname="SCRANTON-WILKES-BARRE, PA MSA" if msaname=="SCRANTON--WILKES-BARRE, PA MSA"
replace msaname="SEATTLE-BELLEVUE-EVERETT, WA MSA" if msaname=="SEATTLE-TACOMA-BELLEVUE, WA MSA"
replace msaname="TAMPA-ST. PETERSBURG-CLEARWATER, FL MSA" if msaname=="TAMPA-ST. PETERSBURG-CLEARWATER, FL"
replace msaname="TEXARKANA, TX-AR MSA" if msaname=="TEXARKANA, TX-TEXARKANA, AR MSA"
replace msaname="WORCESTER, MA-CT MSA" if msaname=="WORCESTER, MA MSA"
replace msaname="YUBA CITY-MARYSVILLE, CA MSA" if msaname=="YUBA CITY, CA MSA"
replace msaname="DETROIT-LIVONIA-DEARBORN, MI MSA" if msaname=="DETROIT-WARREN-LIVONIA, MI MSA"
replace msaname="PHILADELPHIA, PA MSA" if msaname=="PHILADELPHIA-CAMDEN-WILMINGTON, PA-NJ-DE-MD MSA"
replace msaname="SAN FRANCISCO-SAN MATEO-REDWOOD CITY, CA MSA" if msaname=="SAN FRANCISCO-OAKLAND-FREMONT, CA MSA"


** merge with AMA data
rename FYear Year
keep if Year>=2010 & Year<=2015
gen MergeYear=Year
replace MergeYear=2014 if MergeYear==2015
merge m:1 msaname MergeYear using "${DATA_FINAL}AMA_Data.dta", keep(master match) generate(AMA_Merge)

** merge with CH tiers
merge m:1 ID MergeYear using "${DATA_FINAL}CH_Tiers.dta", keep(master match) generate(CHTiers_Merge)

***********************************************************************************
/* New Variables */
replace TotalPop=TotalPop/1000
replace zip=. if zip==0
keep NPINUM Year Nonprofit Profit Teaching_Hospital1 Teaching_Hospital2 BDTOT ///
	System Labor_Phys Labor_Residents Labor_Nurse Labor_Other Capital_Imaging ///
	Capital_CareSetting Capital_Services Percent_Profitable Int_HOS beds ///
	total_discharges mcare_discharges mcaid_discharges tot_charges tot_discounts ///
	TierA TierB TierC TierD NoTier ///
	tot_opexp ip_charges intcare_charges ancserv_charges MissingCharges tot_mcarepymt ///
	sec_mcarepymt hvbp_payment hrrp_payment discount_factor price ///
	Market_Discharges discharge_share CMI Adjusted_Cases Urban ///
	TotalPop Age_Below18 Age_18to34 Age_35to64 Age_65plus Race_White ///
	Race_Black Race_Asian Race_Other Income_75to100 Income_25 Income_25to50 ///
	Income_50to75 Income_100to150 Income_150plus Educ_NoSchool Educ_GradeSchool ///
	Educ_HighSchool Educ_HSGrad Educ_SomeCollege Educ_Assoc Educ_Bach ///
	Educ_Graduate Emp_FullTime TotalHHI Insurer1 Share1 Insurer2 Share2 ///
	HRRCODE state county zip fips msa_no msaname 

***********************************************************************************
/* Save Final Dataset */
save "${DATA_FINAL}Hospital_Data.dta", replace
saveold "${DATA_FINAL}Hospital_Data_v2012.dta", replace version(12)
outsheet using "${DATA_FINAL}Hospital_data.csv", comma replace
outfile using "${DATA_FINAL}Dictionary", dictionary replace
log close
