*****************************************************************************
** Description: 				Form hospital-level dataset by AHA-HNPI (consolidated HNPI)
** Author: 						Ian McCarthy
** Date Edited:					12/10/2020
*****************************************************************************

clear
capture log close
set more off
pause on

*****************************************************************************
*SET PATHS*
global SAS_DATA = "K:\Home\mccarthy-ian-hcci\Documents\Research"
global MAIN = "K:\Research\Raval_Emory\Childrens Hospitals"
global CODE = "$MAIN\data-code"
global LOGS = "$MAIN\logs"
global DATA = "$MAIN\data"
global TEMP = "$MAIN\data\Temp"
global FINAL = "$MAIN\data\Final"

local date = "$S_DATE"
log using "$LOGS\AHAData_`date'.log", replace


*****************************************************************************
** Clean external hospital data
use "${DATA}\Hospital_Data_v12_HNPI.dta", clear
drop if hnpi==""
rename year Year
bys hnpi Year: gen obs=_N
drop if obs>1
drop obs
save "${TEMP}\External_Hospital_Data.dta", replace


*****************************************************************************
** identify hnpi's and consolidated hnpi's from the claims data
foreach yr in 10 11 12 13 14 15 {
	use "${TEMP}\IPClaims_AHA_20`yr'.dta", clear
	gen Year=20`yr'
	keep aha_hnpi hnpi Year
	bys aha_hnpi hnpi: gen obs=_n
	keep if obs==1
	drop obs
	save temp1, replace
	
	use "${TEMP}\OPClaims_AHA_20`yr'.dta", clear
	gen Year=20`yr'
	keep aha_hnpi hnpi Year
	bys aha_hnpi hnpi: gen obs=_n
	keep if obs==1
	drop obs
	save temp2, replace
	
	use temp1, clear
	append using temp2
	bys aha_hnpi hnpi: gen obs=_n
	keep if obs==1
	keep aha_hnpi hnpi Year
	save temp_npi_dat_`yr', replace
}
use temp_npi_dat_10, clear
foreach yr in 11 12 13 14 15 {
	append using temp_npi_dat_`yr'
}
save temp_npi_dat_all, replace

use temp_npi_dat_all, clear
merge m:1 hnpi Year using "${TEMP}\External_Hospital_Data.dta", keep(master match)
gen aha_match=(_merge==3)
gsort aha_hnpi -bdtot -totalpop
collapse (max) nonprofit profit teaching_hospital1 teaching_hospital2 system capital_imaging capital_caresetting capital_services int_hos urban ///
	tiera notier tierb tierc tierd ///
	(mean) labor_phys labor_residents labor_nurse labor_other total_discharges mcare_discharges mcaid_discharges tot_charges tot_discounts ///
	tot_opexp ip_charges intcare_charges ancserv_charges tot_mcarepymt sec_mcarepymt hvbp_payment hrrp_payment adjusted_case price cmi ///
	bdtot percent_profitable beds discount_factor discharge_share ///
	(sum) aha_match ///
	(first) hrrcode state county zip fips msa_no msaname totalpop market_discharges age_below18 age_18to34 age_35to64 age_65plus race_white race_black race_asian ///
	race_other income_75to100 income_25 income_25to50 income_50to75 income_100to150 income_150plus educ_noschool educ_highschool ///
	educ_hsgrad educ_somecollege educ_assoc educ_bach educ_graduate emp_fulltime educ_gradeschool totalhhi insurer1 share1 insurer2 share2, by(aha_hnpi Year)
save "${FINAL}\AHA_ConsolidatedNPI_Data.dta", replace

log close