*****************************************************************************
** Description: 				Calculate Total Inpatient Stays for each Hospital
** Author: 						  Ian McCarthy
** Date Edited:					10/16/18
*****************************************************************************
clear
capture log close
set more off
pause on

*****************************************************************************
*SET PATHS*
global SAS_DATA = "K:\Home\mccarthy-ian-hcci\Documents\Research"
global MAIN = "K:\Research\Raval_Emory\Childrens Hospitals"
global CODE = "$MAIN\Analysis"
global LOGS = "$MAIN\Logs"
global DATA = "$MAIN\Data"
global TEMP = "$MAIN\Data\Temp"
global FINAL = "$MAIN\Data\Final"

local date = "$S_DATE"
log using "$LOGS\IPAdmits_`date'.log", replace


*****************************************************************************
** Generate claims/ip files
do "${CODE}\A1_AdmitsNPI.do"
do "${CODE}\A3_UniqueAdmits.do"

*****************************************************************************
*Collapse Admits to Hospital Level*
foreach yr in 10 11 12 13 14 15 {
	use using "${FINAL}\CleanAdmits`yr'.dta"
	egen admit=group(z_patid dstatus mdc fst_admtdt)
	bys admit: gen admit_obs=_n
	gen new_admit=(admit_obs==1)
	gen new_ped_admit=(admit_obs==1 & age_band_cd==1 & newborn_flag==0)
	collapse (sum) new_admit newborn_flag new_ped_admit, by(aha_hnpi hosp_zip)
	gen Year=20`yr'
	save "${TEMP}\HospitalTotals`yr'.dta", replace
}

*****************************************************************************
*Append final dataset*
use "${TEMP}\HospitalTotals10.dta", clear
foreach yr in 11 12 13 14 15 {
	append using "${TEMP}\HospitalTotals`yr'.dta"
}
save "${FINAL}\TotalHospitalAdmits.dta", replace

log close


