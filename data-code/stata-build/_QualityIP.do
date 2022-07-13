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
log using "$LOGS\QualityIP_`date'.log", replace

*****************************************************************************
** Run relevant code files
do "${CODE}\QIP1_Complications.do"
do "${CODE}\QIP2_Readmissions.do"

*****************************************************************************
** Merge complication and readmission data for inpatient surgeries
use "${FINAL}\ComplicationsData_IP.dta", clear
merge 1:1 z_patid fst_admtdt last_dischdt using "${FINAL}\ReadmissionsData_IP.dta", keep(master match) nogenerate
gen Type="Inpatient"
save "${FINAL}\QualityData_IP.dta", replace


*****************************************************************************
** Average hospital quality
use "${FINAL}\QualityData_IP.dta", clear
collapse (mean) Comp_* any_readmit_*, by(aha_hnpi Year)
save "${FINAL}\Hospital_Quality.dta", replace
log close
