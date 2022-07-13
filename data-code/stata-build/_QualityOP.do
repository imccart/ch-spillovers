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
log using "$LOGS\QualityOP_`date'.log", replace

*****************************************************************************
** Run relevant code files
do "${CODE}\QOP1_Complications.do"
do "${CODE}\QOP2_Readmissions.do"

*****************************************************************************
** Merge complications and readmissions data for outpatient surgeries
use "${FINAL}\ComplicationsData_OP.dta", clear
merge 1:1 z_patid fst_admtdt using "${FINAL}\ReadmissionsData_OP.dta", keep(master match) nogenerate
gen Type="Outpatient"
save "${FINAL}\QualityData_OP.dta", replace
log close
