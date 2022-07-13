*****************************************************************************
** Description: 			Collect all data files
** Author: 						Ian McCarthy
** Date Edited:				8/4/2021
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
log using "$LOGS\BuildData_`date'.log", replace

*****************************************************************************
*Create risk index for pediatric patients*
do "${CODE}\Pediatric_Index.do"
	
use "${FINAL}\FinalInpatientData.dta", clear
keep if fst_admtdt<d(01oct2015)
keep z_patid aha_hnpi fst_admtdt last_dischdt code1-code9 diag_1-diag_9
forvalues i=1/9 {
	rename code`i' pc`i'
	tostring pc`i', replace
	replace pc`i'="" if pc`i'=="."
}
forvalues i=1/9 {
	rename diag_`i' dx`i'
}
ccc temp_index_dat dx 9 pc 9 9
keep z_patid fst_admtdt last_dischdt neuromusc_ccc cvd_ccc respiratory_ccc ///
	renal_ccc gi_ccc hemato_immu_ccc metabolic_ccc congeni_genetic_ccc ///
	malignancy_ccc neonatal_ccc tech_dep_ccc transplant_ccc num_ccc ccc_flag
save temp_ped_index_ip, replace

use "${FINAL}\FinalOutpatientData.dta", clear
keep if last_dischdt<d(01oct2015)
keep z_patid z_visitid diag_1-diag_15
forvalues i=1/15 {
	rename diag_`i' dx`i'
	replace dx`i'=subinstr(dx`i',"&","",.)
}
ccc temp_index_dat dx 15 pc 0 9
keep z_patid z_visitid neuromusc_ccc cvd_ccc respiratory_ccc ///
	renal_ccc gi_ccc hemato_immu_ccc metabolic_ccc congeni_genetic_ccc ///
	malignancy_ccc neonatal_ccc tech_dep_ccc transplant_ccc num_ccc ccc_flag
save temp_ped_index_op, replace

	
*****************************************************************************
*Merge Final Datasets*
use "${FINAL}\FinalInpatientData.dta", clear
drop code*
drop proc_cd_*
drop diag_*
merge 1:1 z_patid fst_admtdt last_dischdt using temp_ped_index_ip, nogenerate
foreach x of varlist neuromusc_ccc cvd_ccc respiratory_ccc ///
	renal_ccc gi_ccc hemato_immu_ccc metabolic_ccc congeni_genetic_ccc ///
	malignancy_ccc neonatal_ccc tech_dep_ccc transplant_ccc num_ccc ccc_flag {
	replace `x'=0 if `x'==.
}
save temp_final_ip, replace

use temp_final_ip, clear
keep aha_hnpi
bys aha_hnpi: gen obs=_n
keep if obs==1
drop obs
save ip_hnpi, replace

use "${FINAL}\FinalOutpatientData.dta", clear
merge m:1 aha_hnpi using ip_hnpi, nogenerate keep(match)
drop code*
drop proc_cd*
drop diag_*
merge 1:1 z_patid z_visitid using temp_ped_index_op, nogenerate keep(master match)
foreach x of varlist neuromusc_ccc cvd_ccc respiratory_ccc ///
	renal_ccc gi_ccc hemato_immu_ccc metabolic_ccc congeni_genetic_ccc ///
	malignancy_ccc neonatal_ccc tech_dep_ccc transplant_ccc num_ccc ccc_flag {
	replace `x'=0 if `x'==.
}
save temp_final_op, replace

use temp_final_ip, clear
append using temp_final_op
save temp_final_proc, replace

use "${FINAL}\QualityData_IP.dta", clear
keep z_patid fst_admtdt Type Comp_* any_readmit_*
save temp_quality_ip, replace

use "${FINAL}\QualityData_OP.dta", clear
keep z_patid fst_admtdt Type Comp_* any_readmit_*
save temp_quality_op, replace

use temp_quality_ip, clear
append using temp_quality_op
save temp_final_quality, replace



*****************************************************************************
*Save final "big" dataset
use temp_final_proc, clear
drop if Type=="Inpatient" & mdc=="15"   /*drop newborns*/
drop if aha_hnpi==""					/*drop missing consolidated npis*/

** quality data
merge m:1 z_patid fst_admtdt Type using temp_final_quality, generate(Quality_Match) keep(master match)

** aggregate hospital-level info (admits, etc.)
merge m:1 aha_hnpi hosp_zip Year using "${FINAL}\TotalHospitalAdmits.dta", generate(Admits_Match) keep(master match)

** Children's Hospital Tiers
gen CH_Tier=.
replace CH_Tier=4 if tierd==1
replace CH_Tier=3 if tierc==1
replace CH_Tier=2 if tierb==1
replace CH_Tier=1 if tiera==1

** Surgery factor variable
gen Surgery=""
local types AntiReflux Appendectomy Circumcision IngHernia Orchiopexy Spine Humerus Strabismus Tonsils Tympanostomy UmbHernia Gallbladder Knee
foreach x of local types {
	replace Surgery="Double" if Surg_`x'==1 & Surgery!=""	
	replace Surgery="`x'" if Surg_`x'==1 & Surgery==""
}

drop _merge
save "${FINAL}\FinalData_All.dta", replace
export delimited using "${FINAL}\FinalData_All.txt", quote delimiter(tab) replace
log close


*****************************************************************************
*Save final "small" dataset
use temp_final_proc, clear
drop if Type=="Inpatient" & mdc=="15"   /*drop newborns*/
drop if aha_hnpi==""					/*drop missing consolidated npis*/

gen SelectedSurgery=0
foreach x of varlist Surg_AntiReflux Surg_Appendectomy Surg_Circumcision Surg_IngHernia ///
	Surg_Orchiopexy Surg_Spine Surg_Humerus Surg_Strabismus Surg_Tonsils ///
	Surg_Tympanostomy Surg_UmbHernia Surg_Gallbladder Surg_Knee {
	replace SelectedSurgery=1 if `x'==1
}
keep if SelectedSurgery==1
drop SelectedSurgery

** quality data
merge m:1 z_patid fst_admtdt Type using temp_final_quality, generate(Quality_Match) keep(master match)

** aggregate hospital-level info (admits, etc.)
merge m:1 aha_hnpi hosp_zip Year using "${FINAL}\TotalHospitalAdmits.dta", generate(Admits_Match) keep(master match)

** Children's Hospital Tiers
gen CH_Tier=.
replace CH_Tier=4 if tierd==1
replace CH_Tier=3 if tierc==1
replace CH_Tier=2 if tierb==1
replace CH_Tier=1 if tiera==1


** Surgery factor variable
gen Surgery=""
local types AntiReflux Appendectomy Circumcision IngHernia Orchiopexy Spine Humerus Strabismus Tonsils Tympanostomy UmbHernia Gallbladder Knee
foreach x of local types {
	replace Surgery="Double" if Surg_`x'==1 & Surgery!=""	
	replace Surgery="`x'" if Surg_`x'==1 & Surgery==""
}

drop _merge Surg_ECMO Surg_CHS Surg_Transplant Surg_Brain
save "${FINAL}\FinalData.dta", replace
export delimited using "${FINAL}\FinalData.txt", quote delimiter(tab) replace
log close



