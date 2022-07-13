*****************************************************************************
** Description: 				Pull/Organize All IP Claims for IP Pricing
** Author: 						Ian McCarthy
** Date Edited:					8/4/2021
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
log using "$LOGS\IPClaims_`date'.log", replace


/*
*****************************************************************************
** Generate claims/ip files
do "${CODE}\ClaimsIP1_AdmitsNPI.do"
do "${CODE}\ClaimsIP2_AHAMatch.do"
do "${CODE}\ClaimsIP3_UniqueAdmits.do"
do "${CODE}\ClaimsIP4_DRG_LatLong.do"
do "${CODE}\ClaimsIP5_Distance.do"
do "${CODE}\ClaimsIP6_Prices.do"
*/

*****************************************************************************
*APPEND INPATIENT DATA (INTERNAL)*
use "${FINAL}\FinalInpatientData_2010.dta", clear
foreach yr in 11 12 13 14 15 {
	append using "${FINAL}\FinalInpatientData_20`yr'.dta"
}

*****************************************************************************
*MERGE HOSPITAL DATA (EXTERNAL)*
merge m:1 aha_hnpi Year using "${FINAL}\AHA_ConsolidatedNPI_Data.dta", keep(master match)

*****************************************************************************
*Form surgery categories*
gen Type="Inpatient"
forvalues i=1/9 {
	rename proc_`i' code`i'
}

foreach x of newlist Surg_AntiReflux Surg_Appendectomy Surg_Circumcision Surg_IngHernia ///
	Surg_Orchiopexy Surg_Spine Surg_Humerus Surg_Strabismus Surg_Tonsils ///
	Surg_Tympanostomy Surg_UmbHernia Surg_Gallbladder Surg_Knee Surg_ECMO Surg_CHS Surg_Transplant Surg_Brain {
	gen `x'=0
}
forvalues i=1/9 {
	replace Surg_AntiReflux=(inlist(code`i',4466,4467)) if Surg_AntiReflux==0
	replace Surg_Appendectomy=(inlist(code`i',470,4701,4709)) if Surg_Appendectomy==0
	replace Surg_Circumcision=(inlist(code`i',640)) if Surg_Circumcision==0
	replace Surg_IngHernia=(inlist(code`i',171,172,530,531)) if Surg_IngHernia==0
	replace Surg_Orchiopexy=(inlist(code`i',625)) if Surg_Orchiopexy==0
	replace Surg_Spine=(inlist(code`i',810,8100,8103,8105,8108)) if Surg_Spine==0
	replace Surg_Humerus=(inlist(code`i',7901,7911,7921,7931)) if Surg_Humerus==0
	replace Surg_Strabismus=(inlist(code`i',1511,1512,1513,152,1521,1522,154)) if Surg_Strabismus==0
	replace Surg_Tonsils=(inlist(code`i',282,283,286)) if Surg_Tonsils==0
	replace Surg_Tympanostomy=(inlist(code`i',200,2001,2009)) if Surg_Tympanostomy==0
	replace Surg_UmbHernia=(inlist(code`i',534,5342,5349,5441)) if Surg_UmbHernia==0
	replace Surg_Gallbladder=(inlist(code`i',5123)) if Surg_Gallbladder==0
	replace Surg_Knee=(inlist(code`i',8006, 8016, 8026, 8036, 8046, 806, 8076, 8086, 8096, 8122, 8142, 8143, 8144, 8145, 8146, 8147)) if Surg_Knee==0	
	replace Surg_ECMO=(inlist(code`i',3965)) if Surg_ECMO==0
	replace Surg_CHS=(inlist(code`i',3766, 335, 3751, 336)) if Surg_CHS==0
	replace Surg_Transplant=(inlist(code`i',505,528,556)) if Surg_Transplant==0
	replace Surg_Brain=(inlist(code`i',1,12,121,122,123,124,131,132,139,141,142,151,152,153,159)) if Surg_Brain==0
}

forvalues i=1/48 {
	destring proc_cd_`i', replace force
	replace Surg_AntiReflux=(inlist(proc_cd_`i',43280,43327,43328)) if Surg_AntiReflux==0
	replace Surg_Appendectomy=(inlist(proc_cd_`i',44950,44960,44970)) if Surg_Appendectomy==0
	replace Surg_Circumcision=(inlist(proc_cd_`i',54150,54160,54161)) if Surg_Circumcision==0
	replace Surg_IngHernia=(inlist(proc_cd_`i',49491,49492,49495,49496,49500,49501,49505,49507,49520,49521,49525,49529,49595,49650)) if Surg_IngHernia==0
	replace Surg_Orchiopexy=(inlist(proc_cd_`i',54560,54640,54650,54692)) if Surg_Orchiopexy==0
	replace Surg_Spine=(inlist(proc_cd_`i',22800,22802,22804)) if Surg_Spine==0
	replace Surg_Humerus=(inlist(proc_cd_`i',24500,24505,24515,24516,24530,24535,24538,24545,24546,24560,24565,24566,24575,24576,24577,24579,24582)) if Surg_Humerus==0
	replace Surg_Strabismus=(inlist(proc_cd_`i',67311,67312,67314,67316,67318)) if Surg_Strabismus==0
	replace Surg_Tonsils=(inlist(proc_cd_`i',42820,42821,42825,42826,42830,42831,42835,42836)) if Surg_Tonsils==0
	replace Surg_Tympanostomy=(inlist(proc_cd_`i',69420,69421,69433,69436)) if Surg_Tympanostomy==0
	replace Surg_UmbHernia=(inlist(proc_cd_`i',49570,49572,49580,49582,49585,49587)) if Surg_UmbHernia==0
	replace Surg_Gallbladder=(inlist(proc_cd_`i',47562,47563,47564)) if Surg_Gallbladder==0
	replace Surg_Knee=(inlist(proc_cd_`i',27332, 27333, 29879, 29880, 29881, 29882, 29883, 29888)) if Surg_Knee==0	
}

*****************************************************************************
*Save FINAL DATASET*
save "${FINAL}\FinalInpatientData.dta", replace

log close


