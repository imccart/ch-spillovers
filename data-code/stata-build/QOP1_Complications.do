*****************************************************************************
use "${FINAL}\FinalInpatientData.dta", clear

** Generate complication indicators
foreach x of newlist Comp_Wound Comp_MinorSSI Comp_MajorSSI Comp_UTI Comp_PostRenal Comp_Pneumonia ///
	Comp_RespFail Comp_Sepsis Comp_DVT Comp_PulEmb Comp_AMI Comp_CardiacArrest Comp_CVA Comp_Intraop ///
	Comp_General {
	gen `x'=0
}

forvalues i=1/19 {
	tostring code`i', gen(str_code`i')
	gen code_3d_`i'=substr(str_code`i',1,3)
	gen code_4d_`i'=substr(str_code`i',1,4)
	destring code_3d_`i', replace
	destring code_4d_`i', replace
	replace Comp_Wound=(inlist(code`i',99883,99812,99813,9983,9986)) if Comp_Wound==0
	replace Comp_MinorSSI=(inlist(code`i',99832,9985,99851,99859)) if Comp_MinorSSI==0
	replace Comp_MajorSSI=(inlist(code`i',99831,56722,99859)) if Comp_MajorSSI==0
	replace Comp_UTI=(inlist(code`i',5990,99664)) if Comp_UTI==0
	replace Comp_PostRenal=(inlist(code_3d_`i',584,586)) if Comp_PostRenal==0
	replace Comp_Pneumonia=(inlist(code_3d_`i',480,482,483) | inlist(code`i',481,484,485,486,4870,99731)) if Comp_Pneumonia==0
	replace Comp_RespFail=(inlist(code`i',5185,51882)) if Comp_RespFail==0
	replace Comp_Sepsis=(code_3d_`i'==038 | inlist(code`i',78552,7907,99591,99592,9980)) if Comp_Sepsis==0
	replace Comp_DVT=(code_4d_`i'==4534 | code`i'==4539) if Comp_DVT==0
	replace Comp_PulEmb=(inlist(code`i',4151,41519)) if Comp_PulEmb==0
	replace Comp_AMI=(inlist(code_4d_`i',4100,4101)) if Comp_AMI==0
	replace Comp_CardiacArrest=(code`i'==4275) if Comp_CardiacArrest==0
	replace Comp_CVA=(code`i'==99702) if Comp_CVA==0
	replace Comp_Intraop=(inlist(code`i',99811,9982,9984)) if Comp_Intraop==0
	replace Comp_General=(inlist(code`i',9971,9973,9974,9975)) if Comp_General==0	
}
save temp_comp, replace


** Identify complications following given surgery
** Step 1: Data of surgeries only
** Step 2: Data of complications only
** Step 3: Merge complications to surgeries if less than x days from procedures

use "${FINAL}\FinalOutpatientData.dta", clear
foreach x of varlist Surg_AntiReflux Surg_Appendectomy Surg_Circumcision Surg_IngHernia ///
	Surg_Orchiopexy Surg_Humerus Surg_Strabismus Surg_Tonsils ///
	Surg_Tympanostomy Surg_UmbHernia Surg_Gallbladder Surg_Knee {
	preserve
	keep if `x'==1
	keep z_patid z_visitid fst_admtdt Surg_* Year 
	save temp_`x', replace
	restore
}

use temp_comp, clear
foreach y of varlist Comp_Wound Comp_MinorSSI Comp_MajorSSI Comp_UTI Comp_PostRenal Comp_Pneumonia ///
	Comp_RespFail Comp_Sepsis Comp_DVT Comp_PulEmb Comp_AMI Comp_CardiacArrest Comp_CVA Comp_Intraop ///
	Comp_General {
	preserve
	collapse (sum) `y', by(z_patid fst_admtdt last_dischdt)
	replace `y'=(`y'>0 & `y' !=.)
	rename fst_admtdt comp_date
	save temp_`y', replace
	restore
}	

clear
foreach x of newlist Surg_AntiReflux Surg_Appendectomy Surg_Circumcision Surg_IngHernia ///
	Surg_Orchiopexy Surg_Humerus Surg_Strabismus Surg_Tonsils ///
	Surg_Tympanostomy Surg_UmbHernia Surg_Gallbladder Surg_Knee {
	use temp_`x', clear
	local step=0
	foreach y of newlist Comp_Wound Comp_MinorSSI Comp_MajorSSI Comp_UTI Comp_PostRenal Comp_Pneumonia ///
		Comp_RespFail Comp_Sepsis Comp_DVT Comp_PulEmb Comp_AMI Comp_CardiacArrest Comp_CVA Comp_Intraop ///
		Comp_General {	
		local step=`step'+1
		merge m:m z_patid using temp_`y', keep(master match) nogenerate
		bys z_patid fst_admtdt: egen all_`y'=sum(`y') if comp_date<=(fst_admtdt+90) & comp_date>=fst_admtdt
		bys z_patid fst_admtdt: gen obs=_n
		keep if obs==1
		replace `y'=(all_`y'>0 & all_`y'!=.)
		keep z_patid fst_admtdt `y'
		save temp2_`x'_`step', replace
	}
	clear
}
clear
foreach x of newlist Surg_AntiReflux Surg_Appendectomy Surg_Circumcision Surg_IngHernia ///
	Surg_Orchiopexy Surg_Humerus Surg_Strabismus Surg_Tonsils ///
	Surg_Tympanostomy Surg_UmbHernia Surg_Gallbladder Surg_Knee {
	use temp_`x', clear
	forvalues i=1/15 {
		merge m:1 z_patid fst_admtdt using temp2_`x'_`i', keep(master match) nogenerate
	}
	save `"${FINAL}\DataOP_`x'.dta"', replace
	clear
}

use "${FINAL}\DataOP_Surg_Appendectomy", clear
foreach x of varlist Surg_AntiReflux Surg_Circumcision Surg_IngHernia ///
	Surg_Orchiopexy Surg_Humerus Surg_Strabismus Surg_Tonsils ///
	Surg_Tympanostomy Surg_UmbHernia Surg_Gallbladder Surg_Knee {
	append using `"${FINAL}\DataOP_`x'.dta"'
}
egen Comp_Any=rowtotal(Comp_*)
bys z_patid fst_admtdt: gen obs=_n
keep if obs==1
drop obs
save `"${FINAL}\ComplicationsData_OP.dta"', replace
