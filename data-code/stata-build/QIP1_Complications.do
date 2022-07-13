*****************************************************************************
use "${FINAL}\FinalInpatientData.dta", clear

** Generate complication indicators
foreach x of newlist Comp_Wound Comp_MinorSSI Comp_MajorSSI Comp_UTI Comp_PostRenal Comp_Pneumonia ///
	Comp_RespFail Comp_Sepsis Comp_DVT Comp_PulEmb Comp_AMI Comp_CardiacArrest Comp_CVA Comp_Intraop ///
	Comp_General {
	gen `x'=0
}

forvalues i=1/16 {
	rename diag_`i' diag`i'
	destring diag`i', force replace
}


forvalues i=1/16 {
	tostring diag`i', gen(str_diag`i')
	gen diag_3d_`i'=substr(str_diag`i',1,3)
	gen diag_4d_`i'=substr(str_diag`i',1,4)
	destring diag_3d_`i', replace
	destring diag_4d_`i', replace
	replace Comp_Wound=(inlist(diag`i',99883,99812,99813,9983,9986)) if Comp_Wound==0
	replace Comp_MinorSSI=(inlist(diag`i',99832,9985,99851,99859)) if Comp_MinorSSI==0
	replace Comp_MajorSSI=(inlist(diag`i',99831,56722,99859)) if Comp_MajorSSI==0
	replace Comp_UTI=(inlist(diag`i',5990,99664)) if Comp_UTI==0
	replace Comp_PostRenal=(inlist(diag_3d_`i',584,586)) if Comp_PostRenal==0
	replace Comp_Pneumonia=(inlist(diag_3d_`i',480,482,483) | inlist(code`i',481,484,485,486,4870,99731)) if Comp_Pneumonia==0
	replace Comp_RespFail=(inlist(diag`i',5185,51882)) if Comp_RespFail==0
	replace Comp_Sepsis=(diag_3d_`i'==038 | inlist(code`i',78552,7907,99591,99592,9980)) if Comp_Sepsis==0
	replace Comp_DVT=(diag_4d_`i'==4534 | code`i'==4539) if Comp_DVT==0
	replace Comp_PulEmb=(inlist(diag`i',4151,41519)) if Comp_PulEmb==0
	replace Comp_AMI=(inlist(diag_4d_`i',4100,4101)) if Comp_AMI==0
	replace Comp_CardiacArrest=(diag`i'==4275) if Comp_CardiacArrest==0
	replace Comp_CVA=(diag`i'==99702) if Comp_CVA==0
	replace Comp_Intraop=(inlist(diag`i',99811,9982,9984)) if Comp_Intraop==0
	replace Comp_General=(inlist(diag`i',9971,9973,9974,9975)) if Comp_General==0	
}

** Identify complications following given surgery
** Step 1: Data of surgeries only
** Step 2: Data of complications only
** Step 3: Merge complications to surgeries if less than x days from admission or discharge
foreach x of varlist Surg_AntiReflux Surg_Appendectomy Surg_Circumcision Surg_IngHernia ///
	Surg_Orchiopexy Surg_Spine Surg_Humerus Surg_Strabismus Surg_Tonsils ///
	Surg_Tympanostomy Surg_UmbHernia Surg_Gallbladder Surg_Knee {
	preserve
	keep if `x'==1
	keep z_patid fst_admtdt last_dischdt los age_band_cd gdr mbr_zip_5_cd Surg_* calc_allwd wtd_price wtd_charge aha_hnpi state county zip hosp_zip Year
	save temp_`x', replace
	restore
}
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
	Surg_Orchiopexy Surg_Spine Surg_Humerus Surg_Strabismus Surg_Tonsils ///
	Surg_Tympanostomy Surg_UmbHernia Surg_Gallbladder Surg_Knee {
	use temp_`x', clear
	local step=0
	foreach y of newlist Comp_Wound Comp_MinorSSI Comp_MajorSSI Comp_UTI Comp_PostRenal Comp_Pneumonia ///
		Comp_RespFail Comp_Sepsis Comp_DVT Comp_PulEmb Comp_AMI Comp_CardiacArrest Comp_CVA Comp_Intraop ///
		Comp_General {	
		local step=`step'+1
		merge m:m z_patid using temp_`y', keep(master match) nogenerate
		bys z_patid fst_admtdt last_dischdt los age_band_cd gdr mbr_zip_5_cd: egen all_`y'=sum(`y') if comp_date<=(fst_admtdt+30+los) & comp_date>=fst_admtdt
		bys z_patid fst_admtdt last_dischdt los age_band_cd gdr mbr_zip_5_cd: gen obs=_n
		keep if obs==1
		replace `y'=(all_`y'>0 & all_`y'!=.)
		keep z_patid fst_admtdt last_dischdt los age_band_cd gdr mbr_zip_5_cd `y'
		save temp2_`x'_`step', replace
	}
	clear
}
clear
foreach x of newlist Surg_AntiReflux Surg_Appendectomy Surg_Circumcision Surg_IngHernia ///
	Surg_Orchiopexy Surg_Spine Surg_Humerus Surg_Strabismus Surg_Tonsils ///
	Surg_Tympanostomy Surg_UmbHernia Surg_Gallbladder Surg_Knee {
	use temp_`x', clear
	forvalues i=1/15 {
		merge 1:1 z_patid fst_admtdt last_dischdt los age_band_cd gdr mbr_zip_5_cd using temp2_`x'_`i', keep(master match) nogenerate
	}
	save `"${FINAL}\DataIP_`x'.dta"', replace
	clear
}

use "${FINAL}\DataIP_Surg_Appendectomy", clear
foreach x of varlist Surg_AntiReflux Surg_Circumcision Surg_IngHernia ///
	Surg_Orchiopexy Surg_Spine Surg_Humerus Surg_Strabismus Surg_Tonsils ///
	Surg_Tympanostomy Surg_UmbHernia Surg_Gallbladder Surg_Knee {
	append using `"${FINAL}\DataIP_`x'.dta"'
}
egen Comp_Any=rowtotal(Comp_*)
bys z_patid fst_admtdt last_dischdt: gen obs=_n
keep if obs==1
drop obs
save `"${FINAL}\ComplicationsData_IP.dta"', replace
