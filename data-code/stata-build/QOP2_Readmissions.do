*****************************************************************************
** Identify readmissions following given surgery
** Step 1: Data of surgeries only
** Step 2: Data of all admits
** Step 3: Merge admits to surgeries if less than x days from discharge

use "${FINAL}\FinalOutpatientData.dta", clear
foreach x of varlist Surg_AntiReflux Surg_Appendectomy Surg_Circumcision Surg_IngHernia ///
	Surg_Orchiopexy Surg_Humerus Surg_Strabismus Surg_Tonsils ///
	Surg_Tympanostomy Surg_UmbHernia Surg_Gallbladder Surg_Knee {
	preserve
	keep if `x'==1
	keep z_patid z_visitid fst_admtdt Surg_* Year
	collapse (min) fst_admtdt, by(z_patid Surg_* Year)
	keep z_patid fst_admtdt Surg_* Year
	save temp_`x', replace
	restore
}

use "${FINAL}\FinalInpatientData.dta", clear
keep z_patid fst_admtdt last_dischdt
gen admit_date=fst_admtdt
save temp_admit, replace
	
clear
foreach x of newlist Surg_AntiReflux Surg_Appendectomy Surg_Circumcision Surg_IngHernia ///
	Surg_Orchiopexy Surg_Humerus Surg_Strabismus Surg_Tonsils ///
	Surg_Tympanostomy Surg_UmbHernia Surg_Gallbladder Surg_Knee {
	use temp_`x', clear

	merge m:m z_patid using temp_admit, keep(master match) nogenerate
	bys z_patid fst_admtdt: gen readmit_30=(admit_date<=(fst_admtdt+30) & admit_date>(fst_admtdt))
	bys z_patid fst_admtdt: gen readmit_60=(admit_date<=(fst_admtdt+60) & admit_date>(fst_admtdt))	
	bys z_patid fst_admtdt: gen readmit_90=(admit_date<=(fst_admtdt+90) & admit_date>(fst_admtdt))		
	bys z_patid fst_admtdt: egen any_readmit_30=max(readmit_30)
	bys z_patid fst_admtdt: egen any_readmit_60=max(readmit_60)
	bys z_patid fst_admtdt: egen any_readmit_90=max(readmit_90)
	bys z_patid fst_admtdt: gen obs=_n
	keep if obs==1
	keep z_patid fst_admtdt any_readmit_*
	save readmit_`x', replace
	clear
}
clear
foreach x of newlist Surg_AntiReflux Surg_Appendectomy Surg_Circumcision Surg_IngHernia ///
	Surg_Orchiopexy Surg_Humerus Surg_Strabismus Surg_Tonsils ///
	Surg_Tympanostomy Surg_UmbHernia Surg_Gallbladder Surg_Knee {
	use temp_`x', clear
	merge m:1 z_patid fst_admtdt using readmit_`x', keep(master match) nogenerate
	save `"${FINAL}\ReadmitOP_`x'.dta"', replace
	clear
}

use "${FINAL}\ReadmitOP_Surg_Appendectomy", clear
foreach x of varlist Surg_AntiReflux Surg_Circumcision Surg_IngHernia ///
	Surg_Orchiopexy Surg_Humerus Surg_Strabismus Surg_Tonsils ///
	Surg_Tympanostomy Surg_UmbHernia Surg_Gallbladder Surg_Knee {
	append using `"${FINAL}\ReadmitOP_`x'.dta"'
}
bys z_patid fst_admtdt: gen obs=_n
keep if obs==1
drop obs
save "${FINAL}\ReadmissionsData_OP.dta", replace
