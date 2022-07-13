global HOSPITAL_VARS bdtot nonprofit profit teaching_hospital1 teaching_hospital2 system labor_phys labor_residents ///
	labor_nurse labor_other capital_imaging capital_caresetting capital_services percent_profitable int_hos state ///
	county beds total_discharges mcare_discharges mcaid_discharges tot_charges tot_discounts tot_opexp ip_charges ///
	intcare_charges ancserv_charges tot_mcarepymt sec_mcarepymt hvbp_payment hrrp_payment missingcharges discount_factor ///
	price market_discharges discharge_share cmi adjusted_cases urban totalpop age_below18 age_18to34 age_35to64 age_65plus ///
	race_white race_black race_asian race_other income_75to100 income_25 income_25to50 income_50to75 income_100to150 ///
	income_150plus educ_noschool educ_gradeschool educ_highschool educ_hsgrad educ_somecollege educ_assoc educ_bach ///
	educ_graduate emp_fulltime fips msa_no msaname totalhhi insurer1 share1 insurer2 share2 tiera notier tierb tierc tierd

** form hospital dataset
use "${FINAL}\FinalInpatientData.dta", clear
bys aha_hnpi Year: gen obs=_n
keep if obs==1
drop obs
save temp_hospital_data, replace
	
*****************************************************************************
** Estimate choice models
forvalues i=2010/2015 {
	use "${FINAL}\FinalChoiceSets_`i'.dta", clear
	gen Year=`i'
	
	** bring in patient data and diagnosis information
	merge m:1 z_patid fst_admtdt last_dischdt using ///
		"${FINAL}\FinalInpatientData.dta", nogenerate ///
		keepusing(mdc drg age_band_cd gdr mbr_zip_5_cd Surg_*) keep(match)

	** bring in hospital characteristics	
	merge m:1 aha_hnpi Year using temp_hospital_data, nogenerate ///
		keepusing($HOSPITAL_VARS) keep(match)

	** bring in hospital quality data
	merge m:1 aha_hnpi Year using "${FINAL}\Hospital_Quality.dta", nogenerate keep(match)
	
	** form interaction variables
	gen female=(gdr==2) if gdr!=.
	gen MajorTeaching=(teaching_hospital1==1) if teaching_hospital1!=.
	gen employment=(int_hos==6) if int_hos!=.
	gen payor_mix=mcare_discharges/total_discharges
	bys z_patid: egen min_distance=min(distance_mi)
	gen diff_distance=distance_mi-min_distance
	foreach x of varlist female bdtot MajorTeaching profit system employment payor_mix ///
		cmi Comp_Any any_readmit_30 {
		gen dist_`x'=`x'*diff_distance
	}
	foreach x of varlist bdtot MajorTeaching profit system employment payor_mix ///
		cmi Comp_Any any_readmit_30 {
		gen female_`x'=`x'*female
	}
	
	
	** keep relevant surgeries
	gen selected_surgery=0
	foreach x of varlist Surg_AntiReflux Surg_Appendectomy Surg_Circumcision Surg_IngHernia ///
		Surg_Orchiopexy Surg_Spine Surg_Humerus Surg_Strabismus Surg_Tonsils ///
		Surg_Tympanostomy Surg_UmbHernia {
		replace selected_surgery=1 if `x'==1
	}
	keep if selected_surgery==1
	
	clogit choice female_* diff_distance dist_* bdtot MajorTeaching profit system employment payor_mix cmi Comp_Any any_readmit_30, group(z_patid)
	est store bchoice
	predict pred_prob1 if e(sample)==1
	bys z_patid: gen set_size=_N
	sum set_size if e(sample)==1
	replace pred_prob1=1 if set_size==1
	
	keep z_patid fst_admtdt last_dischdt aha_hnpi Year choice pred_prob1 set_size
	save "${FINAL}\PredictedProbability_`i'.dta", replace
}
	

	


