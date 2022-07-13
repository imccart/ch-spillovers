foreach yr in 10 11 12 13 14 15 {
	use "${FINAL}\Outpatient_Analysis`yr'.dta", clear
	
	*merge travel times
	merge m:1 pt_zip hosp_zip aha_hnpi using "${TEMP}\OutpatientDistance`yr'.dta", keepus(distance_mi)
	drop _m
	drop if distance_mi > 180
	

	*Drop outlier dollars - secondary payers or payment outliers 
	gen pmnt_ratio = calc_allwd/charge
	sum pmnt_ratio, det
	drop if pmnt_ratio < r(p5) | pmnt_ratio>r(p95)
	drop pmnt_ratio

	*Create variables for analysis file
	gen price = calc_allwd
	
	save "${FINAL}\FinalOutpatientData_20`yr'.dta", replace

}

/*
*WTP calculation
foreach yr in 10 11 12 13 14 15 {

	use "$temp\Master file 20`yr'.dta", clear
	merge m:1 pt_zip using "$temp\geo info.dta"
	keep if _m == 3
	drop _m
	gen obs = 1
	
	gen female = gdr == 2	
	xtile drg_wt_qnt = drg_wt, n(5)
	
	gen sys_hosp = sys_id
	tostring sys_hosp, replace
	replace sys_hosp = aha_hnpi if sys_hosp == "."
	
	*Set parameters for groupings 
	local minsize = 25
	local chars = "pt_fips pt_zip mdc er_flag drg_type drg_wt_qnt drg age_band_cd female prod"
	local numchars = wordcount("`chars'")
	gen final_group = .
	gen group_size = .
		
	while `numchars' >= 1 {
		egen c = total(obs), by(`chars')
		egen g = group(`chars') if c >= `minsize' & final_group == .
			
		*define group number based on prior group num
		qui sum final_group
		local maxgroup = r(max)
		if `maxgroup' == . local maxgroup = 0
		replace final_group = g + `maxgroup' if final_group == .
		replace group_size = c if final_group != . & group_size == .
			
		*update char list 
		local tempchars = "`chars'"
		local numchars = `numchars'-1
		local chars = ""
		forvalues X = 1/`numchars' {
			local chars = "`chars' " + word("`tempchars'", `X')
		}
			
		drop c g
		replace obs = 0 if final_group != .
	}
		
	sum group_size, det
	sum obs if final_group == .
	drop if final_group == .
		
	*collapse groups
	replace obs = 1
	collapse (sum) patients = obs, by(final_group aha_hnpi sys_hosp group_size) fast

	egen N_in_group = total(patients), by(final_group)
		
	*check group sizes
	assert N_in_group == group_size
	assert N_in_group >= `minsize'
	drop group_size

	*Patients = patients at the hospital in the group
	gen share = patients/N_in_group //share of total patients at the hospital 
	egen syspatients = total(patients), by(final_group sys_hosp) //total patients at the system
	gen sysshare = syspatients/N_in_group //share of patients at the system
		
	replace share = .95 if share==1
	replace sysshare = .95 if sysshare==1
		
	gen hospwtp = log(1/(1-share))
	gen syswtp = log(1/(1-sysshare))
		
	replace hospwtp = hospwtp * N_in_group
	replace syswtp = syswtp * N_in_group
		
	*calculate system WTP across all system hospitals
	preserve
		bysort final_group sys_hosp: keep if _n == 1 
		collapse (sum) syspatients syswtp, by(sys_hosp) fast
		save "$temp\sys_wtp_20`yr'.dta", replace
	restore
		
	*calculate hosp WTP & combine with sys WTPs
	collapse (sum) hospwtp patients, by(aha_hnpi sys_hosp) fast

	merge m:1 sys_hosp using "$temp\sys_wtp_20`yr'.dta"
	assert _m == 3
	drop _m
		
	gen pts = 1
	
	collapse (sum) pts, by(aha_hnpi sys_hosp syswtp syspatients hospwtp patients) fast
	gen wtp_pp = syswtp/syspatients
	
	merge 1:m aha_hnpi using "$temp\Master file 20`yr'.dta"
	assert _m != 1
	save "$output\Master file 20`yr'.dta", replace
}
*/

