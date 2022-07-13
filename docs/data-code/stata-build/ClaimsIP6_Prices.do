/*
Description: This file creates the final analytic dataset. We drop patients with big
	distances from the hospital (more than 180 miles) and we drop outlier payments as
	well as transfers within a hospital
*/

foreach yr in 10 11 12 13 14 15 {
    
	use "${FINAL}\Inpatient_Analysis`yr'.dta", clear
	
	*merge travel times
	merge m:1 pt_zip hosp_zip aha_hnpi using "${TEMP}\HospDistance`yr'.dta", keepus(distance_mi)
	drop _m
	drop if distance_mi > 180
	
	*Drop transfers
	sort z_patid fst_admtdt last_dischdt
	gen flag = 1 if fst_admtdt == last_dischdt[_n-1] & z_patid == z_patid[_n-1]
	replace flag = 1 if fst_admtdt < last_dischdt[_n-1] & z_patid == z_patid[_n-1]
	replace flag = 1 if fst_admtdt + 1 == last_dischdt[_n-1]-1 & z_patid == z_patid[_n-1]
	drop if flag == 1
	drop flag

	*Drop outlier dollars - secondary payers or payment outliers 
	gen pmnt_ratio = calc_allwd/charge
	sum pmnt_ratio, det
	drop if pmnt_ratio < r(p5) | pmnt_ratio>r(p95)
	drop pmnt_ratio

	*Create variables for analysis file
	gen wtd_price = calc_allwd/drg_wt
	gen wtd_charge = charge/drg_wt
	
	save "${FINAL}\FinalInpatientData_20`yr'.dta", replace

}

