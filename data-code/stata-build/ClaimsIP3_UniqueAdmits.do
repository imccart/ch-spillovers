/*
Description: This file collapses the inpatient claims into a single row per inpatient stay. We then
	merge back to that data the unique codes from each claim (reshaped into wide form where each row
	is a unique inpatient stay).
*/

foreach yr in 10 11 12 13 14 15 {

	use "${TEMP}\IPClaims_AHA_20`yr'.dta", clear
	gen str5 hosp_zip = string(prov_zip_5_cd, "%05.0f")
	if `yr' == 14 {
		replace ntwrk_ind = 0 if ntwrk_ind == .
		replace primary_cov_ind = "" if primary_cov_ind == "U"
		destring primary_cov_ind, replace
		replace primary_cov_ind = -1 if primary_cov_ind == . 
	}
	
	else if `yr' == 15 {
		replace ntwrk_ind = 0 if ntwrk_ind == .
		replace primary_cov_ind = -1 if primary_cov_ind == . 
	}
	
	else {
		replace ntwrk_ind = 0
		replace primary_cov_ind = 0
	}
	
	save "${TEMP}\raw claims.dta", replace
	
	*collapse IP claims to admissions
	collapse (sum) charge calc_allwd tot_mem_cs coins copay deduct (max) er_flag ntwrk_ind primary_cov_ind, ///
		by(z_patid z_group_id dstatus fst_admtdt last_dischdt ///
		mdc drg age_band_cd gdr mbr_zip_5_cd prod mkt_sgmnt_cd funding aha_hnpi hosp_zip) fast
	
	*find admits with multiple records
	duplicates tag z_patid fst_admtdt, gen(dup)
	tab dup
	
	*Process non-duplicates and save
	preserve
		keep if dup == 0
		drop dup 
		
		save "${TEMP}\claims_clean_pt1.dta", replace 
	restore
	
	*Clean duplicates 
	drop if dup == 0
	drop dup
	
	contract z_patid fst_admtdt  
	merge 1:m z_patid fst_admtdt using "${TEMP}\raw claims.dta"
	keep if _m == 3
	drop _m _f 
	
	bysort z_admit_id: egen max_stay = max(last_dischdt)
	replace last_dischdt = max_stay
	drop max_stay
	
	collapse (sum) charge calc_allwd tot_mem_cs coins copay deduct (max) er_flag ntwrk_ind primary_cov_ind, ///
		by(z_patid z_group_id dstatus fst_admtdt last_dischdt ///
		mdc drg age_band_cd gdr mbr_zip_5_cd prod mkt_sgmnt_cd funding aha_hnpi hosp_zip) fast

	duplicates tag z_patid fst_admtdt, gen(dup)
	tab dup
	drop dup
	
	replace dstatus = 0 if dstatus == .
	gsort z_patid fst_admtdt last_dischdt -dstatus
	replace dstatus = dstatus[_n-1] if dstatus == 0 & z_patid == z_patid[_n-1] ///
		& fst_admtdt == fst_admtdt[_n-1] & last_dischdt == last_dischdt[_n-1]
	
	gsort z_patid fst_admtdt last_dischdt
	replace aha_hnpi = aha_hnpi[_n-1] if aha_hnpi == "" & z_patid == z_patid[_n-1] ///
		& fst_admtdt == fst_admtdt[_n-1] & last_dischdt == last_dischdt[_n-1]
	
	collapse (sum) charge calc_allwd tot_mem_cs coins copay deduct (max) er_flag ntwrk_ind primary_cov_ind, ///
		by(z_patid z_group_id dstatus fst_admtdt last_dischdt ///
		mdc drg age_band_cd gdr mbr_zip_5_cd prod mkt_sgmnt_cd funding aha_hnpi hosp_zip) fast

	duplicates tag z_patid fst_admtdt, gen(dup)
	tab dup

	preserve
		keep if dup == 0 
		drop dup
			
		save "${TEMP}\claims_clean_pt2.dta", replace
	restore

	use  "${TEMP}\claims_clean_pt1.dta", clear
	append using  "${TEMP}\claims_clean_pt2.dta"
	
	duplicates tag z_patid last_dischdt , gen(dup)
	drop if dup>0
	drop dup

	drop if mbr_zip_5_cd == 99999 | mbr_zip_5_cd == .	
	drop if drg == .
	drop if mdc == ""
	drop if drg == 998 | drg == 999
	drop if mdc == "PR" | mdc == "AL"
	tab mdc
	
	gen los = last_dischdt - fst_admtdt 
	assert los != .
	sum los, det
	drop if los < 1 | los > r(p99)
	
	drop if calc_allwd <= 0
	drop if charge <= 0

	compress
	save "${FINAL}\CleanClaims`yr'.dta", replace

	erase "${TEMP}\raw claims.dta"
	erase "${TEMP}\claims_clean_pt1.dta"
	erase "${TEMP}\claims_clean_pt2.dta"
}


foreach yr in 10 11 12 13 14 15 {
	use "${TEMP}\IPClaims_AHA_20`yr'.dta", clear
	gen str5 hosp_zip = string(prov_zip_5_cd, "%05.0f")	
	keep z_patid dstatus fst_admtdt last_dischdt mdc drg age_band_cd gdr mbr_zip_5 prod mkt_sgmnt_cd funding aha_hnpi hosp_zip proc1 proc2 proc3 proc_cd diag1 diag2 diag3
	merge m:1 z_patid dstatus fst_admtdt last_dischdt mdc drg age_band_cd gdr mbr_zip_5 prod mkt_sgmnt_cd funding aha_hnpi hosp_zip ///
		using "${FINAL}\CleanClaims`yr'.dta"
	keep if _merge==3
	drop _merge
	keep z_patid fst_admtdt proc1 proc2 proc3 proc_cd diag1 diag2 diag3
	save "${TEMP}\raw claims.dta", replace
	foreach x in 1 2 3 {
		use "${TEMP}\raw claims.dta", clear
		keep z_patid fst_admtdt proc`x'
		drop if proc`x'==.
		bys z_patid fst_admtdt proc`x': gen obs=_n
		keep if obs==1
		drop obs
		bys z_patid fst_admtdt: gen claim_count=_n
		rename proc`x' proc`x'_		
		reshape wide proc`x'_, i(z_patid fst_admtdt) j(claim_count)
		save "${TEMP}\reshape_claims_`x'.dta", replace
	}

	foreach x in 1 2 3 {
		use "${TEMP}\raw claims.dta", clear
		keep z_patid fst_admtdt diag`x'
		drop if missing(diag`x')==1
		bys z_patid fst_admtdt diag`x': gen obs=_n
		keep if obs==1
		drop obs
		bys z_patid fst_admtdt: gen claim_count=_n
		rename diag`x' diag`x'_		
		reshape wide diag`x'_, i(z_patid fst_admtdt) j(claim_count)
		save "${TEMP}\reshape_claims_diag`x'.dta", replace
	}
		
	use "${TEMP}\raw claims.dta", clear
	keep z_patid fst_admtdt proc_cd
	drop if missing(proc_cd)==1
	bys z_patid fst_admtdt proc_cd: gen obs=_n
	keep if obs==1
	drop obs
	bys z_patid fst_admtdt: gen claim_count=_n
	rename proc_cd proc_cd_
	reshape wide proc_cd_, i(z_patid fst_admtdt) j(claim_count)
	save "${TEMP}\reshape_claims_proc_cd.dta", replace
	
	use "${TEMP}\reshape_claims_1.dta", clear
	merge 1:1 z_patid fst_admtdt using "${TEMP}\reshape_claims_2.dta", nogenerate
	merge 1:1 z_patid fst_admtdt using "${TEMP}\reshape_claims_3.dta", nogenerate
	merge 1:1 z_patid fst_admtdt using "${TEMP}\reshape_claims_proc_cd.dta", nogenerate	
	
	merge 1:1 z_patid fst_admtdt using "${TEMP}\reshape_claims_diag1.dta", nogenerate
	merge 1:1 z_patid fst_admtdt using "${TEMP}\reshape_claims_diag2.dta", nogenerate
	merge 1:1 z_patid fst_admtdt using "${TEMP}\reshape_claims_diag3.dta", nogenerate
	
	save "${TEMP}\reshape_claims_all.dta", replace
	
	use "${FINAL}\CleanClaims`yr'.dta", clear
	merge 1:1 z_patid fst_admtdt using "${TEMP}\reshape_claims_all.dta", nogenerate
	local step=0
	forvalues t=1/3 {
		forvalues z=1/10 {
			capture confirm variable proc`t'_`z'
			if !_rc {
				local step=`step'+1
				gen proc_`step'=proc`t'_`z'
				drop proc`t'_`z'
			}
		}
	}
	local step=0
	forvalues t=1/3 {
		forvalues z=1/10 {
			capture confirm variable diag`t'_`z'
			if !_rc {
				local step=`step'+1
				gen diag_`step'=diag`t'_`z'
				drop diag`t'_`z'
			}
		}
	}
	
	compress
	save "${FINAL}\CleanClaims`yr'.dta", replace
	
	erase "${TEMP}\raw claims.dta"	
	erase "${TEMP}\reshape_claims_1.dta"
	erase "${TEMP}\reshape_claims_2.dta"
	erase "${TEMP}\reshape_claims_3.dta"	
	erase "${TEMP}\reshape_claims_all.dta"	
}
