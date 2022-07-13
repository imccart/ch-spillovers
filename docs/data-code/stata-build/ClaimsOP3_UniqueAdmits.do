
foreach yr in 10 11 12 13 14 15 {
	use "${TEMP}\OPClaims_AHA_20`yr'.dta", clear
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
	
	*collapse OP claims to single outpatient visit
/*
	collapse (sum) charge calc_allwd tot_mem_cs coins copay deduct (max) ntwrk_ind primary_cov_ind, ///
		by(z_patid z_group_id dstatus fst_admtdt  ///
		proc_cd age_band_cd gdr mbr_zip_5_cd prod mkt_sgmnt_cd funding aha_hnpi hosp_zip) fast
*/

	collapse (sum) charge calc_allwd tot_mem_cs coins copay deduct (max) ntwrk_ind primary_cov_ind last_dischdt=fst_admtdt (min) fst_admtdt, ///
		by(z_patid z_group_id dstatus z_visitid age_band_cd gdr mbr_zip_5_cd prod mkt_sgmnt_cd funding aha_hnpi hosp_zip) fast

		
	*find visits with multiple records
	duplicates tag z_patid z_visitid, gen(dup)
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
	
	contract z_patid z_visitid
	merge 1:m z_patid z_visitid using "${TEMP}\raw claims.dta"
	keep if _m == 3
	drop _m _f 
		
	collapse (sum) charge calc_allwd tot_mem_cs coins copay deduct (max) ntwrk_ind primary_cov_ind last_dischdt=fst_admtdt (min) fst_admtdt, ///
		by(z_patid z_group_id dstatus z_visitid age_band_cd gdr mbr_zip_5_cd prod mkt_sgmnt_cd funding aha_hnpi hosp_zip) fast

	duplicates tag z_patid z_visitid, gen(dup)
	tab dup
	drop dup
	
	replace dstatus = 0 if dstatus == .
	gsort z_patid z_visitid -dstatus
	replace dstatus = dstatus[_n-1] if dstatus == 0 & z_patid == z_patid[_n-1] ///
		& z_visitid == z_visitid[_n-1]
	
	gsort z_patid z_visitid
	replace aha_hnpi = aha_hnpi[_n-1] if aha_hnpi == "" & z_patid == z_patid[_n-1] ///
		& z_visitid == z_visitid[_n-1]
	
	collapse (sum) charge calc_allwd tot_mem_cs coins copay deduct (max) ntwrk_ind primary_cov_ind last_dischdt=fst_admtdt (min) fst_admtdt, ///
		by(z_patid z_group_id dstatus z_visitid age_band_cd gdr mbr_zip_5_cd prod mkt_sgmnt_cd funding aha_hnpi hosp_zip) fast
		
	duplicates tag z_patid z_visitid, gen(dup)
	tab dup

	preserve
		keep if dup == 0 
		drop dup
			
		save "${TEMP}\claims_clean_pt2.dta", replace
	restore

	use  "${TEMP}\claims_clean_pt1.dta", clear
	append using  "${TEMP}\claims_clean_pt2.dta"
	
	duplicates tag z_patid z_visitid, gen(dup)
	drop if dup>0
	drop dup

	drop if mbr_zip_5_cd == 99999 | mbr_zip_5_cd == .	
		
	drop if calc_allwd <= 0
	drop if charge <= 0

	compress
	save "${FINAL}\CleanOPClaims`yr'.dta", replace

	erase "${TEMP}\raw claims.dta"
	erase "${TEMP}\claims_clean_pt1.dta"
	erase "${TEMP}\claims_clean_pt2.dta"
}


foreach yr in 10 11 12 13 14 15 {
	use "${TEMP}\OPClaims_AHA_20`yr'.dta", clear
	gen str5 hosp_zip = string(prov_zip_5_cd, "%05.0f")	
	keep z_patid z_visitid dstatus gdr mbr_zip_5 prod mkt_sgmnt_cd funding aha_hnpi hosp_zip diag1 diag2 diag3 proc_cd
	merge m:1 z_patid z_visitid dstatus gdr mbr_zip_5 prod mkt_sgmnt_cd funding aha_hnpi hosp_zip ///
		using "${FINAL}\CleanOPClaims`yr'.dta"
	keep if _merge==3
	drop _merge
	keep z_patid z_visitid diag1 diag2 diag3 proc_cd
	save "${TEMP}\raw claims.dta", replace
	foreach x in 1 2 3 {
		use "${TEMP}\raw claims.dta", clear
		keep z_patid z_visitid diag`x'
		drop if missing(diag`x')==1
		bys z_patid z_visitid diag`x': gen obs=_n
		keep if obs==1
		drop obs
		bys z_patid z_visitid: gen claim_count=_n
		rename diag`x' diag`x'_		
		reshape wide diag`x'_, i(z_patid z_visitid) j(claim_count)
		save "${TEMP}\reshape_claims_`x'.dta", replace
	}
	use "${TEMP}\raw claims.dta", clear
	keep z_patid z_visitid proc_cd
	drop if missing(proc_cd)==1
	bys z_patid z_visitid proc_cd: gen obs=_n
	keep if obs==1
	drop obs
	bys z_patid z_visitid: gen claim_count=_n
	rename proc_cd proc_cd_
	reshape wide proc_cd_, i(z_patid z_visitid) j(claim_count)
	save "${TEMP}\reshape_claims_proc_cd.dta", replace
	
	use "${TEMP}\reshape_claims_1.dta", clear
	merge 1:1 z_patid z_visitid using "${TEMP}\reshape_claims_2.dta", nogenerate
	merge 1:1 z_patid z_visitid using "${TEMP}\reshape_claims_3.dta", nogenerate
	merge 1:1 z_patid z_visitid using "${TEMP}\reshape_claims_proc_cd.dta", nogenerate	
	save "${TEMP}\reshape_claims_all.dta", replace
	
	use "${FINAL}\CleanOPClaims`yr'.dta", clear
	merge 1:1 z_patid z_visitid using "${TEMP}\reshape_claims_all.dta", nogenerate
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
	save "${FINAL}\CleanOPClaims`yr'.dta", replace
	
	erase "${TEMP}\raw claims.dta"	
	erase "${TEMP}\reshape_claims_1.dta"
	erase "${TEMP}\reshape_claims_2.dta"
	erase "${TEMP}\reshape_claims_3.dta"	
	erase "${TEMP}\reshape_claims_all.dta"	
}
