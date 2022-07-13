
foreach yr in 10 11 12 13 14 15 {
	use "${TEMP}\IPAdmits_AHA_20`yr'.dta", clear
	gen str5 hosp_zip = string(prov_zip_5_cd, "%05.0f")
	
	save "${TEMP}\raw claims.dta", replace
	
	*collapse to unique admissions
	collapse (max) newborn_flag, ///
		by(z_patid dstatus fst_admtdt last_dischdt ///
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
	
	collapse (max) newborn_flag, ///
		by(z_patid dstatus fst_admtdt last_dischdt ///
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
	
	collapse (max) newborn_flag, ///
		by(z_patid dstatus fst_admtdt last_dischdt ///
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
	drop if los < 1
	drop if los > r(p99)
	
	compress
	save "${FINAL}\CleanAdmits`yr'.dta", replace

	erase "${TEMP}\raw claims.dta"
	erase "${TEMP}\claims_clean_pt1.dta"
	erase "${TEMP}\claims_clean_pt2.dta"
}

