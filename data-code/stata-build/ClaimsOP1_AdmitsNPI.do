
foreach yr in 10 11 12 13 14 15 {

	*Load and process OP claims
	insheet using "${SAS_DATA}\opclaims`yr'.csv", comma clear
	keep if age_band_cd==1
	
	if `yr' > 13 {
		gen hnpi_re = hnpi
		replace hnpi = hnpi_be if hnpi_be != ""
		drop hnpi_be
	}
	
	else {
		drop hnpi_be
		gen hnpi_re = ""
	}
			
	*drop observations with no NPI
	gen flag = hnpi != ""
	bysort z_visitid: egen max_flag = max(flag)
	drop if max_flag == 0
	drop flag max_flag

	**Keep only admit_ids with the following type of bill ("TOB")
	** -- 13 (hospital outpatient), 83 (ambulatory surgery center), 85 (critical access hospital)
	if `yr' == 13 {
		gen tob2 = substr(string(tob), 1, 2)
	}
	else {
		gen tob2 = substr(tob, 1, 2)
	}
	gen flag = inlist(tob2, "13", "83", "85")
	bysort z_visitid: egen max_flag = max(flag)
	drop if max_flag != 1
	drop tob2 flag max_flag
			
	gen fst = date(fst_dt, "DMY")
	drop fst_dt
	ren (fst) (fst_admtdt)
	format fst_admtdt %td
	
	compress
	save "${TEMP}\opclaims`yr'.dta", replace
}



