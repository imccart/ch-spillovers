/*
Description: This file loads the initial inpatient claims data, drops claims with missing NPIs,
	drops claims that aren't inpatient, swing bed, or hospital related, and reformats
	the admit and discharge dates.
 
The final datasets still include several rows (claims) for any given inpatient stay.
*/

foreach yr in 10 11 12 13 14 15 {
	insheet using "${SAS_DATA}\ipclaims`yr'.csv", comma clear
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
	bysort z_admit_id: egen max_flag = max(flag)
	drop if max_flag == 0
	drop flag max_flag

	**Keep only admit_ids with the following type of bill ("TOB")
	** -- 11 or 12 (inpatient), 18 (swing bed), 41 (non-medical hospital), 85 (critical access hospital)
	gen tob2 = substr(tob, 1, 2)
	gen flag = inlist(tob2, "11", "12", "18", "41", "85")
	bysort z_admit_id: egen max_flag = max(flag)
	drop if max_flag != 1
	drop tob2 flag max_flag
		
	*create ER admit flag
	gen flag = inlist(rvnu_cd, 450, 451, 452, 456, 459, 981)
	bysort z_admit_id: egen er_flag = max(flag)
		
	gen fst = date(fst_admtdt, "DMY")
	gen lst = date(last_dischdt, "DMY")
	drop fst_admtdt last_dischdt
	ren (fst lst) (fst_admtdt last_dischdt)
	format fst_admtdt last_dischdt %td
	
	compress
	save "${TEMP}\ipclaims`yr'.dta", replace
}



