
foreach yr in 10 11 12 13 14 15 {

	*Load and process IP admissions
	insheet using "${SAS_DATA}\ipadmits`yr'.csv", comma clear
	
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
		
	*Flag newborns (mdc 15)
	gen flag = mdc == "15"
	bysort z_admit_id: egen newborn_flag = max(flag)
	drop flag 
	
	gen fst = date(fst_admtdt, "DMY")
	gen lst = date(last_dischdt, "DMY")
	drop fst_admtdt last_dischdt
	ren (fst lst) (fst_admtdt last_dischdt)
	format fst_admtdt last_dischdt %td
	
	compress
	save "${TEMP}\ipadmits`yr'.dta", replace
}



