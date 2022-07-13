global OTHER_TEMP = "K:\Research\Raval_Emory\Cost shifting\Data\Temp"
global OTHER_INPUT = "K:\Research\Raval_Emory\Cost shifting\Data\Input"

*Merge additional variables for constructing prices to claims
foreach yr in 10 11 12 13 14 15 {
	di "Load op claims: 20`yr'"
	use "${FINAL}\CleanOPClaims`yr'.dta", clear
	gen Year = year(fst_admtdt)
		
	*merge patient lat and long
	di "merge lat-long: 20`yr'"
	gen str5 pt_zip = string(mbr_zip_5_cd, "%05.0f")
	merge m:1 pt_zip using "${OTHER_TEMP}\geo info.dta"
	keep if _m == 3
	drop _m
		
	*merge hospital lat and long
	gen str5 hosp_zip2 = hosp_zip
	merge m:1 hosp_zip2 using "${OTHER_TEMP}\hosp zip fill.dta", keepusing(lat_fill long_fill)
	gen hosp_lat = lat_fill
	gen hosp_long = long_fill
	drop if _m == 2
	drop _m lat_fill long_fill hosp_zip2
	
	di "save: 20`yr'"
	tab Year, m
	save "${FINAL}\Outpatient_Analysis`yr'.dta", replace
}


