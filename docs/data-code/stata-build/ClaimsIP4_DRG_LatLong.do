/*
Description: This file merges DRG weights into the collapsed claims data, as well as the 
	longitude and latitude for the patient and the hospital.
*/

global OTHER_TEMP = "K:\Research\Raval_Emory\Cost shifting\Data\Temp"
global OTHER_INPUT = "K:\Research\Raval_Emory\Cost shifting\Data\Input"

clear
save "${TEMP}\CMS DRG wts.dta", replace emptyok
foreach yr in 10 11 12 13 14 15 16 {
	
	*CMS weights - applied by fiscal year
	insheet using "${OTHER_INPUT}\FY 20`yr' DRG WTS.csv", comma clear
	keep msdrg type weights	
	gen Year = 20`yr'-1
	ren (msdrg type weights) (drg drg_type drg_wt)
	append using "${TEMP}\CMS DRG wts.dta"
	save "${TEMP}\CMS DRG wts.dta", replace
}

*Merge additional variables for constructing prices from claims
foreach yr in 10 11 12 13 14 15 {
	
	di "Load claims: 20`yr'"
	use "${FINAL}\CleanClaims`yr'.dta", clear
	gen Year = year(fst_admtdt)
	
	*merge CMS wts
	di "merge DRG wts: 20`yr'"
	merge m:1 drg Year using "${TEMP}\CMS DRG wts.dta"
	keep if _m == 3
	drop _m
	
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
	save "${FINAL}\Inpatient_Analysis`yr'.dta", replace
}


