*****************************************************************************
** Identify "movers"

*Distance calculation
capture program drop finddist 
		program define finddist 
			args lat1 long1 lat2 long2 
			local radius_earth=6.378136e3
			tempvar val 
			gen double `val'=sin(_pi*abs(`lat1')/180)*sin(_pi*abs(`lat2')/180)+cos(_pi*abs(`lat1')/180)*cos(_pi*abs(`lat2')/180)*cos(_pi*abs(`long1')/180-_pi*abs(`long2')/180) 
			qui replace `val'=1 if (`val'>1)&(`val'!=.)
			gen distance=`radius_earth'*acos(`val')
		end 

** collect zip codes and patient locations
forvalues i=2010/2015 {
	use "${FINAL}\FinalInpatientData_`i'.dta", clear
	keep z_patid pt_zip pt_lat pt_long
	destring pt_zip, replace
	bys z_patid pt_zip pt_lat pt_long: gen place=_n
	keep if place==1
	drop place
	bys z_patid: gen place=_n
	rename pt_zip zip_`i'_
	rename pt_lat lat_`i'_
	rename pt_long long_`i'_
	reshape wide zip_`i'_ lat_`i'_ long_`i'_, i(z_patid) j(place)
	save "${TEMP}\PatientLocation_`i'.dta", replace
}

** form unique list of patients in panel
use "${TEMP}\PatientLocation_2010.dta", clear
forvalues i=2011/2015 {
	append using "${TEMP}\PatientLocation_`i'.dta"
}
bys z_patid: gen unique_pat=_n
keep if unique_pat==1
keep z_patid
save "${TEMP}\Patient_IP.dta", replace

use "${TEMP}\Patient_IP.dta", clear
forvalues i=2010/2015 {
	merge 1:1 z_patid using "${TEMP}\PatientLocation_`i'.dta", keep(master match) nogenerate
}

** Generate "mover" variable
loc counts_2010 "1 2"
loc counts_2011 "1 2 3"
loc counts_2012 "1 2 3 4"
loc counts_2013 "1 2 3"
loc counts_2014 "1 2 3"
loc counts_2015 "1 2"

forvalues i=2015(-1)2011 {
	gen mover_`i'=0
	gen lat_post_`i'=.
	gen long_post_`i'=.
	gen lat_pre_`i'=.
	gen long_pre_`i'=.
	local base=`i'-2010+2009
	forvalues s=`base'(-1)2010 {
		foreach t of loc counts_`i' {
			foreach tpre of loc counts_`base' {
				gen mover=(zip_`i'_`t'!=zip_`base'_`tpre' & zip_`i'_`t'!=. & zip_`base'_`tpre'!=.)
				replace lat_post_`i'=lat_`i'_`t' if mover==1 & mover_`i'==0
				replace long_post_`i'=long_`i'_`t' if mover==1 & mover_`i'==0
				replace lat_pre_`i'=lat_`base'_`tpre' if mover==1 & mover_`i'==0
				replace long_pre_`i'=long_`base'_`tpre' if mover==1 & mover_`i'==0
				replace mover_`i'=1 if mover==1	& mover_`i'==0			
				drop mover
			}
			foreach tpre of loc counts_`i' {
				gen mover=(zip_`i'_`t'!=zip_`i'_`tpre' & zip_`i'_`t'!=. & zip_`i'_`tpre'!=.)
				replace lat_post_`i'=lat_`i'_`t' if mover==1 & mover_`i'==0
				replace long_post_`i'=long_`i'_`t' if mover==1 & mover_`i'==0
				replace lat_pre_`i'=lat_`i'_`tpre' if mover==1 & mover_`i'==0
				replace long_pre_`i'=long_`i'_`tpre' if mover==1 & mover_`i'==0
				replace mover_`i'=1 if mover==1	& mover_`i'==0			
				drop mover
			}
		}
	}
}


** start back calculating distance...
** 1. limit to identified movers
** 2. calculate distance
** 3. save mover dataset
** do this separately for each year to accommodate people moving more than once
** save the "current" address so that we can merge it with claims data to identify claims where patient was in the new place

	finddist pt_lat pt_long hosp_lat hosp_long 
	gen distance_mi = distance / 1.609344
	drop distance


log close


