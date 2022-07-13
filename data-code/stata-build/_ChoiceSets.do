
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


*****************************************************************************
** Form Patient Choice Sets: this is the "full" choice set with all hospitals and no
**	restrictions on which hospitals are an option (provided the hospital saw a pediatric patient)

forvalues i=2010/2015 {
	use "${FINAL}\FinalData_All.dta", clear
	keep if Year==`i'
	save temp_final_year, replace


	** unique patients
	use temp_final_year, clear
	keep z_patid pt_zip pt_lat pt_long 
	destring pt_zip, replace
	bys z_patid pt_zip pt_lat pt_long: gen place=_n
	keep if place==1
	drop place
	save "${TEMP}\Patients_`i'.dta", replace

	** unique hospitals
	use temp_final_year, clear
	keep aha_hnpi hosp_zip hosp_lat hosp_long
	destring hosp_zip, replace
	bys aha_hnpi hosp_zip hosp_lat hosp_long: gen place=_n
	keep if place==1
	drop place
	save "${TEMP}\Hospitals_`i'.dta", replace

	** unique hospital zip codes
	use temp_final_year, clear
	keep hosp_zip hosp_lat hosp_long
	destring hosp_zip, replace
	bys hosp_zip: gen place=_n
	keep if place==1
	drop place
	gen m_var=1
	save "${TEMP}\HospitalZip_`i'.dta", replace

	** unique patient zip codes
	use temp_final_year, clear
	keep pt_zip pt_lat pt_long 
	destring pt_zip, replace
	bys pt_zip: gen place=_n
	keep if place==1
	drop place
	gen m_var=1
	save "${TEMP}\PatientZip_`i'.dta", replace

	** full join of patient zip codes to hospital zip codes, then limit by distance
	use "${TEMP}\PatientZip_`i'.dta", clear
	joinby m_var using "${TEMP}\HospitalZip_`i'.dta"
	finddist pt_lat pt_long hosp_lat hosp_long 
	gen distance_mi = distance / 1.609344
	drop distance m_var
	save "${TEMP}\ZipCombine_`i'.dta", replace

	** form choice set for each patient id
	use temp_final_year, clear
	destring pt_zip, replace
	rename hosp_long long_selection
	rename hosp_lat lat_selection
	keep z_patid pt_zip pt_lat pt_long fst_admtdt last_dischdt long_selection lat_selection
	merge m:m pt_zip using "${TEMP}\ZipCombine_`i'.dta", nogenerate
	merge m:m hosp_zip using "${TEMP}\Hospitals_`i'.dta", nogenerate
	gen choice=(long_selection==hosp_long & lat_selection==hosp_lat)
	keep z_patid fst_admtdt last_dischdt aha_hnpi distance choice pt_zip hosp_zip
	save "${FINAL}\FinalChoiceSets_Full`i'.dta", replace
	export delimited using "${FINAL}\FinalChoiceSets_Full`i'.txt", quote delimiter(tab) replace	
}


*****************************************************************************
** Form Patient Choice Sets: this is the smaller choice set based on the sample of selected
**  common procedures

forvalues i=2010/2015 {
	use "${FINAL}\FinalData.dta", clear
	keep if Year==`i'
	save temp_final_year, replace


	** unique patients
	use temp_final_year, clear
	keep z_patid pt_zip pt_lat pt_long 
	destring pt_zip, replace
	bys z_patid pt_zip pt_lat pt_long: gen place=_n
	keep if place==1
	drop place
	save "${TEMP}\Patients_`i'.dta", replace

	** unique hospitals
	use temp_final_year, clear
	keep aha_hnpi hosp_zip hosp_lat hosp_long
	destring hosp_zip, replace
	bys aha_hnpi hosp_zip hosp_lat hosp_long: gen place=_n
	keep if place==1
	drop place
	save "${TEMP}\Hospitals_`i'.dta", replace

	** unique hospital zip codes
	use temp_final_year, clear
	keep hosp_zip hosp_lat hosp_long
	destring hosp_zip, replace
	bys hosp_zip: gen place=_n
	keep if place==1
	drop place
	gen m_var=1
	save "${TEMP}\HospitalZip_`i'.dta", replace

	** unique patient zip codes
	use temp_final_year, clear
	keep pt_zip pt_lat pt_long
	destring pt_zip, replace
	bys pt_zip: gen place=_n
	keep if place==1
	drop place
	gen m_var=1
	save "${TEMP}\PatientZip_`i'.dta", replace

	** full join of patient zip codes to hospital zip codes, then limit by distance
	use "${TEMP}\PatientZip_`i'.dta", clear
	joinby m_var using "${TEMP}\HospitalZip_`i'.dta"
	finddist pt_lat pt_long hosp_lat hosp_long 
	gen distance_mi = distance / 1.609344
	drop distance m_var
	save "${TEMP}\ZipCombine_`i'.dta", replace

	** form choice set for each patient id
	use temp_final_year, clear
	destring pt_zip, replace
	rename hosp_long long_selection
	rename hosp_lat lat_selection
	keep z_patid pt_zip pt_lat pt_long fst_admtdt last_dischdt long_selection lat_selection Type
	merge m:m pt_zip using "${TEMP}\ZipCombine_`i'.dta", nogenerate
	merge m:m hosp_zip using "${TEMP}\Hospitals_`i'.dta", nogenerate
	gen choice=(long_selection==hosp_long & lat_selection==hosp_lat)
	keep z_patid fst_admtdt last_dischdt aha_hnpi distance choice pt_zip hosp_zip Type
	save "${FINAL}\FinalChoiceSets_`i'.dta", replace
	export delimited using "${FINAL}\FinalChoiceSets_`i'.txt", quote delimiter(tab) replace	
}

