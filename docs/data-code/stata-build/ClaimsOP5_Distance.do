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


*Load claims & calculate radius
foreach yr in 10 11 12 13 14 15 {

	use "${FINAL}\Outpatient_Analysis`yr'.dta", clear
	gen obs = 1
	
	collapse (sum) obs, by(pt_zip pt_lat pt_long aha_hnpi hosp_zip hosp_lat hosp_long) fast
		
	finddist pt_lat pt_long hosp_lat hosp_long 
	gen distance_mi = distance / 1.609344
	drop distance obs
		
	save "${TEMP}\OutpatientDistance`yr'.dta", replace 

}





