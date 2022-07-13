***********************************************************************************
** Title:	        Read-in AMA Insurance Data
** Author:        Ian McCarthy
** Date created:  6/25/2018
** Date edited:   6/25/2018
***********************************************************************************

forvalues x=2010/2011 {
	import excel using "${DATA_FINAL}Competition in Health Insurance 2010-2014.xlsx", sheet(`x') firstrow cellrange(A1:F439) clear
	rename Share Share1
	rename F Share2
	drop if StateandMSAs==""
	gen Year=`x'
	save temp_ama_`x', replace
}

forvalues x=2012/2014 {
	import excel using "${DATA_FINAL}Competition in Health Insurance 2010-2014.xlsx", sheet(`x') firstrow cellrange(A1:F441) clear
	rename Share Share1
	rename F Share2
	drop if StateandMSAs==""
	gen Year=`x'	
	save temp_ama_`x', replace
}

** Append AMA data
use temp_ama_2010, clear
forvalues x=2011/2014 {
	append using temp_ama_`x'
}
sort StateandMSAs Year

replace StateandMSAs=subinstr(StateandMSAs,"  "," ",.)
replace StateandMSAs=subinstr(StateandMSAs,";",",",.)
replace StateandMSAs=subinstr(StateandMSAs,", ",",",.)
replace StateandMSAs=subinstr(StateandMSAs,",",", ",.)
replace StateandMSAs=subinstr(StateandMSAs," -","-",.)
replace StateandMSAs=subinstr(StateandMSAs,"- ","-",.)
replace StateandMSAs=subinstr(StateandMSAs," Tx"," TX",.)
replace StateandMSAs=subinstr(StateandMSAs," Fl"," FL",.)
replace StateandMSAs=subinstr(StateandMSAs," tx"," TX",.)
replace StateandMSAs=subinstr(StateandMSAs," A.L."," AL",.)
replace StateandMSAs=subinstr(StateandMSAs," Ak"," AK",.)
replace StateandMSAs=subinstr(StateandMSAs," IL."," IL",.)
replace StateandMSAs=subinstr(StateandMSAs,"?","-",.)
replace StateandMSAs=subinstr(StateandMSAs,"Bismarck, ND","Bismark, ND",.)
replace StateandMSAs=strrtrim(StateandMSAs)
replace StateandMSAs="Albany-Schenectady-Troy, NY" if StateandMSAs=="Albany-Schenectady-Tonawanda, NY"
replace StateandMSAs="Albuquerque, NM" if StateandMSAs=="Albequerque, NM"
replace StateandMSAs="Amarillo, TX" if StateandMSAs=="AmariUo, TX"
replace StateandMSAs="Ann Arbor, MI" if StateandMSAs=="Ann Arbor, Ml"
replace StateandMSAs="Augusta-Richmond County, GA-SC" if StateandMSAs=="Augusta-RichmondCounty, GA-SC"
replace StateandMSAs="Battle Creek, MI" if StateandMSAs=="Battle Creek, Ml"
replace StateandMSAs="Bay City, MI" if StateandMSAs=="Bay City, Ml"
replace StateandMSAs="Beaumont-Port Arthur, TX" if StateandMSAs=="Beumont-Port Arthur, TX"
replace StateandMSAs="Binghamton, NY" if StateandMSAs=="Binghampton, NY"
replace StateandMSAs="Blacksburg-Christiansburg-Radford, VA" if StateandMSAs=="Blacksgburg-Christiansburg-Radford, VA"
replace StateandMSAs="Brownsville-Harlingen, TX" if StateandMSAs=="Brownsville-Harlington, TX"
replace StateandMSAs="Bloomington-Normal, IL" if StateandMSAs=="Bloornington-Normal, IL"
replace StateandMSAs="Buffalo-Cheektowaga-Tonawanda, NY" if StateandMSAs=="Buffal-Cheektowaga-Tonawanda, NY"
replace StateandMSAs="Chattanooga, TN-GA" if StateandMSAs=="Chattanooga, Tn-GA"
replace StateandMSAs="College Station-Bryan, TX" if StateandMSAs=="College St-ti6ri-Bryan, TX"
replace StateandMSAs="Dallas-Plano-Irving, TX" if StateandMSAs=="DaUas-Plano-Irving, TX"
replace StateandMSAs="Davenport-Moline-Rock Island, IA-IL" if StateandMSAs=="Davenport-Moline-RockIsland, IA-IL"
replace StateandMSAs="Deltona-Daytona Beach-Ormond Beach, FL" if StateandMSAs=="Deltona-Daytona Beach-Ormand Beach, FL"
replace StateandMSAs="Detroit-Livonia-Dearborn, MI" if StateandMSAs=="Detroit-Livonia-Dearborn, Ml"
replace StateandMSAs="Des Moines, IA" if StateandMSAs=="Des Moine, IA"
replace StateandMSAs="Duluth, MN-WI" if StateandMSAs=="Duluth, MN-Wi"
replace StateandMSAs="Farmington, NM" if StateandMSAs=="Farmingt on, NM"
replace StateandMSAs="Framingham, MA" if StateandMSAs=="Farmingham, MA"
replace StateandMSAs="Fort Collins-Loveland, CO" if StateandMSAs=="Fort ColIins-Loveland, CO"
replace StateandMSAs="Fort Lauderdale-Pompano Beach-Deerfield Beach, FL" if StateandMSAs=="Fort Lauderdale-Popano Beach-Deerfield Beach, FL"
replace StateandMSAs="Fort Worth-Arlington, TX" if StateandMSAs=="Fort Worth-Arlirigton, TX"
replace StateandMSAs="Gainesville, FL" if StateandMSAs=="Gainesville, l=L"
replace StateandMSAs="Grand Rapids-Wyoming, MI" if StateandMSAs=="Grand Rapids-Wyoming, Ml"
replace StateandMSAs="Hagerstown-Martinsburg, MD-WV" if StateandMSAs=="Hagerstown-Martinsburg, MD-WV ."
replace StateandMSAs="Haverhill-North Andover-Amesbury, MA-NH" if StateandMSAs=="Haverhill-Newburyport-Amesbury Town, MA-NH"
replace StateandMSAs="Haverhill-North Andover-Amesbury, MA-NH" if StateandMSAs=="Haverhill-Newburyport-AmesburyTown, MA-NH"
replace StateandMSAs="Holland-Grand Haven, MI" if StateandMSAs=="Holland-Grand Haven, Ml"
replace StateandMSAs="Houston-Sugar Land-Baytown, TX" if StateandMSAs=="Hciustcin-Sugar Land-Baytown, TX"
replace StateandMSAs="Jackson, MI" if StateandMSAs=="Jackson, Ml"
replace StateandMSAs="Janesville, WI" if StateandMSAs=="Janesville"
replace StateandMSAs="Johnson City, TN" if StateandMSAs=="Johnson, TN"
replace StateandMSAs="Jonesboro, AR" if StateandMSAs=="Jon sbor6, AR··..."
replace StateandMSAs="Kalamazoo-Portage, MI" if StateandMSAs=="Kalamazoo-Portage, Ml"
replace StateandMSAs="Kingsport-Bristol, TN-VA" if StateandMSAs=="Kingsport-Bristol, TN"
replace StateandMSAs="Knoxville, TN" if StateandMSAs=="Knoxville, Tn"
replace StateandMSAs="Lansing-East Lansing, MI" if StateandMSAs=="Lansing-East Lansing, Ml"
replace StateandMSAs="Lincoln, NE" if StateandMSAs=="Lincoln, N"
replace StateandMSAs="Little Rock-North Little Rock, AR" if StateandMSAs=="Little Rock'-North LiUle Rock, AR"
replace StateandMSAs="Little Rock-North Little Rock, AR" if StateandMSAs=="Little Rock-North Little Rock-AR"
replace StateandMSAs="Louisville, KY-IN" if StateandMSAs=="Lousville, KY-IN"
replace StateandMSAs="Lowell-Billerica-Chelmsford, MA-NH" if StateandMSAs=="Lowell-Billerica, Chelmsford, MA-NH"
replace StateandMSAs="McAllen-Edinburg-Mission, TX" if StateandMSAs=="McAlleri-Edinburg-Mission, TX"
replace StateandMSAs="Medford, OR" if StateandMSAs=="Medford, Or"
replace StateandMSAs="Mobile, AL" if StateandMSAs=="Mobile, Al"
replace StateandMSAs="Muskegon-Norton Shores, MI" if StateandMSAs=="Muskegon-Norton Shores, Ml"
replace StateandMSAs="Nashua, NH-MA" if StateandMSAs=="Nashua, NH"
replace StateandMSAs="Nashville-Davidson-Murfreesboro, TN" if StateandMSAs=="Nashville-Davidson--Murfreesboro, TN"
replace StateandMSAs="New Orleans-Metairie-Kenner, LA" if StateandMSAs=="New Orleans· Metairie· Kenner, LA"
replace StateandMSAs="Niles-Benton Harbor, MI" if StateandMSAs=="Niles-Benton Harbor, Ml"
replace StateandMSAs="Norwich-New London, CT-RI" if StateandMSAs=="Norwich-New London-Westerley, CT-RI"
replace StateandMSAs="Norwich-New London, CT-RI" if StateandMSAs=="Norwich-New London-Westerly, CT-RI"
replace StateandMSAs="Norwich-New London, CT-RI" if StateandMSAs=="Norwich-New London, CT-Rl"
replace StateandMSAs="Olympia, WA" if StateandMSAs=="Olympia, W A"
replace StateandMSAs="Parkersburg-Marietta-Vienna, WV-OH" if StateandMSAs=="Parker sburg-Marietta-Vienna, WV-OH"
replace StateandMSAs="Phoenix-Mesa-Scottsdale, AZ" if StateandMSAs=="Pheonix-Mesa-Scottsdale, AZ"
replace StateandMSAs="Port St. Lucie-Fort Pierce, FL" if StateandMSAs=="Port St.Lucie-Fort Pierce, FL"
replace StateandMSAs="Portland-Vancouver-Beaverton, OR-WA" if StateandMSAs=="Protland-Vancouver-Beaverton, OR-WA"
replace StateandMSAs="Providence-Fall River-Warwick, RI-MA" if StateandMSAs=="Providence-Fall River-Warwick, Rt-MA"
replace StateandMSAs="Rapid City, SD" if StateandMSAs=="Rapidy City, SD"
replace StateandMSAs="Reno-Sparks, NV" if StateandMSAs=="Reno Sparks, NV"
replace StateandMSAs="Roanoke, VA" if StateandMSAs=="Roanok VA"
replace StateandMSAs="Rochester, MN" if StateandMSAs=="Rochestor, MN"
replace StateandMSAs="Sacramento-Arden-Arcade-Roseville, CA" if StateandMSAs=="Sacramento-Arden-Aracade-Roseville, CA"
replace StateandMSAs="Saginaw-Saginaw Township North, MI" if StateandMSAs=="Saginaw-Saginaw Township North, Ml"
replace StateandMSAs="Salt Lake City, UT" if StateandMSAs=="Salt Lake City-UT"
replace StateandMSAs="San Angelo, TX" if StateandMSAs=="San Angeio, TX"
replace StateandMSAs="San Diego-Carlsbad-San Marcos, CA" if StateandMSAs=="San Diego-Carlsbad-SanMarcos, CA"
replace StateandMSAs="Santa Fe, NM" if StateandMSAs=="Sante Fe, NM"
replace StateandMSAs="Sarasota-Bradenton-Venice, FL" if StateandMSAs=="Sarasota-Brandenton-Venice, FL"
replace StateandMSAs="Scranton-Wilkes-Barre, PA" if StateandMSAs=="Scranton--Wilkes-Barre, PA"
replace StateandMSAs="Seattle-Bellevue-Everett, WA" if StateandMSAs=="Seattle Bellevue-Everett, WA"
replace StateandMSAs="Sherman-Denison, TX" if StateandMSAs=="Shermancoenison, TX"
replace StateandMSAs="Shreveport-Bossier City, LA" if StateandMSAs=="Shreveport-Boissier City, LA"
replace StateandMSAs="South Bend-Mishawaka, IN-MI" if StateandMSAs=="South Bend-Mishawaka, IN-M l"
replace StateandMSAs="South Bend-Mishawaka, IN-MI" if StateandMSAs=="South Bend-Mishiwaka, IN-MI"
replace StateandMSAs="South Bend-Mishawaka, IN-MI" if StateandMSAs=="South Bend-Mishawaka, IN-Ml"
replace StateandMSAs="St. Louis, MO-IL" if StateandMSAs=="St Louis, MO-IL"
replace StateandMSAs="St. George, UT" if StateandMSAs=="St. Georgia, UT"
replace StateandMSAs="St. Joseph, MO-KS" if StateandMSAs=="St.Joseph, MO-KS"
replace StateandMSAs="St. Cloud, MN" if StateandMSAs=="St.Cloud, MN"
replace StateandMSAs="Suffolk County-Nassau County, NY" if StateandMSAs=="Suffolf County-Nassau County, NY"
replace StateandMSAs="Tulsa, OK" if StateandMSAs=="Tulsa OK"
replace StateandMSAs="Tucson, AZ" if StateandMSAs=="Tuscon, AZ"
replace StateandMSAs="Tyler, TX" if StateandMSAs=="Tyler-TX"
replace StateandMSAs="Vero Beach, FL" if StateandMSAs=="Very Beach, FL"
replace StateandMSAs="Warren-Farmington Hills-Troy, MI" if StateandMSAs=="Warren-Farmington Hills-Troy, Ml"
replace StateandMSAs="Weirton-Steubenville, WV-OH" if StateandMSAs=="Weirton-Steibenville, WV-OH"
replace StateandMSAs="Yakima, WA" if StateandMSAs=="Yakima.WA" | StateandMSAs=="Yakima, W"

bys StateandMSAs Year: gen obs=_n
keep if obs==1

rename StateandMSAs msaname
statastates, name(msaname)
rename state_abbrev state
replace msaname=msaname+" MSA" if state==""
replace msaname=state+" NONMETROPOLITAN AREA" if state!=""
rename Year MergeYear

replace msaname="BISMARCK, ND MSA" if msaname=="BISMARK, ND MSA"

save "${DATA_FINAL}AMA_Data.dta", replace

