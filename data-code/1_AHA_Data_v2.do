***********************************************************************************
** Title:	      AHA Data Input and Formating
** Author:        Ian McCarthy
** Date created:  2/5/2018
** Date edited:   10/7/2019
***********************************************************************************

********************************
** 2008 Data
insheet using "${DATA_AHA}AHA FY 2008\Comma\pubas08.csv", clear case

drop DBEGM DBEGD DBEGY DENDM DENDD DENDY FISM FISD FISY MADMIN TELNO NETPHONE MLOS
foreach x of varlist DTBEG DTEND FISYR {
  gen `x'_clean=date(`x',"MDY")
  drop `x'
  rename `x'_clean `x'
  format `x' %td
}

gen AHAYEAR=2008
tostring MLOCZIP, replace
replace SYSTELN=subinstr(SYSTELN,"-","",1)
destring SYSTELN, replace
gen fips=string(FSTCD)+"_"+string(FCNTYCD)
sort fips
rename NPI_NUM NPINUM
save temp_data_2008, replace


********************************
** 2009 Data
insheet using "${DATA_AHA}AHA FY 2009\Comma\pubas09.csv", clear case

drop DBEGM DBEGD DBEGY DENDM DENDD DENDY FISM FISD FISY MADMIN TELNO NETPHONE MLOS
foreach x of varlist DTBEG DTEND FISYR {
  gen `x'_clean=date(`x',"MDY")
  drop `x'
  rename `x'_clean `x'
  format `x' %td
}

gen AHAYEAR=2009
gen fips=string(FSTCD)+"_"+string(FCNTYCD)
sort fips
save temp_data_2009, replace

********************************
** 2010 Data
insheet using "${DATA_AHA}AHA FY 2010\COMMA\ASPUB10.csv", clear case
replace EHLTH="0" if EHLTH=="N"
replace EHLTH="" if EHLTH=="." | EHLTH=="2"
destring EHLTH, replace

drop TELNO NETPHONE MLOS
foreach x of varlist DTBEG DTEND FISYR {
  gen `x'_clean=date(`x',"MDY")
  drop `x'
  rename `x'_clean `x'
  format `x' %td
}

gen AHAYEAR=2010
gen fips=string(FSTCD)+"_"+string(FCNTYCD)
sort fips
save temp_data_2010, replace


********************************
** 2011 Data
insheet using "${DATA_AHA}AHA FY 2011\COMMA\ASPUB11.csv", clear case

drop TELNO NETPHONE
foreach x of varlist DTBEG DTEND FISYR {
  gen `x'_clean=date(`x',"MDY")
  drop `x'
  rename `x'_clean `x'
  format `x' %td
}

gen AHAYEAR=2011
gen fips=string(FSTCD)+"_"+string(FCNTYCD)
sort fips
save temp_data_2011, replace


********************************
** 2012 Data
insheet using "${DATA_AHA}AHA FY 2012\COMMA\ASPUB12.csv", clear case

drop TELNO NETPHONE
foreach x of varlist DTBEG DTEND FISYR {
  gen `x'_clean=date(`x',"MDY")
  drop `x'
  rename `x'_clean `x'
  format `x' %td
}


gen AHAYEAR=2012
gen fips=string(FSTCD)+"_"+string(FCNTYCD)
sort fips
save temp_data_2012, replace


********************************
** 2013 Data
insheet using "${DATA_AHA}AHA FY 2013\COMMA\ASPUB13.csv", clear case

drop TELNO NETPHONE
foreach x of varlist DTBEG DTEND FISYR {
  todate `x', gen(`x'_clean) p(mmddyy) cend(2100)
  drop `x'
  rename `x'_clean `x'
}

gen byte notnumeric=(real(MCRNUM)==. & MCRNUM!="")
replace MCRNUM="" if notnumeric==1
destring MCRNUM, replace
gen AHAYEAR=2013
gen fips=string(FSTCD)+"_"+string(FCNTYCD)
sort fips
save temp_data_2013, replace

********************************
** 2014 Data
insheet using "${DATA_AHA}AHA FY 2014\COMMA\ASPUB14.csv", clear case

drop TELNO NETPHONE
foreach x of varlist DTBEG DTEND FISYR {
  todate `x', gen(`x'_clean) p(mmddyy) cend(2100)
  drop `x'
  rename `x'_clean `x'
}

gen byte notnumeric=(real(MCRNUM)==. & MCRNUM!="")
replace MCRNUM="" if notnumeric==1
destring MCRNUM, replace
gen AHAYEAR=2014
gen fips=string(FSTCD)+"_"+string(FCNTYCD)
sort fips
save temp_data_2014, replace

********************************
** 2015 Data
insheet using "${DATA_AHA}AHA FY 2015\COMMA\ASPUB15.csv", clear case

drop TELNO NETPHONE
foreach x of varlist DTBEG DTEND FISYR {
  todate `x', gen(`x'_clean) p(mmddyy) cend(2100)
  drop `x'
  rename `x'_clean `x'
}

gen byte notnumeric=(real(MCRNUM)==. & MCRNUM!="")
replace MCRNUM="" if notnumeric==1
destring MCRNUM, replace
gen AHAYEAR=2015
gen fips=string(FSTCD)+"_"+string(FCNTYCD)
sort fips
save temp_data_2015, replace


********************************
** Append All Years for AHA data
use temp_data_2008, clear
forvalues t=2009/2015 {
  append using temp_data_`t'
}

** Fix year data
gen Year=AHAYEAR

** Hospital Characteristics
gen Own_Type=inrange(CNTRL,12,16) + 2*inrange(CNTRL,21,23) + 3*inrange(CNTRL,30,33) + 4*inrange(CNTRL,41,48)
gen Government=(Own_Type==1)
gen Nonprofit=(Own_Type==2)
gen Profit=(Own_Type==3)
gen Teaching_Hospital1=(MAPP8==1) if MAPP8!=.
gen Teaching_Hospital2=(MAPP3==1 | MAPP5==1 | MAPP8==1 | MAPP12==1 | MAPP13==1)
gen System=(SYSID!=. | MHSMEMB==1)
replace BDTOT=BDTOT/100
gen Labor_Phys=FTEMD
gen Labor_Residents=FTERES
gen Labor_Nurse=FTERN+FTELPN
gen Labor_Other=FTEH-Labor_Phys-Labor_Residents-Labor_Nurse
replace Labor_Other=. if Labor_Other<=0
gen Capital_Imaging=MAMMSHOS+ACLABHOS+ENDOCHOS+ENDOUHOS+REDSHOS+CTSCNHOS+DRADFHOS+EBCTHOS+FFDMHOS+MRIHOS+IMRIHOS ///
   + MSCTHOS+MSCTGHOS+PETHOS+PETCTHOS+SPECTHOS+ULTSNHOS
gen Capital_CareSetting=AMBSHOS+EMDEPHOS
gen Capital_Services=ICLABHOS+ADTCHOS+ADTEHOS+CHTHHOS+CAOSHOS+ONCOLHOS+RASTHOS+IMRTHOS+PTONHOS

gen CARIC=(CICBD>0 & CICBD!=.) 
gen NEONIC=(NICBD>0 & NICBD!=.)
gen NEONIMC=(NINTBD>0 & NINTBD!=.)
gen PEDIC=(PEDICBD>0 & PEDICBD!=.)
egen Profitable=rowtotal(BROOMHOS ACLABHOS CARIC CTSCNHOS DRADFHOS ESWLHOS FITCHOS MRIHOS NEONIC NEONIMC ADTCHOS PEDIC PETHOS IGRTHOS IMRTHOS SPECTHOS SPORTHOS ULTSNHOS WOMHCHOS), missing

gen ALCD=(ALCHBD>0 & ALCHBD!=.)
gen BURN=(BRNBD>0 & BRNBD!=.)
gen PSYIP=(PSYBD>0 & PSYBD!=.)
egen Unprofitable=rowtotal(ALCD ALCOPHOS BURN TRAUMHOS PSYCAHOS EMDEPHOS AIDSSHOS PSYLSHOS PSYEDHOS PSYEMHOS PSYIP PSYOPHOS PSYPHHOS ADULTHOS HOSPCHOS PATEDHOS SOCWKHOS VOLSVHOS), missing

gen Percent_Profitable=Profitable/(Profitable+Unprofitable)


** Vertical Integration (Physician Relationship) on Extensive Margin
foreach x of newlist HOS SYS NET {
  rename GPWW`x' GPW`x'
  rename OPHO`x' OPH`x'
  rename CPHO`x' CPH`x'
  rename FOUND`x' FND`x'
  rename EQMOD`x' EQM`x'
}

label define Int_Lab 0 "NONE" 1 "IPA" 2 "GPW" 3 "OPH" 4 "CPH" 5 "MSO" 6 "ISM" 7 "EQUITY"
foreach x of newlist HOS SYS NET {
  gen Int_`x'=0 
  replace Int_`x'=1 if IPA`x'==1
  replace Int_`x'=2 if GPW`x'==1  
  replace Int_`x'=3 if OPH`x'==1  
  replace Int_`x'=4 if CPH`x'==1  
  replace Int_`x'=5 if MSO`x'==1
  replace Int_`x'=6 if ISM`x'==1
  replace Int_`x'=7 if EQM`x'==1
  label values Int_`x' Int_Lab
}
replace Int_HOS=7 if PHYGP==1

label define Int_Lab_2 0 "NONE" 1 "Support" 2 "Referrals" 3 "Employee" 4 "Equity"
gen Int_HOS_2=0 
replace Int_HOS_2=1 if IPAHOS==1 | GPWHOS==1 | MSOHOS==1 
replace Int_HOS_2=2 if OPHHOS==1 | CPHHOS==1 | FNDHOS==1
replace Int_HOS_2=3 if ISMHOS==1
replace Int_HOS_2=4 if EQMHOS==1 | PHYGP==1
label values Int_HOS_2 Int_Lab_2

label define Int_Lab_3 0 "NONE" 1 "Support/Referral" 2 "Employee" 3 "Equity"
gen Int_HOS_3=0 
replace Int_HOS_3=1 if Int_HOS_2==1 | Int_HOS_2==2
replace Int_HOS_3=2 if Int_HOS_2==3
replace Int_HOS_3=3 if Int_HOS_2==4
label values Int_HOS_3 Int_Lab_3

keep ID MCRNUM NPINUM Year fips HRRCODE DTBEG DTEND FISYR BDTOT LAT LONG Teaching_Hospital1 Teaching_Hospital2 System Labor_* Capital_* Profit Nonprofit Profitable Unprofitable Percent_Profitable Int_HOS Int_HOS_2 Int_HOS_3
rename MCRNUM provider_number
bys provider_number Year: gen obs=_N
drop if obs>1
drop obs  


********************************
** Merge with CH Tier Info
keep if Year>=2010 & Year<=2015
gen MergeYear=Year
replace MergeYear=2014 if MergeYear==2015
merge m:1 ID MergeYear using "${DATA_FINAL}CH_Tiers.dta", keep(master match) generate(CHTiers_Merge)

** count of tiers by hrrcode
collapse (sum) TierA TierB TierC, by(HRRCODE Year)
sort HRRCODE Year
save temp_hrr_data, replace

use temp_hrr_data, clear
replace Year=Year+1
sort HRRCODE Year
rename TierA TierA_Lag
rename TierB TierB_Lag
rename TierC TierC_Lag
save temp_hrr_data_m1, replace

use temp_hrr_data, clear
gen any_tiera=(TierA>0)
gen any_tierb=(TierB>0)
gen any_tierc=(TierC>0)
collapse (sum) any_tiera any_tierb any_tierc (mean) TierA TierB TierC, by(Year)


use temp_hrr_data, clear
merge 1:1 HRRCODE Year using temp_hrr_data_m1
