***********************************************************************************
** Title:	      Tiers for CH from Kaitlin
** Author:        Ian McCarthy
** Date created:  2/5/2018
** Date edited:   6/25/2018
***********************************************************************************

insheet using "${DATA_FINAL}AHA_tiers_merge.csv", clear case
reshape long TierA_ TierB_ TierC_ TierD_ NoTier_, i(ID) j(Year)
rename TierA_ TierA
rename TierB_ TierB
rename TierC_ TierC
rename TierD_ TierD
rename NoTier_ NoTier
foreach x of varlist TierA TierB TierC TierD NoTier {
	destring `x', replace
}

tostring ID, replace
rename Year MergeYear
save "${DATA_FINAL}CH_Tiers.dta", replace
