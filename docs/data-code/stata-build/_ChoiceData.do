*****************************************************************************
** Description: 				Calculate predicted probabilities
** Author: 						Ian McCarthy
** Date Edited:					7/24/19
*****************************************************************************
clear
capture log close
set more off
pause on

*****************************************************************************
*SET PATHS*
global SAS_DATA = "K:\Home\mccarthy-ian-hcci\Documents\Research"
global MAIN = "K:\Research\Raval_Emory\Childrens Hospitals"
global CODE = "$MAIN\data-code"
global LOGS = "$MAIN\logs"
global DATA = "$MAIN\data"
global TEMP = "$MAIN\data\Temp"
global FINAL = "$MAIN\data\Final"

local date = "$S_DATE"
log using "$LOGS\IPChoice_`date'.log", replace

*****************************************************************************
** Run individual code files
do "${CODE}\C2_ChoiceSets.do"
do "${CODE}\C3_ChoiceModel.do"


*****************************************************************************
** Calculate predicted shares
forvalues i=2010/2015 {
	use "${FINAL}\PredictedProbability_`i'.dta", clea
}

