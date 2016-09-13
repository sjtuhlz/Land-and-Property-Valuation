//rents & values for office buildings 
//Jan 19, 2012
//Imperial County was added (office parcels were selected according to lu_08, not lu05)-- Jan 28, 2012 

clear all
scalar drop _all 
macro drop _all 
log close _all
log using jan_19_office, replace
set more off

cd "D:\summer, 2011\Third Project\office_builing\Data"



//~~~~~~~~~~~~~~~~~~~~~~~RV ratio ~~~~~~~~~~~~~~~~~
use "D:\summer, 2011\Second Project\practice\Six counties\debug\avavaluerent_processed.dta", clear //more about the dta, see nov_17.do
xi: reg lgrv  lgvsq2000 fsub cbd ocean fwy age 
estimates store rvratio


/*
use SBparcelMZ_office.dta, clear
append using VTparcelMZ_office.dta, force
append using "D:\summer, 2011\Second Project\office parcels\merge_la_or_rv_office_9_16.dta", force
save five_parcelMZ_office.dta, replace
*/

use five_parcelMZ_office.dta, clear
append using IMparcelMZ_office.dta, force

drop if mz == 0 | missing(mz)

replace totvalue07 = . if totvalue07 == 0
replace sale_price = . if sale_price == 0
replace saleyr = . if saleyr == 0 | saleyr == 1989
gen floor_area =  impsqft if impsqft != 0
gen lgvsq  = ln( sale_price/ floor_area ) 
gen lgvsq_tot  = ln( totvalue07/ floor_area ) 
gen age = 2000 - yearbuilt if yearbuilt != 0
format scag_xyid  %14.0f

gen saleyr_bk = saleyr
gen age_bk = age
levelsof city, local(citylist)
foreach cvar of local citylist { 
        summarize age if city == "`cvar'" , meanonly 
        replace age = r(mean) if missing(age) & city == "`cvar'"
}



gen insample = .
gen v2000 = .
gen v2000_tot = . 
gen lgvsq2000 = .
gen lgvsq2000_tot = .


encode city , gen(citynum)
xtset citynum

//============================== begin regression (Riverside & Orange)======================
replace insample = ((mz >= 50 & mz <=66) | (mz >= 81 & mz <=95)) 

//regression and imputation using SALE_PRICE
xtreg  lgvsq  fsub cbd fwy ocean i.saleyr i.lu_08 if insample & !missing(saleyr), fe
levelsof saleyr if e(sample), local(lvsaleyr)
foreach lv of local lvsaleyr {
    capture replace lgvsq2000 =  lgvsq - _b[`lv'.saleyr] + _b[2000.saleyr]  if saleyr == `lv'  & e(sample)
}
replace saleyr = 2000 if e(sample) == 0 & insample
predict lgvsq_p if insample
replace saleyr = saleyr_bk
replace lgvsq2000 = lgvsq_p if e(sample) == 0 & insample
drop  lgvsq_p

//regression and imputation using TOTVALUE07
xtreg lgvsq_tot fsub cbd fwy ocean i.saleyr i.lu_08 if insample & !missing(saleyr), fe
levelsof saleyr if e(sample), local(lvsaleyr)
foreach lv of local lvsaleyr {
        capture replace  lgvsq2000_tot =  lgvsq_tot - _b[`lv'.saleyr] + _b[2000.saleyr] if saleyr == `lv'  & e(sample)
    }
replace saleyr = 2000 if e(sample)==0  & insample
predict lgvsq_p if insample
replace lgvsq2000_tot = lgvsq_p if e(sample) == 0 & insample
drop  lgvsq_p


replace v2000 = exp(lgvsq2000) * floor_area if insample
replace v2000_tot = exp(lgvsq2000_tot) * floor_area if insample

//compare
//----------scalar(xo)
replace insample = (mz >= 50 & mz <=66)  & !missing(v2000) & !missing(v2000_tot) 
egen tp = total(v2000) if insample
egen tv = total(v2000_tot) if insample
gen vp = tp / tv
sum vp, meanonly
scalar xo_office = r(mean)
drop tp tv vp 
//---------scalar(xr)
replace insample = (mz >= 81 & mz <=95)  & !missing(v2000) & !missing(v2000_tot) 
egen tp = total(v2000) if insample
egen tv = total(v2000_tot) if insample
gen vp = tp / tv
sum vp, meanonly
scalar xr_office = r(mean)
drop tp tv vp 
//--------- scalar(xr) = v2000/totval07 , for Imperial
replace insample = (mz >= 81 & mz <=95)  & !missing(v2000) & !missing(totvalue07) 
egen tp = total(v2000) if insample
egen tv = total(totvalue07) if insample
gen vp = tp / tv
sum vp, meanonly
scalar xr_office_im = r(mean)
drop tp tv vp


//========== LA, VT , SB ===========
replace insample = ((mz >= 1 & mz <=46) | (mz >= 47 & mz <=49) | (mz >= 67 & mz <=80))  
xtreg lgvsq_tot fsub cbd fwy ocean i.saleyr i.lu_08 if insample & !missing(saleyr), fe
levelsof saleyr if e(sample), local(lvsaleyr)
foreach lv of local lvsaleyr {
        capture replace  lgvsq2000_tot =  lgvsq_tot - _b[`lv'.saleyr] + _b[2000.saleyr] if saleyr == `lv'  & e(sample)
    }
replace saleyr = 2000 if e(sample)==0  & insample
predict lgvsq_p if insample
replace lgvsq2000_tot = lgvsq_p if e(sample) == 0 & insample
drop  lgvsq_p


replace insample = ((mz >= 1 & mz <=46) | (mz >= 47 & mz <=49))
replace lgvsq2000 = lgvsq2000_tot + ln(scalar(xo_office)) if insample  

replace insample = (mz >= 67 & mz <=80)  
replace lgvsq2000 = lgvsq2000_tot + ln(scalar(xr_office)) if insample


//======= IM ====
replace insample = mz == 96 | mz == 97
replace lgvsq2000 = ln(totvalue07/floor_area) + ln(scalar(xr_office_im)) if insample  



//================================================= rent - value ratio ==========================

estimates restore rvratio
predict lgrv
gen rv_ratio = exp(lgrv)  
replace v2000 = exp(lgvsq2000) * floor_area
gen r2000 = v2000 * rv_ratio  



//===================================aggregate to model zone level ====================================

gen tsample_v = !missing(floor_area) &  !missing(v2000)  
gen tsample_r = !missing(floor_area) &  !missing(r2000) 

bysort mz: egen v_mz = total(v2000) if tsample_v
bysort mz: egen r_mz = total(r2000) if tsample_r
bysort mz: egen fa_mz_v = total(floor_area) if tsample_v
bysort mz: egen fa_mz_r = total(floor_area) if tsample_r
gen vsq_mz = v_mz / fa_mz_v
gen rsq_mz = r_mz / fa_mz_r
drop tsample_r r_mz fa_mz_r tsample_v v_mz fa_mz_v 

save jan19_office.dta, replace


collapse vsq_mz rsq_mz mz, by(name)
outsheet using mz_office_jan19.csv, comma replace


