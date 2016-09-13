//Feb 2, 2012
//Discounting values calculated by Wenwen (totvalue07/floorarea) to year 2000.  Borrow this ratio from each land use type in Riverside County
//Calculating Rent to every major land use category in Imperial County. Borrow rent-value ratio of each land use type in Riverside County


capture log close
log using feb_2_imperial, replace
clear all
set more off
//===================Rent-to-Value Ratio =====================
use "D:\summer, 2011\Second Project\practice\Six counties\debug\avavaluerent_processed.dta", clear
xi: reg lgrv  lgvsq2000 fsub cbd ocean fwy age 
estimates store rvratio


//=================== Non - Residential Non - Vacant ==========================
use  "D:\summer, 2011\Second Project\practice\Six counties\debug\oct_10_five.dta", clear
keep if  mz >=81 & mz <=95
gen fwy = freeway * 0.00062137
replace ocean = ocean * 0.00062137
save test.dta, replace
use "D:\summer, 2011\Third Project\office_builing\Data\Five_parcelMZ_office.dta", clear
keep if  mz >=81 & mz <=95
append using test.dta, force

replace totvalue07 = . if totvalue07 == 0
replace sale_price = . if sale_price == 0
replace saleyr = . if saleyr == 0 | saleyr == 1899
gen floor_area =  impsqft if impsqft  != 0
gen lgvsq  = ln( sale_price/ floor_area ) 
gen age = 2000 - yearbuilt if yearbuilt != 0
format scag_xyid  %14.0f

gen age_bk = age
levelsof city, local(citylist)
foreach cvar of local citylist { 
        summarize age if city == "`cvar'" , meanonly 
        replace age = r(mean) if missing(age) & city == "`cvar'"
}

gen office = lu_08 >= 1210 & lu_08 < 1220
gen retail = lu_08 < 1230 & lu_08 >= 1220
gen other_commercial = lu_08 == 1200 | (lu_08 >= 1230 & lu_08 <1240)
gen public = lu_08 >= 1240 & lu_08 <1270 //(military installations were excluded)
gen industrial_warehousing = lu_08 < 1400 & lu_08 >= 1300
gen transport = lu_08 >= 1400 & lu_08 <1500
gen mixed = lu_08 >= 1500 & lu_08 <1700


//=============LOOP begins =================

//tech note 1: use different lcname (supervar vs var) in netsted foreach loops
//tech note 2: obtain the name of a variable and generate a scalar under that name can be done in: scalar `var', where `var' refers to a variable, instead of a local macro
foreach supervar of varlist office retail other_commercial public industrial_warehousing  transport mixed {
preserve 
keep if `supervar'

char city [omit] "Riverside"  
char saleyr [omit] 2000 

xi: reg  lgvsq  fsub cbd fwy ocean  i.city i.saleyr i.lu_08 if !missing(saleyr)
foreach var of varlist _Isaleyr* { 
    gen b`var' = _b[`var']
}
 
 //pick parcels that actually need to be discounted
gen id_keep = !missing(lgvsq) & !missing(saleyr) 
//pick parcels that will be used in totval07 - saleprice ratio computation, difference btw id & id_XR is the latter does not require non-missing floor area
gen id_XR = !missing(sale_price) & !missing(saleyr) & !missing(totvalue07)


gen lgvsq2000 = .
foreach var of varlist _Isaleyr* {
    replace lgvsq2000 =  lgvsq - b`var' if `var' == 1 & id_keep
} 
foreach var of varlist _Isaleyr* {
    replace `var' = 0 
}
predict lgvsq_p 
replace lgvsq2000 = lgvsq_p if id_keep == 0 

//compute scalar(xr_public) = totvalue07 / salesprice
gen xr_sample = id_XR
gen sp = sale_price if xr_sample
gen dd = 1 if xr_sample
foreach var of varlist _Isaleyr* { 
    replace dd = exp(b`var') if `var' == 1 
}
replace sp = sp / dd
egen tsp = total(sp) if xr_sample
egen tv = total(totvalue07) if xr_sample
foreach var of varlist tsp tv {
    summarize `var' if id_XR, meanonly
    scalar `var' = r(mean)	
}
scalar xr_`supervar' = scalar(tv) / scalar(tsp)
drop xr_sample sp dd tsp tv 
scalar drop tsp tv



//compute scalar(rv_public)
gen vsq2000 = exp(lgvsq2000)
//imputation of rent
estimates restore rvratio
predict lgrv
gen rv_ratio = exp(lgrv)  
gen rsq2000 = vsq2000 * rv_ratio 
gen v2000 = vsq2000 * floor_area
gen r2000 = rsq2000 * floor_area

gen tsample = !missing(v2000) & !missing(r2000)
foreach var of varlist v2000 r2000 {
    summarize `var' if tsample, meanonly
    scalar `var' = r(mean)	
}
scalar rv_`supervar' = scalar(r2000)/scalar(v2000)
scalar drop v2000 r2000
restore
}

scalar list

//=================== VACANT ======================================
