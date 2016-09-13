//July 27, 2013 ====>  an updated version of jan 10, 2012

//version for program to be run on unix server.
//mobile home treated as a separate major category,  the other two are single and multi.
//boats and not part of mobil home



cap log close
log using jan10_unix_july27_update, replace 
clear
set more off


/*
use residential_six_final_3category.dta, clear
drop hos hom homb hrs hrm hrmb vos vom vomb rrs rrm rrmb dup
capture drop _merge
merge m:1 mz using census_mz_3category.dta
drop if _merge == 1
save residential_six_final_3category.dta, replace
*/


* use residential_six_final_3category_unix.dta, clear
use "/Users/huiling/Documents/summer, 2011/Third Project/Final_data/residential_six_final_3category_unix.dta", clear
keep city shape_area lu_08 mz fsub cbd fwy ocean totvalue07 saleyr sale_price floor_area county 

//sample 1

replace county = upper(county)

replace sale_price = . if sale_price == 0 
replace floor_area = . if floor_area == 0
replace totvalue07 = . if totvalue07 == 0
replace shape_area = . if shape_area == 0
replace saleyr = . if saleyr == 1899 | saleyr == 0 | saleyr == 1900
gen saleyr_bk = saleyr

encode city, gen(city_n)

/*
// impute missing floor area from shape area 
reg floor_area shape_area i.lu_08 i.city_n
predict floor_area_p
gen floorarea_bk = floor_area
replace floor_area = floor_area_p if missing(floor_area)
*/

//CLASSIFICATION
//not included is parcels with lu_08 = 1100, which lacks further classification
gen single = lu_08 >= 1110 & lu_08 < 1120
gen multi = lu_08 >= 1120 & lu_08 < 1130
gen mbhome = lu_08 >= 1130 & lu_08 < 1140
gen mixed = lu_08 >= 1140 & lu_08 < 1150
gen rural = lu_08 >= 1150 & lu_08 < 1200


gen lnf = ln(floor_area)
gen lnp = ln(sale_price)
gen lnv = ln(totvalue07)
gen lnl = ln(shape_area)


gen ocean2 = ocean^2
gen cbd2 = cbd^2



gen insample = .
gen lnp2000 = .
gen lnv2000 = .

gen v2000 = .
gen v2000_tot = .

local types single multi mbhome mixed rural 
 
//=================== regression (Orange & Riverside)==========================
replace insample = (county == "OR" | county == "RV")  

    //regression and imputation using SALE_PRICE
	reg lnp lnf lnl fsub cbd cbd2 fwy ocean ocean2 ib(freq).city_n i.saleyr ib(freq).lu_08 if insample & !missing(saleyr)

    levelsof saleyr if e(sample), local(lvsaleyr)
    foreach lv of local lvsaleyr {
        capture replace lnp2000 =  lnp - _b[`lv'.saleyr] + _b[2000.saleyr]  if saleyr == `lv'  & e(sample)
    }
    replace saleyr = 2000 if e(sample) == 0 & insample
    predict lnp_p if insample
    replace saleyr = saleyr_bk
    replace lnp2000 = lnp_p if e(sample) == 0 & insample
    drop  lnp_p
    
    //regression and imputation using TOTVALUE07
    reg lnv lnf lnl fsub cbd cbd2 fwy ocean ocean2 i.city_n i.saleyr i.lu_08 if insample & !missing(saleyr)

    levelsof saleyr if e(sample), local(lvsaleyr)
    foreach lv of local lvsaleyr {
            capture replace   lnv2000 =  lnv - _b[`lv'.saleyr] + _b[2000.saleyr] if saleyr == `lv'  & e(sample)
        }
    replace saleyr = 2000 if e(sample)==0  & insample
    predict lnv_p if insample
    replace lnv2000 = lnv_p if e(sample) == 0 & insample
    drop  lnv_p
    
    replace v2000 = exp(lnp2000)  if insample
    replace v2000_tot = exp(lnv2000)  if insample




//compare
foreach tp of local types {
//----------scalar(xo)
replace insample = county == "OR" & `tp' & !missing(v2000) & !missing(v2000_tot) 
egen tp = total(v2000) if insample
egen tv = total(v2000_tot) if insample
gen vp = tp / tv
sum vp, meanonly
scalar xo_`tp' = r(mean)
drop tp tv vp 
//---------scalar(xr)
replace insample = county == "RV" & `tp' & !missing(v2000) & !missing(v2000_tot) 
egen tp = total(v2000) if insample
egen tv = total(v2000_tot) if insample
gen vp = tp / tv
sum vp, meanonly
scalar xr_`tp' = r(mean)
drop tp tv vp 
}


//========== LA, VT , SB ===========
// Put LA, VT into one regression, and SB into another. 
// Because if put them three together, SB have too large predicted property values. 


//====================== LA, VT ==========================
replace insample = (county == "LA" | county == "VT")   




    reg lnv lnf lnl fsub cbd cbd2 fwy ocean ocean2 ib(freq).city_n i.saleyr i.lu_08 if insample & !missing(saleyr)

    levelsof saleyr if e(sample), local(lvsaleyr)
    foreach lv of local lvsaleyr {
            capture replace  lnv2000 =  lnv - _b[`lv'.saleyr] + _b[2000.saleyr] if saleyr == `lv'  & e(sample)
        }
    replace saleyr = 2000 if e(sample)==0  & insample
    predict lnv_p if insample
    replace saleyr = saleyr_bk
    replace lnv2000 = lnv_p if e(sample) == 0 & insample
    drop  lnv_p
    
foreach tp of local types{
    replace v2000 = exp(lnv2000)  * scalar(xo_`tp') if insample & `tp' 
}


*}
//================================ SB =========================

replace insample = county == "SB"   


    reg lnv lnf lnl fsub cbd cbd2 fwy ocean ocean2 ib(freq).city_n i.saleyr i.lu_08 if insample & !missing(saleyr)

    scalar wt_lnf_lavtsb = _b[lnf] / (_b[lnf] + _b[lnl] )
    levelsof saleyr if e(sample), local(lvsaleyr)
    foreach lv of local lvsaleyr {
            capture replace  lnv2000 =  lnv - _b[`lv'.saleyr] + _b[2000.saleyr] if saleyr == `lv'  & e(sample)
        }
    replace saleyr = 2000 if e(sample)==0  & insample
    predict lnv_p if insample
    replace saleyr = saleyr_bk
    replace lnv2000 = lnv_p if e(sample) == 0 & insample
    drop  lnv_p
    
foreach tp of local types{
    replace v2000 = exp(lnv2000)  * scalar(xr_`tp') if insample & `tp'
}
    






*save results_jan10_unix_sep29_update.dta, replace
drop saleyr_bk insample ocean2 cbd2 lnf lnv lnl lnp city_n

//=================== aggregate to county level ===================
gen id_floorarea = !missing(floor_area) & floor_area != 0
gen id_shape_area = !missing(shape_area) & shape_area != 0

collapse (sum) shape_area floor_area v2000 id_floorarea id_shape_area, by(county single)


//Table 10A-propertyval
*outsheet using table10A_propertyval_imputefloorarea.csv, comma replace








