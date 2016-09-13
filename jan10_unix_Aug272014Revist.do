//Aug 27, 2014 ====>  an updated version of jan 10, 2012
** Objective: to obtain the conversion factor of SFR land use in Riverside County and Orange County. 
** requested by Richard to put on the paper. 


clear
set more off
set mem 2g 


/*
use residential_six_final_3category.dta, clear
drop hos hom homb hrs hrm hrmb vos vom vomb rrs rrm rrmb dup
capture drop _merge
merge m:1 mz using census_mz_3category.dta
drop if _merge == 1
save residential_six_final_3category.dta, replace
*/


use city shape_area lu_08 mz fsub cbd fwy ocean totvalue07 saleyr sale_price floor_area county if lu_08 >= 1110 & lu_08 < 1120 using "/Users/huiling/Documents/summer, 2011/Third Project/Final_data/residential_six_final_3category_unix.dta" , clear
//save sfr_aug272014.dta, replace
** keep if lu_08 >= 1110 & lu_08 < 1120  //SFR land use

replace county = upper(county)
keep if (county == "OR" | county == "RV")

replace sale_price = . if sale_price == 0 
replace floor_area = . if floor_area == 0
replace totvalue07 = . if totvalue07 == 0
replace shape_area = . if shape_area == 0
replace saleyr = . if saleyr == 1899 | saleyr == 0 | saleyr == 1900
gen saleyr_bk = saleyr

encode city, gen(city_n)


/*
gen single = lu_08 >= 1110 & lu_08 < 1120
gen multi = lu_08 >= 1120 & lu_08 < 1130
gen mbhome = lu_08 >= 1130 & lu_08 < 1140
gen mixed = lu_08 >= 1140 & lu_08 < 1150
gen rural = lu_08 >= 1150 & lu_08 < 1200
*/



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






//----------scalar(xo)
replace insample = county == "OR" & !missing(v2000) & !missing(v2000_tot) 
egen tp = total(v2000) if insample
egen tv = total(v2000_tot) if insample
gen vp = tp / tv
sum vp, meanonly
scalar xo = r(mean)
drop tp tv vp 
//---------scalar(xr)
replace insample = county == "RV" & !missing(v2000) & !missing(v2000_tot) 
egen tp = total(v2000) if insample
egen tv = total(v2000_tot) if insample
gen vp = tp / tv
sum vp, meanonly
scalar xr = r(mean)
drop tp tv vp 











