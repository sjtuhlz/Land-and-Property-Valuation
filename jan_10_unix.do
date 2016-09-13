//jan 10, 2012
(3417998 observations deleted)
//version for program to be run on unix server.
//mobile home treated as a separate major category,  the other two are single and multi.
//boats and not part of mobil home


//cd  "D:\summer, 2011\Third Project\Final_data"
cap log close
log using jan_10, replace 
clear
clear matrix
set mem 8g
set more off


use residential_six_final_3category.dta, clear
drop hos hom homb hrs hrm hrmb vos vom vomb rrs rrm rrmb dup
capture drop _merge
merge m:1 mz using census_mz_3category.dta
drop if _merge == 1
save residential_six_final_3category.dta, replace





//data processing
drop if mz ==0 | missing(mz)

//sample 1, by (mz)


capture drop _merge
replace sale_price = . if sale_price == 0 
replace floor_area = . if floor_area == 0
replace totvalue07 = . if totvalue07 == 0
replace shape_area = . if shape_area == 0
replace saleyr = . if saleyr == 1899 | saleyr == 0 | saleyr == 1900
gen saleyr_bk = saleyr

//CLASSIFICATION
//not included is parcels with lu_08 = 1100, which lacks further classification
gen single = lu_08 >= 1110 & lu_08 < 1120
gen multi = lu_08 >= 1120 & lu_08 < 1130
gen mbhome = lu_08 >= 1130 & lu_08 < 1140
gen mixed = lu_08 >= 1140 & lu_08 < 1150
gen rural = lu_08 >= 1150 & lu_08 < 1200

gen lnf = ln(floor_area)
gen lgvsq = ln( sale_price / floor_area)
gen lgvsq_tot = ln( totvalue07 / floor_area)
gen lnp = ln(sale_price)
gen lnv = ln(totvalue07)
gen lnl = ln(shape_area)

gen pos = hos / (hos + hrs)  //percentage of single housing units that are owner occupied 
gen pom = hom / (hom + hrm) //percentage of multi housing units that are owner occupied 
gen pomb = homb / (homb + hrmb) //percentage of mobile homes that are owner occupied 

scalar gamma = 0.6 // A-2 : gamma = Rrs / Ros
gen ros = rrs /scalar(gamma)
gen rom = vom * ros / vos 
gen romb = vomb * ros / vos 
gen zeta = 0.9 //adjust owners overreport their housing values
gen thita = 0.8 // 80% of floor area of multi-unit housing is rentable
//gen lambda = 1 //quality of census is twice high as quality of Scag parcel database
scalar alpha1 = 0.5
scalar alpha2 = 0.6
scalar alpha3 = 0.7
encode city , gen(citynum)
xtset citynum

//====  initial values 
gen insample = .
gen lnp2000 = .
gen lnv2000 = .
//gen vrs = .
gen rvs = .
gen v2000 = .
//gen r2000 = .
gen v2000_tot = .
//gen vrm = .
gen rvm = .
//gen vrmb = .
gen rvmb = .
gen lgvsq2000 = .
gen lgvsq2000_tot = .
gen area = .
local types single multi mbhome mixed rural 
forvalues i = 1/3 {
    gen r2000_`i' = .
    gen vrs`i' = .
    gen vrm`i' = .
    gen vrmb`i' = .
}

foreach tp of local types {
replace insample = .
replace v2000 = .
replace v2000_tot = .
replace lgvsq2000 = .
replace lgvsq2000_tot = .
//=================== regression (Orange & Riverside)==========================
replace insample = ((mz >= 50 & mz <=66) | (mz >= 81 & mz <=95)) & `tp'  

if "`tp'" == "single" | "`tp'" == "mixed" | "`tp'" == "rural"{
    //regression and imputation using SALE_PRICE
    xtreg lnp lnf lnl fsub cbd fwy ocean i.saleyr i.lu_08 if insample & !missing(saleyr), fe
    scalar wt_lnf_orrv = _b[lnf] / (_b[lnf] + _b[lnl] )
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
    xtreg lnv lnf lnl fsub cbd fwy ocean i.saleyr i.lu_08 if insample & !missing(saleyr), fe
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
}

if "`tp'" == "multi" | "`tp'" == "mbhome" {
    xtreg lgvsq  fsub cbd fwy ocean  i.saleyr  i.lu_08 if insample & !missing(saleyr), fe
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
    replace lgvsq2000 = lgvsq_p if e(sample) == 0 & insample
    drop  lgvsq_p
    
    replace v2000 = exp(lgvsq2000) * floor_area if insample
    replace v2000_tot = exp(lgvsq2000_tot) * floor_area if insample
}

//compare
//----------scalar(xo)
replace insample = (mz >= 50 & mz <=66) & `tp' & !missing(v2000) & !missing(v2000_tot) 
egen tp = total(v2000) if insample
egen tv = total(v2000_tot) if insample
gen vp = tp / tv
sum vp, meanonly
scalar xo_`tp' = r(mean)
drop tp tv vp 
//---------scalar(xr)
replace insample = (mz >= 81 & mz <=95) & `tp' & !missing(v2000) & !missing(v2000_tot) 
egen tp = total(v2000) if insample
egen tv = total(v2000_tot) if insample
gen vp = tp / tv
sum vp, meanonly
scalar xr_`tp' = r(mean)
drop tp tv vp 

//========== LA, VT , SB ===========
replace insample = ((mz >= 1 & mz <=46) | (mz >= 47 & mz <=49) | (mz >= 67 & mz <=80)) & `tp' 
if "`tp'" == "single" | "`tp'" == "mixed" | "`tp'" == "rural" {
    xtreg lnv lnf lnl fsub cbd fwy ocean i.saleyr i.lu_08 if insample & !missing(saleyr), fe
    scalar wt_lnf_lavtsb = _b[lnf] / (_b[lnf] + _b[lnl] )
    levelsof saleyr if e(sample), local(lvsaleyr)
    foreach lv of local lvsaleyr {
            capture replace  lnv2000 =  lnv - _b[`lv'.saleyr] + _b[2000.saleyr] if saleyr == `lv'  & e(sample)
        }
    replace saleyr = 2000 if e(sample)==0  & insample
    predict lnv_p if insample
    replace lnv2000 = lnv_p if e(sample) == 0 & insample
    drop  lnv_p
    
    replace insample = ((mz >= 1 & mz <=46) | (mz >= 47 & mz <=49)) & `tp'
    replace v2000 = exp(lnv2000)  * scalar(xo_`tp') if insample
    replace insample = (mz >= 67 & mz <=80) & `tp'
    replace v2000 = exp(lnv2000)  * scalar(xr_`tp') if insample
}
if "`tp'" == "multi" | "`tp'" == "mbhome" {
    xtreg lgvsq_tot fsub cbd fwy ocean i.saleyr i.lu_08 if insample & !missing(saleyr), fe
    levelsof saleyr if e(sample), local(lvsaleyr)
    foreach lv of local lvsaleyr {
            capture replace  lgvsq2000_tot =  lgvsq_tot - _b[`lv'.saleyr] + _b[2000.saleyr] if saleyr == `lv'  & e(sample)
        }
    replace saleyr = 2000 if e(sample)==0  & insample
    predict lgvsq_p if insample
    replace lgvsq2000_tot = lgvsq_p if e(sample) == 0 & insample
    drop  lgvsq_p
    
    replace insample = ((mz >= 1 & mz <=46) | (mz >= 47 & mz <=49)) & `tp' 
    replace v2000 = exp(lgvsq2000_tot) * floor_area * scalar(xo_`tp') if insample
    replace insample = (mz >= 67 & mz <=80) & `tp'   
    replace v2000 = exp(lgvsq2000_tot) * floor_area * scalar(xr_`tp') if insample
}
//==================== rent to value  ratioo  ==========================
//MOST IMPORTANTLY, Vrs / Vos = 0.5, 0.6, 0.7 to see the difference from Vrs = (lambda * vs - zeta * vos * pos) / (1 - pos)  
//V*, R*, H* those variable names are different for diff types, so cannot use loop, use "if" instead
if "`tp'" == "single" {
    forvalues i = 1/3 {
        replace vrs`i' = vos * scalar(alpha`i')
        replace rvs = (ros * pos + rrs * (1 - pos)) / (zeta * vos * pos + zeta * vrs`i' * (1 - pos))    
        replace r2000_`i' = v2000 * rvs * 12  //rents in Census is montly!
    }
}   
if "`tp'" == "multi"{
    forvalues i = 1/3 {
        replace vrm`i' = vrs`i' * rrm / rrs
        replace rvm = (rom * pom + rrm * (1 - pom)) / (zeta * vom * pom + zeta * vrm`i' * (1 - pom))  
        replace r2000_`i' = v2000 * rvm * 12  //rents in Census is montly!
    }
}


if "`tp'" == "mbhome" {
    forvalues i = 1/3 {
        replace vrmb`i' = vrs`i' * rrmb / rrs
        replace rvmb = (romb * pomb + rrmb * (1 - pomb)) / (zeta * vomb * pomb + zeta * vrmb`i' * (1 - pomb))  
        replace r2000_`i' = v2000 * rvmb * 12  //rents in Census is montly!
    }
}
//=====================aggregate to model zone level ======================
if "`tp'" == "multi" | "`tp'" == "mbhome" {
    gen tsample_v = !missing(floor_area) &  !missing(v2000)  
    bysort mz: egen v_mz = total(v2000) if tsample_v
    bysort mz: egen fa_mz_v = total(floor_area) if tsample_v
    gen vsq_mz_`tp' = v_mz / fa_mz_v
    forvalues i = 1/3 {
        gen tsample_r = !missing(floor_area) &  !missing(r2000_`i')  
        bysort mz: egen r_mz = total(r2000_`i') if tsample_r
	bysort mz: egen fa_mz_r = total(floor_area) if tsample_r
        gen rsq_mz_`tp'_`i' = r_mz / fa_mz_r
        drop tsample_r r_mz fa_mz_r
    }
    drop tsample_v v_mz fa_mz_v
}

if "`tp'" == "single" {
    replace area = exp(ln(floor_area) * scalar(wt_lnf_orrv) + ln(shape_area) * (1 - scalar(wt_lnf_orrv))) if (mz >= 50 & mz <=66) | (mz >= 81 & mz <=95)
    replace area = exp(ln(floor_area) * scalar(wt_lnf_lavtsb) + ln(shape_area) * (1 - scalar(wt_lnf_lavtsb))) if (mz >= 1 & mz <=46) | (mz >= 47 & mz <=49) | (mz >= 67 & mz <=80)
    gen tsample_v = !missing(area) &  !missing(v2000)  
    bysort mz: egen v_mz = total(v2000) if tsample_v
    bysort mz: egen fa_mz_v = total(area) if tsample_v
    gen vsq_mz_`tp' = v_mz / fa_mz_v
    forvalues i = 1/3 {
        gen tsample_r = !missing(area) &  !missing(r2000_`i')  
        bysort mz: egen r_mz = total(r2000_`i') if tsample_r
        bysort mz: egen fa_mz_r = total(area) if tsample_r
	gen rsq_mz_`tp'_`i' = r_mz / fa_mz_r
        drop tsample_r r_mz fa_mz_r
    }
    drop tsample_v v_mz fa_mz_v
}


if  "`tp'" == "mixed" | "`tp'" == "rural"{
    gen tsample_v = !missing(floor_area) &  !missing(v2000)  
    bysort mz: egen v_mz = total(v2000) if tsample_v
    bysort mz: egen fa_mz_v = total(floor_area) if tsample_v
    gen vsq_mz_`tp' = v_mz / fa_mz_v
    drop tsample_v  v_mz fa_mz_v 
}

sum 

}

save results_jan10.dta, replace

//collapse hos hom hrs hrm vsq_mz_single rsq_mz_single vsq_mz_multi rsq_mz_multi vsq_mz_mbhome rsq_mz_mbhome vsq_mz_mixed rsq_mz_mixed  vsq_mz_rural rsq_mz_rural vos vom vomb rrs rrm rrmb vs vrs vrm vrmb ros rom romb mz, by(name)
collapse ho* hr* vsq_mz* rsq_mz* vo* rr* vr* ro* mz, by(name)
gen ps = (hos + hrs) / (hom + hrm + hos + hrs)
forvalues i = 1/3 {
    gen rv = rsq_mz_single_`i' / vsq_mz_single * ps + rsq_mz_multi_`i' / vsq_mz_multi * (1 - ps)
    gen rsq_mz_mixed_`i' = vsq_mz_mixed * rv
    gen rsq_mz_rural_`i' = vsq_mz_rural * rv
    drop rv
}

forvalues i = 1/3 {
    outsheet mz name vsq_mz* rsq_mz*`i'  vos vom vomb vrs`i' vrm`i' vrmb`i' rrs rrm rrmb ros rom romb using jan_10_`i'.csv, comma replace
}





