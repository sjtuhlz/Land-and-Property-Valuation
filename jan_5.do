//mobile home treated as a separate major category,  the other two are single and multi.
//Jan 5, 2012
//boats and not part of mobil home

/*
//=========== CENSUS data with TractID, H*, Rr*, Vo*
//mobile home treated as a separate major category,  the other two are single and multi.
//boat and RV/Van were not included
cd "D:\summer, 2011\Third Project\DT_download"
insheet using 2nd_DTDownload_la.csv, clear
save 2nd_DTDownload_la.dta, replace
local counties VT OR SB RV
foreach lc of local counties {
    insheet using 2nd_DTDownload_`lc'.csv, clear
    save 2nd_DTDownload_`lc'.dta, replace
}
use 2nd_DTDownload_la.dta, clear
foreach lc of local counties {
    append using 2nd_DTDownload_`lc'.dta, force
}
foreach var of varlist  rrs rrm rrmb vos vom vomb {
    gen `var'_n = real(`var')
    drop `var'
    rename `var'_n `var'
}

replace tractid = tractid + 6065000000 if county == "Riverside County"
replace tractid = tractid + 6059000000 if county == "Orange County"
replace tractid = tractid + 6037000000 if county == "Los Angeles County"    
save 2nd_DTDownload.dta, replace

*/


//Join wenwen's census-mz correspondence file with census tract data
/*
cd "D:\summer, 2011\Third Project\model_zone_level_census"
insheet using censusid_objectid.csv, clear
save censusid_objectid.dta, replace

insheet using census_mz_objectid_wenwen.csv, clear
merge m:1 objectid using censusid_objectid.dta, force
drop _merge*
rename id tractid
merge m:1 tractid using "D:\summer, 2011\Third Project\DT_download\2nd_DTDownload.dta"
save census_mz_3category.dta,replace
//note that Wenwen's file of censusid_mz correspondence includes also Imperial County, which has 29 census tracts, while my 2nd_DTDownload.dta has no Imperial County
*/

//collapse Wenwen's census file into mz level
/*
use census_mz_3category.dta, clear
drop if _merge == 1 //census tracts in Imperial County
drop if mz == 0
gen total_vos = vos * hos 
gen total_vom = vom * hom
gen total_vomb = vomb * homb
gen total_rrs = hrs * rrs
gen total_rrm = hrm * rrm
gen total_rrmb = hrmb * rrmb

foreach var of varlist  vos vom vomb rrs rrm rrmb {
    replace total_`var' = total_`var' * percentage
}
foreach var of varlist   hos hom homb hrs hrm hrmb {
    replace `var' = `var' * percentage
}

collapse (sum) hos hom homb hrs hrm hrmb total_vos total_vom total_vomb total_rrs total_rrm total_rrmb, by(mz)
gen vos = total_vos / hos
gen vom = total_vom / hom
gen vomb = total_vomb / homb
gen rrs = total_rrs / hrs
gen rrm = total_rrm / hrm
gen rrmb = total_rrmb / hrmb
capture drop _merge
save census_mz_3category.dta, replace
*/


cd  "D:\summer, 2011\Third Project\Final_data"
cap log close
log using jan_5, replace 
clear
clear matrix
set mem 1g
set more off
/*
use residential_six_final.dta, clear
drop hos hom hrs hrm vos vom rrs rrm x y apn tractid lotsqft yearbuilt id
capture drop _merge
merge m:1 mz using "D:\summer, 2011\Third Project\model_zone_level_census\census_mz_3category.dta", force
save residential_six_final_3category.dta, replace  //3categories suffix means here mobile homes were treated as a equally improtant major category as Single and Multi


//fix the problem of majority parcels in la county misses shape_area, done in jan 5, but copied from what did in jan3, 2012
use residential_six_final_3category.dta, clear
duplicates tag scag_xyid, gen(dup)
drop if dup != 0
capture drop _merge
merge 1:1 scag_xyid using parcel_la_residential_final.dta, keepusing(shape_area) update
save residential_six_final_3category.dta, replace

// for Ventura county failed to change those 0s to missing.
use residential_six_final_3category.dta, clear
foreach var of varlist rrs rrm rrmb vos vom vomb {
    capture replace `var' = . if `var' == 0
}
capture drop total_*
save residential_six_final_3category.dta, replace

//fix the problem of San Bernardino County does not have name for its model zones
use residential_six_final_3category.dta, clear
capture drop _merge
merge m:1 mz using mz_name_correspondence.dta, update
save residential_six_final_3category.dta, replace
*/


use residential_six_final_3category.dta, clear
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

gen gamma = 0.6 // A-2 : gamma = Rrs / Ros
gen ros = rrs /gamma
gen zeta = 0.9 //adjust owners overreport their housing values
gen thita = 0.8 // 80% of floor area of multi-unit housing is rentable
gen lambda = 1 //quality of census is twice high as quality of Scag parcel database

encode city , gen(citynum)
xtset citynum

//====  initial values 
gen insample = .
gen lnp2000 = .
gen lnv2000 = .
gen vs =.
gen vrs = .
gen rvs = .
gen v2000 = .
gen r2000 = .
gen v2000_tot = .
gen vrm = .
gen rom = .
gen rvm = .
gen vrmb = .
gen romb = .
gen rvmb = .
gen lgvsq2000 = .
gen lgvsq2000_tot = .
gen area = .
local types single multi mbhome mixed rural 

foreach tp of local types {
replace insample = .
replace v2000 = .
replace r2000 = .
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
    scalar wt_lnf_lasbvt = _b[lnf] / (_b[lnf] + _b[lnl] )
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
//V*, R*, H* those variable names are different for diff types, so cannot use loop, use "if" instead
levelsof mz, local(lv_mz)
if "`tp'" == "single" {
    foreach lv of local lv_mz {
        sum v2000 if mz == `lv', meanonly
        replace vs = r(mean) if mz == `lv' 
    }
    
    replace vrs = (lambda * vs - zeta * vos * pos) / (1 - pos)  
    replace rvs = (ros * pos + rrs * (1 - pos)) / (zeta * vos * pos + vrs * (1 - pos))    
    
    replace r2000 = v2000 * rvs * 12  //rents in Census is montly!
}
   
if "`tp'" == "multi"{
    replace vrm = vrs * rrm / rrs
    replace rom = vom * ros / vos 
    replace rvm = (rom * pom + rrm * (1 - pom)) / (zeta * vom * pom + vrm * (1 - pom))  
    replace r2000 = v2000 * rvm * 12  //rents in Census is montly!
}

if "`tp'" == "mbhome" {
    replace vrmb = vrs * rrmb / rrs
    replace romb = vomb * ros / vos 
    replace rvmb = (romb * pomb + rrmb * (1 - pomb)) / (zeta * vomb * pomb + vrmb * (1 - pomb))  
    replace r2000 = v2000 * rvmb * 12  //rents in Census is montly!
}

//=====================aggregate to model zone level ======================
if "`tp'" == "multi" | "`tp'" == "mbhome" {
    gen tsample_v = !missing(floor_area) &  !missing(v2000)  
    gen tsample_r = !missing(floor_area) &  !missing(r2000)  
    bysort mz: egen v_mz = total(v2000) if tsample_v
    bysort mz: egen r_mz = total(r2000) if tsample_r
    bysort mz: egen fa_mz_v = total(floor_area) if tsample_v
    bysort mz: egen fa_mz_r = total(floor_area) if tsample_r
    gen vsq_mz_`tp' = v_mz / fa_mz_v
    gen rsq_mz_`tp' = r_mz / fa_mz_r
    drop tsample_v tsample_r v_mz r_mz fa_mz_v fa_mz_r
}

if "`tp'" == "single" {
    replace area = exp(ln(floor_area) * scalar(wt_lnf_orrv) + ln(shape_area) * scalar(wt_lnf_orrv)) if (mz >= 50 & mz <=66) | (mz >= 81 & mz <=95)
    replace area = exp(ln(floor_area) * scalar(wt_lnf_lavtsb) + ln(shape_area) * scalar(wt_lnf_lavtsb)) if (mz >= 1 & mz <=46) | (mz >= 47 & mz <=49) | (mz >= 67 & mz <=80)
    gen tsample_v = !missing(area) &  !missing(v2000)  
    gen tsample_r = !missing(area) &  !missing(r2000)  
    bysort mz: egen v_mz = total(v2000) if tsample_v
    bysort mz: egen r_mz = total(r2000) if tsample_r
    bysort mz: egen fa_mz_v = total(area) if tsample_v
    bysort mz: egen fa_mz_r = total(area) if tsample_r
    gen vsq_mz_`tp' = v_mz / fa_mz_v
    gen rsq_mz_`tp' = r_mz / fa_mz_r
    drop tsample_v tsample_r v_mz r_mz fa_mz_v fa_mz_r

}


if  "`tp'" == "mixed" | "`tp'" == "rural"{
    gen tsample_v = !missing(floor_area) &  !missing(v2000)  
    bysort mz: egen v_mz = total(v2000) if tsample_v
    bysort mz: egen fa_mz_v = total(floor_area) if tsample_v
    gen vsq_mz_`tp' = v_mz / fa_mz_v
    drop tsample_v  v_mz fa_mz_v 
}

sum 
/*
preserve
drop if missing(vsq_mz) 
duplicates drop vsq_mz, force
if "`tp'" == "single" {
    outsheet  vsq_mz rsq_mz vos  vs vrs rrs ros  mz name using mz_`tp'_jan5.csv, comma replace
    else if "`tp'" == "multi" {
        outsheet  vsq_mz rsq_mz vom  vs vrs rrs ros  mz name using mz_`tp'_jan5.csv, comma replace
restore
drop vsq_mz  rsq_mz
*/
}

save results_jan5.dta, replace

collapse hos hom hrs hrm vsq_mz_single rsq_mz_single vsq_mz_multi rsq_mz_multi vsq_mz_mbhome rsq_mz_mbhome vsq_mz_mixed rsq_mz_mixed  vsq_mz_rural rsq_mz_rural vos vom vomb rrs rrm rrmb vs vrs vrm vrmb ros rom romb mz, by(name)

gen ps = (hos + hrs) / (hom + hrm + hos + hrs)
gen rv = rsq_mz_single / vsq_mz_single * ps + rsq_mz_multi / vsq_mz_multi * (1 - ps)
gen rsq_mz_mixed = vsq_mz_mixed * rv
gen rsq_mz_rural = vsq_mz_rural * rv


outsheet mz name v* r*  using jan_5.csv, replace




