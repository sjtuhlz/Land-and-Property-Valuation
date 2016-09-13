//Use county level data
//Jan 2, 2012
//  Vrm = Vrs / Rrs * Rrm
// Single and Multi are calculated together~
//originally available floor area data

capture log close
log using jan_2_2, replace
clear
clear matrix
set mem 1g
set more off
cd "D:\summer, 2011\Third Project\Final_data"

/*
insheet using county_level_1230.csv, clear
save county_level_1230.dta, replace
*/
/*
use residential_six_final_single.dta, clear
drop tractid hos hom hrs hrm vos vom rrs rrm 
capture drop cash* 
capture drop _merge*
gen county = ""
replace county = "Los Angeles" if mz >= 1 & mz <=46
replace county = "Orange" if mz >= 50 & mz <=66
replace county = "Riverside" if mz >= 81 & mz <=95
replace county = "San Bernardino" if mz >= 67 & mz <=80
replace county = "Ventura" if mz >= 47 & mz <=49

merge m:1 county using county_level_1230.dta
save residential_six_final_single_dec30.dta, replace
*/

/*
use residential_six_final.dta, clear
drop y id lotsqft yearbuilt county
replace lu_08 = lu08 if missing(lu_08)  
gen single = lu_08 >= 1110 & lu_08 < 1120
gen multi = (lu_08 >= 1120 & lu_08 < 1130)   
drop if single==0 & multi==0
drop if missing(x) 
capture drop _merge*
duplicates tag scag, gen(dup)
drop if dup != 0
drop dup
save residential_six_final_jan2_1.dta, replace
*/


use residential_six_final_jan2.dta, clear
drop tractid hos hom hrs hrm vos vom rrs rrm 
capture drop cash* 
capture drop _merge*
gen county = ""
replace county = "Los Angeles" if mz >= 1 & mz <=46
replace county = "Orange" if mz >= 50 & mz <=66
replace county = "Riverside" if mz >= 81 & mz <=95
replace county = "San Bernardino" if mz >= 67 & mz <=80
replace county = "Ventura" if mz >= 47 & mz <=49

merge m:1 county using county_level_1230.dta
save residential_six_final_jan2_2.dta, replace

//======================begin single========================= 
//data processing
replace sale_price = . if sale_price == 0 
replace floor_area = . if floor_area == 0
replace totvalue07 = . if totvalue07 == 0
replace shape_area = . if shape_area == 0
replace saleyr = . if saleyr == 1899 | saleyr == 0 | saleyr == 1900
gen saleyr_bk = saleyr

//reduce computation by taking a random sample
drop if mz ==0 | missing(mz)
sample 1, by (mz)


gen lnf = ln(floor_area)
gen lgvsq = ln( sale_price / floor_area)
gen lgvsq_tot = ln( totvalue07 / floor_area)
gen lnp = ln(sale_price)
gen lnv = ln(totvalue07)
gen lnl = ln(shape_area)

gen pos = hos / (hos + hrs)  //percentage of single housing units that are owner occupied 
gen pom = hom / (hom + hrm) //percentage of multi housing units that are owner occupied 
gen gamma = 0.6 // A-2 : gamma = Rrs / Ros
gen ros = rrs /gamma
gen zeta = 0.9 //adjust owners overreport their housing values
gen thita = 0.8 // 80% of floor area of multi-unit housing is rentable


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

//=================== regression (Orange & Riverside)======================
replace insample = ((mz >= 50 & mz <=66) | (mz >= 81 & mz <=95)) & single  

//regression and imputation using SALE_PRICE
xtreg lnp lnf lnl fsub cbd fwy ocean i.saleyr i.lu_08 if insample & !missing(saleyr), fe
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


//compare
//----------scalar(xo_residential)
replace insample = (mz >= 50 & mz <=66) & single
replace v2000 = exp(lnp2000)  if insample
replace v2000_tot = exp(lnv2000)  if insample
egen tp = total(v2000) if insample
egen tv = total(v2000_tot) if insample
gen vp = tp / tv
sum vp, meanonly
scalar xo_residential = r(mean)
drop tp tv vp 
//---------scalar(xr_residential)
replace insample = (mz >= 81 & mz <=95) & single
replace v2000 = exp(lnp2000)  if insample
replace v2000_tot = exp(lnv2000)  if insample
egen tp = total(v2000) if insample
egen tv = total(v2000_tot) if insample
gen vp = tp / tv
sum vp, meanonly
scalar xr_residential = r(mean)
drop tp tv vp 

//========== LA, VT , SB ===========
replace insample = ((mz >= 1 & mz <=46) | (mz >= 47 & mz <=49) | (mz >= 67 & mz <=80)) & single   

xtreg lnv lnf lnl fsub cbd fwy ocean i.saleyr i.lu_08 if insample & !missing(saleyr), fe
levelsof saleyr if e(sample), local(lvsaleyr)
foreach lv of local lvsaleyr {
        capture replace  lnv2000 =  lnv - _b[`lv'.saleyr] + _b[2000.saleyr] if saleyr == `lv'  & e(sample)
    }
replace saleyr = 2000 if e(sample)==0  & insample
predict lnv_p if insample
replace lnv2000 = lnv_p if e(sample) == 0 & insample
drop  lnv_p

replace insample = ((mz >= 1 & mz <=46) | (mz >= 47 & mz <=49)) & single 
replace v2000 = exp(lnv2000)  / scalar(xo_residential) if insample
replace insample = (mz >= 67 & mz <=80) & single
replace v2000 = exp(lnv2000)  / scalar(xr_residential) if insample

//================================================= rent to value  ratioo  ==========================

levelsof county, local(lv_county)
foreach lv of local lv_county {
    sum v2000 if county == "`lv'", meanonly
    replace vs = r(mean) if county == "`lv'"
}

replace vrs = (vs - zeta * vos * pos) / (1 - pos)  
replace rvs = (ros * pos + rrs * (1 - pos)) / (vos * pos + vrs * (1 - pos))    

replace r2000 = v2000 * rvs * 12  //rents in Census is montly!

   
//======================================aggregate to model zone level ====================================

gen tsample_v = !missing(floor_area) &  !missing(v2000)  
gen tsample_r = !missing(floor_area) &  !missing(r2000)  
bysort mz: egen v_mz = total(v2000) if tsample_v
bysort mz: egen r_mz = total(r2000) if tsample_r
bysort mz: egen fa_mz_v = total(floor_area) if tsample_v
bysort mz: egen fa_mz_r = total(floor_area) if tsample_r
gen vsq_mz = v_mz / fa_mz_v
gen rsq_mz = r_mz / fa_mz_r
drop tsample_v tsample_r v_mz r_mz fa_mz_v fa_mz_r

sum 


preserve
drop if missing(vsq_mz) 
duplicates drop vsq_mz, force
outsheet  vsq_mz rsq_mz mz name using mz_single_jan2.csv, comma replace

restore
drop vsq_mz  rsq_mz


//======================begin multi========================= 
//====  initial values 
replace insample = .
gen vm =.
gen vrm = .
gen rom = .
gen rvm = .
replace v2000 = .
replace r2000 = .
replace v2000_tot = .

gen lgvsq2000 = .
gen lgvsq2000_tot = .
//=================== regression (Orange & Riverside)======================
replace insample = ((mz >= 50 & mz <=66) | (mz >= 81 & mz <=95)) & multi

//regression and imputation using SALE_PRICE
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


//compare
replace insample = (mz >= 50 & mz <=66) & multi
replace v2000 = exp(lgvsq2000) * floor_area if insample
replace v2000_tot = exp(lgvsq2000_tot) * floor_area if insample
egen tp = total(v2000) if insample
egen tv = total(v2000_tot) if insample
gen vp = tp / tv
sum vp, meanonly
scalar xo_residential = r(mean)
drop tp tv vp 
//---------scalar(xr_residential)
replace insample = (mz >= 81 & mz <=95) & multi
replace v2000 = exp(lgvsq2000) * floor_area if insample
replace v2000_tot = exp(lgvsq2000_tot) * floor_area if insample
egen tp = total(v2000) if insample
egen tv = total(v2000_tot) if insample
gen vp = tp / tv
sum vp, meanonly
scalar xr_residential = r(mean)
drop tp tv vp 

//========== LA, VT , SB ===========
replace insample = ((mz >= 1 & mz <=46) | (mz >= 47 & mz <=49) | (mz >= 67 & mz <=80)) & multi   

xtreg lgvsq_tot fsub cbd fwy ocean i.saleyr i.lu_08 if insample & !missing(saleyr), fe
levelsof saleyr if e(sample), local(lvsaleyr)
foreach lv of local lvsaleyr {
        capture replace  lgvsq2000_tot =  lgvsq_tot - _b[`lv'.saleyr] + _b[2000.saleyr] if saleyr == `lv'  & e(sample)
    }
replace saleyr = 2000 if e(sample)==0  & insample
predict lgvsq_p if insample
replace lgvsq2000_tot = lgvsq_p if e(sample) == 0 & insample
drop  lgvsq_p

replace insample = ((mz >= 1 & mz <=46) | (mz >= 47 & mz <=49)) & multi 
replace v2000 = exp(lgvsq2000_tot) * floor_area / scalar(xo_residential) if insample
replace insample = (mz >= 67 & mz <=80) & multi   
replace v2000 = exp(lgvsq2000_tot) * floor_area / scalar(xr_residential) if insample

//================================================= rent to value  ratioo  ==========================



replace vrm = vrs * rrm / rrs
replace rom = vom * ros / vos 
replace rvm = (rom * pom + rrm * (1 - pom)) / (vom * pom + vrm * (1 - pom))  
replace r2000 = v2000 * rvm * 12  //rents in Census is montly!

   
//======================================aggregate to model zone level ====================================

gen tsample_v = !missing(floor_area) &  !missing(v2000)  
gen tsample_r = !missing(floor_area) &  !missing(r2000)  
bysort mz: egen v_mz = total(v2000) if tsample_v
bysort mz: egen r_mz = total(r2000) if tsample_r
bysort mz: egen fa_mz_v = total(floor_area) if tsample_v
bysort mz: egen fa_mz_r = total(floor_area) if tsample_r
gen vsq_mz = v_mz / fa_mz_v
gen rsq_mz = r_mz / fa_mz_r
drop tsample_v tsample_r v_mz r_mz fa_mz_v fa_mz_r 


sum 

save jan2_2_five_residential.dta, replace

drop if missing(vsq_mz) 
duplicates drop vsq_mz, force
outsheet  vsq_mz rsq_mz mz name using mz_multi_jan2_2.csv, comma replace


