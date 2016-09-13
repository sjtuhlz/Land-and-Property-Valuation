//Confidentiality reason of Census data, so use county-level data instead of census tract level data.
//Dec.30, 2011
// An extension of dec_20_test.do

//==============SINGLE RESIDENTIAL ======================
capture log close
log using dec_30, replace
clear
clear matrix
set mem 1g
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
use residential_six_final_single_dec30.dta, clear
sum impsqft new_impsqft

//======================begin single========================= 
//data processing
replace sale_price = . if sale_price == 0 
replace impsqft = . if impsqft == 0
replace new_impsqft = . if new_impsqft == 0
replace totvalue07 = . if totvalue07 == 0
replace shape_area = . if shape_area == 0
replace saleyr = . if saleyr == 1899 | saleyr == 0 | saleyr == 1900
gen saleyr_bk = saleyr

//reduce computation by taking a random sample
drop if mz ==0 | missing(mz)
sample 1, by (mz)

rename impsqft floor_area1
rename new_impsqft floor_area2
replace floor_area2 = floor_area1 if missing(floor_area2) 
gen lnf1 = ln(floor_area1)
gen lnf2 = ln(floor_area2)

gen lnp = ln(sale_price)
gen lnv = ln(totvalue07)
gen lnl = ln(shape_area)

gen pos = hos / (hos + hrs)  //percentage of single housing units that are owner occupied 
gen gamma = 0.6 // A-2 : gamma = Rrs / Ros
gen ros = rrs /gamma
gen zeta = 0.9 //adjust owners overreport their housing values

encode city , gen(citynum)
xtset citynum

levelsof county, local(lv_county)

//====  initial values for loop
gen insample = .
gen lnp2000 = .
gen lnv2000 = .
gen vs =.
gen vrs = .
gen rvs = .
gen v2000 = .
gen r2000 = .
gen v2000_tot = .



// =========== loop starts ========================
forvalues i = 1/2 {

capture drop floor_area
gen floor_area = floor_area`i'
capture drop lnf
gen lnf = lnf`i'

//=================== regression (Orange & Riverside)======================
replace insample = (mz >= 50 & mz <=66) | (mz >= 81 & mz <=95)

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
replace insample = (mz >= 50 & mz <=66)
replace v2000 = exp(lnp2000)  if insample
replace v2000_tot = exp(lnv2000)  if insample
egen tp = total(v2000) if insample
egen tv = total(v2000_tot) if insample
gen vp = tp / tv
sum vp, meanonly
scalar xo_residential = r(mean)
drop tp tv vp 
//---------scalar(xr_residential)
replace insample = (mz >= 81 & mz <=95)  
replace v2000 = exp(lnp2000)  if insample
replace v2000_tot = exp(lnv2000)  if insample
egen tp = total(v2000) if insample
egen tv = total(v2000_tot) if insample
gen vp = tp / tv
sum vp, meanonly
scalar xr_residential = r(mean)
drop tp tv vp 

//========== LA, VT , SB ===========
replace insample = (mz >= 1 & mz <=46) | (mz >= 47 & mz <=49) | (mz >= 67 & mz <=80)   

xtreg lnv lnf lnl fsub cbd fwy ocean i.saleyr i.lu_08 if insample & !missing(saleyr), fe
levelsof saleyr if e(sample), local(lvsaleyr)
foreach lv of local lvsaleyr {
        capture replace  lnv2000 =  lnv - _b[`lv'.saleyr] + _b[2000.saleyr] if saleyr == `lv'  & e(sample)
    }
replace saleyr = 2000 if e(sample)==0  & insample
predict lnv_p if insample
replace lnv2000 = lnv_p if e(sample) == 0 & insample
drop  lnv_p

replace insample = (mz >= 1 & mz <=46) | (mz >= 47 & mz <=49) 
replace v2000 = exp(lnv2000)  / scalar(xo_residential) if insample
replace insample = (mz >= 67 & mz <=80)   
replace v2000 = exp(lnv2000)  / scalar(xr_residential) if insample

//================================================= rent to value  ratioo  ==========================


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

rename vsq_mz vsq_mz`i'
rename rsq_mz rsq_mz`i'

sum 
}

save dec30_five_residential_single.dta, replace

drop if missing(vsq_mz1) | missing(vsq_mz2)
duplicates drop vsq_mz1 vsq_mz2, force
outsheet  vsq_mz1 vsq_mz2 rsq_mz1 rsq_mz2 mz name using mz_residential_single_dec30.csv, comma replace


