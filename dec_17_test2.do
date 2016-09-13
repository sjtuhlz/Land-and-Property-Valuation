// use data RIVERSIDE only, to test different estimation methods for Yizhen Gu
//surprisingly, in this subsample, the rsq_mz1 and rsq_mz2 are different!!!!!!

set more off
capture log close
log using dec_18, replace
clear
clear matrix
set mem 1g

/*
insheet using "D:\summer, 2011\Yizhen\From Yizhen\RIV_newfloorarea_test3_Dec172011.csv", clear
rename new_impsqft_test3 new_impsqft_test
rename scagxyid scag_xyid  //scagxyid in this file is numeric, and scag_xyid in parcel_rv_residential_merged.dta is also numeric~~~
duplicates tag scag, gen(dup)
drop if dup != 0 
save yizhen_RIV_dec17.dta, replace


use yizhen_rv.dta, clear
rename scagxyid scag_xyid
duplicates tag scag, gen(dup)
drop if dup != 0 
save yizhen_rv.dta, replace


use parcel_rv_residential_merged.dta, clear  //RIVERSIDE ONLY
keep if (mz >= 81 & mz <=95)  
replace lu_08 = lu08 if missing(lu_08)  
keep if lu_08 >= 1120 & lu_08 < 1200
drop if missing(x) 
merge m:1 scag_xyid using yizhen_RIV_dec17.dta, keepusing(new_impsqft_test) force
drop if missing(x) 
capture drop _merge*
merge m:1 scag_xyid using yizhen_rv.dta, keepusing(new_impsqft) force
drop if missing(x) 
save riv_final_multi.dta, replace

*/

use riv_final_multi.dta, clear

sum impsqft new_impsqft new_impsqft_test

//===================== begin Multi ===========================
//data processing
replace sale_price = . if sale_price == 0 
replace impsqft = . if impsqft == 0
replace new_impsqft = . if new_impsqft == 0
replace new_impsqft_test = . if new_impsqft_test == 0
replace saleyr = . if saleyr == 1899 | saleyr == 0 | saleyr == 1900
gen saleyr_bk = saleyr


//sample10, by (mz)

rename impsqft floor_area1
rename new_impsqft floor_area2
rename new_impsqft_test floor_area3
replace floor_area2 = floor_area1 if missing(floor_area2) 
replace floor_area3 = floor_area1 if missing(floor_area3) 
gen lgvsq1 = ln( sale_price / floor_area1)
gen lgvsq2 = ln( sale_price / floor_area2)
gen lgvsq3 = ln( sale_price / floor_area3)

gen pom = hom / (hom + hrm) //percentage of multi housing units that are owner occupied 
gen gamma = 0.6 // A-2 : gamma = Rrs / Ros
levelsof tractid, local(lv_tract)
gen zeta = 1.5 //adjust owners overreport their housing values
gen thita = 0.8 // 80% of floor area of multi-unit housing is rentable

encode city , gen(citynum)
xtset citynum

// ======================== 1st round, use orginal impsqft data ==============================
capture drop lgvsq
gen lgvsq = lgvsq1

capture drop lgvsq2000

capture drop floor_area
gen floor_area = floor_area1

gen vm =.

gen ros = rrs /gamma
gen vrm = . 
gen rom = .
gen rvm = .

gen v2000 = .

gen lgvsq2000 = .



//======== regression (Riverside) ========
//regression and imputation using SALE_PRICE
xtreg lgvsq  fsub cbd fwy ocean  i.saleyr i.lu_08 if !missing(saleyr), fe
levelsof saleyr if e(sample), local(lvsaleyr)
foreach lv of local lvsaleyr {
    capture replace lgvsq2000 =  lgvsq  - _b[`lv'.saleyr] + _b[2000.saleyr] if saleyr == `lv'  & e(sample)
}
replace saleyr = 2000 if e(sample) == 0 
predict lgvsq_p 
replace lgvsq2000 = lgvsq_p if e(sample) == 0 
drop lgvsq_p
replace saleyr = saleyr_bk
replace v2000 = exp(lgvsq2000) * floor_area
//================ rent to value  ratioo  ===================

foreach lv of local lv_tract {
    sum v2000 if tractid == `lv', meanonly
    replace vm = r(mean) if tractid == `lv'
}
replace vrm = (zeta * vm - vom * pom) / (1 - pom)  
replace rom = vom * ros / vos 
replace rvm = (rom * pom + rrm * (1 - pom)) / (vom * pom + vrm * (1 - pom))  
gen r2000 = v2000 * rvm * 12  //rents in Census is montly!

   
//=============aggregate to model zone level =================

gen tsample_v = !missing(floor_area) &  !missing(v2000)  
gen tsample_r = !missing(floor_area) &  !missing(r2000)  
bysort mz: egen v_mz = total(v2000) if tsample_v
bysort mz: egen r_mz = total(r2000) if tsample_r
bysort mz: egen fa_mz_v = total(floor_area) if tsample_v
bysort mz: egen fa_mz_r = total(floor_area) if tsample_r
gen vsq_mz = v_mz / fa_mz_v
gen rsq_mz = r_mz / fa_mz_r
replace rsq_mz = rsq_mz / thita
drop tsample_v tsample_r v_mz r_mz fa_mz_v fa_mz_r 
rename vsq_mz vsq_mz1 
rename rsq_mz rsq_mz1 

sum 

// ============================ 2nd round, use updated floor area data from Yizhen Gu =======
capture drop lgvsq

gen lgvsq = lgvsq2

capture drop lgvsq2000

capture drop floor_area
gen floor_area = floor_area2

capture drop vm ros vrm rom rvm 
capture drop v2000 r2000

gen vm =.
gen ros = rrs /gamma
gen vrm = . 
gen rom = .
gen rvm = .
gen v2000 = .
gen r2000 = .

gen lgvsq2000 = .


//regression and imputation using SALE_PRICE
xtreg lgvsq  fsub cbd fwy ocean  i.saleyr i.lu_08 if !missing(saleyr), fe
levelsof saleyr if e(sample), local(lvsaleyr)
foreach lv of local lvsaleyr {
    capture replace lgvsq2000 =  lgvsq  - _b[`lv'.saleyr] + _b[2000.saleyr] if saleyr == `lv'  & e(sample)
}
replace saleyr = 2000 if e(sample) == 0 
predict lgvsq_p 
replace lgvsq2000 = lgvsq_p if e(sample) == 0 
drop lgvsq_p
replace v2000 = exp(lgvsq2000) * floor_area


//=================== rent to value  ratioo  ================


foreach lv of local lv_tract {
    sum v2000 if tractid == `lv', meanonly
    replace vm = r(mean) if tractid == `lv'
}
replace vrm = (zeta * vm - vom * pom) / (1 - pom)  
replace rom = vom * ros / vos 
replace rvm = (rom * pom + rrm * (1 - pom)) / (vom * pom + vrm * (1 - pom))  
replace r2000 = v2000 * rvm * 12  //rents in Census is montly!

   
//=======================aggregate to model zone level ================

gen tsample_v = !missing(floor_area) &  !missing(v2000)  
gen tsample_r = !missing(floor_area) &  !missing(r2000)  
bysort mz: egen v_mz = total(v2000) if tsample_v
bysort mz: egen r_mz = total(r2000) if tsample_r
bysort mz: egen fa_mz_v = total(floor_area) if tsample_v
bysort mz: egen fa_mz_r = total(floor_area) if tsample_r
gen vsq_mz = v_mz / fa_mz_v
gen rsq_mz = r_mz / fa_mz_r
replace rsq_mz = rsq_mz / thita
drop tsample_v tsample_r v_mz r_mz fa_mz_v fa_mz_r  
rename vsq_mz vsq_mz2 
rename rsq_mz rsq_mz2 

sum 
// ============================ 3rd round, use updated floor area in December data from Yizhen Gu =======
capture drop lgvsq

gen lgvsq = lgvsq3

capture drop lgvsq2000

capture drop floor_area
gen floor_area = floor_area3

capture drop vm ros vrm rom rvm 
capture drop v2000 r2000

gen vm =.
gen ros = rrs /gamma
gen vrm = . 
gen rom = .
gen rvm = .
gen v2000 = .
gen r2000 = .

gen lgvsq2000 = .


//regression and imputation using SALE_PRICE
xtreg lgvsq  fsub cbd fwy ocean  i.saleyr i.lu_08 if !missing(saleyr), fe
levelsof saleyr if e(sample), local(lvsaleyr)
foreach lv of local lvsaleyr {
    capture replace lgvsq2000 =  lgvsq  - _b[`lv'.saleyr] + _b[2000.saleyr] if saleyr == `lv'  & e(sample)
}
replace saleyr = 2000 if e(sample) == 0 
predict lgvsq_p 
replace lgvsq2000 = lgvsq_p if e(sample) == 0 
drop lgvsq_p
replace v2000 = exp(lgvsq2000) * floor_area


//=================== rent to value  ratioo  ================


foreach lv of local lv_tract {
    sum v2000 if tractid == `lv', meanonly
    replace vm = r(mean) if tractid == `lv'
}
replace vrm = (zeta * vm - vom * pom) / (1 - pom)  
replace rom = vom * ros / vos 
replace rvm = (rom * pom + rrm * (1 - pom)) / (vom * pom + vrm * (1 - pom))  
replace r2000 = v2000 * rvm * 12  //rents in Census is montly!

   
//=======================aggregate to model zone level ================

gen tsample_v = !missing(floor_area) &  !missing(v2000)  
gen tsample_r = !missing(floor_area) &  !missing(r2000)  
bysort mz: egen v_mz = total(v2000) if tsample_v
bysort mz: egen r_mz = total(r2000) if tsample_r
bysort mz: egen fa_mz_v = total(floor_area) if tsample_v
bysort mz: egen fa_mz_r = total(floor_area) if tsample_r
gen vsq_mz = v_mz / fa_mz_v
gen rsq_mz = r_mz / fa_mz_r
replace rsq_mz = rsq_mz / thita
drop tsample_v tsample_r v_mz r_mz fa_mz_v fa_mz_r  
rename vsq_mz vsq_mz3 
rename rsq_mz rsq_mz3

sum 
keep if !missing(vsq_mz1) & !missing(vsq_mz2) & !missing(vsq_mz3)
duplicates drop vsq_mz1 vsq_mz2 vsq_mz3, force
outsheet  vsq_mz1 vsq_mz2  vsq_mz3 rsq_mz1 rsq_mz2 rsq_mz3 mz name using mz_multi_dec18_riverside.csv, comma replace

