//regress LA, Orange and Riverside together, regress VT,SB together
// an updated version of dec_17_test_1.do
//justification: totvalue07 has some values like 7, 101, so maybe we could use sale_value for LA county

set more off
capture log close
log using dec_17_test1, replace
clear
clear matrix
set mem 1g

 use residential_six_final_multi.dta, clear

sum impsqft new_impsqft

//===================== begin Multi ===========================
//data processing
replace sale_price = . if sale_price == 0 
replace impsqft = . if impsqft == 0
replace new_impsqft = . if new_impsqft == 0
replace totvalue07 = . if totvalue07 == 0
replace saleyr = . if saleyr == 1899 | saleyr == 0 | saleyr == 1900
gen saleyr_bk = saleyr

//reduce computation by taking a random sample
drop if mz ==0
/*
gen random = runiform()
keep if random < 0.1
*/

sample 10, by (mz)

rename impsqft floor_area1
rename new_impsqft floor_area2
replace floor_area2 = floor_area1 if missing(floor_area2) 
gen lgvsq1 = ln( sale_price / floor_area1)
gen lgvsq2 = ln( sale_price / floor_area2)
gen lgvsq_tot_1 = ln( totvalue07 / floor_area1)
gen lgvsq_tot_2 = ln( totvalue07 / floor_area2)


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
gen lgvsq_tot = lgvsq_tot_1
capture drop lgvsq2000
capture drop lgvsq2000_tot
capture drop floor_area
gen floor_area = floor_area1

gen vm =.

gen ros = rrs /gamma
gen vrm = . 
gen rom = .
gen rvm = .

gen v2000 = .
gen v2000_tot = .
gen lgvsq2000 = .
gen lgvsq2000_tot = .
//=================== regression (LA & Orange & Riverside)======================
gen insample = (mz >= 50 & mz <=66) | (mz >= 81 & mz <=95)  | (mz >= 1 & mz <=46) 

//regression and imputation using SALE_PRICE
xtreg lgvsq  fsub cbd fwy ocean  i.saleyr  i.lu_08 if insample & !missing(saleyr), fe
levelsof saleyr if e(sample), local(lvsaleyr)
foreach lv of local lvsaleyr {
    capture replace  lgvsq2000 =  lgvsq - _b[`lv'.saleyr] + _b[2000.saleyr] if saleyr == `lv'  & e(sample)
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


//compare by Riverside and Orange, two scalars
//----------scalar(xo_residential)
replace insample = (mz >= 50 & mz <=66)
replace v2000 = exp(lgvsq2000) * floor_area if insample
replace v2000_tot = exp(lgvsq2000_tot) * floor_area if insample
egen tp = total(v2000) if insample
egen tv = total(v2000_tot) if insample
gen vp = tp / tv
sum vp, meanonly
scalar xo_residential = r(mean)
drop tp tv vp 
//---------scalar(xr_residential)
replace insample = (mz >= 81 & mz <=95)  
replace v2000 = exp(lgvsq2000) * floor_area if insample
replace v2000_tot = exp(lgvsq2000_tot) * floor_area if insample
egen tp = total(v2000) if insample
egen tv = total(v2000_tot) if insample
gen vp = tp / tv
sum vp, meanonly
scalar xr_residential = r(mean)
drop tp tv vp 

//==========  VT, SB ===========
replace insample =  (mz >= 47 & mz <=49) | (mz >= 67 & mz <=80)   

xtreg lgvsq_tot fsub cbd fwy ocean i.saleyr i.lu_08 if insample & !missing(saleyr), fe
levelsof saleyr if e(sample), local(lvsaleyr)
foreach lv of local lvsaleyr {
        capture replace  lgvsq2000_tot =  lgvsq_tot - _b[`lv'.saleyr] + _b[2000.saleyr] if saleyr == `lv'  & e(sample)
    }
replace saleyr = 2000 if e(sample)==0  & insample
predict lgvsq_p if insample
replace lgvsq2000_tot = lgvsq_p if e(sample) == 0 & insample
drop  lgvsq_p

replace insample = (mz >= 1 & mz <=46) | (mz >= 47 & mz <=49) 
replace v2000 = exp(lgvsq2000_tot) * floor_area / scalar(xo_residential) if insample
replace insample = (mz >= 67 & mz <=80)   
replace v2000 = exp(lgvsq2000_tot) * floor_area / scalar(xr_residential) if insample

//================================================= rent to value  ratioo  ==========================


foreach lv of local lv_tract {
    sum v2000 if tractid == `lv', meanonly
    replace vm = r(mean) if tractid == `lv'
}
replace vrm = (zeta * vm - vom * pom) / (1 - pom)  
replace rom = vom * ros / vos 
replace rvm = (rom * pom + rrm * (1 - pom)) / (vom * pom + vrm * (1 - pom))  
gen r2000 = v2000 * rvm * 12  //rents in Census is montly!

   
//======================================aggregate to model zone level ====================================

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
save dec18_five_residential_multi.dta, replace

rename vsq_mz vsq_mz1 
rename rsq_mz rsq_mz1 

// ======================== 2nd round, use updated floor area data from Yizhen Gu ==============================
capture drop lgvsq
capture drop lgvsq_tot
gen lgvsq = lgvsq2
gen lgvsq_tot = lgvsq_tot_2
capture drop lgvsq2000
capture drop lgvsq2000_tot
capture drop floor_area
gen floor_area = floor_area2

capture drop vm ros vrm rom rvm 
capture drop v2000 r2000 v2000_tot 

gen vm =.
gen ros = rrs /gamma
gen vrm = . 
gen rom = .
gen rvm = .
gen v2000 = .
gen r2000 = .
gen v2000_tot = .
gen lgvsq2000 = .
gen lgvsq2000_tot = .

//=================== regression (Orange & Riverside)======================
replace insample = (mz >= 50 & mz <=66) | (mz >= 81 & mz <=95)  

//regression and imputation using SALE_PRICE
xtreg lgvsq  fsub cbd fwy ocean  i.saleyr  i.lu_08 if insample & !missing(saleyr), fe
levelsof saleyr if e(sample), local(lvsaleyr)
foreach lv of local lvsaleyr {
    capture replace  lgvsq2000 =  lgvsq - _b[`lv'.saleyr] + _b[2000.saleyr] if saleyr == `lv'  & e(sample)
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


//compare by Riverside and Orange, two scalars
//----------scalar(xo_residential)
replace insample = (mz >= 50 & mz <=66)
replace v2000 = exp(lgvsq2000) * floor_area if insample
replace v2000_tot = exp(lgvsq2000_tot) * floor_area if insample
egen tp = total(v2000) if insample
egen tv = total(v2000_tot) if insample
gen vp = tp / tv
sum vp, meanonly
scalar xo_residential = r(mean)
drop tp tv vp 
//---------scalar(xr_residential)
replace insample = (mz >= 81 & mz <=95)  
replace v2000 = exp(lgvsq2000) * floor_area if insample
replace v2000_tot = exp(lgvsq2000_tot) * floor_area if insample
egen tp = total(v2000) if insample
egen tv = total(v2000_tot) if insample
gen vp = tp / tv
sum vp, meanonly
scalar xr_residential = r(mean)
drop tp tv vp 

//========== LA, VT, SB ===========
replace insample = (mz >= 1 & mz <=46) | (mz >= 47 & mz <=49) | (mz >= 67 & mz <=80)   

xtreg lgvsq_tot fsub cbd fwy ocean i.saleyr i.lu_08 if insample & !missing(saleyr), fe
levelsof saleyr if e(sample), local(lvsaleyr)
foreach lv of local lvsaleyr {
        capture replace  lgvsq2000_tot =  lgvsq_tot - _b[`lv'.saleyr] + _b[2000.saleyr] if saleyr == `lv'  & e(sample)
    }
replace saleyr = 2000 if e(sample)==0  & insample
predict lgvsq_p if insample
replace lgvsq2000_tot = lgvsq_p if e(sample) == 0 & insample
drop  lgvsq_p

replace insample = (mz >= 1 & mz <=46) | (mz >= 47 & mz <=49) 
replace v2000 = exp(lgvsq2000_tot) * floor_area / scalar(xo_residential) if insample
replace insample = (mz >= 67 & mz <=80)   
replace v2000 = exp(lgvsq2000_tot) * floor_area / scalar(xr_residential) if insample

//================================================= rent to value  ratioo  ==========================


foreach lv of local lv_tract {
    sum v2000 if tractid == `lv', meanonly
    replace vm = r(mean) if tractid == `lv'
}
replace vrm = (zeta * vm - vom * pom) / (1 - pom)  
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
replace rsq_mz = rsq_mz / thita
drop tsample_v tsample_r v_mz r_mz fa_mz_v fa_mz_r 
save dec18_five_residential_multi.dta, replace

rename vsq_mz vsq_mz2 
rename rsq_mz rsq_mz2 

save dec18_five_residential_multi.dta, replace

duplicates drop vsq_mz1 vsq_mz2, force
outsheet  vsq_mz1 vsq_mz2 rsq_mz1 rsq_mz2 mz name using mz_multi_dec20.csv, comma replace

