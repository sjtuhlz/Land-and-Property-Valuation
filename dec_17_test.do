//apply a new idea of calculating sales price from totvalue07
// PROBLEM UNSOLVED: results under two different sets of floor area are exactly the same!!!!!!!!!!!

set more off
capture log close
log using dec_18, replace
clear
clear matrix
set mem 1g

/*
use residential_six_final_multi.dta, clear
capture drop _merge*
merge m:m scag_xyid using yizhen_newimpsqft.dta, keepusing(new_impsqft) force
save residential_six_final_multi.dta, replace
*/
// process Yizhen's updated floor area
/*
use yizhen_rv.dta, clear
keep scagxyid new_impsqft lu08
append using yizhen_or.dta yizhen_sb.dta yizhen_vt.dta, keep(scagxyid new_impsqft lu08)
gen scag_xyid = string(scagxyid, "%14.0f")
drop scagxyid
duplicates tag scag, gen(dup)
drop if dup != 0
save yizhen_newimpsqft.dta, replace
*/

use residential_six_final.dta, clear
replace lu_08 = lu08 if missing(lu_08)  
keep if lu_08 >= 1120 & lu_08 < 1200
drop if missing(x) 

merge m:1 scag_xyid using yizhen_newimpsqft.dta, keepusing(new_impsqft) force
drop if missing(x) 
save residential_six_final_multi.dta, replace

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
drop if mz ==0 | missing(mz)
/*
gen random = runiform()
keep if random < 0.1
*/

sample 1, by (mz)

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
//=================== regression (Orange )======================
gen insample = (mz >= 50 & mz <=66) 

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

/*         
//XO part begin
gen xo_sample = !missing(sale_price) & !missing(saleyr) & !missing(totvalue07) & insample
gen sp = sale_price if xo_sample
gen dd = 0 if xo_sample
foreach lv of local lvsaleyr {
    capture replace dd = _b[`lv'.saleyr] - _b[2000.saleyr] if saleyr == `lv' & id_keep == 1
}
replace sp = sp / exp(dd)
egen tsp = total(sp) if xo_sample
egen tv = total(totvalue07) if xo_sample
foreach var of varlist tsp tv {
    summarize `var', meanonly
    scalar `var' = r(mean)	
}
scalar xo_residential = scalar(tsp) / scalar(tv)
drop xo_sample sp dd tsp tv 
scalar drop tsp tv
//XO part end           
*/

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


//compare
replace v2000 = exp(lgvsq2000) * floor_area if insample
replace v2000_tot = exp(lgvsq2000_tot) * floor_area if insample
egen tp = total(v2000) if insample
egen tv = total(v2000_tot) if insample
gen vp = tp / tv
sum vp, meanonly
scalar xo_residential = r(mean)
drop tp tv vp 

//========== LA, VT ===========
replace insample = (mz >= 1 & mz <=46) | (mz >= 47 & mz <=49)  

xtreg lgvsq_tot fsub cbd fwy ocean i.saleyr i.lu_08 if insample & !missing(saleyr), fe
levelsof saleyr if e(sample), local(lvsaleyr)
foreach lv of local lvsaleyr {
        capture replace  lgvsq2000_tot =  lgvsq_tot - _b[`lv'.saleyr] + _b[2000.saleyr] if saleyr == `lv'  & e(sample)
    }
replace saleyr = 2000 if e(sample)==0  & insample
predict lgvsq_p if insample
replace lgvsq2000_tot = lgvsq_p if e(sample) == 0 & insample
drop  lgvsq_p

replace v2000 = exp(lgvsq2000_tot) * floor_area / scalar(xo_residential) if insample

//========================================= regression (Riverside) =============================
replace insample = (mz >= 81 & mz <=95)  
//regression and imputation using SALE_PRICE
xtreg lgvsq  fsub cbd fwy ocean  i.saleyr i.lu_08 if insample & !missing(saleyr), fe
levelsof saleyr if e(sample), local(lvsaleyr)
foreach lv of local lvsaleyr {
    capture replace lgvsq2000 =  lgvsq  - _b[`lv'.saleyr] + _b[2000.saleyr] if saleyr == `lv'  & e(sample)
}
replace saleyr = 2000 if e(sample) == 0 & insample
predict lgvsq_p if insample
replace lgvsq2000 = lgvsq_p if e(sample) == 0 & insample
drop lgvsq_p
replace saleyr = saleyr_bk
/*
//XR part begin
gen xr_sample = !missing(sale_price) & !missing(saleyr) & !missing(totvalue07) & insample
gen sp = sale_price if xr_sample
gen dd = 0 if xr_sample
foreach lv of local lvsaleyr {
    capture replace dd = _b[`lv'.saleyr] - _b[2000.saleyr] if saleyr == `lv' & id_keep == 1
}
replace sp = sp / exp(dd)
egen tsp = total(sp) if xr_sample
egen tv = total(totvalue07) if xr_sample
foreach var of varlist tsp tv {
    summarize `var', meanonly
    scalar `var' = r(mean)	
}
scalar xr_residential = scalar(tsp) / scalar(tv)
drop xr_sample sp dd tsp tv 
scalar drop tsp tv
//XR part end
*/

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


//compare
replace v2000 = exp(lgvsq2000) * floor_area if insample
replace v2000_tot = exp(lgvsq2000_tot) * floor_area if insample
egen tp = total(v2000) if insample
egen tv = total(v2000_tot) if insample
gen vp = tp / tv
sum vp, meanonly
scalar xr_residential = r(mean)
drop tp tv vp 

//==============SB=================
replace insample = (mz >= 67 & mz <=80)  

xtreg lgvsq_tot fsub cbd fwy ocean i.saleyr i.lu_08 if insample & !missing(saleyr), fe
levelsof saleyr if e(sample), local(lvsaleyr)
foreach lv of local lvsaleyr {
        capture replace  lgvsq2000_tot =  lgvsq_tot - _b[`lv'.saleyr] + _b[2000.saleyr] if saleyr == `lv'  & e(sample)
    }
replace saleyr = 2000 if e(sample)==0  & insample
predict lgvsq_p if insample
replace lgvsq2000_tot = lgvsq_p if e(sample) == 0 & insample
drop  lgvsq_p
replace v2000 = exp(lgvsq2000_tot) * floor_area / scalar(xo_residential) if insample
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

//duplicates drop vsq_mz, force
//outsheet  vsq_mz rsq_mz mz name using mz_residential_multi_dec18_1.csv, comma replace

//use dec18_five_residential_multi_1.dta, clear
rename vsq_mz vsq_mz1 
rename rsq_mz rsq_mz1 

sum 

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
//=================== regression (Orange )======================
replace insample = (mz >= 50 & mz <=66) 

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

/*
//XO part begin
gen xo_sample = !missing(sale_price) & !missing(saleyr) & !missing(totvalue07) & insample
gen sp = sale_price if xo_sample
gen dd = 0 if xo_sample
foreach lv of local lvsaleyr {
    capture replace dd = _b[`lv'.saleyr] - _b[2000.saleyr] if saleyr == `lv' & id_keep == 1
}
replace sp = sp / exp(dd)
egen tsp = total(sp) if xo_sample
egen tv = total(totvalue07) if xo_sample
foreach var of varlist tsp tv {
    summarize `var', meanonly
    scalar `var' = r(mean)	
}
scalar xo_residential = scalar(tsp) / scalar(tv)
drop xo_sample sp dd tsp tv 
scalar drop tsp tv
//XO part end 
*/

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


//compare
replace v2000 = exp(lgvsq2000) * floor_area if insample
replace v2000_tot = exp(lgvsq2000_tot) * floor_area if insample
egen tp = total(v2000) if insample
egen tv = total(v2000_tot) if insample
gen vp = tp / tv
sum vp, meanonly
scalar xo_residential = r(mean)
drop tp tv vp 

//========== LA, VT ===========
replace insample = (mz >= 1 & mz <=46) | (mz >= 47 & mz <=49)  

xtreg lgvsq_tot fsub cbd fwy ocean i.saleyr i.lu_08 if insample & !missing(saleyr), fe
levelsof saleyr if e(sample), local(lvsaleyr)
foreach lv of local lvsaleyr {
        capture replace  lgvsq2000_tot =  lgvsq_tot - _b[`lv'.saleyr] + _b[2000.saleyr] if saleyr == `lv'  & e(sample)
    }
replace saleyr = 2000 if e(sample)==0  & insample
predict lgvsq_p if insample
replace lgvsq2000_tot = lgvsq_p if e(sample) == 0 & insample
drop  lgvsq_p

replace v2000 = exp(lgvsq2000_tot) * floor_area / scalar(xo_residential) if insample

//========================================= regression (Riverside) =============================
replace insample = (mz >= 81 & mz <=95)  
//regression and imputation using SALE_PRICE
xtreg lgvsq  fsub cbd fwy ocean  i.saleyr i.lu_08 if insample & !missing(saleyr), fe
levelsof saleyr if e(sample), local(lvsaleyr)
foreach lv of local lvsaleyr {
    capture replace lgvsq2000 =  lgvsq  - _b[`lv'.saleyr] + _b[2000.saleyr] if saleyr == `lv'  & e(sample)
}
replace saleyr = 2000 if e(sample) == 0 & insample
predict lgvsq_p if insample
replace lgvsq2000 = lgvsq_p if e(sample) == 0 & insample
drop lgvsq_p
replace saleyr = saleyr_bk
/*
//XR part begin
gen xr_sample = !missing(sale_price) & !missing(saleyr) & !missing(totvalue07) & insample
gen sp = sale_price if xr_sample
gen dd = 0 if xr_sample
foreach lv of local lvsaleyr {
    capture replace dd = _b[`lv'.saleyr] - _b[2000.saleyr] if saleyr == `lv' & id_keep == 1
}
replace sp = sp / exp(dd)
egen tsp = total(sp) if xr_sample
egen tv = total(totvalue07) if xr_sample
foreach var of varlist tsp tv {
    summarize `var', meanonly
    scalar `var' = r(mean)	
}
scalar xr_residential = scalar(tsp) / scalar(tv)
drop xr_sample sp dd tsp tv 
scalar drop tsp tv
//XR part end
*/

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


//compare
replace v2000 = exp(lgvsq2000) * floor_area if insample
replace v2000_tot = exp(lgvsq2000_tot) * floor_area if insample
egen tp = total(v2000) if insample
egen tv = total(v2000_tot) if insample
gen vp = tp / tv
sum vp, meanonly
scalar xr_residential = r(mean)
drop tp tv vp 

//==============SB=================
replace insample = (mz >= 67 & mz <=80)  

xtreg lgvsq_tot fsub cbd fwy ocean i.saleyr i.lu_08 if insample & !missing(saleyr), fe
levelsof saleyr if e(sample), local(lvsaleyr)
foreach lv of local lvsaleyr {
        capture replace  lgvsq2000_tot =  lgvsq_tot - _b[`lv'.saleyr] + _b[2000.saleyr] if saleyr == `lv'  & e(sample)
    }
replace saleyr = 2000 if e(sample)==0  & insample
predict lgvsq_p if insample
replace lgvsq2000_tot = lgvsq_p if e(sample) == 0 & insample
drop  lgvsq_p
replace v2000 = exp(lgvsq2000_tot) * floor_area / scalar(xo_residential) if insample
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
drop tsample_v tsample_r v_mz r_mz fa_mz_v fa_mz_r insample
rename vsq_mz vsq_mz2 
rename rsq_mz rsq_mz2 

sum 
save dec18_five_residential_multi.dta, replace

duplicates drop vsq_mz1 vsq_mz2, force
outsheet  vsq_mz1 vsq_mz2 rsq_mz1 rsq_mz2 mz name using mz_residential_multi_dec18.csv, comma replace

