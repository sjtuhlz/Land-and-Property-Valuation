//compare the vsq_mz results using Yizhen's imputed floor area and orginal floor area data.

//==============remember to use shape_area instead of lotsqft as lot size!!!!

set more off
capture log close
log using dec_17, replace
cd "D:\summer, 2011\Third Project\Final_data"
clear 
set mem 1G
use residential_six_final_single.dta, clear
capture drop _merge*
rename scag_xyid scagxyid 
merge m:m scagxyid using yizhen_newimpsqft.dta, keepusing(new_impsqft) force
save residential_six_final_single.dta, replace

//===================== begin Single ===========================
drop if mz ==0
gen random = runiform()
keep if random > 0.99


capture replace new_impsqft = . if new_impsqft == 0
rename floor_area floor_area1
rename new_impsqft floor_area2
replace floor_area2 = floor_area1 if missing(floor_area2) 

replace sale_price = . if sale_price == 0
replace lotsqft = . if lotsqft == 0
gen lnv = ln(sale_price)
gen lnl = ln(lotsqft)
gen lnf = .
gen lnv2000 = .

gen saleyr_bk = saleyr

gen pos = hos / (hos + hrs)  //percentage of single housing units that are owner occupied 
gen vs =.
gen gamma = 0.6 // A-2 : gamma = Rrs / Ros
gen ros = rrs /gamma
gen zeta = 1.5 //adjust owners overreport their housing values
gen vrs = .
gen rvs = .


encode city , gen(citynum)
xtset citynum


forevalues i = 1/2 {

replace floor_area = floor_area`i'
replace lnf = ln(floor_area)
replace saleyr = saleyr_bk
replace lnv2000 = .

//===================================== regression (Orange)======================

gen insample = (mz >= 50 & mz <=66) 

xtreg lnv lnf lnl fsub cbd fwy ocean i.saleyr i.lu_08 if insample & !missing(saleyr)
estimates store residential_ora

gen id_keep = e(sample)
gen xo_sample = !missing(sale_price) & !missing(saleyr) & !missing(totvalue07) & insample

//XO part begin
gen sp = sale_price if xo_sample
gen dd = 0 if xo_sample
levelsof saleyr if e(sample), local(lvsaleyr)
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


foreach lv of local lvsaleyr {
    capture replace lnv2000 =  lnv - _b[`lv'.saleyr] + _b[2000.saleyr] if saleyr == `lv'  & id_keep == 1 
}
replace saleyr = 2000 if id_keep == 0 & insample
predict lnv_p if insample
replace lnv2000 = lnv_p if id_keep == 0 & insample
drop  lnv_p  

//========== LA, VT ===========
replace insample = (mz >= 1 & mz <=46) | (mz >= 47 & mz <=49)  
replace lnv2000 = ln(totvalue07 * scalar(xo_residential)) if insample

xtreg lnv2000  lnf lnl fsub cbd fwy ocean i.lu_08 if insample, fe
estimates store residential_la_vt
predict lnv_p if insample
replace lnv2000 = lnv_p if insample & missing(totvalue07)
drop lnv_p 



//========================================= regression (Riverside) =============================
replace insample = (mz >= 81 & mz <=95)  

xtreg lnv lnf lnl fsub cbd fwy ocean  i.saleyr i.lu_08 if insample & !missing(saleyr), fe
estimates store residential_riv

replace id_keep = e(sample)
gen xr_sample = !missing(sale_price) & !missing(saleyr) & !missing(totvalue07) & insample

//XR part begin
gen sp = sale_price if xr_sample
gen dd = 0 if xr_sample

levelsof saleyr if e(sample), local(lvsaleyr)
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


foreach lv of local lvsaleyr {
    capture replace lnv2000 =  lnv - _b[`lv'.saleyr] + _b[2000.saleyr] if saleyr == `lv'  & id_keep == 1 
}

replace saleyr = 2000 if id_keep == 0 & insample
predict lnv_p if insample
replace lnv2000 = lnv_p if id_keep == 0 & insample

drop lnv_p

//==============SB=================
replace insample = (mz >= 67 & mz <=80)  
replace lnv2000 = ln(totvalue07 * scalar(xr_residential))  if insample

xtreg lnv2000  lnf lnl fsub cbd fwy ocean i.lu_08 if insample, fe
estimates store residential_sb 
predict lnv_p if insample
replace lnv2000 = lnv_p if insample & missing(totvalue07)
drop  lnv_p



//================================================= rent to value  ratioo  ==========================

gen v2000 = exp(lnv2000)  

levelsof tractid, local(lv_tract)
foreach lv of local lv_tract {
    sum v2000 if tractid == `lv', meanonly
    replace vs = r(mean) if tractid == `lv'
}

replace vrs = (zeta * vs - vos * pos) / (1 - pos)  
replace rvs = (ros * pos + rrs * (1 - pos)) / (vos * pos + vrs * (1 - pos))  

gen r2000 = .
//rents in Census is montly!
replace r2000 = v2000 * rvs * 12


   
//======================================aggregate to model zone level ====================================

gen tsample_v = !missing(floor_area) &  !missing(v2000)  
gen tsample_r = !missing(floor_area) &  !missing(r2000)  
bysort mz: egen v_mz = total(v2000) if tsample_v
bysort mz: egen r_mz = total(r2000) if tsample_r
bysort mz: egen fa_mz_v = total(floor_area) if tsample_v
bysort mz: egen fa_mz_r = total(floor_area) if tsample_r
gen vsq_mz`i' = v_mz / fa_mz_v 
gen rsq_mz`i' = r_mz / fa_mz_r 
drop tsample_v tsample_r v_mz r_mz fa_mz_v fa_mz_r

}

save dec16_residential_single.dta, replace

duplicates drop vsq_mz1 vsq_mz2 , force
//esttab * using Oct10, b p obslast label mtitles rtf replace
outsheet  vsq_mz rsq_mz mz name using mz_residential_single_dec17.csv, comma replace
