// use updated regression framework in Richard's PPT 
//lnv = f(lnF,lnL, accessibility vars, saleyr dummies)
// single family residential only

scalar drop _all
log close _all
log using nov_28_single, replace
clear 
clear matrix
set more off
set mem 1G
cd "D:\summer, 2011\Third Project\Final_data"
/*
use residential_six_final.dta, clear

replace saleyr = . if saleyr == 0 | saleyr == 1899
gen lgvsq = ln(sale_price / floor_area)
gen lgvsq2000 = lgvsq  
drop if missing(mz)   //usually missing mz has missing scagxyid
gen single = lu_08 >= 1110 & lu_08 < 1120
gen multi = lu_08 >= 1120 & lu_08 < 1200

preserve 
drop if multi == 0
save residential_six_final_multi.dta, replace
restore
drop if single == 0
save residential_six_final_single.dta, replace
*/
use residential_six_final_single.dta, clear
//===================== begin Single ===========================
drop if mz ==0
gen random = runiform()
keep if random > 0.99

replace sale_price = . if sale_price == 0
replace lotsqft = . if lotsqft == 0
gen lnv = ln(sale_price)
gen lnf = ln(floor_area)
gen lnl = ln(lotsqft)
gen lnv2000 = lnv

encode city , gen(citynum)
xtset citynum


//===================================== regression (Orange & Riverside)======================

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

gen pos = hos / (hos + hrs)  //percentage of single housing units that are owner occupied 
gen vs =.
gen gamma = 0.6 // A-2 : gamma = Rrs / Ros
gen ros = rrs /gamma


levelsof tractid, local(lv_tract)
foreach lv of local lv_tract {
    sum v2000 if tractid == `lv', meanonly
    replace vs = r(mean) if tractid == `lv'
}

gen zeta = 1.5 //adjust owners overreport their housing values
gen vrs = (zeta * vs - vos * pos) / (1 - pos)  
gen rvs = (ros * pos + rrs * (1 - pos)) / (vos * pos + vrs * (1 - pos))  

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
gen vsq_mz = v_mz / fa_mz_v 
gen rsq_mz = r_mz / fa_mz_r 
drop tsample_v tsample_r v_mz r_mz fa_mz_v fa_mz_r


/*
gen vsq_mz_final = .
gen rsq_mz_final = .
forvalues i = 1(1) 95 {
    sum vsq_mz if mz == `i', meanonly
    replace vsq_mz_final = r(mean) if mz == `i'
sum rsq_mz_ if mz == `i', meanonly
    replace rsq_mz_final = r(mean) if mz == `i'
}
*/

save nov28_five_residential_single.dta, replace

duplicates drop vsq_mz, force
//esttab * using Oct10, b p obslast label mtitles rtf replace
outsheet  vsq_mz rsq_mz mz name using mz_residential_single_nov28.csv, comma replace

clear
use nov28_five_residential_single.dta

/*
levelsof tractid, local(lvtractid)
foreach lv of local lvtractid {
    count if tractid == `lv'
    replace s_scag = r(N)
}
*/