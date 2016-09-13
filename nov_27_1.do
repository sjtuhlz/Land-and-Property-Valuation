scalar drop _all
log close _all
log using nov_27_six, replace
clear 
clear matrix
set more off
set mem 1G
cd "D:\summer, 2011\Third Project\Final_data"

/*
use parcel_la_residential_merged.dta, clear
append using parcel_vt_residential_merged.dta, force
append using parcel_or_residential_merged.dta, force
append using parcel_sb_residential_merged.dta, force
append using parcel_rv_residential_merged.dta, force
save residential_six_final.dta, replace
*/

use residential_six_final.dta, clear




replace saleyr = . if saleyr == 0 | saleyr == 1899
replace totvalue07 = . if totvalue07 == 0
gen lgvsq = ln(sale_price / floor_area)
gen lgvsq2000 = lgvsq  
drop if missing(mz)   //usually missing mz has missing scagxyid

gen random = runiform()
keep if random > 0.99

encode city /* if insample */, gen(citynum)
xtset citynum

//======================================================== regression (Orange)======================

gen insample = (mz >= 50 & mz <=66)  //| (mz == 0 & county == "or")





xtreg lgvsq  fsub cbd fwy ocean  i.saleyr  i.lu_08 if insample & !missing(saleyr), fe
estimates store residential_ora

gen id_keep = e(sample)
gen xo_sample = !missing(sale_price) & !missing(saleyr) & !missing(totvalue07) & insample

//XO part begin
//xo = sale_price / totvalue07 computed across all parcels in Orange county
//To impute saleprice from totval07 for LA & VT
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
    capture replace lgvsq2000 =  lgvsq - _b[`lv'.saleyr] + _b[2000.saleyr] if saleyr == `lv'  & id_keep == 1 
}

replace saleyr = 2000 if id_keep == 0 & insample
predict lgvsq_p if insample
replace lgvsq2000 = lgvsq_p if id_keep == 0 & insample

drop  lgvsq_p  citynum


//========== LA===========
//gen salevalue = sale_price if county == "la"
replace insample = (mz >= 1 & mz <=46)  //| (mz == 0 & county == "la")
//replace sale_price = totvalue07 * scalar(xo_residential) if insample
replace lgvsq2000 = ln((totvalue07 * scalar(xo_residential)) / floor_area) if insample

encode city /* if insample*/, gen(citynum)
xtset citynum
xtreg lgvsq2000  fsub cbd fwy ocean i.lu_08 if insample & !missing(saleyr), fe
estimates store residential_la_xo
predict lgvsq_p if insample
replace lgvsq2000 = lgvsq_p if insample & missing(totvalue07)
drop lgvsq_p citynum 

//try using salevalue for LA county
replace lgvsq = ln(sale_price / floor_area) if insample
encode city  if insample, gen(citynum)
xtset citynum
xtreg lgvsq  fsub cbd fwy ocean i.saleyr i.lu_08 if insample & !missing(saleyr), fe
estimates store residential_la

replace id_keep = !missing(lgvsq) & !missing(saleyr) & insample
gen lgvsq2000_la = . 
foreach lv of local lvsaleyr {
    capture replace lgvsq2000_la =  lgvsq - _b[`lv'.saleyr] + _b[2000.saleyr] if saleyr == `lv'  & id_keep == 1  
}
replace saleyr = 2000 if id_keep == 0 & insample
predict lgvsq_p if insample
replace lgvsq2000_la = lgvsq_p if id_keep == 0 & insample
drop lgvsq_p citynum

//==============VT=================
replace insample = (mz >= 47 & mz <=49)  //| (mz == 0 & county == "vt")
replace lgvsq2000 = ln((totvalue07 * scalar(xo_residential)) / floor_area) if insample
encode city /* if insample*/, gen(citynum)
xtset citynum
xtreg lgvsq2000  fsub cbd fwy ocean i.lu_08 if insample & !missing(saleyr), fe
estimates store residential_vt_xo
predict lgvsq_p if insample
replace lgvsq2000 = lgvsq_p if insample & missing(totvalue07)
drop citynum lgvsq_p

//========================================= regression (Riverside) =============================
replace insample = (mz >= 81 & mz <=95) // | (mz == 0 & county == "rv")

encode city /* if insample */, gen(citynum)
xtset citynum
xtreg lgvsq  fsub cbd fwy ocean  i.saleyr i.lu_08 if insample & !missing(saleyr), fe
estimates store residential_riv

replace id_keep = !missing(lgvsq) & !missing(saleyr) & insample
gen xr_sample = !missing(sale_price) & !missing(saleyr) & !missing(totvalue07) & insample

//XR part begin
//xr = sale_price / totvalue07 computed across all parcels in Riverside county
//To impute saleprice from totval07 for SB & IM
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
    capture replace lgvsq2000 =  lgvsq - _b[`lv'.saleyr] + _b[2000.saleyr] if saleyr == `lv'  & id_keep == 1 
}

replace saleyr = 2000 if id_keep == 0 & insample
predict lgvsq_p if insample
replace lgvsq2000 = lgvsq_p if id_keep == 0 & insample

drop citynum lgvsq_p

//==============SB=================
replace insample = (mz >= 67 & mz <=80)  //| (mz == 0 & county == "sb")
replace lgvsq2000 = ln((totvalue07 * scalar(xr_residential)) / floor_area) if insample
encode city  /*if insample */, gen(citynum)
xtset citynum
xtreg lgvsq2000  fsub cbd fwy ocean i.lu_08 if insample & !missing(saleyr), fe
estimates store residential_sb_xo
predict lgvsq_p if insample
replace lgvsq2000 = lgvsq_p if insample & missing(totvalue07)
drop lgvsq_p


//================================================= rent to value  ratioo  ==========================
gen single = lu_08 >= 1110 & lu_08 < 1120
gen multi = lu_08 >= 1120 & lu_08 < 1200
gen v2000 = exp(lgvsq2000) * floor_area

gen pos = hos / (hos + hrs)  //percentage of single housing units that are owner occupied 
gen pom = hom / (hom + hrm) //percentage of multi housing units that are owner occupied 
gen vs =.
gen vm = .
gen gamma = 0.6 // A-2 : gamma = Rrs / Ros
gen ros = rrs /gamma


levelsof tractid, local(lv_tract)
foreach lv of local lv_tract {
    sum v2000 if single == 1 & tractid == `lv', meanonly
    replace vs = r(mean) if tractid == `lv'
    
    sum v2000 if multi == 1 & tractid == `lv' , meanonly
    replace vm = r(mean) if tractid == `lv'
}


gen vrs = (vs - vos * pos) / (1 - pos)  
gen vrm = (vm - vom * pom) / (1 - pom)  
gen rom = vom * ros / vos
gen rvs = (ros * pos + rrs * (1 - pos)) / (vos * pos + vrs * (1 - pos))  
gen rvm = (rom * pom + rrm * (1 - pom)) / (vom * pom + vrm * (1 - pom))  


gen r2000 = .
replace r2000 = v2000 * rvs if single == 1
replace r2000 = v2000 * rvm if multi == 1
   
//======================================aggregate to model zone level ====================================
/*
gen tsample_v = !missing(floor_area) &  !missing(v2000)
gen tsample_r = !missing(floor_area) &  !missing(r2000)

bysort mz: egen v_mz = total(v2000) if tsample_v
bysort mz: egen r_mz = total(r2000) if tsample_r
bysort mz: egen fa_mz_v = total(floor_area) if tsample_v
bysort mz: egen fa_mz_r = total(floor_area) if tsample_r
gen vsq_mz = v_mz / fa_mz_v 
gen rsq_mz = r_mz / fa_mz_r 
*/
//differentiate btw single and multi
drop if mz == 0

foreach var of varlist single multi {
    gen tsample_v = !missing(floor_area) &  !missing(v2000) & `var' == 1
    gen tsample_r = !missing(floor_area) &  !missing(r2000) & `var' == 1
    bysort mz: egen v_mz = total(v2000) if tsample_v
    bysort mz: egen r_mz = total(r2000) if tsample_r
    bysort mz: egen fa_mz_v = total(floor_area) if tsample_v
    bysort mz: egen fa_mz_r = total(floor_area) if tsample_r
    gen vsq_mz_`var' = v_mz / fa_mz_v 
    gen rsq_mz_`var' = r_mz / fa_mz_r 
    drop tsample_v tsample_r v_mz r_mz fa_mz_v fa_mz_r
}



foreach var of varlist single multi {
    gen vsq_mz_`var'_final = .
	gen rsq_mz_`var'_final = .
    forvalues i = 1(1) 95 {
        sum vsq_mz_`var' if mz == `i', meanonly
        replace vsq_mz_`var'_final = r(mean) if mz == `i'
		sum rsq_mz_`var' if mz == `i', meanonly
        replace rsq_mz_`var'_final = r(mean) if mz == `i'
    }
}


save nov27_six_residential.dta, replace


duplicates drop vsq_mz_single_final vsq_mz_multi_final, force
//esttab * using Oct10, b p obslast label mtitles rtf replace
outsheet  vsq_mz_single_final rsq_mz_single_final vsq_mz_multi_final rsq_mz_multi_final mz name using mz_residential_six_nov27.csv, comma replace

/*
//============================== process the part for LA county that uses imperfect data on sale value 
use nov27_six_residential.dta, clear
keep if county == "la"
replace lgvsq2000 = lgvsq2000_la 

replace v2000 = exp(lgvsq2000) * floor_area

levelsof tractid, local(lv_tract)
foreach lv of local lv_tract {
    sum v2000 if single == 1 & tractid == `lv', meanonly
    replace vs = r(mean) if tractid == `lv'
    
    sum v2000 if multi == 1 & tractid == `lv' , meanonly
    replace vm = r(mean) if tractid == `lv'
}


replace vrs = (vs - vos * pos) / (1 - pos)  
replace vrm = (vm - vom * pom) / (1 - pom)  
replace rom = vom * ros / vos
replace rvs = (ros * pos + rrs * (1 - pos)) / (vos * pos + vrs * (1 - pos))  
replace rvm = (rom * pom + rrm * (1 - pom)) / (vom * pom + vrm * (1 - pom))  

replace r2000 = v2000 * rvs if single == 1
replace r2000 = v2000 * rvm if multi == 1
   
//aggregate to model zone level


foreach var of varlist single multi {
    gen tsample_v = !missing(floor_area) &  !missing(v2000) & `var' == 1
    gen tsample_r = !missing(floor_area) &  !missing(r2000) & `var' == 1
    bysort mz: egen v_mz = total(v2000) if tsample_v
    bysort mz: egen r_mz = total(r2000) if tsample_r
    bysort mz: egen fa_mz_v = total(floor_area) if tsample_v
    bysort mz: egen fa_mz_r = total(floor_area) if tsample_r
    replace vsq_mz_`var' = v_mz / fa_mz_v 
    replace rsq_mz_`var' = r_mz / fa_mz_r 
    drop tsample_v tsample_r v_mz r_mz fa_mz_v fa_mz_r
}



foreach var of varlist single multi {
    replace vsq_mz_`var'_final = .
	replace rsq_mz_`var'_final = .
    forvalues i = 1(1) 95 {
        sum vsq_mz_`var' if mz == `i', meanonly
        replace vsq_mz_`var'_final = r(mean) if mz == `i'
		sum rsq_mz_`var' if mz == `i', meanonly
        replace rsq_mz_`var'_final = r(mean) if mz == `i'
    }
}
duplicates drop vsq_mz_single_final vsq_mz_multi_final, force
outsheet  vsq_mz_single_final rsq_mz_single_final vsq_mz_multi_final rsq_mz_multi_final mz name using la_salevalue_nov27.csv, comma replace
*/
