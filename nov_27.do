//nov_27.do
//single family and multi-family residential

scalar drop _all
log close _all
log using nov_27, replace
set more off
cd "D:\summer, 2011\Third Project\Final_data"
 //join census tract data with parcel data
/*
insheet using "D:\summer, 2011\Third Project\ORparcel_RS_census.txt", clear
rename id tractid
rename near_ro_di fwy
rename near_co_di ocean
replace tractid = substr(tractid,2,)

merge m:1 tractid using DTDownload_or.csv
*/


use parcel_rv_residential_merged.dta, clear

gen random = runiform()
keep if random < 0.01

//====================================== variable processing =======================
gen single = lu_08 >= 1110 & lu_08 < 1120
gen multi = lu_08 >= 1120 & lu_08 < 1200

replace totvalue07 = . if totvalue07 == 0
rename near_ro__2 fwy
rename near_co__2 ocean
drop conf_sell_
gen sale_price = conf_sell1 if conf_sell1 != 0
replace saleyr = . if saleyr == 0 | saleyr == 1899
gen floor_area = impsqft if impsqft != 0
gen lgvsq  = ln( sale_price/ floor_area ) 
gen age = 2000 - yearbuilt if yearbuilt != 0
format scag_xyid  %14.0f
//destring hos, generate(hos_n) ignore(",")
foreach var of varlist hrs hrm rrs rrm vos vom {
    gen `var'_n = real(`var')
}


gen age_bk = age
levelsof city, local(citylist)
foreach cvar of local citylist { 
        summarize age if city == "`cvar'" , meanonly 
        replace age = r(mean) if missing(age) & city == "`cvar'"
}


//======================================================== regression (Orange)======================
//replace insample = (county == "Orange")
//replace insample = mz >= 50 & mz <=66

keep if mz >= 81 & mz <=95


gen insample = 1 // as I use only Orange County here.
gen lgvsq2000 = lgvsq if insample

encode city  if insample, gen(citynum)
xtset citynum

//char city [omit] "Orange"  
//char saleyr [omit] 2000 
//xi: quietly reg  lgvsq  fsub cbd fwy ocean  i.city i.saleyr i.lu_08 if insample & !missing(saleyr) 
xtreg lgvsq  fsub cbd fwy ocean  i.saleyr i.lu_08 if insample & !missing(saleyr), fe

//estimates store retail_ora
/*
foreach var of varlist _Isaleyr* { 
    gen b`var' = _b[`var']
}
*/

gen id_keep = !missing(lgvsq) & !missing(saleyr) & insample
gen id_XO = !missing(sale_price) & !missing(saleyr) & !missing(totvalue07) & insample

//XO part begin
//xo = sale_price / totvalue07 computed across all parcels in Riverside county
//To impute saleprice from totval07 for LA & VT
gen xo_sample = (id_XO == 1)
gen sp = sale_price if xo_sample
gen dd = 0 if xo_sample

levelsof saleyr, local(lvsaleyr)
foreach lv of local lvsaleyr {
    capture replace dd = _b[`lv'.saleyr] - _b[2000.saleyr] if saleyr == `lv' & id_keep == 1
}
/*
foreach var of varlist _Isaleyr* { 
    replace dd = exp(b`var') if `var' == 1 
}
replace sp = sp / dd
*/
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

/*
foreach var of varlist _Isaleyr* {
    replace lgvsq2000 =  lgvsq - b`var' if `var' == 1 & id_keep == 1
} 
foreach var of varlist _Isaleyr* {
    replace `var' = 0 
}
predict lgvsq_p if insample 
replace lgvsq2000 = lgvsq_p if id_keep == 0 & insample
*/

foreach lv of local lvsaleyr {
    capture replace lgvsq2000 =  lgvsq - _b[`lv'.saleyr] + _b[2000.saleyr] if saleyr == `lv'  & id_keep == 1 & insample
}

replace saleyr = 2000 if id_keep == 0 & insample
predict lgvsq_p if insample
replace lgvsq2000 = lgvsq_p if id_keep == 0 & insample


/*
//========== LA, VT===========
drop b_* lgvsq_p
//replace insample = (county == "Los Angeles" | county == "Ventura")
replace insample = mz > 0 & mz <= 49 
replace sale_price = totvalue07 * scalar(xo_retail) if insample
replace lgvsq2000 = ln(sale_price / floor_area) if insample
char city [omit] "Los Angeles"  
xi: quietly reg  lgvsq2000  fsub cbd fwy ocean  i.city i.lu_08 if insample 
estimates store retail_la_vt
predict lgvsq_p if insample
replace lgvsq2000 = lgvsq_p if insample & missing(totvalue07)
*/

//================================================= rent to value  ratioo  ==========================
gen vsq2000 = exp(lgvsq2000)

foreach var of varlist hrs hrm rrs rrm vos vom {
	drop `var'
	capture rename `var'_n `var'
	replace `var' = . if `var' == 0
}



gen v2000 = vsq2000 * floor_area
gen pos = hos / (hos + hrs)
gen pom = hom / (hom + hrm)
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
    forvalues i = 81(1) 95 {
        sum vsq_mz_`var' if mz == `i', meanonly
        replace vsq_mz_`var'_final = r(mean) if mz == `i'
		sum rsq_mz_`var' if mz == `i', meanonly
        replace rsq_mz_`var'_final = r(mean) if mz == `i'
    }
}


save nov27_rv_residential.dta, replace


duplicates drop vsq_mz_single_final vsq_mz_multi_final, force
//esttab * using Oct10, b p obslast label mtitles rtf replace
outsheet  vsq_mz_single_final rsq_mz_single_final vsq_mz_multi_final rsq_mz_multi_final mz name using mz_residential_rv_nov27.csv, comma replace


