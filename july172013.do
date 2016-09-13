//July 17, 2013

***************************
* This code runs setup2: lnv ~ lna 
* Setup 1 was run in july17_vacant_vsq.do: lgvsq ~ 
***************************


clear
cd "/Users/huiling/Dropbox/Vacant Land"
capture log close 
log using july172013, replace
* use jan_20_vacant.dta, clear // this version has x,y in projected coordinates 
use myfile_may16.dta, clear // this version has x, y in regular coordinates 
* format scag_xyid %14.0f //myfile_may16.dta does not have scag_xyid !!

** use city_name instead of city, as the former has far less missing 

*gen rdn = runiform()
*keep if rdn < 0.01



capture drop _merge
merge m:1 saleyr using CA_landpriceindex.dta
drop if _merge == 2
gen saleyr_bk = saleyr
replace saleyr = . if _merge != 3

save vacant_lpi_sep24.dta, replace


replace fwy = fwy * 0.00062137
replace ocean = ocean * 0.00062137
replace totvalue07 = . if totvalue07 == 0
replace shape_area = . if shape_area == 0                            
* replace  scag_gp_co = . if  scag_gp_co == 0 | scag_gp_co == 9999 
replace  scag_gp_co = 0 if scag_gp_co == 9999 // Code missing scag_gp_co as one of the categories in dummy regression
replace saleyr = . if saleyr == 1899 | saleyr == 0 

replace city_name = "Unknown" if missing(city_name)

gen cbd2 = cbd^2
gen ocean2 = ocean^2


gen lnv = ln(totvalue07)
gen lna = ln(shape_area)
gen vsq = totvalue07 / shape_area *0.09290304



** WARNING of dropping parcels affect tables!
drop if missing(lna)




gen county = ""
replace county = "LA" if mz >= 1 & mz <= 46
replace county = "VT" if mz >= 47 & mz <= 49
replace county = "OR" if mz >= 50 & mz <= 66
replace county = "SB" if mz >= 67 & mz <= 80
replace county = "RV" if mz >= 81 & mz <= 95
replace county = "IM" if mz >= 96 & mz <= 97
* replace county = "NA" if mz == 0



//there is no parcel that misses city_name, so we can infer missing counties from city_name
replace county = "LA" if city_name == "unincorporated_la"
replace county = "OR" if city_name == "unincorporated_or"
replace county = "RV" if city_name == "unincorporated_rv"
replace county = "SB" if city_name == "unincorporated_sb"
replace county = "VT" if city_name == "unincorporated_vn"


encode county, gen(county_n)


levelsof city_name, local(citylist)
foreach lc of local citylist {
		sum county_n if city_name == "`lc'" & !missing(county), meanonly
		replace county_n = r(mean) if city_name == "`lc'" & missing(county)
}

//some cities have parcels in more than one county, then county_n is no longer integer 
//list if mod(county_n,1) != 0
//just a few of them, so change one-by-one


decode county_n, gen(county_s)	
replace county_s = "LA" if city_name == "Avalon"	
replace county_s = "SB" if city_name == "Chino" | city_name == "Colton" | city_name == "Fontana" | city_name == "Yucaipa"


drop county
rename county_s county

	 


//remove outliers 
* shape_area too small and value per square foot too large
* gen insample = shape_area > 10 & vsq < 10000 

// last sale year earlier than 1964 (# of parcels less than a hundred)
* replace insample = insample & saleyr >= 1964
  


gen lnv2000 = .




* replace saleyr = 9999 if missing(saleyr)




/*
// use two-digit land use instead of three-digit land use 
gen lu08_2d = lu_08
replace lu08_2d = 2600 if lu_08 == 2000 
replace lu08_2d = 3100 if lu_08 == 3000
*/



drop if county == "IM"


**** Use land price index instead of a saleyr dummy (updated on Sep 24, 2013) ******
sum lpi if saleyr == 2000
replace lpi = lpi/r(mean)

replace lnv2000 = lnv - ln(lpi) if !missing(lpi)
*replace insample = insample & !missing(lpi)
gen insample = !missing(lpi)

**** Developable vacant land 

char city_name [omit] "Unknown"

/*
xi: reg lnv lna  fsub cbd cbd2 fwy ocean ocean2 i.city_name i.saleyr i.lu_08 i.scag_gp_co if insample & dvlp
estimates store developable
estimates save developable, replace

levelsof saleyr if e(sample), local(lvsaleyr)
foreach lv of local lvsaleyr {
    *capture replace lnv2000 =  lnv - _b[`lv'.saleyr] + _b[2000.saleyr]  if saleyr == `lv'  & e(sample)
    capture replace lnv2000 =  lnv - _b[_Isaleyr_`lv'] + _b[_Isaleyr_2000]  if saleyr == `lv'  & e(sample)
}


foreach var of varlist _Isaleyr_* {
	replace `var' = 0
}
	

replace _Isaleyr_2000 = 1
* predict lnv_p if dvlp
predict lnv_p



replace lnv2000 = lnv_p if e(sample) == 0 & dvlp
drop lnv_p
*/

xi: reg lnv2000 lna  fsub cbd cbd2 fwy ocean ocean2 i.city_name i.lu_08 i.scag_gp_co if insample & dvlp  
estimates store developable
estimates save developable, replace

predict lnv_p if dvlp 
replace lnv2000 = lnv_p if e(sample) == 0 & dvlp
drop lnv_p





***** Undevelopable vacant land 

/*
xi: reg lnv lna  fsub cbd cbd2 fwy ocean ocean2 i.city_name i.saleyr i.lu_08 i.scag_gp_co if insample & !dvlp 
estimates store undevelopable
estimates save undevelopable, replace


*xi: reg lnv lna  fsub cbd cbd2 fwy ocean ocean2 i.scag_gp_co if insample & !dvlp
*estimates save undevelopable_nocity
*estimates save undevelopable_nocity_nosaleyr
*estimates save undevelopable_nocitysaleyrlu08



levelsof saleyr if e(sample), local(lvsaleyr)
foreach lv of local lvsaleyr {
    *capture replace lnv2000 =  lnv - _b[`lv'.saleyr] + _b[2000.saleyr]  if saleyr == `lv'  & e(sample)
    capture replace lnv2000 =  lnv - _b[_Isaleyr_`lv'] + _b[_Isaleyr_2000]  if saleyr == `lv'  & e(sample)
}


foreach var of varlist _Isaleyr_* {
	replace `var' = 0
}


replace _Isaleyr_2000 = 1
*predict lnv_p if !dvlp
predict lnv_p

replace lnv2000 = lnv_p if e(sample) == 0 & !dvlp
*drop lnv_p
*/

xi: reg lnv2000 lna  fsub cbd cbd2 fwy ocean ocean2 i.city_name i.lu_08 i.scag_gp_co if insample & !dvlp  
estimates store undevelopable
estimates save undevelopable, replace

predict lnv_p if !dvlp 
replace lnv2000 = lnv_p if e(sample) == 0 & !dvlp


gen v2000 = exp(lnv2000)

stop

*save july17_results.dta, replace
save sep24_lpi_results.dta, replace

estout developable undevelopable using test.doc, style(fixed) cells(b(star fmt(%9.3f)) se(par)) replace drop(o.* _I*)///
stats(r2_a N, fmt(%9.3f %9.0g) labels(R-squared))///
legend label collabels(none) varlabels(_cons Constant)

*replace shape_area = shape_area * 10.7639  //change to square feet

collapse (sum) v2000 shape_area, by(county dvlp)
gen vsq2000 = v2000/shape_area
*outsheet using dvlp1.txt if dvlp, replace 
*outsheet using dvlp0.txt if !dvlp, replace 


** vsq2000 by land use and county 
collapse (sum) v2000 shape_area, by(county lu_08 dvlp)
gen vsq2000 = v2000/shape_area
format vsq2000 %14.02f
table lu_08 county, c(mean vsq2000)





// ************************ Prepare tables for paper ***************************
***** This is problem using count(saleyr) to calculate nparcel!!!!!!!!!!!
*** Correct later :(
//Table 2: 
use july17_results.dta, clear
keep if !missing(saleyr) & !missing(totvalue07)
bysort county: egen nparcel = count(saleyr)
collapse (mean) nparcel (sum) totarea = shape_area totv2000 = v2000, by(county)
format totarea totv2000 %14.0f
gen vsq2000 = totv2000 / totarea 
outsheet using table2.xls, replace




//Table 2: Remove outliers 
use july17_results.dta, clear
keep if !missing(saleyr) & !missing(totvalue07) & insample
bysort county: egen nparcel = count(saleyr)
collapse (mean) nparcel (sum) totarea = shape_area totv2000 = v2000, by(county)
format totarea totv2000 %14.0f
gen vsq2000 = totv2000 / totarea 
outsheet using table2_nooutlier.xls, replace


//Table 3B: 
use july17_results.dta, clear
keep if missing(saleyr) | missing(totvalue07) 
bysort county: egen nparcel = count(saleyr)
collapse (mean) nparcel (sum) totarea = shape_area totv2000 = v2000, by(county)
* format totarea totv2000 %14.0f
gen vsq2000 = totv2000 / totarea 
replace totarea = totarea / 1e9
replace totv2000 = totv2000 / 1e9
format totarea totv2000 %9.2f
outsheet using table3B.xls, replace

//Table 4B:
use july17_results.dta, clear
drop if missing(county) | county == "IM"
bysort county: egen nparcel = count(saleyr)
collapse (mean) nparcel (sum) totarea = shape_area totv2000 = v2000, by(county)
gen vsq2000 = totv2000 / totarea 
replace totarea = totarea / 1e9
replace totv2000 = totv2000 / 1e9
format totarea totv2000 %9.2f
outsheet using table4B.xls, replace

//Table 5B:
use july17_results.dta, clear
drop if missing(county) | county == "IM"
keep if dvlp
bysort county: egen nparcel = count(saleyr)
collapse (mean) nparcel (sum) totarea = shape_area totv2000 = v2000, by(county)
gen vsq2000 = totv2000 / totarea 
replace totarea = totarea / 1e9
replace totv2000 = totv2000 / 1e9
format totarea totv2000 %9.2f
outsheet using table5B.xls, replace

//Table 6B:
use july17_results.dta, clear
drop if missing(county) | county == "IM"
keep if !dvlp
bysort county: egen nparcel = count(saleyr)
collapse (mean) nparcel (sum) totarea = shape_area totv2000 = v2000, by(county)
gen vsq2000 = totv2000 / totarea 
replace totarea = totarea / 1e9
replace totv2000 = totv2000 / 1e9
format totarea totv2000 %9.2f
outsheet using table6B.xls, replace
