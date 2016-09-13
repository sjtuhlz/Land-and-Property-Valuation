clear
cd "/Users/huiling/Dropbox/Vacant Land"
capture log close 
log using aug122014, replace
set more off


/*
//For these three counties, only need to idenfity the county information
// didn't use other information at all 
insheet using LAparcel_vacant.txt, clear
keep scag_xyid lu_08
destring scag_xyid, gen(scag_xyid_n) force
replace scag_xyid_n = 37856197534917 if scag_xyid == "37856197534917-1"
replace scag_xyid_n = 38603935673457 if scag_xyid == "38603935673457-1"
replace scag_xyid_n = 37747339740695 if scag_xyid == "37747339740695-1"
replace scag_xyid_n = 37592422132284 if scag_xyid == "37592422132284-1"
replace scag_xyid_n = 38607864205613 if scag_xyid == "38607864205613-1"
drop scag_xyid
rename scag_xyid_n scag_xyid
gen county = "LA"
save LAparcel_vacant.dta, replace

insheet using OCparcel_vacant.txt, clear
keep scag_xyid lu_08
gen county = "OR"
save OCparcel_vacant.dta, replace

insheet using SBparcel_vacant.txt, clear
keep scag_xyid lu_08
gen county = "SB"
save SBparcel_vacant.dta, replace
*/

/*
// VT county missed a lot of lu_08 for its parcels, so has to update it totally
// fortunately VTparcel_ro_co_di.bdf by Yuntao has all the information needed. 

insheet using VTparcel_vacant.txt, clear
drop oid_ fid_1 objectid apn acres area x y lu08 impsqft yearbuilt year_adapt 
rename near_ro_di fwy 
rename near_co_di ocean
rename city city_name 
merge 1:1 scag_xyid using "/Users/huiling/Documents/summer, 2011/yuntao_guo/Developability/Und_VTSBRVOCLAIM_2.dta", force
drop if _merge == 2
replace dvlp = 0 if dvlp == 1
replace dvlp = 0 if (lu_08 >= 1820 & lu_08 < 1840) | (lu_08 > 1840 & lu_08 <= 1880)
replace dvlp = 1 if missing(dvlp)
capture drop _merge
gen county = "VT"
save parcel_vt_vacant_aug122014.dta, replace
*/

/*
use parcel_ri_vacant_nov102013.dta, clear
merge 1:1 scag_xyid using "/Users/huiling/Documents/summer, 2011/yuntao_guo/Developability/Und_VTSBRVOCLAIM_2.dta", force
drop if _merge == 2
replace dvlp = 0 if dvlp == 1
replace dvlp = 0 if (lu_08 >= 1820 & lu_08 < 1840) | (lu_08 > 1840 & lu_08 <= 1880)
replace dvlp = 1 if missing(dvlp)
capture drop _merge
rename city city_name
gen county = "RV"
save parcel_ri_vacant_aug122014.dta, replace
*/

/*
use jan_20_vacant.dta, clear
replace lu_08 = . if lu_08 == 0
drop x y  objectid city near_fid near_dist _merge 
merge m:1 scag_xyid using parcel_vt_vacant_aug122014.dta, force update replace 
drop _merge  
merge m:1 scag_xyid using parcel_ri_vacant_aug122014.dta, force update replace
capture drop _merge //note sale_price variable is for riverside county data only.
merge m:1 scag_xyid using OCparcel_vacant.dta, force update
drop if _merge == 2
drop _merge
merge m:m scag_xyid using LAparcel_vacant.dta, force update
drop if _merge == 2
drop _merge
merge m:1 scag_xyid using SBparcel_vacant.dta, force update
drop if _merge == 2
drop _merge
save aug12014_vacant.dta, replace
*/



/*
use parcel_vt_vacant_aug122014.dta, clear
append using OCparcel_vacant.dta, force
append using LAparcel_vacant.dta, force
append using SBparcel_vacant.dta, force
append using parcel_ri_vacant_aug122014.dta, force
duplicates drop 
save testing.dta, replace

use jan_20_vacant.dta, clear
replace lu_08 = . if lu_08 == 0
drop x y  objectid city near_fid near_dist _merge 
gen county = ""
merge m:1 scag_xyid using testing.dta, force update replace
save aug242014_vacant.dta, replace
*/


use aug12014_vacant.dta, clear
duplicates drop //1270 out of 585607 observations are duplicates 

drop if  mz >= 96 & mz <= 97
//(two methods, one is to keep a copy of all duplicates, the other is to not use duplicate observations in regressions)


replace fwy = fwy * 0.00062137
replace ocean = ocean * 0.00062137
replace totvalue07 = . if totvalue07 == 0
replace shape_area = . if shape_area == 0                            
replace  scag_gp_co = 0 if scag_gp_co == 9999 // Code missing scag_gp_co as one of the categories in dummy regression
replace saleyr = . if saleyr == 1899 | saleyr == 0 
gen saleyr_bk = saleyr
replace saleyr = . if saleyr < 1980  //Exclude parcels with saleyr earlier than 1980.
replace city_name = "Unknown" if missing(city_name)


gen cbd2 = cbd^2 
gen ocean2 = ocean^2

gen lnv = ln(totvalue07)
gen lna = ln(shape_area)

** WARNING of dropping parcels affect tables!
drop if missing(lna)

replace county = "LA" if city_name == "unincorporated_la" & missing(county)
replace county = "OR" if city_name == "unincorporated_or" & missing(county)
replace county = "RV" if city_name == "unincorporated_rv" & missing(county)
replace county = "SB" if city_name == "unincorporated_sb" & missing(county)
replace county = "VT" if city_name == "unincorporated_vn" & missing(county)
replace county = "LA" if mz >= 1 & mz <= 46 & missing(county)
replace county = "VT" if mz >= 47 & mz <= 49 & missing(county)
replace county = "OR" if mz >= 50 & mz <= 66 & missing(county)
replace county = "SB" if mz >= 67 & mz <= 80 & missing(county)
replace county = "RV" if mz >= 81 & mz <= 95 & missing(county)



/*
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



decode county_n, gen(county_s)	
replace county_s = "LA" if city_name == "Avalon" | city_name == "Cerritos"| city_name == "La Mirada" | city_name == "Lakewood"
replace county_s = "LA" if city_name == "Long Beach" | city_name == "Los Angeles" | city_name == "Hawaiian Gardens"
replace county_s = "SB" if city_name == "Chino" | city_name == "Colton" | city_name == "Fontana" | city_name == "Yucaipa" | city_name == "Montclair" | city_name == "Ontario"
replace county_s = "OR" if city_name == "La Habra" | city_name == "Los Alamitos" 
replace county_s = "VT" if city_name == "Thousand Oaks"

drop county
rename county_s county
drop if county == "IM"
*/





gen lnv2000 = .
gen insample = !missing(lnv) & !missing(saleyr)


drop if lu_08 == 0
***********************************************




**** Developable vacant land 

char city_name [omit] "Unknown"


xi: reg lnv lna  fsub cbd cbd2 fwy ocean ocean2 i.city_name i.saleyr i.lu_08 i.scag_gp_co if insample & dvlp

* encode city_name, gen(city_n)
* reg lnv lna fsub cbd cbd2 fwy ocean ocean2 i.city_n i.saleyr i.lu_08 i.scag_gp_co if insample & dvlp
estimates store developable_aug122014
estimates save developable_aug122014, replace


levelsof saleyr if e(sample), local(lvsaleyr)
foreach lv of local lvsaleyr {
    *capture replace lnv2000 =  lnv - _b[`lv'.saleyr] + _b[2000.saleyr]  if saleyr == `lv'  & e(sample)
    capture replace lnv2000 =  lnv - _b[_Isaleyr_`lv'] + _b[_Isaleyr_2000]  if saleyr == `lv'  & e(sample)
}
// base year probelm:
replace lnv2000 = lnv + _b[_Isaleyr_2000] if saleyr == 1980 & e(sample)

foreach var of varlist _Isaleyr_* {
	replace `var' = 0
}
	

replace _Isaleyr_2000 = 1
predict lnv_p if dvlp




replace lnv2000 = lnv_p if e(sample) == 0 & dvlp
drop lnv_p






***** Undevelopable vacant land 


xi: reg lnv lna  fsub cbd cbd2 fwy ocean ocean2 i.city_name i.saleyr i.lu_08 i.scag_gp_co if insample & !dvlp 
** reg lnv lna fsub cbd cbd2 fwy ocean ocean2 i.city_n i.saleyr i.lu_08 i.scag_gp_co if insample & !dvlp


estimates store undevelopable_july262014
estimates save undevelopable_july262014, replace




levelsof saleyr if e(sample), local(lvsaleyr)
foreach lv of local lvsaleyr {
    *capture replace lnv2000 =  lnv - _b[`lv'.saleyr] + _b[2000.saleyr]  if saleyr == `lv'  & e(sample)
    capture replace lnv2000 =  lnv - _b[_Isaleyr_`lv'] + _b[_Isaleyr_2000]  if saleyr == `lv'  & e(sample)
}
// base year probelm:
replace lnv2000 = lnv + _b[_Isaleyr_2000] if saleyr == 1980 & e(sample)

foreach var of varlist _Isaleyr_* {
	replace `var' = 0
}


replace _Isaleyr_2000 = 1
predict lnv_p if !dvlp

replace lnv2000 = lnv_p if e(sample) == 0 & !dvlp
drop lnv_p




gen v2000 = exp(lnv2000)

drop _I* 
save aug122014_vacant_results.dta, replace

************************************************************
** Table 6 in paper ** 
keep if insample
gen nparcel = 1
collapse (sum) nparcel v2000 shape_area, by(county)
gen shape_area_sqft = shape_area * 10.7639104
gen vsq2000 = v2000/ shape_area_sqft
format v2000 shape_area shape_area_sqft %14.0f
format vsq2000 %14.2f
outsheet using table6_aug122014.csv, comma replace


** Table 7 in paper ** 
use aug122014_vacant_results.dta, clear
keep if !insample
gen nparcel = 1
collapse (sum) nparcel v2000 shape_area, by(county)
gen shape_area_sqft = shape_area * 10.7639104
gen vsq2000 = v2000/ shape_area_sqft
format v2000 shape_area shape_area_sqft %14.0f
format vsq2000 %14.2f
outsheet using table7_aug122014.csv, comma replace


** Table 8 in paper ** 
use aug122014_vacant_results.dta, clear
gen nparcel = 1
collapse (sum) nparcel v2000 shape_area, by(county)
gen shape_area_sqft = shape_area * 10.7639104
gen vsq2000 = v2000/ shape_area_sqft
format v2000 shape_area shape_area_sqft %14.0f
format vsq2000 %14.2f
outsheet using table8_aug122014.csv, comma replace

** Table 9 in paper ** 
use aug122014_vacant_results.dta, clear
keep if dvlp
gen nparcel = 1
collapse (sum) nparcel v2000 shape_area, by(county)
gen shape_area_sqft = shape_area * 10.7639104
gen vsq2000 = v2000/ shape_area_sqft
format v2000 shape_area shape_area_sqft %14.0f
format vsq2000 %14.2f
outsheet using table9_aug122014.csv, comma replace


** Table 10 in paper ** 
use aug122014_vacant_results.dta, clear
keep if !dvlp
gen nparcel = 1
collapse (sum) nparcel v2000 shape_area, by(county)
gen shape_area_sqft = shape_area * 10.7639104
gen vsq2000 = v2000/ shape_area_sqft
format v2000 shape_area shape_area_sqft %14.0f
format vsq2000 %14.2f
outsheet using table10_aug122014.csv, comma replace


** Table 5 in paper ** 
use aug122014_vacant_results.dta, clear
encode city_name, gen(city_n)
fvset base 2000 saleyr
fvset base 3000 lu_08 
fvset base 0 scag_gp_co 
reg lnv lna fsub cbd cbd2 fwy ocean ocean2 i.city_n i.saleyr i.lu_08 i.scag_gp_co if insample & dvlp
estimates store developable_aug122014
estimates save developable_aug122014, replace

reg lnv lna fsub cbd cbd2 fwy ocean ocean2 i.city_n i.saleyr i.lu_08 i.scag_gp_co if insample & !dvlp
estimates store undevelopable_aug122014
estimates save undevelopable_aug122014, replace

estout developable_aug122014 undevelopable_aug122014 using aug122014_regcoef.doc, style(fixed) cells(b(star fmt(%9.3f))) replace stats(r2_a N, fmt(%9.3f %9.0g) labels(Adjusted R-squared))legend label collabels(none) varlabels(_cons Constant)

*estout developable_aug122014 using aug122014_regcoef_dvlp.doc, style(fixed) cells(b(star fmt(%9.3f))) replace stats(r2_a N, fmt(%9.3f %9.0g) labels(Adjusted R-squared))legend label collabels(none) varlabels(_cons Constant)
*estout undevelopable_aug122014 using aug122014_regcoef_undvlp.doc, style(fixed) cells(b(star fmt(%9.3f))) replace stats(r2_a N, fmt(%9.3f %9.0g) labels(Adjusted R-squared))legend label collabels(none) varlabels(_cons Constant)


** Table 4 in paper ** 
use aug122014_vacant_results.dta, clear
collapse (sum) insample (count) nparcel= scag_xyid, by(county)
gen ratio = 1 - insample / nparcel
outsheet using table4_aug122014.csv, comma replace

/*
//estout developable undevelopable using test.doc, style(fixed) cells(b(star fmt(%9.3f)) se(par)) replace drop(o.* _I*)///
estout developable_july262014 undevelopable_july262014 using July262014_regcoef.doc, style(fixed) cells(b(star fmt(%9.3f)) se(par)) replace///
drop(*.*) stats(r2_a N, fmt(%9.3f %9.0g) labels(R-squared))///
legend label collabels(none) varlabels(_cons Constant)


estout developable_july262014 undevelopable_july262014 using July262014_regcoef.doc, style(fixed) cells(b(star fmt(%9.3f)) se(par)) replace///
 stats(r2_a N, fmt(%9.3f %9.0g) labels(R-squared))///
legend label collabels(none) varlabels(_cons Constant)

*/


/*
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

*/



