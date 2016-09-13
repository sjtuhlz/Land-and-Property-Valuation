//Updated on Aug 10, 2014 
** Correction ***
*1) replace saleyr = 1980 if saleyr < 1980 (this doesn't make sense), change to
* replace saleyr = . if saleyr < 1980

// nov 10, 2013 
** compare the results using sales price versus results using assessed value

cd "/Users/huiling/Documents/summer, 2011"
clear
/*
insheet using "/Users/huiling/Documents/summer, 2011/parcel_ri_nov102013.txt", clear
rename near_ro_di fwy
rename near_co_di ocean 
rename conf_sell_ sale_price
keep scag_xyid shape_area city name  mz lu_08 fsub cbd fwy ocean totvalue07 scag_gp_co sale_price saleyr
keep if (lu_08 >= 1700 & lu_08 < 4000) | (lu_08 >= 1274 & lu_08 <= 1276) 
save parcel_ri_vacant_nov102013.dta, replace

insheet using "/Users/huiling/Documents/summer, 2011/parcel_or_nov102013.txt", clear
rename near_ro_di fwy
rename near_co_di ocean 
keep scag_xyid shape_area city name  mz lu_08 fsub cbd fwy ocean totvalue07 scag_gp_co sale_price saleyr 
keep if (lu_08 >= 1700 & lu_08 < 4000) | (lu_08 >= 1274 & lu_08 <= 1276) 
save parcel_or_vacant_nov102013.dta, replace

gen county = "OR"
append using parcel_ri_vacant_nov102013.dta, force
replace county = "RV" if missing(county)

save parcel_or_ri_vacant_nov102013.dta, replace
*/

use parcel_ri_vacant_nov102013.dta, clear

merge 1:1 scag_xyid using "/Users/huiling/Documents/summer, 2011/yuntao_guo/Developability/Und_VTSBRVOCLAIM_2.dta", force
drop if _merge == 2
replace dvlp = 0 if dvlp == 1
replace dvlp = 0 if (lu_08 >= 1820 & lu_08 < 1840) | (lu_08 > 1840 & lu_08 <= 1880)
replace dvlp = 1 if missing(dvlp)


gen lnp = ln(sale_price)
gen lnv = ln(totvalue07)
gen lna = ln(shape_area)
gen cbd2 = cbd^2  //added on Aug10,2014
gen ocean2 = ocean^2  //added on Aug10,2014

replace saleyr = . if saleyr == 0 | saleyr == 1899 | saleyr == 1900
* replace saleyr = 1980 if saleyr < 1980 
replace saleyr = . if saleyr < 1980

encode city, gen(city_id)

fvset base 2000 saleyr
gen saleyr_bk = saleyr

gen v2000_p = sale_price
gen v2000 = totvalue07

//updated on Aug10,2014
quietly reg lnp lna fsub cbd cbd2 fwy ocean ocean2 i.city_id i.lu_08 i.scag_gp_co i.saleyr if dvlp 
estimates store developable_lnp_aug102014
quietly reg lnp lna fsub cbd cbd2 fwy ocean ocean2 i.city_id i.lu_08 i.scag_gp_co i.saleyr if !dvlp
estimates store undevelopable_lnp_aug102014

quietly reg lnv lna fsub cbd cbd2 fwy ocean ocean2 i.city_id i.lu_08 i.scag_gp_co i.saleyr if dvlp 
estimates store developable_lnv_aug102014
quietly reg lnv lna fsub cbd cbd2 fwy ocean ocean2 i.city_id i.lu_08 i.scag_gp_co i.saleyr if !dvlp
estimates store undevelopable_lnv_aug102014


estout developable_lnp_aug102014 developable_lnv_aug102014 using Aug102014_dvlp_regcoef.doc, style(fixed) cells(b(star fmt(%9.3f))) replace stats(r2_a N, fmt(%9.3f %9.0g) labels(R-squared)) legend label collabels(none) varlabels(_cons Constant)
estout undevelopable_lnp_aug102014 undevelopable_lnv_aug102014 using Aug102014_undvlp_regcoef.doc, style(fixed) cells(b(star fmt(%9.3f))) replace stats(r2_a N, fmt(%9.3f %9.0g) labels(R-squared)) legend label collabels(none) varlabels(_cons Constant)

/*
quietly reg lnp lna fsub cbd fwy ocean i.city_id i.lu_08 i.scag_gp_co i.saleyr if dvlp
estimates store developable_lnp
estimates save developable_lnp, replace
replace saleyr = 2000 
predict lnp_p
replace v2000_p = exp(lnp_p) if dvlp & !e(sample)
levelsof saleyr if e(sample), local(lv_saleyr)
foreach lv of local lv_saleyr {
	replace v2000_p = v2000_p / exp(_b[`lv'.saleyr]) if saleyr == `lv' & e(sample)
}
drop lnp_p
replace saleyr = saleyr_bk



quietly reg lnp lna fsub cbd fwy ocean i.city_id i.lu_08 i.scag_gp_co i.saleyr if !dvlp
estimates store undevelopable_lnp
estimates save undevelopable_lnp, replace
replace saleyr = 2000
predict lnp_p
replace v2000_p = exp(lnp_p) if !dvlp & !e(sample)
levelsof saleyr if e(sample), local(lv_saleyr)
foreach lv of local lv_saleyr {
	replace v2000_p = v2000_p / exp(_b[`lv'.saleyr]) if saleyr == `lv' & e(sample)
}
drop lnp_p
replace saleyr = saleyr_bk



quietly reg lnv lna fsub cbd fwy ocean i.city_id i.lu_08 i.scag_gp_co i.saleyr if dvlp
estimates store developable_lnv
estimates save developable_lnv, replace
replace saleyr = 2000
predict lnv_p
replace v2000 = exp(lnv_p) if dvlp & !e(sample)
levelsof saleyr if e(sample), local(lv_saleyr)
foreach lv of local lv_saleyr {
	replace v2000 = v2000 / exp(_b[`lv'.saleyr]) if saleyr == `lv' & e(sample)
}
drop lnv_p
replace saleyr = saleyr_bk



quietly reg lnv lna fsub cbd fwy ocean i.city_id i.lu_08 i.scag_gp_co i.saleyr if !dvlp
estimates store undevelopable_lnv
estimates save undevelopable_lnv, replace
replace saleyr = 2000
predict lnv_p
replace v2000 = exp(lnv_p) if !dvlp & !e(sample)
levelsof saleyr if e(sample), local(lv_saleyr)
foreach lv of local lv_saleyr {
	replace v2000 = v2000 / exp(_b[`lv'.saleyr]) if saleyr == `lv' & e(sample)
}
drop lnv_p
replace saleyr = saleyr_bk

format v2000_p v2000 shape_area %14.0f
* save nov102013_results.dta, replace
save Aug102014_results.dta, replace

// part 1: aggregate land value of vacant land
*/


/*
collapse (sum) v2000_p v2000 shape_area, by(county dvlp) 
gen pvratio = v2000_p/ v2000
outsheet using nov102013_vacant_saleprice_totvalue_comparison.csv, comma replace

collapse (sum) v2000_p v2000 shape_area if dvlp, by(county lu_08)
format v2000_p v2000 shape_area %14.0f 
gen pvratio = v2000_p/ v2000
outsheet using nov102013_dvlp_saleprice_totvalue_comparison.csv, comma replace

collapse (sum) v2000_p v2000 shape_area if !dvlp, by(county lu_08)
format v2000_p v2000 shape_area %14.0f 
gen pvratio = v2000_p/ v2000
outsheet using nov102013_undvlp_saleprice_totvalue_comparison.csv, comma replace


use nov102013_results.dta, clear
gen nparcel_v = totvalue07 >0 
gen nparcel_p = sale_price >0 
gen nparcel = 1
save nov102013_results.dta, replace 

collapse (sum) v2000_p v2000 shape_area nparcel_p nparcel_v nparcel if !dvlp, by(county lu_08)
gen pct_p = nparcel_p / nparcel
gen pct_v = nparcel_v / nparcel
*/


/*
collapse (sum) v2000_p v2000 shape_area, by(county dvlp) 
gen pvratio = v2000_p/ v2000
outsheet using Aug102014_vacant_saleprice_totvalue_comparison.csv, comma replace

use Aug102014_results.dta, clear
collapse (sum) v2000_p v2000 shape_area if dvlp, by(county lu_08)
format v2000_p v2000 shape_area %14.0f 
gen pvratio = v2000_p/ v2000
outsheet using Aug102014_dvlp_saleprice_totvalue_comparison.csv, comma replace

use Aug102014_results.dta, clear
collapse (sum) v2000_p v2000 shape_area if !dvlp, by(county lu_08)
format v2000_p v2000 shape_area %14.0f 
gen pvratio = v2000_p/ v2000
outsheet using Aug102014_undvlp_saleprice_totvalue_comparison.csv, comma replace


use Aug102014_results.dta, clear
gen nparcel_v = totvalue07 >0 
gen nparcel_p = sale_price >0 
gen nparcel = 1
save Aug102014_results.dta, replace 

collapse (sum) v2000_p v2000 shape_area nparcel_p nparcel_v nparcel if !dvlp, by(county lu_08)
gen pct_p = nparcel_p / nparcel
gen pct_v = nparcel_v / nparcel
*/

/*


// comparison by scag_gp_co


use nov102013_results.dta, clear
collapse (sum) v2000_p v2000 shape_area if dvlp, by(county scag_gp_co)
format v2000_p v2000 shape_area %14.0f 
gen pvratio = v2000_p/ v2000
outsheet using aug102014_dvlp_saleprice_totvalue_comparison.csv, comma replace

use nov102013_results.dta, clear
collapse (sum) v2000_p v2000 shape_area if !dvlp, by(county scag_gp_co)
format v2000_p v2000 shape_area %14.0f 
gen pvratio = v2000_p/ v2000
outsheet using aug102014_undvlp_saleprice_totvalue_comparison.csv, comma replace


// comparison by lu_08, on sample that have data on sales price
use nov102013_results.dta, clear
keep if !missing(sale_price) & sale_price != 0
collapse (sum) v2000_p v2000 shape_area if dvlp, by(county lu_08)
format v2000_p v2000 shape_area %14.0f 
gen pvratio = v2000_p/ v2000
outsheet using aug102014_dvlp_saleprice_totvalue_comparison_commonsample.csv, comma replace

*/



