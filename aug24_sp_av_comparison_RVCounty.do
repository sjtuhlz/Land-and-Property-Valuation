
clear
cd "/Users/huiling/Dropbox/Vacant Land"
capture log close 
log using aug242014, replace
set more off



use aug12014_vacant.dta, clear
duplicates drop //1270 out of 585607 observations are duplicates 


drop if  mz >= 96 & mz <= 97
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

keep if county == "RV"

replace fwy = fwy * 0.00062137
replace ocean = ocean * 0.00062137
replace totvalue07 = . if totvalue07 == 0
replace shape_area = . if shape_area == 0                            
replace  scag_gp_co = 0 if scag_gp_co == 9999 // Code missing scag_gp_co as one of the categories in dummy regression
replace saleyr = . if saleyr == 1899 | saleyr == 0 
replace saleyr = . if saleyr < 1980  //Exclude parcels with saleyr earlier than 1980.

gen saleyr_bk = saleyr


replace city_name = "Unknown" if missing(city_name)
encode city, gen(city_id)

gen cbd2 = cbd^2 
gen ocean2 = ocean^2

gen lnp = ln(sale_price)
gen lnv = ln(totvalue07)
gen lna = ln(shape_area)

drop if missing(lna)




gen v2000_p = sale_price
gen v2000 = totvalue07
fvset base 2000 saleyr

*********************  regression ********************
quietly reg lnp lna fsub cbd cbd2 fwy ocean ocean2 i.city_id i.lu_08 i.scag_gp_co i.saleyr if dvlp
estimates store developable_lnp
estimates save developable_lnp, replace
replace saleyr = 2000 
predict lnp_p
replace v2000_p = exp(lnp_p) if dvlp & !e(sample)
replace saleyr = saleyr_bk //dont forget this 
levelsof saleyr if e(sample), local(lv_saleyr)
foreach lv of local lv_saleyr {
	replace v2000_p = v2000_p / exp(_b[`lv'.saleyr]) if saleyr == `lv' & e(sample)
}
drop lnp_p
replace saleyr = saleyr_bk



quietly reg lnp lna fsub cbd cbd2 fwy ocean ocean2 i.city_id i.lu_08 i.scag_gp_co i.saleyr if !dvlp
estimates store undevelopable_lnp
estimates save undevelopable_lnp, replace
replace saleyr = 2000
predict lnp_p
replace v2000_p = exp(lnp_p) if !dvlp & !e(sample)
replace saleyr = saleyr_bk //dont forget this
levelsof saleyr if e(sample), local(lv_saleyr)
foreach lv of local lv_saleyr {
	replace v2000_p = v2000_p / exp(_b[`lv'.saleyr]) if saleyr == `lv' & e(sample)
}
drop lnp_p
replace saleyr = saleyr_bk



quietly reg lnv lna fsub cbd cbd2 fwy ocean ocean2 i.city_id i.lu_08 i.scag_gp_co i.saleyr if dvlp
estimates store developable_lnv
estimates save developable_lnv, replace
replace saleyr = 2000
predict lnv_p
replace v2000 = exp(lnv_p) if dvlp & !e(sample)
replace saleyr = saleyr_bk //dont forget this
levelsof saleyr if e(sample), local(lv_saleyr)
foreach lv of local lv_saleyr {
	replace v2000 = v2000 / exp(_b[`lv'.saleyr]) if saleyr == `lv' & e(sample)
}
drop lnv_p
replace saleyr = saleyr_bk



quietly reg lnv lna fsub cbd cbd2 fwy ocean ocean2 i.city_id i.lu_08 i.scag_gp_co i.saleyr if !dvlp
estimates store undevelopable_lnv
estimates save undevelopable_lnv, replace
replace saleyr = 2000
predict lnv_p
replace v2000 = exp(lnv_p) if !dvlp & !e(sample)
replace saleyr = saleyr_bk //dont forget this
levelsof saleyr if e(sample), local(lv_saleyr)
foreach lv of local lv_saleyr {
	replace v2000 = v2000 / exp(_b[`lv'.saleyr]) if saleyr == `lv' & e(sample)
}
drop lnv_p
replace saleyr = saleyr_bk

format v2000_p v2000 shape_area %14.0f
save Aug242014_saleprice_assessedvalue_results.dta, replace




// Table 19, all vacant parcels
use Aug242014_saleprice_assessedvalue_results.dta, clear
gen shape_area_sqft = shape_area * 10.7639104
format shape_area_sqft %14.0f
gen nparcel = 1
gen nsp = !missing(lnp) & !missing(saleyr)
gen nav = !missing(lnv) & !missing(saleyr)
collapse (sum) nparcel nsp nav v2000_p v2000 shape_area shape_area_sqft, by(dvlp) 
gen pvratio = v2000_p/ v2000
outsheet using aug242014_vacant_saleprice_totvalue_comparison.csv, comma replace




// Table 20-1, ALVsp / ALVav by lu_08 for developable vacant parcels only 
use Aug242014_saleprice_assessedvalue_results.dta, clear
gen shape_area_sqft = shape_area * 10.7639104
format shape_area_sqft %14.0f
gen nparcel = 1
gen nsp = !missing(lnp) & !missing(saleyr)
gen nav = !missing(lnv) & !missing(saleyr)
collapse (sum) nparcel nsp nav v2000_p v2000 shape_area shape_area_sqft if dvlp, by(lu_08) 
gen pvratio = v2000_p/ v2000
outsheet using aug242014_vacant_dvlp_saleprice_totvalue_comparison.csv, comma replace


// Table 20-2, ALVsp / ALVav by lu_08 for undevelopable vacant parcels only 
use Aug242014_saleprice_assessedvalue_results.dta, clear
gen shape_area_sqft = shape_area * 10.7639104
format shape_area_sqft %14.0f
gen nparcel = 1
gen nsp = !missing(lnp) & !missing(saleyr)
gen nav = !missing(lnv) & !missing(saleyr)
collapse (sum) nparcel nsp nav v2000_p v2000 shape_area shape_area_sqft if !dvlp, by(lu_08) 
gen pvratio = v2000_p/ v2000
outsheet using aug242014_vacant_undvlp_saleprice_totvalue_comparison.csv, comma replace

