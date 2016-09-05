** Updated from aug24_sp_av_comparison_RV.do
** use only the subset of parcels for which data on BOTH assessed value and sales price are available

clear
cd "/Users/huiling/Dropbox/Vacant Land"
capture log close 
log using aug242014_aug30Revist, replace
set more off



use aug12014_vacant.dta, clear
duplicates drop //1270 out of 585607 observations are duplicates 

*** UPdated on Aug 30, 2014 **** 
replace dvlp = 0 if lu_08 == 1800


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

** What exactly is the subset of parcels that have data on both sales price and assessed value?
gen insample = !missing(lnp) & !missing(lnv) & !missing(saleyr)

*********************  regression ********************
quietly reg lnp lna fsub cbd cbd2 fwy ocean ocean2 i.city_id i.lu_08 i.scag_gp_co i.saleyr if dvlp & insample
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



quietly reg lnp lna fsub cbd cbd2 fwy ocean ocean2 i.city_id i.lu_08 i.scag_gp_co i.saleyr if !dvlp & insample
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



quietly reg lnv lna fsub cbd cbd2 fwy ocean ocean2 i.city_id i.lu_08 i.scag_gp_co i.saleyr if dvlp & insample
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



quietly reg lnv lna fsub cbd cbd2 fwy ocean ocean2 i.city_id i.lu_08 i.scag_gp_co i.saleyr if !dvlp & insample
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

gen v2000_updated = v2000 / 1.02^7


format v2000_p v2000 v2000_updated shape_area %14.0f
save Aug302014_sp_av_results.dta, replace

/*
** Insert a new table 
use Aug302014_sp_av_results.dta, clear
gen nparcel = 1
gen nspav = insample
gen nsp = !missing(lnp)
gen nav = !missing(lnv)
gen nreg = insample & !missing(saleyr)

gen shape_area_sqft = shape_area * 10.7639104

bysort nspav: egen totarea_spav = total(shape_area_sqft) if nspav
bysort nspav: egen totarea_sp = total(shape_area_sqft) if nsp
bysort nspav: egen totarea_av = total(shape_area_sqft) if nav
bysort nspav: egen totarea_reg = total(shape_area_sqft) if nreg

format totarea* %14.0f

collapse (sum) nparcel nspav nsp nav nreg (mean) totarea_spav totarea_sp totarea_av totarea_reg, by(dvlp)
gen avgarea_spav = totarea_spav / nspav
gen avgarea_sp = totarea_sp / nsp
gen avgarea_av = totarea_av / nav
gen avgarea_reg = totarea_reg / nreg

outsheet using aug302014_sp_av_nparceltable.csv, comma replace
*/

** updated Table 18
use Aug302014_sp_av_results.dta, clear
keep if insample
gen nparcel = 1
gen shape_area_sqft = shape_area * 10.7639104
collapse (sum) v2000_p v2000_updated shape_area shape_area_sqft nparcel, by(dvlp)
format shape_area_sqft %14.0f
gen ratio = v2000_p / v2000_updated
outsheet using aug302014_sp_av_table18.csv, comma replace


** updated Table 19
use Aug302014_sp_av_results.dta, clear
keep if !insample
gen nparcel = 1
gen shape_area_sqft = shape_area * 10.7639104
collapse (sum) v2000_p v2000_updated shape_area shape_area_sqft nparcel, by(dvlp)
format shape_area_sqft %14.0f
gen ratio = v2000_p / v2000_updated
outsheet using aug302014_sp_av_table19.csv, comma replace


** updated Table 20
use Aug302014_sp_av_results.dta, clear
gen nparcel = 1
gen shape_area_sqft = shape_area * 10.7639104
collapse (sum) v2000_p v2000_updated shape_area shape_area_sqft nparcel, by(dvlp)
format shape_area_sqft %14.0f
gen ratio = v2000_p / v2000_updated
outsheet using aug302014_sp_av_table20.csv, comma replace


** Full regression coefs for Appendix B ***
 
label variable lna "ln (land area)"
label variable fsub "Distance to the nearest sub-center"
label variable cbd "Distance to the CBD"
label variable cbd2 "Squared distance to the CBD"
label variable fwy "Distance to the nearest freeway or highway"
label variable ocean "Distance to the nearest coast"
label variable ocean2 "Squared distance to the coast"
label variable saleyr "sale year "
label variable lu_08 "current land use "
label variable scag_gp_co "planned land use "


estout developable_lnp developable_lnv using aug312014_sp_av_regcoef_dvlp.doc, style(fixed) cells(b(star fmt(%9.3f))) replace stats(r2_a N, fmt(%9.3f %9.0g) labels(R-squared))legend label collabels(none) varlabels(_cons Constant) varwidth(35)
estout undevelopable_lnp undevelopable_lnv using aug312014_sp_av_regcoef_undvlp.doc, style(fixed) cells(b(star fmt(%9.3f))) replace stats(r2_a N, fmt(%9.3f %9.0g) labels(R-squared))legend label collabels(none) varlabels(_cons Constant) varwidth(35)




/*
** updated Table B1
use Aug302014_sp_av_results.dta, clear
keep if insample
gen nparcel = 1
gen shape_area_sqft = shape_area * 10.7639104
collapse (sum) v2000_p v2000_updated shape_area shape_area_sqft nparcel if dvlp, by(lu_08)
format shape_area_sqft %14.0f
gen ratio = v2000_p / v2000_updated
outsheet using aug302014_sp_av_tableB1.csv, comma replace

** updated Table B2
use Aug302014_sp_av_results.dta, clear
keep if insample
gen nparcel = 1
gen shape_area_sqft = shape_area * 10.7639104
collapse (sum) v2000_p v2000_updated shape_area shape_area_sqft nparcel if !dvlp, by(lu_08)
format shape_area_sqft %14.0f
gen ratio = v2000_p / v2000_updated
outsheet using aug302014_sp_av_tableB2.csv, comma replace
*/


/*
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
*/
