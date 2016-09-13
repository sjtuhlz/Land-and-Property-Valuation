*******
* This version uses xi: i.dummy to see if there is any differnce 
*******
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


gen lnv2000 = .
gen v2000 = totvalue07


*********************  regression ********************
**** Developable vacant land 


**** Method 1: uses xi: reg ******
char city_name [omit] "Unknown"

xi: quietly reg lnv lna  fsub cbd cbd2 fwy ocean ocean2 i.city_name i.saleyr i.lu_08 i.scag_gp_co if dvlp
estimates store dvlp_lnv_aug242014_xi
estimates save dvlp_lnv_aug242014_xi, replace


levelsof saleyr if e(sample), local(lvsaleyr)
foreach lv of local lvsaleyr {
    capture replace lnv2000 =  lnv - _b[_Isaleyr_`lv'] + _b[_Isaleyr_2000]  if saleyr == `lv'  & e(sample)
}

replace lnv2000 = lnv + _b[_Isaleyr_2000] if saleyr == 1980 & e(sample)

foreach var of varlist _Isaleyr_* {
	replace `var' = 0
}
	

replace _Isaleyr_2000 = 1
predict lnv_p if dvlp




replace lnv2000 = lnv_p if e(sample) == 0 & dvlp
drop lnv_p

gen v2000_xi = exp(lnv2000)


***** Method 2: uses factor variable *****
fvset base 2000 saleyr
quietly reg lnv lna fsub cbd cbd2 fwy ocean ocean2 i.city_id i.lu_08 i.scag_gp_co i.saleyr if dvlp
estimates store developable_lnv_aug242014
estimates save developable_lnv_aug242014, replace
replace saleyr = 2000
predict lnv_p
replace v2000 = exp(lnv_p) if dvlp & !e(sample)
replace saleyr = saleyr_bk
levelsof saleyr if e(sample), local(lv_saleyr)
foreach lv of local lv_saleyr {
	replace v2000 = v2000 / exp(_b[`lv'.saleyr]) if saleyr == `lv' & e(sample)
}
drop lnv_p



