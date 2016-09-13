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
set more off
** use city_name instead of city, as the former has far less missing 

*gen rdn = runiform()
*keep if rdn < 0.01




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
replace county_s = "LA" if city_name == "Avalon" | city_name == "Cerritos" | city_name == "Lakewood"	
replace county_s = "SB" if city_name == "Chino" | city_name == "Colton" | city_name == "Fontana" | city_name == "Yucaipa" | city_name == "Ontario"
replace county_s = "OR" if city_name == "La Habra" | city_name == "Los Alamitos"

drop county
rename county_s county

	 


//remove outliers 
* shape_area too small and value per square foot too large
* gen insample = shape_area > 10 & vsq < 10000 

// last sale year earlier than 1964 (# of parcels less than a hundred)
* replace insample = insample & saleyr >= 1964
* gen insample = saleyr >= 1980 & !missing(saleyr) 
gen insample = !missing(saleyr)

gen lnv2000 = .





/*
// use two-digit land use instead of three-digit land use 
gen lu08_2d = lu_08
replace lu08_2d = 2600 if lu_08 == 2000 
replace lu08_2d = 3100 if lu_08 == 3000
*/



drop if county == "IM"






**** Developable vacant land 

char city_name [omit] "Unknown"
char saleyr [omit] 2000




xi: reg lnv lna  fsub cbd cbd2 fwy ocean ocean2 i.city_name i.saleyr i.lu_08 i.scag_gp_co if insample & dvlp
estimates store developable
estimates save developable, replace

*levelsof saleyr if e(sample), local(lvsaleyr)
levelsof saleyr if e(sample) & saleyr != 2000, local(lvsaleyr)
foreach lv of local lvsaleyr {
    *capture replace lnv2000 =  lnv - _b[`lv'.saleyr] + _b[2000.saleyr]  if saleyr == `lv'  & e(sample)
    *replace lnv2000 =  lnv - _b[_Isaleyr_`lv'] + _b[_Isaleyr_2000]  if saleyr == `lv'  & e(sample)
    replace lnv2000 =  lnv - _b[_Isaleyr_`lv']  if saleyr == `lv'  & e(sample)
}


foreach var of varlist _Isaleyr_* {
	replace `var' = 0
}
	

*replace _Isaleyr_2000 = 1
predict lnv_p if dvlp




replace lnv2000 = lnv_p if e(sample) == 0 & dvlp
drop lnv_p







***** Undevelopable vacant land 

char city_name [omit] "Unknown"
char saleyr [omit] 2000

xi: reg lnv lna  fsub cbd cbd2 fwy ocean ocean2 i.city_name i.saleyr i.lu_08 i.scag_gp_co if insample & !dvlp 
estimates store undevelopable
estimates save undevelopable, replace


*levelsof saleyr if e(sample), local(lvsaleyr)
levelsof saleyr if e(sample) & saleyr != 2000, local(lvsaleyr)
foreach lv of local lvsaleyr {
    *capture replace lnv2000 =  lnv - _b[`lv'.saleyr] + _b[2000.saleyr]  if saleyr == `lv'  & e(sample)
    *replace lnv2000 =  lnv - _b[_Isaleyr_`lv'] + _b[_Isaleyr_2000]  if saleyr == `lv'  & e(sample)
    replace lnv2000 =  lnv - _b[_Isaleyr_`lv']  if saleyr == `lv'  & e(sample)
}


foreach var of varlist _Isaleyr_* {
	replace `var' = 0
}


*replace _Isaleyr_2000 = 1
predict lnv_p if !dvlp


replace lnv2000 = lnv_p if e(sample) == 0 & !dvlp
drop lnv_p


gen v2000 = exp(lnv2000)


estout developable undevelopable, cells(b) keep(_Isaleyr*)

save july172013_results.dta, replace




collapse (sum) v2000 shape_area, by(county dvlp)
gen vsq2000 = v2000/shape_area
table county dvlp, c(mean vsq2000)

stop


collapse (sum) v2000 shape_area, by(county lu_08 dvlp)
gen vsq2000 = v2000/shape_area
format vsq2000 %14.02f
table lu_08 county if dvlp, c(mean vsq2000)
table lu_08 county if !dvlp, c(mean vsq2000)



*** Percentage of land uses (Sep 25, 2013) **** 
use myfile_may16.dta, clear
table lu_08, c(sum shape_area)


use july172013_results.dta, clear
table lu_08 county, c(sum shape_area)
collapse (sum) shape_area, by(lu_08 county)
bysort county: egen totarea = total(shape_area)
gen pct_area = shape_area / totarea
table lu_08 county, c(mean pct_area)

* residential
use "/Users/huiling/Documents/summer, 2011/Third Project/Final_data/residential_six_final_3category_unix.dta"
collapse (sum) shape_area, by(lu_08 county)
bysort county: egen totarea = total(shape_area)
gen pct_area = shape_area / totarea
table lu_08 county, c(mean pct_area)

* non-residential non-vacant 
use "/Users/huiling/Dropbox/Vacant Land/aggregate_land_values/six_counties_nonresidential_nonvacant.dta"
* note that the non-residential non-vacant includes also 4*** land use categories. 
drop if lu_08 >= 4000 
collapse (sum) shape_area, by(lu_08 county)
bysort county: egen totarea = total(shape_area)
gen pct_area = shape_area / totarea
table lu_08 county, c(mean pct_area)
