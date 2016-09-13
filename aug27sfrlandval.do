cd "/Users/huiling/Dropbox/Vacant Land"

capture log close 
log using aug27sflandval, replace
set more off

******* Characteristics **************
* Impute the aggregate land value for all SFR parcels of which data on floor area is available
* In order to compute the correct ratio of ALVsfr / APVsfr 

*****************************************
clear
clear matrix
set mem 1g


** use "/Users/huiling/Dropbox/Vacant Land/jan_20_vacant.dta" if dvlp, clear 
use if dvlp using "/Users/huiling/Dropbox/Vacant Land/aug12014_vacant.dta", clear 
drop if mz >= 96 & mz <= 97                                                                                    
replace fwy = fwy * 0.00062137
replace ocean = ocean * 0.00062137
replace totvalue07 = . if totvalue07 == 0
replace shape_area = . if shape_area == 0                            
replace saleyr = . if saleyr == 1899 | saleyr == 0 
** remove parcels that have sale year before 1980 (added on Aug27, 2014)
replace saleyr = . if saleyr < 1980

gen lnv = ln( totvalue07)
gen lna = ln( shape_area)      

gen cbd2 = cbd^2
gen ocean2 = ocean^2

// to apply the coefficients to other land uses, cannot have current and planned land uses.
* xtreg  lgvsq  fsub cbd fwy ocean i.saleyr i.lu_08 i.scag_gp_co if !missing(saleyr) & dvlp, fe
* xi: reg lnv lna fsub cbd cbd2 fwy ocean ocean2 i.city_name i.saleyr if !missing(saleyr)
//use scag_gp_co as lu_08 for developed parcels
* char _dta[omit] "prevelant"
* xi: reg lnv lna fsub cbd cbd2 fwy ocean ocean2 i.city_name i.saleyr i.scag_gp_co if !missing(saleyr)
encode city_name, gen(city_n)
reg lnv lna fsub cbd cbd2 fwy ocean ocean2 i.city_n i.saleyr i.scag_gp_co if !missing(saleyr)
estimates save developable_tool_lnv, replace  //note the difference from standard dsevelopable coefficients

collapse city_n, by(city_name)
save cityid_name_correspondence.dta, replace


******************************
** Part 1: residential
******************************


use fsub cbd fwy ocean city  shape_area floor_area lu_08 mz saleyr sale_price using "/Users/huiling/Documents/summer, 2011/Third Project/Final_data/residential_six_final_3category.dta", clear
keep if lu_08 >= 1110 & lu_08 < 1120 //sfr parcels only 
rename city city_name 
gen cbd2 = cbd^2
gen ocean2 = ocean^2
rename lu_08 scag_gp_co
replace shape_area = . if shape_area == 0
gen lna = ln(shape_area)

merge m:1 city_name using cityid_name_correspondence.dta
tab city_name if _merge == 1
replace city_n = 89 if _merge == 1 // los angeles city (BE CAREFUL)
drop if _merge == 2


********** Impute missing County ****************
gen county = ""
replace county = "LA" if mz >= 1 & mz <= 46
replace county = "VT" if mz >= 47 & mz <= 49
replace county = "OR" if mz >= 50 & mz <= 66
replace county = "SB" if mz >= 67 & mz <= 80
replace county = "RV" if mz >= 81 & mz <= 95
replace county = "IM" if mz >= 96 & mz <= 97


//Infer missing counties from city_name
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
replace county_s = "LA" if city_name == "Avalon" | city_name == "Cerritos"| city_name == "La Mirada" | city_name == "Lakewood" 
replace county_s = "LA" if city_name == "Long Beach" | city_name == "Los Angeles"
replace county_s = "SB" if city_name == "Chino" | city_name == "Colton" | city_name == "Fontana" | city_name == "Yucaipa" | city_name == "Montclair"
replace county_s = "OR" if city_name == "La Habra" | city_name == "Los Alamitos" 
replace county_s = "VT" if city_name == "Thousand Oaks"





drop county
rename county_s county

drop if county == "IM"
**************************************************




/*
// be careful with large scale missing shape_area in la county
replace shape_area = . if shape_area == 0
count if missing(shape_area)
* weird, this problem is gone. 
*/



//potential problem: some city may not have vacant land, so their coefs may be unknown
//in vacant sample, there are 176 unique cities, while in residential sample there are 167
estimates use developable_tool_lnv
replace saleyr = 2000


predict lnv2000
gen landval = exp(lnv2000) 
capture drop insample
replace floor_area = . if floor_area == 0
gen insample = !missing(floor_area)
save aug27landval_residential.dta, replace
keep if insample

bysort county: egen nparcel = count(fsub)
collapse (mean) nparcel (sum) landval shape_area, by(county)








