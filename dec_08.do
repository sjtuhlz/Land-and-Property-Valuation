

*** Goal
* (1) impute the land values of non-vacant parcels using their accessibility measures and the estimated coefficients from developable vacant land





*** Potential problems *****
* (1) "Fixed effects model" does not generate coefficients for group dummies, which would yield wrong results if used in prediction.
* (2) Some city does not have vacant land, so its coefficients on city dummy is unavailable. (160 cities in developable vacant sample, compared to 192 cities in vacant sample, compared to 195 cities in all sample)
* (3) In the original estimates of developable vacant land, there are saleyr dummies coefficients, but now we don't have any in the data file (b/c we don't care at all), but then applying the estimate stored to 
* current data file results in erorr msg.


* ===> Solution 1: For saleyr, generate artificial dummies _Isaleyr1920 ~ _Isalyer2008, b/c I don't care about saleyr at all, if only I can elimiate those coefficients from the saved estimate !!!
* 					For city, to avoid the trouble of picking which cities are included in the developable sample and which are not, simply do prediction over all parcels (including vacant developable parcels)




clear all
log close _all
cd "/Users/huiling/Documents/summer, 2011/Vacant Land" //notice the disk location change
* cd "/Volumes/Sophia/summer project/summer, 2011_backup/Vacant Land"
log using dec_08, replace
set more off




insheet using "/Volumes/Sophia/summer project/summer, 2011_backup/yuntao_guo/six_county_access_update.txt", clear
* the above data file includes vacant land uses
* Note that there is no duplicate in scag_xyid in this file. (surprising!)
save six_county_access_update.dta, replace


rename near_ro_di fwy
rename near_co_di ocean

gen landuse_type = "vacant" if lu_08 >= 1700 & lu_08 <4000
replace landuse_type = "water" if lu_08 >4000 & lu_08 < 5000
replace landuse_type = "9999" if lu_08 == 9999
replace landuse_type = "0" if lu_08 == 0
replace landuse_type = "non_vacant" if missing(landuse_type)


bysort landuse_type: egen area_type = total(shape_area)


replace lu_08 = 1241 if lu_08 == 12410
// quite some parcels have lu_08 == 0 or 8888, 9999
// 26008 parcels out of 4692056 (ratio = .0055) have mz == 0

drop if landuse_type == "water" | landuse_type == "9999"

gen saleyr = 2000
rename city city_name
char city_name [omit] "Los Angeles"
xi i.city_name 


/*
forvalues i = 1920(1)2008 {
	gen _Isaleyr_`i' = 0
}
*/

gen _Icity_name_103 = 0




//estimates use developable
//estimates use developable_dummy
estimates use developable_dummy_nosaleyr
predict lgvsq2000 

//	keep if landuse_type == "non_vacant" | lu_08 == 0
keep if landuse_type == "non_vacant"

//1 square meter = 10.7639 square feet(shape_area unit: square meter)
//1 square feet = 0.092903 square meter

gen landval = exp(lgvsq2000) * shape_area * 10.7639

//save dec08.dta, replace

collapse (sum) landval (sum) shape_area, by(county)
format landval shape_area %16.0fc









/*
**** collapse at model zone level *****
collapse (sum) landval (sum) shape_area, by(mz)
outsheet using dec04_test1.csv, comma replace


use dec04.dta, clear

collapse (count) nn=scag_xyid (mean) landval shape_area, by(mz)
gen tot_landval = nn * landval
gen tot_area = nn * shape_area

outsheet using dec04_test2.csv, replace
*/







