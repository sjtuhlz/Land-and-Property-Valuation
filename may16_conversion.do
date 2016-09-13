//May 16, 2013
//Converting UTM east,north coordinates to latitude,longtitude coordinates

* start with vacant parcel regression to see if there is predicted value changes.

clear
cd "/Users/huiling/Dropbox/Vacant Land"
capture log close 
log using may16_conversion, replace
use jan_20_vacant.dta, clear
format scag_xyid %14.0f
outsheet x y scag_xyid using coord_vacant.txt, replace

drop x y near* 
outsheet using nocoord_vacant.csv, comma replace

notes x: any parcel that has a UTM x coordinate equal to zero also has shape_area equals to zero. \\\
About 2% (10649 out of 48468) of vacant parcels.

notes y: none of the parcel misses a y coordinate (what problem?)

//conversion done in matlab using may16_conversion.m, output is latlon_vacant.txt
//vacant_may16.txt is the updated final file with all necessary variables for regression, and coordiantes in lat,lon format



** problem with the three header strings end up as one string (not solved)
insheet using vacant_may16.txt, clear
save vacant_may16.dta, replace

** use comma separated .csv files instead
insheet using myfile.csv, comma clear
save myfile_may16.dta, replace




/* spatwmat command cannot deal with large-size matrix -> move to Matlab
drop if x == 0
sum x y 
dis sqrt((35.809 - 32.655)^2 + (-114.14 - -119.48)^2) // = 6.20188
spatwmat, name(weights) xcoord(x) ycoord(y) band(0 3)
*/

/*
//prepare for matlab input 
encode city_name, gen(city_id)
gen city_n = city_id
drop name city_name city_id city
destring apn, gen(apn_n) force
drop apn
drop objectid
outsheet using jun16.txt, replace
*/


gen lnp = ln(totvalue07)
replace lnp = . if lnp == 0
gen lna = ln(shape_area)

gen insample = x!= 0 & !missing(lnp) & !missing(lnp) 


//take a random 1/100 sample
* sample 0.1 if insample, by(city_id)
sample 1, by(insample)
sample 10, by(insample)
spatwmat, name(W) xcoord(x) ycoord(y) band(0 3) eigenval(E) 



spatreg lnp lna fsub  cbd fwy ocean, weights(W) eigenval(E) model(error)

