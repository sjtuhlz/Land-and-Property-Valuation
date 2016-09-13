

*** Goal
** impute the land values of non-vacant parcels using their accessibility measures and the estimated coefficients from developable vacant land

clear all
log close _all
* cd "/Users/huiling/Dropbox/Vacant Land" //notice the disk location change
cd "/Volumes/Sophia/summer project/summer, 2011_backup/Vacant Land"
log using dec_02_nonvacant, replace
set more off

/*
insheet using "/Volumes/Sophia/summer project/summer, 2011_backup/yuntao_guo/six_county_access.txt", clear
* the above data file includes vacant land uses



rename near_ro_di fwy
rename near_co_di ocean



replace lu_08 = 1241 if lu_08 == 12410
// quite some parcels have lu_08 == 0 or 8888, 9999
// 26008 parcels out of 4692056 (ratio = .0055) have mz == 0


gen saleyr = 2000




estimates use developable

predict lgvsq2000
gen landval = exp(lgvsq2000) * shape_area

save dec04.dta, replace

*/

use dec04.dta, clear





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







