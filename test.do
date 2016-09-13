//calculate the # of parcels of each land use type in SCAG parcel database

//office

use five_parcelMZ_office.dta, clear
bysort mz: egen n = count(mz)
collapse n, by(mz)
outsheet using test.csv, comma replace


//non-residential non-vacant non-office
use "D:\summer, 2011\Second Project\practice\Six counties\debug\oct_10_five.dta", clear
gen industrial_warehousing = lu_08 < 1400 & lu_08 >= 1300
gen mixed = lu_08 >= 1500 & lu_08 <1700
gen public = lu_08 >= 1240 & lu_08 <1300
gen transport = lu_08 >= 1400 & lu_08 <1500
gen other_commercial = lu_08 == 1200 | (lu_08 >= 1230 & lu_08 <1240)
gen retail = lu_08 < 1230 & lu_08 >= 1220



foreach var of varlist industrial_warehousing mixed public transport  other_commercial  retail {
    preserve 
    drop if `var' == 0
    bysort mz: egen n = count(mz)
    collapse n, by(mz)
    outsheet using test_`var'.csv, comma replace
    restore
}




