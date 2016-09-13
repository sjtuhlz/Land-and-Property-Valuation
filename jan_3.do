//jan 3, 2012
//generate model zone level census data
//mobile homes & boat, RV are all treated as Multi

//Corrected an error in calculating scalar(xo_residential), and scalar(xr_residential)
//corrected an error in applying the above two scalars, it should be multiplication instead of division.
//corrected an error in calculating RVm, multiply zeta to Vom.  => rvm = (rom * pom + rrm * (1 - pom)) / (zeta * vom * pom + vrm * (1 - pom))  
//corrected an error in calculating RVs, multiply zeta to Vos

//adjusted quality differences between census and Scag database. lambda

capture log close
log using jan_3, replace
clear
clear matrix
set mem 1g
set more off

/*
cd "D:\summer, 2011\Third Project\model_zone_level_census"

insheet using "D:\summer, 2011\Third Project\model_zone_level_census\CensusTract_ModelZone.txt", clear
drop newmz 
rename id tractid //initial blank was lost due to unexpected exported as numeric format

drop if join_count == 0   
drop if mz > 95
capture rename tracitd tractid
merge 1:1 tractid using "D:\summer, 2011\Third Project\DT_download\DTDownload_la.dta", keep(master match)
capture drop cash* 
drop if _merge ==2
drop _merge*
merge 1:1 tractid using "D:\summer, 2011\Third Project\DT_download\DTDownload_vt.dta", update force
drop if _merge ==2
capture drop cash*  
drop _merge*
merge 1:1 tractid using "D:\summer, 2011\Third Project\DT_download\DTDownload_or.dta", update force
drop if _merge ==2
capture drop cash* 
drop _merge*
merge 1:1 tractid using "D:\summer, 2011\Third Project\DT_download\DTDownload_sb.dta", update force
drop if _merge ==2
capture drop cash*  
drop _merge*
merge 1:1 tractid using "D:\summer, 2011\Third Project\DT_download\DTDownload_rv.dta", update force
drop if _merge ==2
capture drop cash*  
drop _merge*
drop v6

foreach var of varlist  hrs hrm rrs rrm vos vom {
    gen `var'_n = real(`var')
    drop `var'
    rename `var'_n `var'
}


drop if mz == 0
gen total_vos = vos * hos
gen total_vom = vom * hom
gen total_rrs = hrs * rrs
gen total_rrm = hrm * rrm

collapse mz (sum) hos hom hrs hrm total_vos total_vom total_rrs total_rrm, by(name)
gen vos = total_vos / hos
gen vom = total_vom / hom
gen rrs = total_rrs / hrs
gen rrm = total_rrm / hrm

save censustract_modelzone_final.dta, replace
*/

cd  "D:\summer, 2011\Third Project\Final_data"
/*
use residential_six_final.dta, clear
drop hos hom hrs hrm vos vom rrs rrm
capture drop _merge
merge m:1 mz using "D:\summer, 2011\Third Project\model_zone_level_census\censustract_modelzone_final.dta", force
save residential_six_final_jan.dta, replace   //time sufix as jan means this is to be data file that tens of other do files projects would be based on in earlier January.
*/
/*
//fix the problem of majority parcels in la county misses shape_area
//jan3, 2012
use residential_six_final_jan.dta, clear
merge 1:1 scag_xyid using parcel_la_residential_final.dta, keepusing(shape_area) update
save residential_six_final_jan.dta, replace
*/

//=======================BEGIN =============

use residential_six_final_jan.dta, clear
//======================begin single========================= 
//data processing

drop if mz ==0 | missing(mz)
drop x y apn tractid lotsqft
capture drop _merge
replace sale_price = . if sale_price == 0 
replace floor_area = . if floor_area == 0
replace totvalue07 = . if totvalue07 == 0
replace shape_area = . if shape_area == 0
replace saleyr = . if saleyr == 1899 | saleyr == 0 | saleyr == 1900
gen saleyr_bk = saleyr
gen single = lu_08 >= 1110 & lu_08 < 1120
gen multi = (lu_08 >= 1120 & lu_08 < 1140)  //mobile homes were included in multi, mixed residential and rurual residential were temporarily discarded.
drop if single == 0 & multi == 0
sample 1, by (mz single )


gen lnf = ln(floor_area)
gen lgvsq = ln( sale_price / floor_area)
gen lgvsq_tot = ln( totvalue07 / floor_area)
gen lnp = ln(sale_price)
gen lnv = ln(totvalue07)
gen lnl = ln(shape_area)

gen pos = hos / (hos + hrs)  //percentage of single housing units that are owner occupied 
gen pom = hom / (hom + hrm) //percentage of multi housing units that are owner occupied 
gen gamma = 0.6 // A-2 : gamma = Rrs / Ros
gen ros = rrs /gamma
gen zeta = 0.9 //adjust owners overreport their housing values
gen thita = 0.8 // 80% of floor area of multi-unit housing is rentable
gen lambda = 2 //quality of census is twice high as quality of Scag parcel database

encode city , gen(citynum)
xtset citynum



//====  initial values 
gen insample = .
gen lnp2000 = .
gen lnv2000 = .
gen vs =.
gen vrs = .
gen rvs = .
gen v2000 = .
gen r2000 = .
gen v2000_tot = .

//=================== regression (Orange & Riverside)======================
replace insample = ((mz >= 50 & mz <=66) | (mz >= 81 & mz <=95)) & single  

//regression and imputation using SALE_PRICE
xtreg lnp lnf lnl fsub cbd fwy ocean i.saleyr i.lu_08 if insample & !missing(saleyr), fe
levelsof saleyr if e(sample), local(lvsaleyr)
foreach lv of local lvsaleyr {
    capture replace lnp2000 =  lnp - _b[`lv'.saleyr] + _b[2000.saleyr]  if saleyr == `lv'  & e(sample)
}
replace saleyr = 2000 if e(sample) == 0 & insample
predict lnp_p if insample
replace saleyr = saleyr_bk
replace lnp2000 = lnp_p if e(sample) == 0 & insample
drop  lnp_p

//regression and imputation using TOTVALUE07
xtreg lnv lnf lnl fsub cbd fwy ocean i.saleyr i.lu_08 if insample & !missing(saleyr), fe
levelsof saleyr if e(sample), local(lvsaleyr)
foreach lv of local lvsaleyr {
        capture replace   lnv2000 =  lnv - _b[`lv'.saleyr] + _b[2000.saleyr] if saleyr == `lv'  & e(sample)
    }
replace saleyr = 2000 if e(sample)==0  & insample
predict lnv_p if insample
replace lnv2000 = lnv_p if e(sample) == 0 & insample
drop  lnv_p

replace v2000 = exp(lnp2000)  if insample
replace v2000_tot = exp(lnv2000)  if insample

//compare
//----------scalar(xo_residential)
replace insample = (mz >= 50 & mz <=66) & single & !missing(v2000) & !missing(v2000_tot) 
egen tp = total(v2000) if insample
egen tv = total(v2000_tot) if insample
gen vp = tp / tv
sum vp, meanonly
scalar xo_single = r(mean)
drop tp tv vp 
//---------scalar(xr_residential)
replace insample = (mz >= 81 & mz <=95) & single & !missing(v2000) & !missing(v2000_tot) 
egen tp = total(v2000) if insample
egen tv = total(v2000_tot) if insample
gen vp = tp / tv
sum vp, meanonly
scalar xr_single = r(mean)
drop tp tv vp 

//========== LA, VT , SB ===========
replace insample = ((mz >= 1 & mz <=46) | (mz >= 47 & mz <=49) | (mz >= 67 & mz <=80)) & single   

xtreg lnv lnf lnl fsub cbd fwy ocean i.saleyr i.lu_08 if insample & !missing(saleyr), fe
levelsof saleyr if e(sample), local(lvsaleyr)
foreach lv of local lvsaleyr {
        capture replace  lnv2000 =  lnv - _b[`lv'.saleyr] + _b[2000.saleyr] if saleyr == `lv'  & e(sample)
    }
replace saleyr = 2000 if e(sample)==0  & insample
predict lnv_p if insample
replace lnv2000 = lnv_p if e(sample) == 0 & insample
drop  lnv_p

replace insample = ((mz >= 1 & mz <=46) | (mz >= 47 & mz <=49)) & single 
replace v2000 = exp(lnv2000)  * scalar(xo_single) if insample
replace insample = (mz >= 67 & mz <=80) & single
replace v2000 = exp(lnv2000)  * scalar(xr_single) if insample

//================================================= rent to value  ratioo  ==========================

levelsof mz, local(lv_mz)
foreach lv of local lv_mz {
    sum v2000 if mz == `lv' & single, meanonly
    replace vs = r(mean) if mz == `lv'
}

replace vrs = (lambda * vs - zeta * vos * pos) / (1 - pos)  
replace rvs = (ros * pos + rrs * (1 - pos)) / (zeta * vos * pos + vrs * (1 - pos))    

replace r2000 = v2000 * rvs * 12  //rents in Census is montly!

   
//======================================aggregate to model zone level ====================================

gen tsample_v = !missing(floor_area) &  !missing(v2000)  
gen tsample_r = !missing(floor_area) &  !missing(r2000)  
bysort mz: egen v_mz = total(v2000) if tsample_v
bysort mz: egen r_mz = total(r2000) if tsample_r
bysort mz: egen fa_mz_v = total(floor_area) if tsample_v
bysort mz: egen fa_mz_r = total(floor_area) if tsample_r
gen vsq_mz = v_mz / fa_mz_v
gen rsq_mz = r_mz / fa_mz_r
drop tsample_v tsample_r v_mz r_mz fa_mz_v fa_mz_r

sum 


preserve
drop if missing(vsq_mz) 
duplicates drop vsq_mz, force
outsheet  vsq_mz rsq_mz vos  vs vrs rrs ros  mz name using mz_single_jan3.csv, comma replace

restore
drop vsq_mz  rsq_mz


//======================begin multi========================= 
//====  initial values 
replace insample = .
gen vm =.
gen vrm = .
gen rom = .
gen rvm = .
replace v2000 = .
replace r2000 = .
replace v2000_tot = .

gen lgvsq2000 = .
gen lgvsq2000_tot = .
//=================== regression (Orange & Riverside)======================
replace insample = ((mz >= 50 & mz <=66) | (mz >= 81 & mz <=95)) & multi

//regression and imputation using SALE_PRICE
xtreg lgvsq  fsub cbd fwy ocean  i.saleyr  i.lu_08 if insample & !missing(saleyr), fe
levelsof saleyr if e(sample), local(lvsaleyr)
foreach lv of local lvsaleyr {
    capture replace lgvsq2000 =  lgvsq - _b[`lv'.saleyr] + _b[2000.saleyr]  if saleyr == `lv'  & e(sample)
}
replace saleyr = 2000 if e(sample) == 0 & insample
predict lgvsq_p if insample
replace saleyr = saleyr_bk
replace lgvsq2000 = lgvsq_p if e(sample) == 0 & insample
drop  lgvsq_p

//regression and imputation using TOTVALUE07
xtreg lgvsq_tot fsub cbd fwy ocean i.saleyr i.lu_08 if insample & !missing(saleyr), fe
levelsof saleyr if e(sample), local(lvsaleyr)
foreach lv of local lvsaleyr {
        capture replace  lgvsq2000_tot =  lgvsq_tot - _b[`lv'.saleyr] + _b[2000.saleyr] if saleyr == `lv'  & e(sample)
    }
replace saleyr = 2000 if e(sample)==0  & insample
predict lgvsq_p if insample
replace lgvsq2000 = lgvsq_p if e(sample) == 0 & insample
drop  lgvsq_p

replace v2000 = exp(lgvsq2000) * floor_area if insample
replace v2000_tot = exp(lgvsq2000_tot) * floor_area if insample

//compare
replace insample = (mz >= 50 & mz <=66) & multi & !missing(v2000) & !missing(v2000_tot) 
egen tp = total(v2000) if insample
egen tv = total(v2000_tot) if insample
gen vp = tp / tv
sum vp, meanonly
scalar xo_multi = r(mean)
drop tp tv vp 
//---------scalar(xr_residential)
replace insample = (mz >= 81 & mz <=95) & multi & !missing(v2000) & !missing(v2000_tot) 
egen tp = total(v2000) if insample
egen tv = total(v2000_tot) if insample
gen vp = tp / tv
sum vp, meanonly
scalar xr_multi = r(mean)
drop tp tv vp 

//========== LA, VT , SB ===========
replace insample = ((mz >= 1 & mz <=46) | (mz >= 47 & mz <=49) | (mz >= 67 & mz <=80)) & multi   

xtreg lgvsq_tot fsub cbd fwy ocean i.saleyr i.lu_08 if insample & !missing(saleyr), fe
levelsof saleyr if e(sample), local(lvsaleyr)
foreach lv of local lvsaleyr {
        capture replace  lgvsq2000_tot =  lgvsq_tot - _b[`lv'.saleyr] + _b[2000.saleyr] if saleyr == `lv'  & e(sample)
    }
replace saleyr = 2000 if e(sample)==0  & insample
predict lgvsq_p if insample
replace lgvsq2000_tot = lgvsq_p if e(sample) == 0 & insample
drop  lgvsq_p

replace insample = ((mz >= 1 & mz <=46) | (mz >= 47 & mz <=49)) & multi 
replace v2000 = exp(lgvsq2000_tot) * floor_area * scalar(xo_multi) if insample
replace insample = (mz >= 67 & mz <=80) & multi   
replace v2000 = exp(lgvsq2000_tot) * floor_area * scalar(xr_multi) if insample

//================================================= rent to value  ratioo  ==========================



replace vrm = vrs * rrm / rrs
replace rom = vom * ros / vos 
replace rvm = (rom * pom + rrm * (1 - pom)) / (zeta * vom * pom + vrm * (1 - pom))  
replace r2000 = v2000 * rvm * 12  //rents in Census is montly!

   
//======================================aggregate to model zone level ====================================

gen tsample_v = !missing(floor_area) &  !missing(v2000)  
gen tsample_r = !missing(floor_area) &  !missing(r2000)  
bysort mz: egen v_mz = total(v2000) if tsample_v
bysort mz: egen r_mz = total(r2000) if tsample_r
bysort mz: egen fa_mz_v = total(floor_area) if tsample_v
bysort mz: egen fa_mz_r = total(floor_area) if tsample_r
gen vsq_mz = v_mz / fa_mz_v
gen rsq_mz = r_mz / fa_mz_r
drop tsample_v tsample_r v_mz r_mz fa_mz_v fa_mz_r 


sum 

save jan_3_five_residential.dta, replace

drop if missing(vsq_mz) 
duplicates drop vsq_mz, force
outsheet  vsq_mz rsq_mz mz vom vrm rrm rom name using mz_multi_jan3.csv, comma replace





