//Oct 14, 2012


******* Characteristics **************
* (1) classify vacant land into developable & undevelopable, and run TWO separate regressions (from jan_20_vacant.do)
* (2) prepare comparastatics table vsq_ij, where i=dvlp(1yes,0no), j=combined(0)/original(1)/imputed(2) (from apr_29_vacant.do)


* Below was done on Oct 28, 2012
* (3) calcuate total shape area by developability, which was done previously by original/imputed.  
*****************************************




* developability are based from LU_08 and Ross' calculations D:\summer, 2011\Yuntao\developability\



clear all
log close _all
* cd "C:\Users\Huiling\Desktop\Vacant Land" //notice the disk location change
cd "/Users/huiling/Dropbox/Vacant Land" //notice the disk location change
log using oct_14_vacant, replace
set more off
set mem 800m


/*
insheet using "I:\summer project\summer, 2011_backup\zone\Vacant_3000\vacant3000_fsub_cbd3.csv", clear
//in this 3*** vacant parcels' data processing in ArcGis, LAX_ATT served as input tables, while LAparcelMZ served as join tables. So scagxyid and lu08 were used as primary indicator of selection, compared to scag_xyid and lu_08 of non 3*** vacant parcels. Now just put this matter away, and assume that they are the same.
capture rename freeway fwy
drop scag_xyid lu_08
rename scagxyid  scag_xyid 
rename lu08 lu_08
save vacant3000_fsub_cbd3.dta, replace


insheet using "I:\summer project\summer, 2011_backup\zone\join2_backup\Keep_all_records\SIX_merge.csv", clear
capture rename freeway fwy
capture drop scagxyid
capture drop lu08
destring scag_xyid, replace force
save SIX_merge.dta, replace
*/


/*
use SIX_merge.dta, clear
append using vacant3000_fsub_cbd3.dta, force
//there are serious duplication in scag_xyid in the 3*** data, will look at it later
duplicates tag scag_xyid, gen(dup)
drop if dup != 0
drop dup

merge 1:1 scag_xyid using "D:\summer, 2011\Yuntao\Und_VTSBRVOCLAIM_2.dta", keepusing(dvlp) force

drop if _merge == 2  //appeared only in using dataset (# = 96341/581024 = 17%, this is because of LAX_ATT.dbf selection based on lu08 and LAparcelMZ.dbf selection based on lu_08, so double selection reduced sample size)
//data processing in this part has been started, and waiting to be finished in February 


replace dvlp = 0 if dvlp == 1   //miscoding of develpability in original data
replace dvlp = 1 if missing(dvlp)
replace dvlp = 0 if lu_08 >= 1800 & lu_08 < 1900 & lu_08 != 1810 & lu_08 != 1840

save jan_20_vacant.dta, replace                                                            
*/


use jan_20_vacant.dta, clear                                                                                      
replace fwy = fwy * 0.00062137
replace ocean = ocean * 0.00062137
replace totvalue07 = . if totvalue07 == 0
replace shape_area = . if shape_area == 0                            
//replace  scag_gp_co = . if  scag_gp_co == 0 | scag_gp_co == 9999 
replace saleyr = . if saleyr == 1899 | saleyr == 0 
gen saleyr_bk = saleyr

gen lgvsq = ln( totvalue07/ shape_area*0.09290304)      
gen lgvsq2000 = .
encode city_name , gen(citynum)
xtset citynum



*********************
//developable vacant land 
*********************
xtreg  lgvsq  fsub cbd fwy ocean i.saleyr i.lu_08 i.scag_gp_co if !missing(saleyr) & dvlp, fe
estimates store developable

levelsof saleyr if e(sample), local(lvsaleyr)
foreach lv of local lvsaleyr {
    capture replace lgvsq2000 =  lgvsq - _b[`lv'.saleyr] + _b[2000.saleyr]  if saleyr == `lv'  & e(sample)
}

replace saleyr = 2000 if e(sample) == 0 & dvlp
predict lgvsq_p if dvlp
replace saleyr = saleyr_bk
replace lgvsq2000 = lgvsq_p if e(sample) == 0 & dvlp
drop lgvsq_p


gen shape_area_dvlp = shape_area if dvlp
gen shape_area_ndvlp = shape_area if dvlp == 0


if dvlp {                                                                                                 
gen v2000_0 =  exp(lgvsq2000) * shape_area * 10.7639104
gen v2000_1 = v2000_0 if !missing(totvalue07) & !missing(shape_area) & !missing(saleyr)
gen v2000_2 = v2000_0 if missing(v2000_1) & !missing(v2000_0)

gen shape_area_1 = shape_area if !missing(v2000_1)
gen shape_area_2 = shape_area if !missing(v2000_2)
}









******************
//undevelopable vacant land
*****************
xtreg  lgvsq  fsub cbd fwy ocean i.saleyr i.lu_08 i.scag_gp_co if !missing(saleyr) & !dvlp, fe
estimates store undevelopable

levelsof saleyr if e(sample), local(lvsaleyr)
foreach lv of local lvsaleyr {
    capture replace lgvsq2000 =  lgvsq - _b[`lv'.saleyr] + _b[2000.saleyr]  if saleyr == `lv'  & e(sample)
}
replace saleyr = 2000 if e(sample) == 0 & !dvlp

predict lgvsq_p if !dvlp
replace saleyr = saleyr_bk
replace lgvsq2000 = lgvsq_p if e(sample) == 0 & !dvlp
drop  lgvsq_p


*if dvlp==0 {                                                                                                 
replace v2000_0 =  exp(lgvsq2000) * shape_area * 10.7639104 if !dvlp
replace v2000_1 = v2000_0 if !missing(totvalue07) & !missing(shape_area) & !missing(saleyr) & !dvlp
replace v2000_2 = v2000_0 if missing(v2000_1) & !missing(v2000_0) & !dvlp

replace shape_area_1 = shape_area if !missing(v2000_1) & !dvlp
replace shape_area_2 = shape_area if !missing(v2000_2) & !dvlp
*}




forvalues j = 0/2 {
    gen tsample_`j' = !missing(v2000_`j')
    forvalues i = 0/1 {
        gen tsample = tsample_`j' & dvlp == `i'
        bysort mz: egen area_total = total(shape_area) if tsample
        by mz: egen val_total = total(v2000_`j') if tsample
        gen vsq_mz_`i'_`j' = val_total / area_total  
        drop area_total val_total
	drop tsample
    }
    
}






save oct_14_vacant_results.dta, replace


collapse vsq_mz* (sum) shape_area* , by(mz)
outsheet  mz vsq_mz*  shape_area* using oct_14_vacant.csv, comma replace




