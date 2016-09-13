
clear all
log close _all
log using jun_27_vacant.txt, replace
set more off
set mem 1g
cd "I:\summer project\summer, 2011_backup\Vacant Land"
/*
/*
insheet using "I:\summer project\summer, 2011_backup\zone\Vacant_3000\vacant3000_fsub_cbd3.csv", clear

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

use SIX_merge.dta, clear
append using vacant3000_fsub_cbd3.dta, force
//there are serious duplication in scag_xyid in the 3*** data, will look at it later



//note that Und_VTSBRVOCLAIM_2.dta has no duplicate scag_xyid.

merge m:1 scag_xyid using "D:\summer, 2011\Yuntao\Und_VTSBRVOCLAIM_2.dta", force
drop if _merge == 2  //appeared only in using dataset (# = 96341/581024 = 17%, this is because of LAX_ATT.dbf selection based on lu08 and LAparcelMZ.dbf selection based on lu_08, so double selection reduced sample size)
//data processing in this part has been started, and waiting to be finished in February 

replace dvlp = 0 if dvlp == 1   //miscoding of develpability in original data
replace dvlp = 1 if missing(dvlp)
replace dvlp = 0 if lu_08 >= 1800 & lu_08 < 1900 & lu_08 != 1810 & lu_08 != 1840


save jan_20_vacant.dta, replace  

*/                                                          

//use jan_20_vacant.dta, clear                                                                                      
replace fwy = fwy * 0.00062137
replace ocean = ocean * 0.00062137
replace totvalue07 = . if totvalue07 == 0
replace shape_area = . if shape_area == 0                            
replace saleyr = . if saleyr == 1899 | saleyr == 0 
gen saleyr_bk = saleyr
//replace  scag_gp_co = . if  scag_gp_co == 0 | scag_gp_co == 9999 

gen lgvsq = ln( totvalue07/ shape_area * 0.09290304)      
gen lgvsq2000 = .
encode city_name , gen(citynum)
xtset citynum

//label variables
label variable fsub "Distance to the nearest subcenter"
label variable cbd "Distance to CBD"
label variable fwy "Distance to the nearest major road"
label variable ocean "Distance to the nearest coast"


//exclude top and bottom 1% of points to exclude outliers. 
egen n = count(lgvsq)
egen r = rank(lgvsq)
gen rp = r/n

preserve
drop if rp < 0.05 | rp > 0.95  //a crude way of eliminating outliers

//developable as a dummy explanatory variable
xtreg  lgvsq  fsub cbd fwy ocean i.dvlp i.saleyr i.lu_08 i.scag_gp_co if !missing(saleyr), fe
estimates store dvlp_dummy


levelsof saleyr if e(sample), local(lvsaleyr)
foreach lv of local lvsaleyr {
    capture replace lgvsq2000 =  lgvsq - _b[`lv'.saleyr] + _b[2000.saleyr]  if saleyr == `lv'  & e(sample)
}
replace saleyr = 2000 if e(sample) == 0  
predict lgvsq_p if dvlp
replace saleyr = saleyr_bk
replace lgvsq2000 = lgvsq_p if e(sample) == 0
drop  lgvsq_p   
                                                                                                 
gen v2000_0 =  exp(lgvsq2000) * shape_area * 10.7639104
gen v2000_1 = v2000_0 if !missing(totvalue07) & !missing(shape_area) & !missing(saleyr)
gen v2000_2 = v2000_0 if missing(v2000_1) & !missing(v2000_0)


gen shape_area_1 = shape_area if !missing(v2000_1)
gen shape_area_2 = shape_area if !missing(v2000_2)



// i indexes dvlp (i=1 means developable), while j indexes calculated from original data or not ( 0 = combined,  1 = original ,2 = predicted)

forvalues j = 0/2 {
    gen tsample_`j' = !missing(v2000_`j')
    forvalues i = 0/1 {
        gen tsample = tsample_`j' & dvlp == `i'
        bysort mz: egen area_total = total(shape_area) if tsample
        by mz: egen val_total = total(v2000_`j') if tsample
        gen vsq_mz_`i'_`j' = val_total / area_total  
        drop area_total val_total tsample
    }
    
}





//collapse vsq_mz* if !missing(vsq_mz_1) & !missing(vsq_mz_0), by(mz)
collapse vsq_mz* (sum) shape_area* , by(mz)


* mat vsq_result = (mz, name, vsq_mz* ,shape_area*)
* outtable using apr_29_result.tex, mat(vsq_result)


//esttab using apr_29_vacant_dvlp_dummy.tex, keep(fsub cbd fwy ocean i.dvlp) addnotes()



