//Based on Oct 19, 2012


*** Goals Dec2, 2012
* Estimate the land value of non-vacant land by applying the regression coefficients from developable vacant land to their accessibility measures.


*** Update, Dec 8, 2012
* Use dummy variable regression instead of fixed effect regression, to generate coefficients on city dummies. 



* Run two separate regressions for developable and undevelopable vacant land, without current and planned land use codes as control variables.
* developability are based from LU_08 and Ross' calculations D:\summer, 2011\Yuntao\developability\



clear all
log close _all
* cd "/Users/huiling/Dropbox/Vacant Land" //notice the disk location change
cd "/Volumes/Sophia/summer project/summer, 2011_backup/Vacant Land"
log using dec_02_vacant, replace
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
//(note added Dec-10-2012: the total shape area of vacant land is 5.720e+10 square meters)
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



gen county = "LA" if mz >=1 & mz <= 46
replace county = "VT" if mz >=47 & mz <= 49
replace county = "OC" if mz >=50 & mz <= 66
replace county = "SB" if mz >=67 & mz <= 80
replace county = "RV" if mz >=81 & mz <= 95
replace county = "IM" if mz >=96 & mz <= 97



                                                                                   
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




fvset base 2000 saleyr 
fvset base 1700 lu_08
fvset base 1700 scag_gp_co


***************
* Land Area composition
***************
gen landuse =  "Residential" if lu_08 >= 1100 & lu_08 < 1200
replace landuse = "Commercial" if lu_08 >= 1200 & lu_08 < 1300
replace landuse = "Industrial" if lu_08 >= 1300 & lu_08 < 1400
replace landuse = "Transport_Communication_Utility" if lu_08 >= 1400 & lu_08 < 1500
replace landuse = "Mixed" if lu_08 >= 1500 & lu_08 < 1700
replace landuse = "Under_Construction" if lu_08 >= 1700 & lu_08 < 1800
replace landuse = "OpenSpace_Recreation" if lu_08 >= 1800 & lu_08 < 1900
replace landuse = "Agriculture" if lu_08 >= 2000 & lu_08 < 3000
replace landuse = "Vacant3000" if lu_08 >= 3000 & lu_08 < 4000
replace landuse = "Vacant3000_dvlp" if lu_08 >= 3000 & lu_08 < 4000 & dvlp
replace landuse = "Vacant3000_ndvlp" if lu_08 >= 3000 & lu_08 < 4000 & dvlp == 0
replace landuse = "Commercial" if lu_08 >= 1200 & lu_08 < 1300
replace landuse = "zero" if lu_08 == 0
replace landuse = "9999" if lu_08 == 9999



*********************
//developable vacant land 
*********************



/*
char city_name [omit] "Los Angeles"

xi: reg lgvsq fsub cbd fwy ocean i.saleyr i.city_name if !missing(saleyr) & dvlp
estimates save developable_dummy, replace

replace saleyr = 2000  
predict lgvsq_p if e(sample)

xi: reg lgvsq_p fsub cbd fwy ocean i.saleyr i.city_name if !missing(saleyr_bk) & dvlp
estimates save developable_dummy_nosaleyr, replace


replace saleyr = saleyr_bk
*/


xtreg  lgvsq  fsub cbd fwy ocean i. lu_08 i.saleyr i.scag_gp_co if !missing(saleyr) & dvlp, fe
estimates store developable
//estimates save developable, replace




levelsof saleyr if e(sample), local(lvsaleyr)
foreach lv of local lvsaleyr {
    capture replace lgvsq2000 =  lgvsq - _b[`lv'.saleyr] + _b[2000.saleyr]  if saleyr == `lv'  & e(sample)
}

replace saleyr = 2000 if e(sample) == 0 & dvlp
predict lgvsq_p if dvlp
replace saleyr = saleyr_bk
replace lgvsq2000 = lgvsq_p if e(sample) == 0 & dvlp
drop lgvsq_p


//without differentiating orginal or imputed, so this is the combined
gen v2000 =  exp(lgvsq2000) * shape_area * 10.7639104  

/*
if dvlp {                                                                                                 
gen v2000_0 =  exp(lgvsq2000) * shape_area * 10.7639104
gen v2000_1 = v2000_0 if !missing(totvalue07) & !missing(shape_area) & !missing(saleyr)
gen v2000_2 = v2000_0 if missing(v2000_1) & !missing(v2000_0)

gen shape_area_1 = shape_area if !missing(v2000_1)
gen shape_area_2 = shape_area if !missing(v2000_2)
}
*/



******************
//undevelopable vacant land
*****************


xtreg  lgvsq  fsub cbd fwy ocean i.saleyr i.lu_08 i.scag_gp_co if !missing(saleyr) & !dvlp, fe
estimates store undevelopable
//estimates save undevelopable, replace







levelsof saleyr if e(sample), local(lvsaleyr)
foreach lv of local lvsaleyr {
    capture replace lgvsq2000 =  lgvsq - _b[`lv'.saleyr] + _b[2000.saleyr]  if saleyr == `lv'  & e(sample)
}
replace saleyr = 2000 if e(sample) == 0 & !dvlp

predict lgvsq_p if !dvlp
replace saleyr = saleyr_bk
replace lgvsq2000 = lgvsq_p if e(sample) == 0 & !dvlp
drop  lgvsq_p


//As above in developable, without differentiating orginal or imputed, so this is the combined
replace v2000 =  exp(lgvsq2000) * shape_area * 10.7639104 if !dvlp

/*
if dvlp==0 {                                                                                                 
replace v2000_0 =  exp(lgvsq2000) * shape_area * 10.7639104 if !dvlp
replace v2000_1 = v2000_0 if !missing(totvalue07) & !missing(shape_area) & !missing(saleyr) & !dvlp
replace v2000_2 = v2000_0 if missing(v2000_1) & !missing(v2000_0) & !dvlp

replace shape_area_1 = shape_area if !missing(v2000_1) & !dvlp
replace shape_area_2 = shape_area if !missing(v2000_2) & !dvlp
*/




/*
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
*/



local landuse zero Agriculture OpenSpace_Recreation Under_Construction Vacant3000

foreach lc of local lc_landuse {
	forvalues i = 0/1 {
    	gen tsample = !missing(v2000) & dvlp == `i' & landuse == `lc'
    	bysort mz: egen area_total = total(shape_area) if tsample
    	by mz: egen val_total = total(v2000) if tsample
    	gen vsq_mz_`i'_`lc' = val_total / area_total  
    	drop area_total val_total
		drop tsample
	}
}



//save oct_14_vacant_results.dta, replace
save dec_09_vacant_results.dta, replace

collapse vsq_mz* (sum) shape_area , by(mz landuse)
//outsheet  mz vsq_mz*  shape_area* using oct_14_vacant.csv, comma replace
outsheet  mz vsq_mz*  shape_area using dec_09_vacant.csv, comma replace

** esttab developable undevelopable using dec_02_est_coef.rtf, replace keep(fsub cbd fwy ocean) mtitles(developable undevelopable)





  



