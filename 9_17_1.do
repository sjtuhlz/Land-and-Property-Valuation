// Riverside 



clear
scalar drop _all 
macro drop _all 
log close _all
//log using Sep16_rv
log using Sep16_rv, replace

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~coeffs for saleyr < 1989 ~~~~~~~~~~~~~~~~~~~~~~~~~~~
cd "D:\summer, 2011\Second Project\office parcels"
insheet using "D:\summer, 2011\Second Project\office parcels\merge_la_or_rv_office_9_16.csv"



gen age = 2000 - yearbuilt if yearbuilt != 0
gen floor_area = impsqft if impsqft != 0
replace saleyr = . if saleyr == 1899 | saleyr == 0
replace freeway = freeway * 0.00062137
replace ocean = ocean * 0.00062137
replace sale_price = . if sale_price == 0
gen lgvsqft = ln(sale_price / floor_area)
gen insample = !missing(sale_price) & !missing(saleyr) & !missing(age) &!missing(floor_area)

char saleyr [omit] 1989
xi: quietly reg lgvsqft fsub cbd freeway ocean age i.saleyr if insample

summarize saleyr if saleyr < 1989, meanonly
local yrmin = r(min)
local yrmax = r(max)
foreach var of varlist _Isaleyr_`yrmin' - _Isaleyr_`yrmax' {
	scalar yr`var' = _b[`var']
}


//=========== adjust quality differences of value
replace insample = 0 if saleyr < 1989

scalar yr1989 = 0
scalar yr1990 = 0.1247
scalar yr1991 = -0.077694
scalar yr1992 = -0.154
scalar yr1993 = -0.74709
scalar yr1994 = -0.74
scalar yr1995 = -0.5522
scalar yr1996 = -0.4904
scalar yr1997 = -0.277246
scalar yr1998 = -0.12674
scalar yr1999 = 0.11867
scalar yr2000 = 0.088
scalar yr2001 = 0.09285
scalar yr2002 = 0.22017
scalar yr2003 = 0.39518
scalar yr2004 = 0.5341
scalar yr2005 = 0.7559
scalar yr2006 = 1.3228
scalar yr2007 = 1.7942
scalar yr2008 = 0.39667
scalar c_v = 5.262
scalar age_v = -0.0074416
scalar cbd_v = -0.013794
scalar fsub_v = -0.016555
scalar ocean_v = -0.012215
scalar fwy_v = -0.048755

gen sp2000 = sale_price * exp(scalar(yr2000)) if insample
gen lgvsqft_i = scalar(c_v) + scalar(age_v) * age + scalar(cbd_v) * cbd + scalar(fsub_v) * fsub + scalar(ocean_v) * scalar(fwy_v) * freeway if insample

levelsof saleyr if insample, local (levels) 
foreach lv of local levels {
	replace sp2000 = sp2000 / exp(scalar(yr`lv')) if saleyr == `lv'
	replace lgvsqft_i = lgvsqft_i + scalar(yr`lv')
}

gen sp2000_i = exp(lgvsqft_i) * floor_area 

/* aggregate sale price, aggregate imputed sale price. both in 2000*/
egen a = total(sp2000) 
egen b = total(sp2000_i) 

/*quality adjust factor */
scalar quality = a / b


clear
                     
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Riverside ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
cd "D:\summer, 2011\Second Project\practice\Six counties"  
insheet using "D:\summer, 2011\Second Project\practice\Six counties\RV.csv"
                                                                                         
replace saleyr = saleyr_1                                                             
replace saleyr = . if saleyr == 1899 | saleyr == 0     
gen sale_price = conf_sell1 if conf_sell1 != 0    
gen floor_area = impsqft if impsqft != 0                               
gen age = 2000 - yearbuilt if yearbuilt != 0
replace freeway = freeway * 0.00062137
replace ocean = ocean * 0.00062137
format scag_xyid scagxyid %14.0f
gen lgvsq = ln( sale_price/ floor_area)   
// group total floor area
bysort mz: egen fa_mz = total(floor_area)

/*pick parcels that actually need to be discounted*/
/*pick parcels that will be used in totval07 - saleprice ratio computation*/
/*difference btw id & id_XR is the latter does not require non-missing floor area*/
gen id_keep = !missing(lgvsq) & !missing(saleyr)
gen id_XR = !missing(sale_price) & !missing(saleyr) 

gen public = lu_08 < 1300 & lu_08 >= 1240
gen retail = lu_08 < 1230 & lu_08 >= 1220
gen transportation = lu_08 < 1500 & lu_08 >= 1400
gen warehousing = lu_08 == 1340
gen water = lu_08 < 5000 & lu_08 >= 4000
gen other_commercial = (lu_08 < 1240 & lu_08 >= 1230) | (lu_08 == 1500) | (lu_08 == 1600)
gen industrial = lu_08 < 1340 & lu_08 >= 1310
local land_use  public retail transportation /*warehousing*/ industrial other_commercial
// No parcel as warehousing has complete data on age, lgvsqft, sale_price. so a regression cannot be done.


//======================== age =======================
//regression & imputation, separate for landuse
foreach var of varlist `land_use' {
	char city [omit] "Riverside"                
	xi: quietly reg age fsub cbd freeway ocean i.city if `var' == 1 
	predict age_p if `var' == 1
	replace age = age_p if missing(age) & `var' == 1
	drop age_p
}



// =========================rent-value ratio ===============
gen rv_lgr = 5.605241 - 0.1125505 * cbd - 0.005535731 * age - 0.0752174 * fsub - 0.01547721 * ocean - 0.007329758 * freeway 
gen rv_lgv = scalar(c_v) + scalar(age_v) * age + scalar(cbd_v) * cbd + scalar(fsub_v) * fsub + scalar(ocean_v) * scalar(fwy_v) * freeway + scalar(yr2000)

/* rent deflator from Bill-Wheaton table */
scalar d_riv = 16.15 / 22.12
gen rv_ratio = exp(rv_lgr - rv_lgv) * scalar(d_riv)





//========== reg & impute of saleprice (for each land use) ================
foreach supervar of varlist `land_use' {
	gen sample = `supervar' == 1	
	char city [omit] "Riverside"  
	char saleyr [omit] 2000 
	xi: quietly reg  lgvsq  fsub cbd freeway ocean  i.city i.saleyr if sample
	estimates store `supervar'
	
	foreach var of varlist _Isaleyr* { 
		gen b`var' = _b[`var']
	}

/*XR part begin*/
/*xr = sale_price / totvalue07 computed across all parcels in Riverside county*/
/*To impute saleprice from totval07 for SB & IM*/
	gen xr_sample = `supervar' == 1 & id_XR == 1
	gen sp = sale_price if xr_sample
	gen dd = 1 if xr_sample
	//use clean _Isaleyr
	foreach var of varlist _Isaleyr* { 
		replace dd = exp(b`var') if `var' == 1 
	}
	replace sp = sp / dd
	egen tsp = total(sp)
	egen tv = total(totvalue07) if xr_sample
	foreach var of varlist tsp tv {
		summarize `var', meanonly
		scalar `var' = r(mean)	
	}
	scalar xr`supervar' = scalar(tsp) / scalar(tv)
	
	drop xr_sample sp dd tsp tv
	scalar drop tsp tv
/*XR part end*/


/*estimate missing values(out of reg sample) in 2000 */
/* unless neither lgvsq nor saleyr is missing, estimate */
/* which means set all year dummies to zero to impute a lgvsq in 2000 default year */
/*if neither lgvsq nor saleyr is missing, keep original lgvsq*/
/* otherwise, use the predicted value */

/* discount lgvsqft of parcels who misses nothing */
	gen lgvsq_2000 = lgvsq if sample
	foreach var of varlist _Isaleyr* {
		replace lgvsq_2000 =  lgvsq - b`var' if `var' == 1 & id_keep == 1
	} 

	
	foreach var of varlist _Isaleyr* {
		replace `var' = 0 
	}
	predict lgvsq_p if sample
	
	replace lgvsq_2000 = lgvsq_p if id_keep != 1
	gen vsq_2000 = exp(lgvsq_2000) if sample  
/*imputation of rent*/
	gen rsq_2000 = vsq_2000 * rv_ratio * scalar(quality) if sample
                                                             
	
	
	gen v2000 = vsq_2000 * floor_area if sample
	gen r2000 = rsq_2000 * floor_area if sample
	bysort mz: egen v_mz = total(v2000)
	bysort mz: egen r_mz = total(r2000)
	gen vsq_mz = v_mz / fa_mz if sample
	gen rsq_mz = r_mz / fa_mz if sample
	
	
	outsheet  scagxyid vsq_2000 rsq_2000 floor_area rv_ratio vsq_mz rsq_mz mz using 9_18_`supervar'.csv if sample, comma
	
	drop sample b_Isaleyr* lgvsq_p lgvsq_2000 vsq_2000 rsq_2000 
	drop v2000 r2000 v_mz r_mz vsq_mz rsq_mz		
} 

//estout * , cells(b(star fmt(3))  se(par fmt(2))) 
estout * , cells(b(star fmt(3))) using est_table.txt
                             
 esttab * using test.rtf, label mtitles      
   
 
 
 /* . scalar list
xrother_commercial =  3.3318034
xrindustrial =  .24262173
xrtransportation =  .47011658
  xrretail =  .35364837
  xrpublic =  .86192423
     d_riv =   .7301085
   quality =  .05360816
*/
                                                                                      