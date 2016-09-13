cd "/Users/huiling/Documents/summer, 2011/Second Project/practice/Six counties"
capture log close 
log using sep27aggregate_propertyval, replace
clear 
set more off


** Version 2, compared to version 1( sep27aggregate_property_val.do)
** (1) This version run the regression on sale_price of LA, OR, RV together, while the other version runs three separate regressions. 
** (2) This version use one regression with lu_08 dummies over all land uses, while the other version runs one regerssion for each major land use
** (3) Note: SB, VT regressions are still separate. 
** (4) Note: imputation of floor area from shape area is included. 
******************************
** Part 2: Retail
******************************


/***************
* prepare data in nonresidential nonvacant land uses 
use LA_1.dta, clear

local counties VT OR SB RV 
foreach lc of local counties {
append using `lc'_1, force
}

rename freeway fwy
save sep27_five_counties_nrnv.dta, replace
*************/
use sep27_five_counties_nrnv.dta, clear
capture rename saleprice sale_price 
replace fwy = fwy * 0.00062137
replace ocean = ocean * 0.00062137
replace shape_area = . if shape_area == 0
replace impsqft = . if impsqft == 0
count if missing(shape_area)
replace saleyr = . if saleyr == 0 | saleyr == 1899
replace sale_price = . if sale_price == 0
replace totvalue07 = . if totvalue07 == 0
gen age = 2000 - yearbuilt if yearbuilt != 0
gen lgvsq = ln(sale_price/ impsqft)

gen lgvsq2000 = lgvsq  
gen v2000 = .
gen insample = .

gen public = lu_08 < 1300 & lu_08 >= 1240
gen retail = lu_08 < 1230 & lu_08 >= 1220
gen transportation = lu_08 < 1500 & lu_08 >= 1400
gen warehousing = lu_08 == 1340
*gen water = lu_08 < 5000 & lu_08 >= 4000
drop if lu_08 >= 4000
gen other_commercial = (lu_08 < 1240 & lu_08 >= 1230) | (lu_08 == 1500) | (lu_08 == 1600)
gen industrial = lu_08 < 1340 & lu_08 >= 1310

local land_use  public retail transportation warehousing industrial other_commercial

gen id_keep = !missing(lgvsq) & !missing(saleyr)
gen id_OR = !missing(sale_price) & !missing(saleyr) & !missing(totvalue07)


*****************  impute impsqft from shape_area **************
/*
* method 1:
xi: truncreg impsqft shape_area i.city i.lu_08 
predict impsqft_p
gen impsqft_bk = impsqft 
replace impsqft = impsqft_p if missing(impsqft)


* method 2:
gen shparea = shape_area
replace impsqft = 0 if missing(impsqft)
replace shparea = 0 if impsqft == 0


bysort city lu_08: egen totshapearea = total(shparea) 
bysort city lu_08: egen totimpsqft = total(impsqft)
gen ratio = totimpsqft / totshapearea 
replace impsqft = shape_area * ratio if impsqft == 0
*/

/*
* method 3:
gen shparea = shape_area
replace impsqft = 0 if missing(impsqft)
replace shparea = 0 if impsqft == 0


bysort county lu_08: egen totshapearea = total(shparea) 
bysort county lu_08: egen totimpsqft = total(impsqft)
gen ratio = totimpsqft / totshapearea 
replace impsqft = shape_area * ratio if impsqft == 0
************
*/






replace insample = county == "OR" | county == "LA" | county == "RV"
char saleyr [omit] 2000 
xi: reg  lgvsq  fsub cbd fwy ocean  i.city i.saleyr i.lu_08 if insample
estimates store orlarv_nrnv
estimates save orlarv_nrnv, replace

foreach var of varlist _Isaleyr* { 
	gen b`var' = _b[`var']
}


foreach var of varlist _Isaleyr* {
	replace lgvsq2000 =  lgvsq - b`var' if `var' == 1 & id_keep & insample
} 
 
         
foreach var of varlist _Isaleyr* {
	replace `var' = 0 
}
predict lgvsq_p if insample
         
replace lgvsq2000 = lgvsq_p if id_keep != 1 & insample
replace v2000 = exp(lgvsq2000)* impsqft if insample
           
drop lgvsq_p                



/*XO part begin*/
foreach supervar of local land_use {
        gen xo_sample = `supervar' == 1 & id_OR == 1 & county == "OR"
        gen sp = sale_price if xo_sample 
        gen dd = 1 if xo_sample
        //use clean _Isaleyr
         foreach var of varlist _Isaleyr* { 
                 replace dd = exp(b`var') if `var' == 1 
         }
         replace sp = sp / dd
         egen tsp = total(sp)
         egen tv = total(totvalue07) if xo_sample
         foreach var of varlist tsp tv {
                 summarize `var', meanonly
                 scalar `var' = r(mean)  
         }
         scalar xo`supervar' = scalar(tsp) / scalar(tv)         
         drop xo_sample sp dd tsp tv
         scalar drop tsp tv
}
/*XO part end*/


/*XR part begin*/
foreach supervar of local land_use {
        gen xr_sample = `supervar' == 1 & id_OR == 1 & county == "RV"
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
}
/*XR part end*/




**** SB county ******
replace insample = county == "SB"
gen sp2000 = totvalue07 if totvalue07 != 0 & insample
local land_use  public retail transportation /*warehousing*/ industrial other_commercial
foreach var of local land_use{
     replace sp2000 = sp2000 * scalar(xr`var')
}


replace lgvsq = ln(sp2000 / impsqft) if insample


// notice: dep.var is already in 2000 dollars
         xi: quietly reg  lgvsq  fsub cbd fwy ocean  i.city i.lu_08 if insample
         estimates store SB_nrnv
         
         predict lgvsq_p if insample
         replace v2000 = sp2000 if insample
         replace v2000 = exp(lgvsq_p)*impsqft if missing(v2000) & insample          
         drop lgvsq_p               
  

***** VT county ********

replace insample = county == "VT"
capture drop sp2000
gen sp2000 = totvalue07 if totvalue07 != 0 & insample
local land_use  public retail transportation /*warehousing*/ industrial other_commercial
foreach var of local land_use {
     replace sp2000 = sp2000 * scalar(xo`var')
}


replace lgvsq = ln(sp2000 / impsqft) if insample

// notice: dep.var is already in 2000 dollars
         xi: quietly reg  lgvsq  fsub cbd fwy ocean  i.city i.lu_08 if insample
         estimates store VT_nrnv
         
         predict lgvsq_p if insample
         replace v2000 = sp2000 if insample
         replace v2000 = exp(lgvsq_p)*impsqft if missing(v2000) & insample          
         drop lgvsq_p               






save sep27_nrnv_propertyval_results_2.dta, replace

gen id_impsqft = !missing(impsqft) & impsqft != 0
gen id_shape_area = !missing(shape_area) & shape_area != 0

collapse (sum) shape_area impsqft v2000 id_impsqft id_shape_area, by(county)


*** To prepare Table 8: blanks and zeros in non-residential non-vacant properties. 
bysort county: egen nparcel = count(fsub)

replace saleyr = . if saleyr == 0 | saleyr == 1899
bysort county: egen nsaleyr = count(saleyr)

replace totvalue07 = . if totvalue07 == 0
bysort county: egen ntotvalue = count(totvalue07)

replace sale_price = . if sale_price == 0
bysort county: egen nsaleprice = count(sale_price)

gen id_miss_either_tot = missing(totvalue07) | missing(saleyr)
replace id_miss_either_tot = . if id_miss_either_tot == 0
bysort county : egen n_misseither_tot = count(id_miss_either_tot)

gen id_miss_either_price = missing(sale_price) | missing(saleyr)
replace id_miss_either_price = . if id_miss_either_price == 0
bysort county : egen n_misseither_price = count(id_miss_either_price)

collapse (mean) nparcel nsaleyr ntotvalue nsaleprice n_misseither_tot n_misseither_price, by(county)



