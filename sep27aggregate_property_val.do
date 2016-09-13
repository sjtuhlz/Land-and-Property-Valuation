cd "/Users/huiling/Documents/summer, 2011/Second Project/practice/Six counties"
capture log close 
log using sep27aggregate_propertyval, replace
clear 



** Version 1, compared to version 2( sep27apv_nr_nv_2.do)
** (1) This version run three regressions on sale_price of LA, OR, RV separately, while the other version runs one regresion only. 
** (2) This version runs one regerssion for each major land use, while the other version uses one regression with lu_08 dummies over all land uses 
 


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
gen water = lu_08 < 5000 & lu_08 >= 4000
gen other_commercial = (lu_08 < 1240 & lu_08 >= 1230) | (lu_08 == 1500) | (lu_08 == 1600)
gen industrial = lu_08 < 1340 & lu_08 >= 1310

local land_use  public retail transportation warehousing industrial other_commercial

gen id_keep = !missing(lgvsq) & !missing(saleyr)
gen id_OR = !missing(sale_price) & !missing(saleyr) & !missing(totvalue07)




**** Orange county *****

//  reg & impute of saleprice (for each land use)
foreach supervar of varlist `land_use' {
         replace insample = `supervar' == 1 & county == "OR"
         char city [omit] "Orange"  
         char saleyr [omit] 2000 
         xi: quietly reg  lgvsq  fsub cbd fwy ocean  i.city i.saleyr i.lu_08 if insample
         estimates store `supervar'_OR_nrnv
         
         foreach var of varlist _Isaleyr* { 
                 gen b`var' = _b[`var']
         }

/*XO part begin*/
/*xo = sale_price / totvalue07 computed across all parcels in Orange county*/
/*To impute saleprice from totval07 for Ventura*/
        gen xo_sample = `supervar' == 1 & id_OR == 1 & insample
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
/*XO part end*/


         
         foreach var of varlist _Isaleyr* {
                 replace lgvsq2000 =  lgvsq - b`var' if `var' == 1 & id_keep & insample
         } 
 
         
         foreach var of varlist _Isaleyr* {
                 replace `var' = 0 
         }
         predict lgvsq_p if insample
         
         replace lgvsq2000 = lgvsq_p if id_keep != 1 & insample
         replace v2000 = exp(lgvsq2000)* impsqft if insample
           
         drop lgvsq_p b_Isaleyr*                
} 





**** Riverside County ****** 
local land_use  public retail transportation /*warehousing*/ industrial other_commercial /*mixed*/
//  reg & impute of saleprice (for each land use)
foreach supervar of varlist `land_use' {
         replace insample = `supervar' == 1 & county == "RV" 
         char saleyr [omit] 2000 
         xi: quietly reg  lgvsq  fsub cbd fwy ocean  i.city i.saleyr i.lu_08 if insample
         estimates store `supervar'_RV_nrnv
         
         foreach var of varlist _Isaleyr* { 
                 gen b`var' = _b[`var']
         }

/*XR part begin*/
/*xr = sale_price / totvalue07 computed across all parcels in SB county*/
/*To impute saleprice from totval07 for San Bernardino*/
        gen xr_sample = `supervar' == 1 & id_OR == 1 & insample
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


         
         foreach var of varlist _Isaleyr* {
                 replace lgvsq2000 =  lgvsq - b`var' if `var' == 1 & id_keep & insample
         } 
 
         
         foreach var of varlist _Isaleyr* {
                 replace `var' = 0 
         }
         predict lgvsq_p if insample
         
         replace lgvsq2000 = lgvsq_p if id_keep != 1 & insample
         replace v2000 = exp(lgvsq2000) * impsqft if insample
           
         drop lgvsq_p b_Isaleyr*                
} 




****  LA county **********
foreach supervar of varlist `land_use' {
         replace insample = `supervar' == 1 & county == "LA" 
         char saleyr [omit] 2000 
         xi: quietly reg  lgvsq  fsub cbd fwy ocean  i.city i.saleyr i.lu_08 if insample
         estimates store `supervar'_LA_nrnv
         
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
         replace v2000 = exp(lgvsq2000)*impsqft if insample
           
         drop lgvsq_p b_Isaleyr*                
} 



**** SB county ******
replace insample = county == "SB"
gen sp2000 = totvalue07 if totvalue07 != 0 & insample

foreach var of varlist `land_use' {
     replace sp2000 = sp2000 * scalar(xr`var')
}


replace lgvsq = ln(sp2000 / impsqft) if insample

local land_use  public retail transportation /*warehousing*/ industrial other_commercial
// notice: dep.var is already in 2000 dollars
foreach supervar of varlist `land_use' {
         replace insample = `supervar' == 1 & county == "SB" 
         char saleyr [omit] 2000 
         xi: quietly reg  lgvsq  fsub cbd fwy ocean  i.city i.lu_08 if insample
         estimates store `supervar'_SB_nrnv
         
         predict lgvsq_p if insample
         replace v2000 = sp2000 if insample
         replace v2000 = exp(lgvsq2000)*impsqft if missing(v2000) & insample          
         drop lgvsq_p               
}  

***** VT county ********
local land_use  public retail transportation /*warehousing*/ industrial other_commercial
replace insample = county == "VT"
replace sp2000 = totvalue07 if totvalue07 != 0 & insample

foreach var of varlist `land_use' {
     replace sp2000 = sp2000 * scalar(xo`var')
}


replace lgvsq = ln(sp2000 / impsqft) if insample

// notice: dep.var is already in 2000 dollars
foreach supervar of varlist `land_use' {
         replace insample = `supervar' == 1 & county == "VT" 
         char saleyr [omit] 2000 
         xi: quietly reg  lgvsq  fsub cbd fwy ocean  i.city i.lu_08 if insample
         estimates store `supervar'_VT_nrnv
         
         predict lgvsq_p if insample
         replace v2000 = sp2000 if insample
         replace v2000 = exp(lgvsq2000)*impsqft if missing(v2000) & insample          
         drop lgvsq_p               
}  




save sep27_nrnv_propertyval_results.dta, replace



collapse (sum) shape_area impsqft v2000, by(county)





