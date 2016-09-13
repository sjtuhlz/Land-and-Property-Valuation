set more off
capture log close
log using dec_16, replace

/*
use residential_six_final_multi.dta, clear
capture drop _merge*
merge m:m scagxyid using yizhen_newimpsqft.dta, keepusing(new_impsqft) force
save residential_six_final_multi.dta, replace
*/

use residential_six_final_multi.dta, clear

//===================== begin Multi ===========================
drop if mz ==0
gen random = runiform()
keep if random > 0.99

replace sale_price = . if sale_price == 0

encode city , gen(citynum)
xtset citynum

capture replace new_impsqft = . if new_impsqft == 0
rename floor_area floor_area1
rename new_impsqft floor_area2
replace floor_area2 = floor_area1 if missing(floor_area2) 
rename lgvsq lgvsq1
gen lgvsq2 = ln( sale_price / floor_area2)

gen lgvsq = .
capture gen lgvsq2000 = .
gen v2000 = .
gen r2000 = .

gen pom = hom / (hom + hrm) //percentage of multi housing units that are owner occupied 
gen vm =.
gen gamma = 0.6 // A-2 : gamma = Rrs / Ros
gen ros = rrs /gamma
levelsof tractid, local(lv_tract)
gen zeta = 1.5 //adjust owners overreport their housing values
gen vrm = . 
gen rom = .
gen rvm = .
gen thita = 0.8 // 80% of floor area of multi-unit housing is rentable

gen saleyr_bk = saleyr

 forvalues i = 1/2 {
    replace lgvsq = lgvsq`i'
    replace lgvsq2000 = .
    replace v2000 = .
    replace r2000 = .
    replace saleyr = saleyr_bk
//===================================== regression (Orange & Riverside)======================

    gen insample = (mz >= 50 & mz <=66) 
    
    xtreg lgvsq  fsub cbd fwy ocean  i.saleyr  i.lu_08 if insample & !missing(saleyr), fe
    estimates store residential_ora
    
    gen id_keep = e(sample)
    gen xo_sample = !missing(sale_price) & !missing(saleyr) & !missing(totvalue07) & insample
    
    //XO part begin
    gen sp = sale_price if xo_sample
    gen dd = 0 if xo_sample
    levelsof saleyr if e(sample), local(lvsaleyr)
    foreach lv of local lvsaleyr {
        capture replace dd = _b[`lv'.saleyr] - _b[2000.saleyr] if saleyr == `lv' & id_keep == 1
    }
    replace sp = sp / exp(dd)
    egen tsp = total(sp) if xo_sample
    egen tv = total(totvalue07) if xo_sample
    foreach var of varlist tsp tv {
        summarize `var', meanonly
        scalar `var' = r(mean)	
    }
    scalar xo_residential = scalar(tsp) / scalar(tv)
    drop xo_sample sp dd tsp tv 
    scalar drop tsp tv
    //XO part end
    
    
    foreach lv of local lvsaleyr {
        capture replace  lgvsq2000 =  lgvsq - _b[`lv'.saleyr] + _b[2000.saleyr] if saleyr == `lv'  & id_keep == 1 
    }
    replace saleyr = 2000 if id_keep == 0 & insample
    predict lgvsq_p if insample
    replace lgvsq2000 = lgvsq_p if id_keep == 0 & insample
    drop  lgvsq_p
    
    //========== LA, VT ===========
    replace insample = (mz >= 1 & mz <=46) | (mz >= 47 & mz <=49)  
    replace lgvsq2000 = ln((totvalue07 * scalar(xo_residential)) / floor_area`i') if insample
    
    xtreg lgvsq2000  fsub cbd fwy ocean  i.saleyr  i.lu_08 if insample & !missing(saleyr), fe
    estimates store residential_la_vt
    predict lgvsq_p if insample
    replace lgvsq2000 = lgvsq_p if insample & missing(totvalue07)
    drop lgvsq_p
    
    
    //========================================= regression (Riverside) =============================
    replace insample = (mz >= 81 & mz <=95)  
    
    xtreg lgvsq  fsub cbd fwy ocean  i.saleyr i.lu_08 if insample & !missing(saleyr), fe
    estimates store residential_riv
    
    replace id_keep = e(sample)
    gen xr_sample = !missing(sale_price) & !missing(saleyr) & !missing(totvalue07) & insample
    
    //XR part begin
    gen sp = sale_price if xr_sample
    gen dd = 0 if xr_sample
    
    levelsof saleyr if e(sample), local(lvsaleyr)
    foreach lv of local lvsaleyr {
        capture replace dd = _b[`lv'.saleyr] - _b[2000.saleyr] if saleyr == `lv' & id_keep == 1
    }
    
    replace sp = sp / exp(dd)
    
    egen tsp = total(sp) if xr_sample
    egen tv = total(totvalue07) if xr_sample
    foreach var of varlist tsp tv {
        summarize `var', meanonly
        scalar `var' = r(mean)	
    }
    scalar xr_residential = scalar(tsp) / scalar(tv)
    drop xr_sample sp dd tsp tv 
    scalar drop tsp tv
    //XR part end
    
    
    foreach lv of local lvsaleyr {
        capture replace lgvsq2000 =  lgvsq  - _b[`lv'.saleyr] + _b[2000.saleyr] if saleyr == `lv'  & id_keep == 1 
    }
    
    replace saleyr = 2000 if id_keep == 0 & insample
    predict lgvsq_p if insample
    replace lgvsq2000 = lgvsq_p if id_keep == 0 & insample
    drop lgvsq_p
    
    //==============SB=================
    replace insample = (mz >= 67 & mz <=80)  
    replace lgvsq2000 = ln((totvalue07 * scalar(xr_residential)) / floor_area`i') if insample
    
    xtreg lgvsq2000  fsub cbd fwy ocean i.lu_08 if insample & !missing(saleyr), fe
    estimates store residential_sb 
    predict lgvsq_p if insample
    replace lgvsq2000 = lgvsq_p if insample & missing(totvalue07)
    drop lgvsq_p
    
    
    
    //================================================= rent to value  ratioo  ==========================
    
    replace v2000 = exp(lgvsq2000) * floor_area`i'
    foreach lv of local lv_tract {
        sum v2000 if tractid == `lv', meanonly
        replace vm = r(mean) if tractid == `lv'
    }
    replace vrm = (zeta * vm - vom * pom) / (1 - pom)  
    replace rom = vom * ros / vos 
    replace rvm = (rom * pom + rrm * (1 - pom)) / (vom * pom + vrm * (1 - pom))  
    replace r2000 = v2000 * rvm * 12  //rents in Census is montly!
    
       
    //======================================aggregate to model zone level ====================================
    
    gen tsample_v`i' = !missing(floor_area`i') &  !missing(v2000)  
    gen tsample_r`i' = !missing(floor_area`i') &  !missing(r2000)  
    bysort mz: egen v_mz`i' = total(v2000) if tsample_v`i'
    bysort mz: egen r_mz`i' = total(r2000) if tsample_r`i'
    bysort mz: egen fa_mz_v`i' = total(floor_area`i') if tsample_v`i'
    bysort mz: egen fa_mz_r`i' = total(floor_area`i') if tsample_r`i'
    gen vsq_mz`i' = v_mz`i' / fa_mz_v`i' 
    gen rsq_mz`i' = r_mz`i' / fa_mz_r`i' 
    replace rsq_mz`i' = rsq_mz`i' / thita
    //drop tsample_v tsample_r v_mz r_mz fa_mz_v fa_mz_r //for debug, why vsq_mz1 and vsq_mz2 are the same???
    drop insample id_keep  
    
    gen v2000`i' = v2000
    gen r2000`i' = r2000
    gen lgvsq2000`i' = lgvsq2000

}

save dec16_five_residential_multi.dta, replace

duplicates drop vsq_mz1 vsq_mz2, force

//esttab * using Oct10, b p obslast label mtitles rtf replace
outsheet  vsq_mz1 rsq_mz1 vsq_mz2 rsq_mz2 mz name using mz_residential_multi_dec16.csv, comma replace
use dec16_five_residential_multi.dta, clear
