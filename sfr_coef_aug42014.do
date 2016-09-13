cd "/Users/huiling/Documents/summer, 2011/Third Project/Final_data"

use if (lu_08 >= 1110 & lu_08 < 1120) using residential_six_final_3category.dta, clear



capture drop _merge
replace sale_price = . if sale_price == 0 
replace floor_area = . if floor_area == 0
replace shape_area = . if shape_area == 0
replace saleyr = . if saleyr == 1899 | saleyr == 0 | saleyr == 1900
gen saleyr_bk = saleyr

//CLASSIFICATION
//not included is parcels with lu_08 = 1100, which lacks further classification


gen lnf = ln(floor_area)
gen lnp = ln(sale_price)
gen lnv = ln(totvalue07)
gen lnl = ln(shape_area)

encode city , gen(citynum)
gen insample = (mz >= 50 & mz <=66) | (mz >= 81 & mz <=95)


reg lnp lnf lnl fsub cbd fwy ocean i.saleyr i.lu_08 i.citynum if insample & !missing(saleyr)
estimates store sfr_sp_ORRV_coef

estout using test.doc, style(fixed) cells(b(star fmt(%9.3f))) replace drop(*.*) stats(r2_a N, fmt(%9.3f %9.0g) labels(R-squared)) legend label collabels(none) varlabels(_cons Constant)


reg lnv lnf lnl fsub cbd fwy ocean i.saleyr i.lu_08 i.citynum if insample & !missing(saleyr)
estimates store sfr_av_ORRV_coef




reg lnv lnf lnl fsub cbd fwy ocean i.saleyr i.lu_08 i.citynum if !insample & !missing(saleyr)
estimates store sfr_LASBVT_coef
