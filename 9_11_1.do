// input distance vars, totval07, last-sale-year
// output scagid, value per square foot discounted to year 2000


clear 
log close _all
log using Sep14_1
//log using Sep14_1, replace



cd "D:\summer, 2011\Second Project\practice\Six counties"                          
 
insheet using "D:\summer, 2011\Second Project\practice\Six counties\last_sale_merge_3counties_2.csv"
                                                                                         
foreach var of varlist fsub cbd freeway ocean shape_area{
	drop if missing(`var')   
}
                                                                
replace saleyr = . if saleyr == 1899      
replace saleyr = . if saleyr == 0                                         

gen sale_price = conf_sell_  
replace  sale_price = . if sale_price == 0                                                                                               

                                             
gen lgvsq = ln( sale_price/ shape_area*0.09290304)                                             
/*gen year = string(saleyr)*/                                                                                                 


gen freeway_mile = freeway * 0.00062137
gen ocean_mile = ocean * 0.00062137

replace freeway = freeway_mile
replace ocean = ocean_mile                                      

gen scagxyid = string(scag_xyid,"%14.0f") 
gen id = !missing(lgvsq) & !missing(saleyr) 



gen public = lu_08 < 1300 & lu_08 >= 1240
gen retail = lu_08 < 1230 & lu_08 >= 1220
gen transportation = lu_08 < 1500 & lu_08 >= 1400
gen warehousing = lu_08 == 1340
gen water = lu_08 < 5000 & lu_08 >= 4000
gen other_commercial = (lu_08 < 1240 & lu_08 >= 1230) | (lu_08 == 1500) | (lu_08 == 1600)
gen industrial = lu_08 < 1340 & lu_08 >= 1310

local land_use  public retail transportation warehousing water industrial other_commercial



foreach supervar of varlist `land_use' {

		// begins regression                                                                                                 
                                // add 'if' for every statemt          
                                char  city [omit] "Los Angeles"  
		char  saleyr [omit] 2000 
                            
		xi: reg  lgvsq  fsub cbd freeway ocean  i.city i.saleyr if `supervar' == 1
		estimates save `supervar'
		foreach var of varlist _Isaleyr* {
			gen b`var' = _b[`var']
			replace `var' = 0 if `var' == .   /*estimate missing values in 2000 */
			replace `var' = 0 if id == 0     /* unless neither totval07 nor saleyr is missing, estimate */
		}

		predict lgvsq_p`supervar' if `supervar' == 1
		gen lgvsq_int`supervar' = lgvsq_p`supervar' if `supervar' == 1
		replace lgvsq_int`supervar' = lgvsq if id == 1 & `supervar' == 1


		gen lgvsq_adj`supervar' = lgvsq_int`supervar' if `supervar' == 1

		foreach var of varlist _Isaleyr* {
			replace lgvsq_adj`supervar' =  lgvsq_int`supervar' - b`var' if `var' == 1 & `supervar' == 1
		}        



		gen vsq_adj`supervar' = exp(lgvsq_adj`supervar') if `supervar' == 1  
		gen val_adj`supervar' =  vsq_adj`supervar' * shape_area * 10.7639104 if `supervar' == 1
                                                             
		//outsheet  scagxyid vsq_adj`supervar' shape_area val_adj`supervar' mz using 9_9_`supervar'.csv if `supervar' == 1, comma
		by mz, sort: egen total_val_adj`supervar' = total(val_adj`supervar') if `supervar' == 1
		by mz, sort: egen total_shape_area`supervar' = total(shape_area) if `supervar' == 1

		gen vsq_mz`supervar' = total_val_adj`supervar'/ total_shape_area`supervar' if `supervar' == 1
		outsheet  scagxyid vsq_mz`supervar' mz using 9_9_mz_`supervar'.csv if `supervar' == 1, comma
 		
		foreach var of varlist b_Isaleyr* {
			gen `supervar'`var' = `var'
		}
		
		
}                              
       
                                                                                                                                                                                           

