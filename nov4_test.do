use "/Users/huiling/Dropbox/Vacant Land/july17_results_oct6revist.dta", clear
estimates use developable
estimates store developable
estimates use undevelopable
estimates store undevelopable

estout developable undevelopable using novtest.doc, varwidth(30) style(fixed) cells(b(star fmt(%9.3f))) label replace drop(o.*)



encode city_name, gen(city_code)

quietly reg lnv lna  fsub cbd cbd2 fwy ocean ocean2 i.city_code i.saleyr i.lu_08 i.scag_gp_co if  dvlp
estimates store developable

quietly reg lnv lna  fsub cbd cbd2 fwy ocean ocean2 i.city_code i.saleyr i.lu_08 i.scag_gp_co if !dvlp 
estimates store undevelopable

estout developable using nov4testdvlp.doc, style(fixed) cells(b(star fmt(%9.3f)) se(par)) label replace  
estout undevelopable using nov4testundvlp.doc, style(fixed) cells(b(star fmt(%9.3f)) se(par)) label replace  


estout developable using nov4testdvlp.doc, style(fixed) cells(b(star fmt(%9.3f))) label replace  
estout undevelopable using nov4testundvlp.doc, style(fixed) cells(b(star fmt(%9.3f))) label replace 




quietly xi: reg lnv lna  fsub cbd cbd2 fwy ocean ocean2 i.city_name i.saleyr i.lu_08 i.scag_gp_co if  dvlp
estimates store developable_xi

quietly xi: reg lnv lna  fsub cbd cbd2 fwy ocean ocean2 i.city_name i.saleyr i.lu_08 i.scag_gp_co if !dvlp 
estimates store undevelopable_xi



estout developable_xi undevelopable_xi using novtest.doc, varwidth(30) style(fixed) cells(b(star fmt(%9.3f))) label replace drop(o.*)


** Updated November 10, 2013 
quietly reg lnv lna  fsub cbd cbd2 fwy ocean ocean2 i.mz i.saleyr i.lu_08 i.scag_gp_co if  dvlp
estimates store developable
estout developable using nov10_mz_dvlp.doc, style(fixed) cells(b(star fmt(%9.3f))) label replace 


