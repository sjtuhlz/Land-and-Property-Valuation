
// Los angeles and Imperial is not available of updated floor area data

cd "D:\summer, 2011\Third Project\Final_data"
use yizhen_rv.dta, clear
keep scagxyid new_impsqft lu08
append using yizhen_or.dta yizhen_sb.dta yizhen_vt.dta, keep(scagxyid new_impsqft lu08)
gen scag_xyid = string(scagxyid, "%14.0f")
drop scagxyid
save yizhen_newimpsqft.dta, replace



