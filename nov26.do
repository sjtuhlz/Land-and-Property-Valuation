//nov  26, 2011
//process data from Census Burea, to obtain Vos, Vom, Rrs, Rrm

clear
cd "D:\summer, 2011\Third Project\Final_data"

local counties  la or sb vt rv
foreach var of local  counties {
   use parcel_`var'_residential_merged.dta, clear
   gen county = "`var'"
    save parcel_`var'_residential_merged.dta, replace
}

use parcel_la_residential_merged.dta, clear
append using parcel_vt_residential_merged.dta, force
append using parcel_or_residential_merged.dta, force
append using parcel_sb_residential_merged.dta, force
append using parcel_rv_residential_merged.dta, force
save residential_six_final.dta, replace

/*
local counties  la or sb vt rv
foreach var of local  counties {
    use DTDownload_`var'.dta, clear
    rename tractid_st id
    save DTDownload_`var'.dta, replace
}
*/

/*
local counties  la or sb vt rv
foreach var of local  counties {
    use parcel_`var'_residential_final.dta, clear
    merge m:1 id using "D:\summer, 2011\Third Project\DT_download\DTDownload_`var'.dta"
    save parcel_`var'_residential_merged.dta, replace
}

*/



