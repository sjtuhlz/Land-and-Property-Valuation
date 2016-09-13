//dec 21, 2011
// merging the five counties' data files together has problem: scag_xyid is string/numeric, and one format will produce missing values if forced to merge with the other format.

use parcel_la_residential_merged.dta, clear
rename scag_xyid s
gen scag_xyid = real(s)
drop s
save parcel_la_residential_merged.dta, replace
append using parcel_vt_residential_merged.dta, clear
gen rrm1 = real(rrm)
drop rrm
rename rrm1 rrm
save parcel_vt_residential_merged.dta, replace
append using parcel_or_residential_merged.dta, force
append using parcel_sb_residential_merged.dta, force
append using parcel_rv_residential_merged.dta, force
drop cash
save residential_six_final.dta, replace