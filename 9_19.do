// merge all six counties' exported csv files to produce model zone aggregated results

// lots of "clear" command were used.
clear
log close _all
log using 9_19_mz, replace

local land_use  public retail transportation /*warehousing*/ industrial other_commercial
local county Imperial Los_angeles Orange Riverside San_Bernardino Ventura


    
foreach var of local land_use {
    foreach cvar of local county {
        cd "D:/summer, 2011/Second Project/practice/Six counties/`cvar'"
        insheet using "9_18_`var'.csv", clear
        duplicates drop mz , force
        gen mz_str = string(mz)
        replace mz_str = "0_`cvar'" if mz_str == "0"
        keep mz_str  vsq_mz rsq_mz
        cd "D:/summer, 2011/Second Project/practice/Six counties/append_mz"
        save 9_19_`var'`cvar'.dta, replace
        clear
    }
    
}


foreach var of local land_use{
    foreach cvar of local county {
        append using 9_19_`var'`cvar'.dta
    }
    outsheet using "9_19_`var'.csv", comma
    clear 
}

// for warehousing, only OR,LA,VT are available


clear

local land_use  warehousing
local county Los_angeles Orange Ventura 

foreach var of local land_use {
    foreach cvar of local county {
        cd "D:/summer, 2011/Second Project/practice/Six counties/`cvar'"
        insheet using "9_18_`var'.csv", clear 
        duplicates drop mz , force
        gen mz_str = string(mz)
        replace mz_str = "0_`cvar'" if mz_str == "0"
        keep mz_str  vsq_mz rsq_mz
        cd "D:/summer, 2011/Second Project/practice/Six counties/append_mz"
        save 9_19_`var'`cvar'.dta, replace
        clear
    }
    
}


foreach var of local land_use{
    foreach cvar of local county {
        append using 9_19_`var'`cvar'.dta
    }
    outsheet using "9_19_`var'.csv", comma
    clear
}

