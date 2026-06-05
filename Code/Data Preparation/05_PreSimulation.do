* Working version Apr 16 2026 3:42 pm
* This version produces consistent welfare aggregates using food expenditure
* and non-food expenditure (excluding rent)
* converts 998 to missing in food and non-food

***********************************************************************
*	Prepare Vectors for R
**********************************************************************

****************************************************
//Deflators 
****************************************************
import excel using "$data/NCPI_series.xlsx", sheet("subgroups_ncpi") firstrow clear 

save "$data/NCPI_series_subgroups", replace 
********************************************************************
//				HIES											//
********************************************************************

//Merge comparable covariates in HIES with comparable consumption vector 

use "$hies2019/harmonized_HIES.dta" , clear 

merge 1:1 hhid_tiloka using "$rundata/HIES_comparable_full.dta" 
drop _m

//check welfare variable 
gen popwt= weight*hhmem 
gen poor = welfare<6966 
tabstat poor [aw=popwt]
drop poor 

svyset psu [pw=popwt]

//Temporal and spatial deflation: for now use same approach as LFS: total avg CPI monthly
tab month
tab year

merge m:1 year month using "$data/NCPI_series_subgroups", keepusing(cpi_base2013 cpi_base2013_food cpi_base2013_nonfood avg2019 avg2019food avg2019nonfood) 
keep if _merge==3 
drop _merge 

merge m:1 district using "$data/HIES/RAW/spatial_priceindex.dta",  nogen

//Deflate comparable expenditure variables 
//Food 
gen rpcexpfood_nc  = (HH_monthly_foodexp_comp3_pc*avg2019food)/cpi_base2013_food   //strict food, no cigarettes
gen rpcexpfood_c  = (HH_monthly_foodexp_comp2_pc*avg2019food)/cpi_base2013_food   //strict food, cigarettes
//Non-food 
gen rpcexpnfood1_s  = (HH_monthly_nfexp_comp2_pc*avg2019nonfood)/ cpi_base2013_nonfood //strict non-food
gen rpcexpnfood1_b  = (HH_monthly_nfexp_comp1_pc*avg2019nonfood)/ cpi_base2013_nonfood //broad non-food
gen rpcexprent  = (HH_monthly_rent_pc*avg2019nonfood)/ cpi_base2013_nonfood

//Totals:
//Non-Food
egen rpcexpnfood_s = rowtotal(rpcexpnfood1_s rpcexprent)
egen rpcexpnfood_b = rowtotal(rpcexpnfood1_b rpcexprent)
//Total 
egen rpcexptot_nc_s = rowtotal(rpcexpfood_nc rpcexpnfood_s) 
egen rpcexptot_nc_b = rowtotal(rpcexpfood_nc rpcexpnfood_b) 
egen rpcexptot_c_s = rowtotal(rpcexpfood_c rpcexpnfood_s) 
egen rpcexptot_c_b = rowtotal(rpcexpfood_c rpcexpnfood_b) 
//Total strict food, no cigarettes, strict non-food, excl. rent
egen rpcexptot_nc_s_nr = rowtotal(rpcexpfood_nc rpcexpnfood1_s)
//spatial deflation 2019 
foreach var of varlist rpcexpfood* rpcexpnfood* rpcexprent rpcexptot* {
    replace `var' = `var' * lpindex1
}

xtile quintiles_nc_s = rpcexptot_nc_s [aw=popwt], nq(5)
xtile quintiles_nc_b = rpcexptot_nc_b [aw=popwt], nq(5)
xtile quintiles_c_s = rpcexptot_c_s [aw=popwt], nq(5)
xtile quintiles_c_b = rpcexptot_c_b [aw=popwt], nq(5)
xtile quintiles_nc_s_nr = rpcexptot_nc_s_nr [aw=popwt], nq(5)

*svy : mean rpcexptot_nc_s rpcexpfood_nc rpcexpnfood1_s rpcexprent, over(quintiles_nc_s)
*svy : mean rpcexptot_nc_b rpcexpfood_nc rpcexpnfood1_b rpcexprent, over(quintiles_nc_b)
*svy : mean rpcexptot_c_s rpcexpfood_c rpcexpnfood1_s rpcexprent, over(quintiles_c_s)
*svy : mean rpcexptot_c_b rpcexpfood_c rpcexpnfood1_b rpcexprent, over(quintiles_c_b)
svy : mean rpcexptot_nc_s_nr rpcexpfood_nc rpcexpnfood1_s, over(quintiles_nc_s_nr) 
ren rpcexptot_nc_s_nr rpcexptot
ren rpcexpfood_nc rpcexpfood 
ren rpcexpnfood1_s rpcexpnfood
save "$out/HIES_Comp_2019" , replace 

********************************************************************
//				BRIGHT 											//
********************************************************************

//Merge comparable covariates in BRIGHT with comparable consumption vector 

use "$rundata/harmonized_BRIGHT.dta" , clear 
merge 1:1 hhid using "$rundata/BRIGHT_comparablecons.dta" 
drop _m


//Temporal and spatial deflation: for now use same approach as LFS: total avg CPI monthly
tab month
tab year

merge m:1 year month using "$data/NCPI_series_subgroups", keepusing(cpi_base2013 cpi_base2013_food cpi_base2013_nonfood avg2019 avg2019food avg2019nonfood) 
keep if _merge==3 
drop _merge 

merge m:1 district using "$data/HIES/RAW/spatial_priceindex.dta",  nogen

svyset [pw=popweight]

//Deflate comparable expenditure variables 
//Food 
gen rpcexpfood_nc  = (HH_monthly_fah_comp2_pc		*avg2019food)/cpi_base2013_food  //strict food, no cigarettes
gen rpcexpfood_c  = (HH_monthly_foodexp_comp2_pc		*avg2019food)/cpi_base2013_food  //strict food, cigarettes
//Non-food 
gen rpcexpnfood1_s = (HH_monthly_nfe_wutility2_pc * avg2019nonfood)/ cpi_base2013_nonfood  //strict non-food
gen rpcexpnfood1_b = (HH_monthly_nfe_wutility1_pc * avg2019nonfood)/ cpi_base2013_nonfood  //broad non-food
gen rpcexprent  = (HH_monthly_rent_pc*avg2019nonfood)/ cpi_base2013_nonfood
//Totals:
//Non-Food
egen rpcexpnfood_s = rowtotal(rpcexpnfood1_s rpcexprent)
egen rpcexpnfood_b = rowtotal(rpcexpnfood1_b rpcexprent)
//Total:
egen rpcexptot_nc_s = rowtotal(rpcexpfood_nc rpcexpnfood_s) 
egen rpcexptot_nc_b = rowtotal(rpcexpfood_nc rpcexpnfood_b) 
egen rpcexptot_c_s = rowtotal(rpcexpfood_c rpcexpnfood_s) 
egen rpcexptot_c_b = rowtotal(rpcexpfood_c rpcexpnfood_b) 
//Total strict food, no cigarettes, strict non-food, excl. rent
egen rpcexptot_nc_s_nr = rowtotal(rpcexpfood_nc rpcexpnfood1_s)
//spatial deflation 2019 
foreach var of varlist rpcexpfood* rpcexpnfood* rpcexprent rpcexptot* {
    replace `var' = `var' * lpindex1
}

xtile quintiles_nc_s = rpcexptot_nc_s [aw=popweight], nq(5)
xtile quintiles_nc_b = rpcexptot_nc_b [aw=popweight], nq(5)
xtile quintiles_c_s = rpcexptot_c_s [aw=popweight], nq(5)
xtile quintiles_c_b = rpcexptot_c_b [aw=popweight], nq(5)
xtile quintiles_nc_s_nr = rpcexptot_nc_s_nr [aw=popweight], nq(5)

*svy : mean rpcexptot_nc_s rpcexpfood_nc rpcexpnfood1_s rpcexprent, over(quintiles_nc_s)
*svy : mean rpcexptot_nc_b rpcexpfood_nc rpcexpnfood1_b rpcexprent, over(quintiles_nc_b)
*svy : mean rpcexptot_c_s rpcexpfood_c rpcexpnfood1_s rpcexprent, over(quintiles_c_s)
*svy : mean rpcexptot_c_b rpcexpfood_c rpcexpnfood1_b rpcexprent, over(quintiles_c_b)
svy : mean rpcexptot_nc_s_nr rpcexpfood_nc rpcexpnfood1_s, over(quintiles_nc_s_nr) 
ren rpcexptot_nc_s_nr rpcexptot
ren rpcexpfood_nc rpcexpfood 
ren rpcexpnfood1_s rpcexpnfood
save "$out/BRIGHT_Comp_202425" , replace 