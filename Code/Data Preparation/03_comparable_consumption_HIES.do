* Working version Apr 6 2026 4:05 pm
* This version produces consistent welfare aggregates using food expenditure
* and non-food expenditure (excluding rent)

***********************************************************************
*	CREATE COMPARABLE CONSUMPTION AGGREGATE: 2
***********************************************************************

** 4.1 HOUSEHOLD FOOD EXPENDITURE 
**not including boarders and servents

//import delimited "SEC_4_1_FOOD_EXP.csv", clear

//use "rundata/sec4_1x.dta", clear
use "$hies2019/RAW/rundata/sec4_1x.dta", clear
drop hhid 
tostring district sector month psu snumber hhno nhh, replace force
replace month="0"+month if strlen(month)==1
replace snumber="0"+snumber if strlen(snumber)==1

gen hhid=district+sector+month+psu+snumber+hhno
destring month , replace 

sort hhid

bysort hhid code: gen item_count = _N
tab item_count

keep if inlist(code,101,102,103,104,106,107,110,111,113,115,116,117,118,119,120,201,202,203,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,301,302,304,305,306,307,309,402,403,404,406,407,408,409,410,411,412,413,415,416,417,418,419,420,421,422,423,424,426,428,430,431,434,441,442,443,444,445,446,447,448,501,504,505,506,507,601,602,603,604,605,609,701,702,703,704,705,706,707,708,709,710,711,712,713,714,715,717,718,719,720,721,726,801,802,803,804,805,806,807,808,809,810,811,812,813,901,1001,1103,1104,1111,1120,1301,1304,1305,1307,1308,1309,1310,1401,1402,1405,1501,1503,1504,1601,1602,1603,1604,1605,1606,1607,1608,1610,1611,1612,1613,1614,1615,1616,1651,1653,1701,1702,1703,1704,1705,1707,1708,1709,1711,1712,1713,1801,1802,1803,1804,1805,1806,1808,1812,1904,1907,1910,1912,1920,1924)
notes: (277,373 observations deleted)

preserve
use "$hies2019/harmonized_HIES.dta", clear
keep hhid_tiloka hhmem
rename hhid_tiloka hhid
rename hhmem hhsize
tempfile hhsize
save `hhsize'
restore

merge m:1 hhid using `hhsize'
drop _merge

gen pc_item_exp = value/hhsize

//Outliers of per capita expenditure
bysort district code: egen p25 = pctile(pc_item_exp), p(25)
bysort district code: egen p75 = pctile(pc_item_exp), p(75)
gen iqr = p75 - p25
gen upper_bound = p75 + 3 * iqr
gen outlier = (pc_item_exp > upper_bound)

tab code outlier
//Winsorize
replace pc_item_exp = upper_bound if outlier==1
gen value_w = pc_item_exp * hhsize 

summ value value_w

bysort hhid: egen weekly_foodexp_comp1=total(value_w)
replace weekly_foodexp_comp1=0 if weekly_foodexp_comp1==.

gen HH_monthly_foodexp_comp1=(weekly_foodexp_comp1/7)*30

label variable  HH_monthly_foodexp_comp1 "MONTHLY HH FOOD EXPENSES (comparable basket - widest)"
label variable  weekly_foodexp_comp1     "WEEKLY HH FOOD EXPENSES (comparable basket - widest)"
*tabstat HH_monthly_foodexp_comp1 if month<=3 | month>=11 & HH_monthly_foodexp_comp1>0


//Drop the FAFH items
drop if inlist(code,119,206,207,208,209,210,211,212,213,214,215,216,217,218,219,220,1601,1602,1603,1604,1605,1651,1702,1703,1705,1707,1708,1711,1712,1713,1803,1804,1805,1806,1808,1904,1907)

bysort hhid: egen weekly_foodexp_comp2=total(value_w)
replace weekly_foodexp_comp2=0 if weekly_foodexp_comp2==.

gen HH_monthly_foodexp_comp2=(weekly_foodexp_comp2/7)*30

label variable  HH_monthly_foodexp_comp2 "MONTHLY HH FOOD EXPENSES (comparable basket - strict, cigarettes)"  // should be used
label variable  weekly_foodexp_comp2 "WEEKLY HH FOOD EXPENSES (comparable basket - strict, cigarettes)"

//Drop Cigarettes. 
drop if inlist(code,1910,1912,1920,1924)

bysort hhid: egen weekly_foodexp_comp3=total(value_w)
replace weekly_foodexp_comp3=0 if weekly_foodexp_comp3==.

gen HH_monthly_foodexp_comp3=(weekly_foodexp_comp3/7)*30

label variable  HH_monthly_foodexp_comp3 "MONTHLY HH FOOD EXPENSES (comparable basket - strict, no cigarettes)"
label variable  weekly_foodexp_comp3 "WEEKLY HH FOOD EXPENSES (comparable basket - strict, no cigarettes)"
*tabstat HH_monthly_foodexp_comp* 


keep hhid HH_monthly_foodexp_comp* weekly_foodexp_comp*

duplicates drop 
sort hhid
save "$rundata/HIES_comparable_food.dta", replace


** 4.2 HOUSEHOLD NON-FOOD EXPENDUTRE ***


**rent (incl imputed)

use "$hies2019/RAW/rundata/sec4_2x.dta", clear
drop hhid 
tostring district sector month psu snumber hhno nhh, replace force
replace month="0"+month if strlen(month)==1
replace snumber="0"+snumber if strlen(snumber)==1

gen hhid=district+sector+month+psu+snumber+hhno

keep if inlist(nf_code, 2001)

*TOTAL NON_FOOD EXP - for any recall period

merge m:1 hhid using `hhsize'
drop _merge

gen pc_item_exp = nf_value/hhsize

//Outliers of per capita expenditure
bysort district nf_code: egen p25 = pctile(pc_item_exp), p(25)
bysort district nf_code: egen p75 = pctile(pc_item_exp), p(75)
gen iqr = p75 - p25
gen upper_bound = p75 + 3 * iqr
gen outlier = (pc_item_exp > upper_bound)

tab nf_code outlier

//Winsorize
replace pc_item_exp = upper_bound if outlier==1
gen nf_value_w = pc_item_exp * hhsize 

summ nf_value nf_value_w

gen monthly_NFexp=nf_value_w //changed (not removing in kind now) - not winsorizing rent

replace monthly_NFexp=(nf_value_w)/6 if nf_code>=3001 & nf_code<3220
replace monthly_NFexp=(nf_value_w)/12 if nf_code==2421 | nf_code==2422 | (nf_code>=2609 & nf_code<=2613) | (nf_code>=3301 & nf_code<=3339) | (nf_code>=3501 & nf_code<=3519)

bysort hhid: egen HH_monthly_rent=sum(monthly_NFexp)

label variable HH_monthly_rent "HH monthly expenditure rent"
keep hhid HH_monthly_rent
duplicates drop

save "$rundata\HIES_rent.dta", replace

*************
**electricity
*************
use "$hies2019/RAW/rundata/sec4_2x.dta", clear
drop hhid 
tostring district sector month psu snumber hhno nhh, replace force
replace month="0"+month if strlen(month)==1
replace snumber="0"+snumber if strlen(snumber)==1

gen hhid=district+sector+month+psu+snumber+hhno

keep if inlist(nf_code, 2101)


merge m:1 hhid using `hhsize'
drop _merge

gen pc_item_exp = nf_value/hhsize

//Outliers of per capita expenditure
bysort district nf_code: egen p25 = pctile(pc_item_exp), p(25)
bysort district nf_code: egen p75 = pctile(pc_item_exp), p(75)
gen iqr = p75 - p25
gen upper_bound = p75 + 3 * iqr
gen outlier = (pc_item_exp > upper_bound)

tab nf_code outlier

//Winsorize
replace pc_item_exp = upper_bound if outlier==1 & pc_item_exp!=.
gen nf_value_w = pc_item_exp * hhsize 

summ nf_value nf_value_w

gen monthly_NFexp=nf_value_w //changed (not removing in kind now) - not winsorizing rent

replace monthly_NFexp=(nf_value_w)/6 if nf_code>=3001 & nf_code<3220
replace monthly_NFexp=(nf_value_w)/12 if nf_code==2421 | nf_code==2422 | (nf_code>=2609 & nf_code<=2613) | (nf_code>=3301 & nf_code<=3339) | (nf_code>=3501 & nf_code<=3519)

bysort hhid: egen HH_monthly_electricity=sum(monthly_NFexp)

label variable HH_monthly_electricity "HH monthly expenditure electricity"
keep hhid HH_monthly_electricity
duplicates drop

save "$rundata\HIES_electricity.dta", replace

*************
**water
*************
use "$hies2019/RAW/rundata/sec4_2x.dta", clear
drop hhid 
tostring district sector month psu snumber hhno nhh, replace force
replace month="0"+month if strlen(month)==1
replace snumber="0"+snumber if strlen(snumber)==1

gen hhid=district+sector+month+psu+snumber+hhno

keep if inlist(nf_code, 2003)


merge m:1 hhid using `hhsize'
drop _merge

gen pc_item_exp = nf_value/hhsize

//Outliers of per capita expenditure
bysort district nf_code: egen p25 = pctile(pc_item_exp), p(25)
bysort district nf_code: egen p75 = pctile(pc_item_exp), p(75)
gen iqr = p75 - p25
gen upper_bound = p75 + 3 * iqr
gen outlier = (pc_item_exp > upper_bound)

tab nf_code outlier

//Winsorize
replace pc_item_exp = upper_bound if outlier==1 & pc_item_exp!=.
gen nf_value_w = pc_item_exp * hhsize 

summ nf_value nf_value_w

gen monthly_NFexp=nf_value_w //changed (not removing in kind now) 

replace monthly_NFexp=(nf_value_w)/6 if nf_code>=3001 & nf_code<3220
replace monthly_NFexp=(nf_value_w)/12 if nf_code==2421 | nf_code==2422 | (nf_code>=2609 & nf_code<=2613) | (nf_code>=3301 & nf_code<=3339) | (nf_code>=3501 & nf_code<=3519)

bysort hhid: egen HH_monthly_water=sum(monthly_NFexp)

label variable HH_monthly_water "HH monthly expenditure water"
keep hhid HH_monthly_water
duplicates drop

save "$rundata\HIES_water.dta", replace


**Other non-food

use "$hies2019/RAW/rundata/sec4_2x.dta", clear
drop hhid 
tostring district sector month psu snumber hhno nhh, replace force
replace month="0"+month if strlen(month)==1
replace snumber="0"+snumber if strlen(snumber)==1

gen hhid=district+sector+month+psu+snumber+hhno

keep if inlist(nf_code,2103,2104,2105,2106,2107,2108,2201,2202,2203,2204,2205,2206,2207,2208,2209,2211,2214,2215,2301,2302,2303,2304,2305,2306,2307,2308,2309,2401,2402,2403,2404,2405,2406,2411,2412,2413,2414,2415,2416,2417,2421,2422,2503,2505,2601,2602,2603,2604,2606,2607,2609,2610,2611,2612,2613,2701,2702,2704,2708,2711,2801,2802,2803,2804,2805,2806,2807,2902,2903,3001,3002,3003,3004,3005,3006,3007,3008,3009,3010,3011,3012,3013,3014,3015,3016,3017,3018,3019,3020,3021,3022,3023,3024,3041,3042,3043,3049,3061,3062,3101,3102,3103,3104,3106,3334,3342,3402,3501,3502,3504,3505)

*TOTAL NON_FOOD EXP - for any recall period

merge m:1 hhid using `hhsize'
drop _merge

gen pc_item_exp = nf_value/hhsize

//Outliers of per capita expenditure
bysort district nf_code: egen p25 = pctile(pc_item_exp), p(25)
bysort district nf_code: egen p75 = pctile(pc_item_exp), p(75)
gen iqr = p75 - p25
gen upper_bound = p75 + 3 * iqr
gen outlier = (pc_item_exp > upper_bound)

tab nf_code outlier

//Winsorize
replace pc_item_exp = upper_bound if outlier==1
gen nf_value_w = pc_item_exp * hhsize 

summ nf_value nf_value_w

gen monthly_NFexp=nf_value_w //changed (not removing in kind now)

replace monthly_NFexp=(nf_value_w)/6 if nf_code>=3001 & nf_code<3220
replace monthly_NFexp=(nf_value_w)/12 if nf_code==2421 | nf_code==2422 | (nf_code>=2609 & nf_code<=2613) | (nf_code>=3301 & nf_code<=3339) | (nf_code>=3501 & nf_code<=3519)

bysort hhid: egen HH_monthly_nfexp_comp1=sum(monthly_NFexp)

label variable HH_monthly_nfexp_comp1 "HH monthly expenditure non food & goods/services - broad"

***NON FOOD ONLY KEEPING CONSISTENT RECALL PERIOD***
drop if inlist(nf_code,2301,2302,2303,2304,2305,2306,2307,2308,2309,2401,2402,2403,2404,2405,2406,2604,2606,2607,2609,2610,2611,2612,2613)

***Drop additional non-comparable
drop if inlist(nf_code,2105,2601,2602,2603,2701,2702,2708,2903,3018,3334,3342)


bysort hhid: egen HH_monthly_nfexp_comp2=sum(monthly_NFexp)

label variable HH_monthly_nfexp_comp2 "HH monthly expenditure non food & goods/services - strict"

keep hhid HH_monthly_nfexp_comp2 HH_monthly_nfexp_comp1
duplicates drop

save "$rundata\HIES_comparable_nonfood.dta", replace

****************************************************************
//Load Individual Data with original vectors to double check 
****************************************************************

use "$data/poverty_simulations_05012024.dta", clear

merge m:1 hhid using "$rundata/HIES_comparable_food.dta"
drop _m
merge m:1 hhid using "$rundata/HIES_comparable_nonfood.dta"
drop _m
merge m:1 hhid using "$rundata/HIES_rent.dta"
drop _m
merge m:1 hhid using "$rundata/HIES_electricity.dta"
drop _m
merge m:1 hhid using "$rundata/HIES_water.dta"
drop _m

egen HH_monthly_nfe_wutility1 = rowtotal(HH_monthly_nfexp_comp1 HH_monthly_electricity HH_monthly_water)
egen HH_monthly_nfe_wutility2 = rowtotal(HH_monthly_nfexp_comp2 HH_monthly_electricity HH_monthly_water)


egen HH_monthly_exp_comp = rowtotal(HH_monthly_foodexp_comp2 HH_monthly_nfe_wutility1 HH_monthly_rent) //strict food, broad non-food, cigarettes
egen HH_monthly_exp_comp1 = rowtotal(HH_monthly_foodexp_comp3 HH_monthly_nfe_wutility2 HH_monthly_rent) //strict food, strict non-food, no cigarettes
egen HH_monthly_exp_comp2 = rowtotal(HH_monthly_foodexp_comp2 HH_monthly_nfe_wutility2 HH_monthly_rent) //strict food, strict non-food, cigarettes
egen HH_monthly_exp_comp3 = rowtotal(HH_monthly_foodexp_comp3 HH_monthly_nfe_wutility1 HH_monthly_rent) //strict food, broad non-food, no cigarettes

foreach var in HH_monthly_exp_comp HH_monthly_exp_comp1 HH_monthly_exp_comp2 HH_monthly_exp_comp3 HH_monthly_foodexp_comp2 HH_monthly_foodexp_comp3 HH_monthly_rent HH_monthly_nfe_wutility1 HH_monthly_nfe_wutility2 HH_monthly_electricity HH_monthly_water HH_monthly_nfexp_comp2 HH_monthly_nfexp_comp1{
	gen `var'_pc = `var'/ hhsize 
}

label var HH_monthly_exp_comp "Monthly total HH expenditure - strict food, broad non-food, cigarettes"
label variable  HH_monthly_exp_comp1  "Monthly total HH expenditure - strict food, strict non-food, no cigarettes"
label variable  HH_monthly_exp_comp2  "Monthly total HH expenditure - strict food, strict non-food, cigarettes"
label variable  HH_monthly_exp_comp3  "Monthly total HH expenditure - strict food, broad non-food, no cigarettes"

keep HH_monthly_foodexp_comp3* HH_monthly_foodexp_comp2* HH_monthly_nfexp_comp2* HH_monthly_nfexp_comp1* HH_monthly_rent* HH_monthly_nfe_wutility1* HH_monthly_nfe_wutility2* HH_monthly_electricity* HH_monthly_water* hhid  
duplicates drop  hhid, force

ren hhid hhid_tiloka 
save "$rundata/HIES_comparable_full.dta", replace

