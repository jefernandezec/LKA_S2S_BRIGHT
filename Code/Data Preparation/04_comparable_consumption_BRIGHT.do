* Working version Apr 16 2026 3:42 pm
* This version produces consistent welfare aggregates using food expenditure
* and non-food expenditure (excluding rent)
* converts 998 to missing in food and non-food

***********************************************************************
*	CREATE COMPARABLE CONSUMPTION AGGREGATE: 2019 HIES 
**********************************************************************
** Section J1: FOOD EXPENDITURE AT HOME 
** no distinction boarders/servers 
use "$long/mod_j1_fah_long" , clear 

bysort hhcode j1_respondent: gen first_obs = (_n == 1)
bysort hhcode: egen n_respondents = total(first_obs)
drop first_obs
tab n_respondents

bysort hhcode j1_item: gen item_count = _N
tab item_count

drop item_count n_respondents

keep if j1_01==1
notes: (641,361 observations deleted)

replace j1_08 = . if j1_08==998
notes: (20,022 real changes made, 20,022 to missing)


keep if inlist(j1_item, 1, 2, 3, 4, 5, 6, 7, 8, 12, 13, 14, 15, 17, 18, 19, 21, 22, 23, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 54, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 72, 73, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 87, 88, 89, 90, 91, 92, 94, 95, 96, 97, 98, 99, 101, 102, 103, 105, 107, 108, 110, 111, 112, 113, 114, 117, 119, 120, 121, 122, 123, 124, 125, 127, 128, 129, 131, 132)
**There is no item 9 in BRIGHT. Other excluded items either cannot be mapped or fall into "other xxx", which may be incomparable
notes: (12,462 observations deleted)

**NOTE: Mapping not sure for 1 (white rice), 6 (cereal flour), 8 (flour products)

//item code 
tab j1_01 
tab j1_item , nol 
//item other code not specified - needs extra cleaning 
tab j1_01_oth




replace j1_08=0 if j1_08==.
notes: (41,338 real changes made)

//Bring hhsize
*Household Roster 

preserve
use "$long/mod_b0_roster_long" , clear
keep if b0_01a==1 & b0_01b==1
 
merge 1:1 hhcode pid using "$long/mod_b1_roster_long" , nogen 
bys hhcode: egen hhsize=count(pid)
sort hhcode pid 
keep hhcode hhsize
duplicates drop
ren hhcode hhid
merge 1:1 hhid using "$rundata/harmonized_BRIGHT.dta", keepusing (district)
drop _merge
ren hhid hhcode
tempfile hhsize
save `hhsize'
restore

merge m:1 hhcode using `hhsize'
keep if _merge==3
drop _merge

gen pc_item_exp = j1_08/hhsize

//Outliers of per capita expenditure
bysort district j1_item: egen p25 = pctile(pc_item_exp), p(25)
bysort district j1_item: egen p75 = pctile(pc_item_exp), p(75)
gen iqr = p75 - p25
gen upper_bound = p75 + 3 * iqr
gen outlier = (pc_item_exp > upper_bound)

tab j1_item outlier

//Winsorize
replace pc_item_exp = upper_bound if outlier==1
gen j1_08_w = pc_item_exp * hhsize 

summ j1_08 j1_08_w
 
bys hhcode: egen fah_week_comp1 = total(j1_08_w)
gen HH_monthly_fah_comp1 = (fah_week_comp1/7)*30

label variable  fah_week     "WEEKLY HH FAH EXPENSES (comparable basket broad)"
label variable  HH_monthly_fah "MONTHLY HH FAH EXPENSES (comparable basket broad)"

drop if inlist(j1_item,8,76,91,105,108,110,113,114,117,119,120,121,123,127,128,129)

bys hhcode: egen fah_week_comp2 = total(j1_08_w)
gen HH_monthly_fah_comp2 = (fah_week_comp2/7)*30

label variable  fah_week_comp2     "WEEKLY HH FAH EXPENSES (comparable basket strict)"
label variable  HH_monthly_fah_comp2 "MONTHLY HH FAH EXPENSES (comparable basket strict)" // should be used

keep hhcode HH_monthly_fah_comp1 HH_monthly_fah_comp2

duplicates drop 
sort hhcode 

save "$rundata/j1_fah_comparable.dta", replace

********************************************************************************
** Section K: NON FOOD EXPENDITURE (CIGARETTES, BEEDI, SARUWATH)  
** In HIES This is part of food expenditure module 

use "$long/mod_k_nfe_long" , clear 
*Missing rent and utilities 

numlabel, add 

keep if inlist(k_item, 25) 

keep if k_expend!=0 & k_expend!=.

//convert to monthly 
gen nf_expend     = k_expend if k_expend!=. & k_period == 2
replace nf_expend = (k_expend / 7)*30 if k_expend!=. & k_period==1 
replace nf_expend = (k_expend / 365)*30 if k_expend!=. & k_period==3
replace nf_expend = (k_expend / 6) if k_expend!=. & k_period==4 

merge m:1 hhcode using `hhsize'
keep if _merge==3
drop _merge

gen pc_item_exp = nf_expend/hhsize

//Outliers of per capita expenditure
bysort district k_item: egen p25 = pctile(pc_item_exp), p(25)
bysort district k_item: egen p75 = pctile(pc_item_exp), p(75)
gen iqr = p75 - p25
gen upper_bound = p75 + 3 * iqr
gen outlier = (pc_item_exp > upper_bound)

tab k_item outlier

//Winsorize
replace pc_item_exp = upper_bound if outlier==1
gen nf_expend_w = pc_item_exp * hhsize 

summ nf_expend nf_expend_w

bys hhcode : egen HH_monthly_cigarettes= total(nf_expend_w)

label variable  HH_monthly_cigarettes  "MONTHLY HH EXPENDITURE ON CIGARETTES"

keep hhcode HH_monthly_cigarettes
duplicates drop 
sort hhcode 

save "$rundata/k_cigarette.dta", replace

********************************************************************************
// Food Expenditure Aggregate  
********************************************************************************
use "$rundata/j1_fah_comparable", clear 
 
merge 1:1 hhcode using "$rundata/k_cigarette", nogen 

egen HH_monthly_foodexp_comp1 = rowtotal (HH_monthly_fah_comp1 HH_monthly_cigarettes) 
label variable  HH_monthly_foodexp_comp1 "MONTHLY HH FOOD EXPENDITURE (broad plus cigarettes)"
egen HH_monthly_foodexp_comp2 = rowtotal (HH_monthly_fah_comp2 HH_monthly_cigarettes) 
label variable HH_monthly_foodexp_comp2 "MONTHLY HH FOOD EXPENDITURE (strict plus cigarettes)"

save "$rundata/hh_foodexp_comparable" , replace 

********************************************************************************
** Section K: NON-FOOD EXPENDITURE  
*Two options: 
*1. Convert to the recall in HIES - might be a bit inconsistent
*2. keep only if the reported recall is same as in HIES - may lose most of the households. 

use "$long/mod_k_nfe_long" , clear 
*Missing rent and utilities 

numlabel, add 
tab k_item 
tab k_period , nol 

replace k_expend=. if k_expend==998

keep if k_expend!=0 & k_expend!=.

keep if inlist(k_item, 6, 7, 8, 9, 10, 12, 13, 14, 15, 16, 17, 18, 19, 21, 22, 24, 26, 27, 28, 29, 30, 31, 33, 34, 35, 36, 37, 38, 39, 40, 41, 43, 44, 45, 46, 49,  51, 52, 53, 54, 55,  56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77) 
*NOTE: no code 11,20,42, 
*Excluded: 23 (religious goods/services), 32 (other adhoc expenses), 47 (plates, bowls etc.) 48 (eating and cooking utensils), 50 (rugs, carpets, drapes, etc)
*NOTE take 25 (cigarettes, beedi, saruwath) to food expenditure module

//Option 1: convert everything to monthly 
gen nf_expend     = k_expend if k_expend!=. & k_period == 2
replace nf_expend = (k_expend / 7)*30 if k_expend!=. & k_period==1 
replace nf_expend = (k_expend / 365)*30 if k_expend!=. & k_period==3
replace nf_expend = (k_expend / 6) if k_expend!=. & k_period==4 

merge m:1 hhcode using `hhsize'
keep if _merge==3
drop _merge

gen pc_item_exp = nf_expend/hhsize

//Outliers of per capita expenditure
bysort district k_item: egen p25 = pctile(pc_item_exp), p(25)
bysort district k_item: egen p75 = pctile(pc_item_exp), p(75)
gen iqr = p75 - p25
gen upper_bound = p75 + 3 * iqr
gen outlier = (pc_item_exp > upper_bound)

tab k_item outlier

//Winsorize
replace pc_item_exp = upper_bound if outlier==1
gen nf_expend_w = pc_item_exp * hhsize 

summ nf_expend nf_expend_w

bys hhcode : egen HH_monthly_nfe_comp1= total(nf_expend_w)

sum HH_monthly_nfe_comp1 , d 

label variable  HH_monthly_nfe_comp1  "MONTHLY HH NON-FOOD EXPENDITURE (comparable basket)"

keep hhcode HH_monthly_nfe_comp1  
duplicates drop 
sort hhcode 

save "$rundata/k_nfe_comparable1.dta", replace


//Option 2: drop items for which recall period is different to HIES
use "$long/mod_k_nfe_long" , clear 
*Missing rent and utilities 

numlabel, add 
tab k_item 
tab k_period , nol 

replace k_expend=. if k_expend==998

keep if k_expend!=0 & k_expend!=.

keep if inlist(k_item, 6, 7, 8, 9, 10, 12, 13, 14, 15, 16, 17, 18, 19, 21, 22, 24, 26, 27, 28, 29, 30, 31, 33, 34, 35, 36, 37, 38, 39, 40, 41, 43, 44, 45, 46, 49,  51, 52, 53, 54, 55,  56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77) 

gen annual_recall_time = inlist(k_item,15, 27, 28, 29, 30, 31)
gen sixmonth_recall_time = inlist(k_item, 51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73)
gen monthly_recall_time = inlist(k_item,6,7,8,9,10,12,13,14,16,17,18,19,21,22,24,26,33,34,35,36,37,38,39,40,41,43,44,45,46,49,74,75,76,77)

gen inconsistent_recall = 0
replace inconsistent_recall = 1 if annual_recall_time==1   & k_period != 3
replace inconsistent_recall = 1 if sixmonth_recall_time==1 & k_period != 4
replace inconsistent_recall = 1 if monthly_recall_time==1  & k_period != 2


tab k_item if inconsistent_recall==1
//item codes are 9,10,43,44,49,74,75,76,77 (transport, health, education, firewood)

drop if inlist(k_item, 9,10,43,44,49,74,75,76,77)

*drop additional non-comparable
drop if inlist(k_item, 7,18,17,19,46,15,24)

gen nf_expend     = k_expend if k_expend!=. & k_period == 2
replace nf_expend = (k_expend / 7)*30 if k_expend!=. & k_period==1 
replace nf_expend = (k_expend / 365)*30 if k_expend!=. & k_period==3
replace nf_expend = (k_expend / 6) if k_expend!=. & k_period==4 

merge m:1 hhcode using `hhsize'
keep if _merge==3
drop _merge

gen pc_item_exp = nf_expend/hhsize

//Outliers of per capita expenditure
bysort district k_item: egen p25 = pctile(pc_item_exp), p(25)
bysort district k_item: egen p75 = pctile(pc_item_exp), p(75)
gen iqr = p75 - p25
gen upper_bound = p75 + 3 * iqr
gen outlier = (pc_item_exp > upper_bound)

tab k_item outlier

//Winsorize
replace pc_item_exp = upper_bound if outlier==1
gen nf_expend_w = pc_item_exp * hhsize 

summ nf_expend nf_expend_w

bys hhcode : egen HH_monthly_nfe_comp2= total(nf_expend_w)

sum HH_monthly_nfe_comp2 , d 

label variable  HH_monthly_nfe_comp2  "MONTHLY HH NON-FOOD EXPENDITURE (comparable basket, consistent with recall period)"

keep hhcode HH_monthly_nfe_comp2  
duplicates drop 
sort hhcode 

save "$rundata/k_nfe_comparable2.dta", replace


********UTILITIES************
*****************************
use "$wide/mod_k_nfe_wide.dta", clear

gen HH_monthly_electricity = k_05a
gen HH_monthly_imprent = k_01a
replace HH_monthly_imprent = k_01c if k_01a==.
gen HH_monthly_water = k_03a

replace HH_monthly_imprent=. if HH_monthly_imprent==998
replace HH_monthly_electricity=. if HH_monthly_electricity==998
replace HH_monthly_water=. if HH_monthly_water==998



merge 1:1 hhcode using `hhsize'
keep if _merge==3
drop _merge

gen pc_elec = HH_monthly_electricity/hhsize
gen pc_elec_pos = pc_elec if pc_elec > 0
bysort district : egen p25_elec = pctile(pc_elec_pos), p(25)
bysort district : egen p75_elec = pctile(pc_elec_pos), p(75)
drop pc_elec_pos
gen iqr_elec = p75_elec - p25_elec
gen upper_bound_elec = p75_elec + 3 * iqr_elec
gen outlier_elec = (pc_elec > upper_bound_elec)
tab outlier_elec
//Winsorize
replace pc_elec = upper_bound_elec if outlier_elec==1
replace HH_monthly_electricity = pc_elec * hhsize 

gen pc_water = HH_monthly_water/hhsize
gen pc_water_pos = pc_water if pc_water > 0
bysort district : egen p25_water = pctile(pc_water_pos), p(25)
bysort district : egen p75_water = pctile(pc_water_pos), p(75)
drop pc_water_pos
gen iqr_water = p75_water - p25_water
gen upper_bound_water = p75_water + 3 * iqr_water
gen outlier_water = (pc_water > upper_bound_water)
tab outlier_water
//Winsorize
replace pc_water = upper_bound_water if outlier_water==1
replace HH_monthly_water = pc_water * hhsize 

gen pc_rent = HH_monthly_imprent/hhsize
gen pc_rent_pos = pc_rent if pc_rent > 0
bysort district : egen p25_rent = pctile(pc_rent_pos), p(25)
bysort district : egen p75_rent = pctile(pc_rent_pos), p(75)
drop pc_rent_pos
gen iqr_rent = p75_rent - p25_rent
gen upper_bound_rent = p75_rent + 3 * iqr_rent
gen outlier_rent = (pc_rent > upper_bound_rent)
tab outlier_rent
//Winsorize
replace pc_rent = upper_bound_rent if outlier_rent==1
replace HH_monthly_imprent = pc_rent * hhsize    //Not winsorizing rent


keep hhcode HH_monthly_electricity HH_monthly_imprent HH_monthly_water
ren HH_monthly_imprent HH_monthly_rent
save "$rundata/utilities.dta", replace

********************************************************************************
// Merge data 
********************************************************************************
*Household Roster 
use "$long/mod_b0_roster_long" , clear
keep if b0_01a==1 & b0_01b==1
 
merge 1:1 hhcode pid using "$long/mod_b1_roster_long" , nogen 
bys hhcode: egen hhsize=count(pid)
sort hhcode pid 

merge m:1 hhcode using "$wide/mod_a_household_identification" , nogen keepusing(a_01_province a_02_district a_05 hhweight ppweight adweight)


//merge 1:1 hhcode pid using "$rawdata/mod_b2_roster_long" , nogen 
//merge 1:1 hhcode pid using "$rawdata/mod_b3_roster_long" , nogen 

isid hhcode pid 

merge m:1 hhcode using "$rundata/hh_foodexp_comparable" 
drop if _m==2
drop _m 
merge m:1 hhcode using "$rundata/k_nfe_comparable1"		, nogen 
merge m:1 hhcode using "$rundata/k_nfe_comparable2"		, nogen 
merge m:1 hhcode using "$rundata/utilities"		, nogen 

********************************************************************************
// Food expenditure aggregate 
********************************************************************************

egen HH_monthly_nfe_wutility1 = rowtotal(HH_monthly_nfe_comp1 HH_monthly_electricity HH_monthly_water)
egen HH_monthly_nfe_wutility2 = rowtotal(HH_monthly_nfe_comp2 HH_monthly_electricity HH_monthly_water)

//Go ahead with excluding FAFH and including all compatible non-food codes
egen HH_monthly_exp_comp = rowtotal(HH_monthly_foodexp_comp2 HH_monthly_nfe_wutility1 HH_monthly_rent)  //strict food, broad non-food, cigarettes

egen HH_monthly_exp_comp1 = rowtotal (HH_monthly_fah_comp2 HH_monthly_nfe_wutility2 HH_monthly_rent) //strict food, strict non-food, no cigarettes
egen HH_monthly_exp_comp2 = rowtotal (HH_monthly_foodexp_comp2 HH_monthly_nfe_wutility2 HH_monthly_rent) //strict food, strict non-food, cigarettes
egen HH_monthly_exp_comp3 = rowtotal (HH_monthly_fah_comp2 HH_monthly_nfe_wutility1 HH_monthly_rent) //strict food, broad non-food, no cigarettes

foreach var in HH_monthly_exp_comp HH_monthly_exp_comp1 HH_monthly_exp_comp2 HH_monthly_exp_comp3 HH_monthly_foodexp_comp1 HH_monthly_foodexp_comp2 HH_monthly_fah_comp2 HH_monthly_nfe_wutility1 HH_monthly_nfe_wutility2 HH_monthly_cigarettes HH_monthly_rent HH_monthly_electricity HH_monthly_water {
	gen `var'_pc = `var'/ hhsize 
}


label var HH_monthly_exp_comp "Monthly total HH expenditure - strict food, broad non-food, cigarettes" //USING
label variable  HH_monthly_exp_comp1  "Monthly total HH expenditure - strict food, strict non-food, no cigarettes"
label variable  HH_monthly_exp_comp2  "Monthly total HH expenditure - strict food, strict non-food, cigarettes"
label variable  HH_monthly_exp_comp3  "Monthly total HH expenditure - strict food, broad non-food, no cigarettes"
label variable  HH_monthly_nfe_wutility1  "Monthly total HH non-food expenditure (incl. utilities, broad)"
label variable  HH_monthly_nfe_wutility2  "Monthly total HH non-food expenditure (incl. utilities, strict)"
label variable  HH_monthly_rent  "Monthly total HH rent expenditure (incl. imputed rent)"
label variable  HH_monthly_cigarettes  "Monthly total HH cigarettes expenditure"

ren hhcode hhid 

keep hhid HH_monthly_fah_comp2* HH_monthly_foodexp_comp2* HH_monthly_nfe_wutility2* HH_monthly_nfe_wutility1* HH_monthly_rent* HH_monthly_electricity* HH_monthly_water*
duplicates drop
isid hhid 
save "$rundata/BRIGHT_comparablecons.dta", replace