***********************************************************************
*	Clean 2024/25 BRIGHT  
***********************************************************************

**************************************************************
*DEMOGRAPHIC
**************************************************************
use "$long\mod_b0_roster_long.dta" , clear 
merge 1:1 hhcode pid using "$long\mod_b1_roster_long.dta" , nogen 
merge 1:1 hhcode pid using "$long\mod_b2_roster_long.dta"  
drop if _m==2
drop _m
merge 1:1 hhcode pid using "$long\mod_b3_roster_long.dta"  
drop if _m==2
drop _m
merge m:1 hhcode using "$wide/mod_a_household_identification.dta" , nogen 

//Month and year
gen newclock = clock(starttime, "MDYhms")
format newclock %tc
gen month = month(dofc(newclock))
gen year = year(dofc(newclock))

/*There are some differences in month when using start/end time: use start for CPI match
gen 	newclock_check = clock(endtime, "MDYhms")
format 	newclock_check %tc

gen month_check = month(dofc(newclock_check))
gen year_check	 = year(dofc(newclock_check))


gen diff1 = year-year_check
gen diff2= month-month_check
tab diff1 
tab diff2
*/

ren hhweight weight 
ren ppweight popweight

ren hhcode hhid


keep if b0_01a==1 & b0_01b==1
bys hhid: egen hhmem=count(pid)
tabstat hhmem

*Provinces/districts
gen district=11 if a_02_district==1 //Col
replace district=12 if a_02_district==2 //Gam
replace district=13 if a_02_district==3 //Kalu
replace district=21 if a_02_district==17 //Kandy
replace district=22 if a_02_district==18 //Matale
replace district=23 if a_02_district==19 //N-Eliya
replace district=31 if a_02_district==4 //Galle
replace district=32 if a_02_district==6 //Matara
replace district=33 if a_02_district==5 //Hambantota
replace district=41 if a_02_district==7 //Jaffna
replace district=42 if a_02_district==9 //Mannar
replace district=43 if a_02_district==11 //Vavuniya
replace district=44 if a_02_district==10 //Mullaitivu
replace district=45 if a_02_district== 8 //Kili
replace district=51 if a_02_district==13 //Batti
replace district=52 if a_02_district==12 //Ampara
replace district=53 if a_02_district==14 //Trinco
replace district=61 if a_02_district==20 //Kuru
replace district=62 if a_02_district==21 //Puttalam
replace district=71 if a_02_district==15 //Anu
replace district=72 if a_02_district==16 //Polon
replace district=81 if a_02_district==22 //Badulla
replace district=82 if a_02_district==23 //Mona
replace district=91 if a_02_district==25 //Rat
replace district=92 if a_02_district==24 //Keg

label define dist 11 "COL" 12 "GAM" 13 "KAL" 21 "KAN" 22 "MATALE" 23 "NEL" 31 "GAL" 32 "MATARA" 33 "HAM" 41 "JAF" 42 "MANNAR" 43 "VAV" 44 "MULL" 45 "KILI" 51 "BAT" 52 "AMP" 53 "TRINC" 61 "KUR" 62 "PUT" 71 "ANU" 72 "POL" 81 "BAD" 82 "MON" 91 "RAT" 92 "KEG"
label values district dist

tostring district, gen(district1)
gen province=substr(district1,1,1)

destring province, replace

label define province 1 "WP" 2 "CP" 3 "SP" 4 "NP" 5 "EP" 6 "NWP" 7 "NCP" 8 "UVA" 9 "SAB"
label values province province

gen sector=a_05

*District dummies
tab district, gen(dist_)
tab province, gen(prov_)
tab sector, gen(sector_)

gen urban=(sector==1)
gen rural=(sector==2)

*ethnicity 
label define  ethn 1 "Sinhala" 2"Tamil" 3"Indian Tamil" 4"Moor/Muslim" 5"Burgher" 6"Malay" 9"Other"
ren b1_08 ethnicity
recode ethnicity (999=9)
label values ethnicity ethn 

gen sinhala=ethnicity==1

*religion 
label define rel 1"Buddhist" 2 "Hindu" 3"Islam" 4"Catholic" 5"Christian" 9"Other"
ren b1_09 religion
recode religion (6 7 = 9)
label values religion rel 

gen buddhist=religion==1 

*sex
recode b0_02 (1=0) (0=1), gen(male)

*marital_status
recode b1_06 (1=1) (2/5=2) (6=3) (7=4) (8=5) (998 = 9), gen(marstat)

label define marital 1 "Never married" 2"Married" 3"Widowed" 4"Divorced" 5"Separated" 9 "Don't know"
//Married includes living together informally (only 15 people)
label values marstat marital

gen married = marstat==2

ren b0_04_years age

ren b0_03 relationship

* Derive head flag, reassigning headship if original head was filtered out
gen head_flag = (relationship == 1)

bysort hhid: egen has_head = max(head_flag)

* First fallback: promote spouse
replace head_flag = (relationship == 2) if has_head == 0

bysort hhid: egen has_head2 = max(head_flag)

* Second fallback: oldest remaining member
bysort hhid: egen max_age_hh = max(age) if has_head2 == 0
replace head_flag = (age == max_age_hh) if has_head2 == 0

drop has_head has_head2 max_age_hh


********************************************************************************
*HH level Variables: district province sector dist_* prov_* sector_* 
********************************************************************************
 
********************************************************************************
*Education 
********************************************************************************
ren b2_02 currently_enrolled

recode b2_08 (1 = 1) (2 = 2) (3 = 3) (4 = 4) (5 6 7 8 9 10 = 5) if age>=5, g(educat5)
replace educat5=1 if b2_06==0 & age>=5 //never attended school
tab b2_03 if b2_08==. //no currently studying respondents have skipped b2_08

label define edu 1 "Less than primary" 2 "Completed primary" 3 "Completed O/L" 4 "Completed A/L" 5 "Anything more than A/L"
label values educat5 edu

gen noedu=(b2_06==0 & age>=5) //never attended school

gen atleast_sec=(educat5==3 | educat5==4 | educat5==5)

gen schoolage_noschool=(age>=5 & age<=16 & currently_enrolled==0)

bysort hhid: egen have_atleast_secedu=max(atleast_sec)

bysort hhid: egen have_schoolage_noschl=max(schoolage_noschool)

//Currently in education 
gen curr_school =  b2_03 <=13

bys hhid : egen has_in_school = max(curr_school)
gen sh_in_school = has_in_school / hhmem 

*******************************************************************************
*Head of household Characteristics 
*******************************************************************************

gen femaleHHH=(head_flag==1 & male==0)
bysort hhid: egen female_hhh=max(femaleHHH)

gen age_HHH = age if head_flag==1
bysort hhid: egen age_hhh=max(age_HHH)

gen hhh_married = (head_flag==1 & married==1)
bysort hhid: egen married_hhh=max(hhh_married)

gen hhh_buddhist = (head_flag==1 & buddhist==1)
bysort hhid: egen buddhist_hhh=max(hhh_buddhist)

gen hhh_sinhala = (head_flag==1 & sinhala==1)
bysort hhid: egen sinhala_hhh=max(hhh_sinhala)

*******************************************************************************
*Household Characteristics 
*******************************************************************************
bys hhid: egen age_avg=mean(age)

gen dep=age<15 | age>=65

gen child=age<15
gen old= age>=65

gen labor = age>=15 & age<65 

bysort hhid: egen num_deps=total (dep) , missing 
bysort hhid: egen num_kids=total (child) , missing 
bysort hhid: egen num_old=total (old) , missing 
bysort hhid: egen num_labor=total (labor) , missing 

gen dep_ratio=num_deps/num_labor
gen share_dep=num_deps/hhmem
gen share_kids=num_kids/hhmem

tab hhmem if dep_ratio==.  
tab num_deps if dep_ratio==. 


	*Sex structure 
		g aux_fem 	= male==0
		g aux_male	= male==1
		bys hhid: egen aux_fem_tot = total(aux_fem)
		bys hhid: egen aux_male_tot = total(aux_male)
		g sex_ratio = aux_male_tot/aux_fem_tot  
		lab var sex_ratio "Ratio men to women among all HH members"
		
	* Age structure 
		g aux_014		= age<15
		g aux_1564 		= age>=15 & age<=64
		g aux_65plus 	= age>=65 & age<.
		g aux_0			= age==0
		g aux_1			= age==1
		g aux_2			= age==2
		g aux_3			= age==3
		g aux_4 		= age==4
		
		foreach var in _014 _1564 _0 _1 _2 _3 _4 _male _fem {
		bys hhid: egen hh_mem`var' = total(aux`var')
		g sh_mem`var' = hh_mem`var'/ hhmem
		}
		
* Edu structure 
		
g aux_edu_hhh_none	= noedu==1 & head_flag==1
g aux_edu_hhh_lessprim	= educat5==1 & head_flag==1
g aux_edu_hhh_primary	= educat5==2 & head_flag==1
g aux_edu_hhh_OL		= educat5==3 & head_flag==1
g aux_edu_hhh_AL		= educat5==4 & head_flag==1
g aux_edu_hhh_moreAL	= educat5==5 & head_flag==1

	
	loc tomax edu_hhh_none edu_hhh_lessprim edu_hhh_prim edu_hhh_OL edu_hhh_AL edu_hhh_moreAL 
	foreach var of loc tomax {
	
	bys hhid: egen `var'=max(aux_`var')
	assert `var'!=.
	drop aux_`var'
	
	}
	
	g aux_edu_sh_5plus_none 	= noedu==1 & age>=5 & age<.
	g aux_edu_sh_1564_none 		= noedu==1 & age>=15 & age<65
		
	loc tomean edu_sh_5plus_none edu_sh_1564_none
	
	foreach var of loc tomean{
	bys hhid: egen `var'=mean(aux_`var')
	drop aux_`var'
	}
	
	lab var edu_hhh_none 			"Household head: no education"
	lab var edu_hhh_lessprim		"Household head with below primary (incl no education)"
	lab var edu_hhh_prim			"Household head with completed primary education"
	lab var edu_hhh_OL				"Household head with completed OL"
	lab var edu_hhh_AL				"Household head with completed AL"
	lab var edu_hhh_moreAL			"Household head with more than AL"
	lab var edu_sh_5plus_none 		"Share of hh members (5+) with no education"
	lab var edu_sh_1564_none 		"Share of hh members (15-64) with no education"

*******************************************************************************
//Chronic illness
*******************************************************************************
split b3_02, destring //allows multiple diseases (HIES only allows one)

gen heart_disease 	= b3_021 ==1 | b3_022==1 | b3_023==1 | b3_024==1 | b3_025==1 | b3_026==1 | b3_027==1 | b3_028==1
gen blood_pressure 	= b3_021 ==10 | b3_022==10 | b3_023==10 | b3_024==10 | b3_025==10 | b3_026==10 | b3_027==10 | b3_028==10
gen diabetes		= b3_021 ==11 | b3_022==11 | b3_023==11 | b3_024==11 | b3_025==11 | b3_026==11 | b3_027==11 | b3_028==11
gen asthma			= b3_021 ==12 | b3_022==12 | b3_023==12 | b3_024==12 | b3_025==12 | b3_026==12 | b3_027==12 | b3_028==12
gen kidney_disease 	= b3_021 ==15 | b3_022==15 | b3_023==15 | b3_024==15 | b3_025==15 | b3_026==15 | b3_027==15 | b3_028==15
gen arthritis		= b3_021 ==2 | b3_022==2 | b3_023==2 | b3_024==2 | b3_025==2 | b3_026==2 | b3_027==2 | b3_028==2
gen mental_illness	= b3_021 ==3 | b3_022==3 | b3_023==3 | b3_024==3 | b3_025==3 | b3_026==3 | b3_027==3 | b3_028==3
gen any_chronillness= b3_01==1
gen no_chronicillness= b3_01==0

foreach var in heart_disease blood_pressure diabetes asthma kidney_disease arthritis mental_illness any_chronillness no_chronicillness {
	bys hhid: egen num_`var' = sum(`var') //number of people in the HH with the disease
	bys hhid: egen have_`var' = max(`var') //someone in the HH has the disease
	}
	
********************************************************************************
*HH level Variables: have_atleast_secedu have_schoolage_noschlhas_in_school sh_in_school female_hhh age_hhh married_hhh buddhist_hhh sinhala_hhh age_avg num_deps num_kids num_old num_labor dep_ratio share_dep share_kids sex_ratio hh_mem_014 hh_mem_1564 hh_mem_0 hh_mem_1 hh_mem_2 hh_mem_3 hh_mem_4 hh_mem_male hh_mem_fem sh_mem_014 sh_mem_1564 sh_mem_0 sh_mem_1 sh_mem_2 sh_mem_3 sh_mem_4 sh_mem_male sh_mem_fem edu_hhh_none edu_hhh_lessprim edu_hhh_prim edu_hhh_OL edu_hhh_AL edu_hhh_moreAL edu_sh_5plus_none edu_sh_1564_none num_heart_disease - no_chronicillness_hhh
********************************************************************************

save "$rundata\demo.dta", replace

keep hhid district province sector year month dist_* prov_* sector_* hhmem have_atleast_secedu have_schoolage_noschl has_in_school sh_in_school female_hhh age_hhh married_hhh buddhist_hhh sinhala_hhh age_avg num_deps num_kids num_old num_labor dep_ratio share_dep share_kids sex_ratio hh_mem_014 hh_mem_1564 hh_mem_0 hh_mem_1 hh_mem_2 hh_mem_3 hh_mem_4 hh_mem_male hh_mem_fem sh_mem_014 sh_mem_1564 sh_mem_0 sh_mem_1 sh_mem_2 sh_mem_3 sh_mem_4 sh_mem_male sh_mem_fem edu_hhh_none edu_hhh_lessprim edu_hhh_prim edu_hhh_OL edu_hhh_AL edu_hhh_moreAL edu_sh_5plus_none edu_sh_1564_none num_heart_disease - have_no_chronicillness weight popweight

duplicates drop
save "$rundata\demo_HH.dta", replace

*******************************************************************************
// Housing conditions
*******************************************************************************
use "$wide/mod_c_housing.dta", clear

*Structure
recode c1_01 (999 10 = 99), gen(structure)
gen single_storey = structure==1
gen line_room = structure==8
gen shanty = structure==9

ren c1_02 bed_rooms

ren c1_03 area
gen lessthan_500sqft = area<=2

recode c1_04 (6 7 8 9 10 11 999 = 9), gen(wall_type)
recode wall_type (1 2 4 = 1) (3 5 9 = 0), gen(brickwalls)

recode c1_05 (7 999 = 9), gen(floor)
gen floortiles=(floor==2)

recode c1_06 (7 999 = 9), gen(roof)
gen semiperm_roof=(roof==4 | roof==5 | roof==6)

recode c1_07 (1 3=1) (2 5 =2) (6 = 3) (4 = 4) (7 8 = 5) (999 = 99), gen(house_tenure)
gen house_owned = (house_tenure==1 | house_tenure==2)

*******************************************************************************
// Sanitation 
*******************************************************************************
recode c1_08 (1 2 3 4 5 = 1) (6 7 8 9 11 12 = 2) (10 13 =3), gen(type_toilet)

gen exclusive_toilet = (c1_09==1 | c1_09==2) & c1_10==0

gen inside_toilet = (c1_09==1 | c1_09==2)

gen garbage_collected=(c1_12_1==1 | c1_12_2==1)
gen garbage_burned=(c1_12_5==1)

ren hhcode hhid

keep hhid structure single_storey-shanty bed_rooms area lessthan_500sqft wall_type brickwalls floor floortiles roof semiperm_roof house_tenure house_owned type_toilet exclusive_toilet inside_toilet garbage_collected garbage_burned

save "$rundata/housing_characteristics.dta", replace


use "$wide/mod_l4_water_source.dta", replace

ren hhcode hhid

recode l4_01 (1 2 3 = 1) (4 5 6 7 = 2) (8 9 10 11 12 13 14 15 16 999 = 9), gen(water_source)

gen water_safe_boil=l4_04_1==1
gen water_safe_nothing = l4_04_8==1

keep hhid water_source water_safe_boil water_safe_nothing

save "$rundata/water_source.dta", replace

*******************************************************************************
// Asset ownership
*******************************************************************************
use "$wide/mod_d_assets.dta", clear

ren hhcode hhid

gen tv=(d_05>0)
gen computers=(d_06>0)
gen washing_mechine=d_12
gen fridge=d_15
gen cookers=d_16
gen electric_fans=(d_17>0)
gen airconditioners=(d_18>0)
gen bicycle=(d_19>0)
gen motor_bicycle=(d_20>0)
gen three_wheeler=(d_21>0)
gen motor_car_van=(d_22>0)
gen bus_lorry=(d_23>0)

keep hhid tv-bus_lorry

save "$rundata/assets.dta", replace

*******************************************************************************
// Productive asset ownership
*******************************************************************************
use "$wide/mod_f11_ag_assets.dta", clear

ren hhcode hhid

gen threshers=f11_08a_1==1
gen waterpumps=f11_09a_1==1
gen mechine=f11_10a_1==1

keep hhid threshers-mechine

save "$rundata/agri_assets.dta", replace

use "$wide/mod_g1_livestock.dta", clear

ren hhcode hhid

gen chickens=(g1_01>0 & g1_01<.)
gen cattle_buffaloes=(g1_07>0 & g1_07<.)
replace cattle_buffaloes=1 if (g1_10>0 & g1_10<.)
gen goats_sheeps=(g1_14>0 & g1_14<.)

keep hhid chickens-goats_sheeps

save "$rundata/livestock.dta", replace

use "$wide/mod_g2_fish.dta", clear

ren hhcode hhid

gen boats=(g2_12==1)

keep hhid boats

save "$rundata/fishing_boat.dta", replace

*******************************************************************************
// Land ownership
*******************************************************************************
use "$long/mod_e_land_long.dta", clear

ren hhcode hhid

gen HH_owns_land=e1_00

gen housing_land=(e1_05_8==1 |e1_05_12==1 | e1_05_14==1)
gen agri_land=(e1_05_1==1| e1_05_2==1| e1_05_3==1| e1_05_4==1) //don't include kitchen garden
gen business_land=(e1_05_11 ==1 |e1_05_13 ==1)

bysort hhid: egen have_housing_land=max(housing_land)
bysort hhid: egen have_agri_land=max(agri_land)
bysort hhid: egen have_business_land=max(business_land)

keep hhid have_housing_land have_agri_land have_business_land 
duplicates drop

save "$rundata/land.dta", replace

********************************************************************************
*SP Incomes   
********************************************************************************
use "$long\mod_r1_cash_long.dta", clear

ren hhcode hhid
keep hhid r1_02 r1_04

keep if inlist(r1_02, 1, 2,3,4,5,6,7,8,9,11,13)
reshape wide r1_04, i(hhid) j(r1_02)

gen hh_samurdhi_aswes=r1_041
egen hh_pensions = rowtotal(r1_042 r1_043 r1_045 r1_046 r1_047 r1_048)
egen hh_elder = rowtotal(r1_044 r1_0413)
gen hh_tb = r1_0411
gen hh_disability = r1_049

keep hhid hh_samurdhi_aswes - hh_disability

save "$rundata\cash_transfers.dta", replace

********************************************************************************
*Indebtedness
********************************************************************************
use "$wide\mod_n1_debt.dta", clear

gen debt_banks = (n1_01_1==1 | n1_01_2==1)
gen debt_financecomp = (n1_01_3==1 | n1_01_4==1)
gen debt_pawn_moneylender = (n1_01_5==1 | n1_01_7==1)
gen debt_employer = (n1_01_12==1)
gen debt_retail = (n1_01_9==1 | n1_01_10==1)
gen debt_other = (n1_01_6==1 | n1_01_8==1 | n1_01_11==1 | n1_01_999==1)

gen debt_any = n1_0==1

ren hhcode hhid
keep  hhid debt_banks -debt_any

save "$rundata\debt.dta", replace

********************************************************************************
*Food insecurity
********************************************************************************
use "$wide\mod_l1_food_security_w.dta", clear

gen worry_food=l1_01==1
gen nohealthy_food=l1_02==1
gen limited_food=l1_03==1
gen skipmeals_food=l1_04==1
gen eatless_food=l1_05==1
gen runout_food=l1_06==1
gen hungry_food=l1_07==1
gen fulldaywo_food=l1_08==1

ren hhcode hhid

keep hhid worry_food-fulldaywo_food

save "$rundata/food_insecurity.dta", replace

********************************************************************************
*Involved in agricultural activities in last 12 months
********************************************************************************
use "$wide/mod_f1_ag_seasons.dta", clear

ren f1_00a HH_has_agriactivity

ren hhcode hhid

keep hhid HH_has_agriactivity

save "$rundata/HH_in_agri.dta", replace

*Profit from fishing
use "$wide/mod_g2_fish.dta", clear

ren hhcode hhid

gen HH_fishing_profit = g2_06-g2_07 //monthly

keep hhid HH_fishing_profit

save "$rundata/fishing_profit.dta", replace

*Profit from livestock
use "$wide/mod_g1_livestock.dta", clear

ren hhcode hhid

egen HH_livestock_revenue=rowtotal(g1_02 g1_02a g1_03 g1_05 g1_05a g1_06 g1_08 g1_08a g1_09 g1_11 g1_12 g1_15 g1_15a g1_16 g1_17_b) //annual
egen HH_livestock_costs = rowtotal(g1_17 g1_18 g1_20 g1_21 g1_22) //annual

gen HH_livestock_profit= ( HH_livestock_revenue-HH_livestock_costs)/12 //monthly

keep hhid HH_livestock_profit

save "$rundata/livestock_profit.dta", replace

*Profit from cultivation
*Yala 2024
use "$long\mod_f2_ag_yala_2024_long.dta", clear

ren hhcode hhid

bysort hhid: egen HH_yala_profits=total(f2_28) //annual
replace HH_yala_profits=HH_yala_profits/12

keep hhid HH_yala_profits
duplicates drop

save "$rundata\agri_yala_profits.dta", replace

*Maha 2023/24
use "$long\mod_f3_ag_maha_2023-2024_long.dta", clear

ren hhcode hhid

bysort hhid: egen HH_maha_profits=total(f3_28) //annual
replace HH_maha_profits=HH_maha_profits/12

keep hhid HH_maha_profits
duplicates drop

save "$rundata\agri_maha_profits.dta", replace

*Year-long crops
use "$long\mod_f4_ag_yearly_long.dta", replace

ren hhcode hhid

bysort hhid: egen HH_yearlong_profits=total(f4_28)
replace HH_yearlong_profits=HH_yearlong_profits/12

keep hhid HH_yearlong_profits //annual
duplicates drop

save "$rundata\agri_yearlong_profits.dta", replace

*Perennial crops
use "$long\mod_f6_ag_perennial_crops_long.dta", replace

ren hhcode hhid

bysort hhid: egen HH_perennial_profits=total(f4_28)
replace HH_perennial_profits=HH_perennial_profits/12

keep hhid HH_perennial_profits //annual
duplicates drop

save "$rundata\agri_perennial_profits.dta", replace

use "$rundata\HH_in_agri.dta", clear

merge 1:1 hhid using "$rundata/fishing_profit.dta", nogen
merge 1:1 hhid using "$rundata/livestock_profit.dta", nogen
merge 1:1 hhid using "$rundata/agri_yala_profits.dta", nogen
merge 1:1 hhid using "$rundata/agri_maha_profits.dta", nogen
merge 1:1 hhid using "$rundata/agri_yearlong_profits.dta", nogen
merge 1:1 hhid using "$rundata/agri_perennial_profits.dta", nogen

*following HIES cleaning, recode negative profits as zero
foreach var of varlist HH_fishing_profit-HH_perennial_profits {
	replace `var'=0 if `var'<0
}

egen HH_agritotal_profit = rowtotal(HH_fishing_profit HH_livestock_profit HH_yala_profits HH_maha_profits HH_yearlong_profits HH_perennial_profits)

save "$rundata\agri_profits.dta", replace

********************************************************************************
*Involved in non-agricultural activities in last 12 months
********************************************************************************
use "$wide\mod_i_nonfarm_business.dta", clear

ren i_01 HH_has_nonagriactivity
keep HH_has_nonagriactivity hhcode i_06_1 i_08_1 i_07_1 i_06_2 i_07_2 i_08_2 i_06_3 i_07_3 i_08_3
reshape long i_06_ i_07_ i_08_ , i(hhcode) j(bcode)

recode i_06_ (4 5 = 1) (26=2) (1 2 3 6 7 8 10 11 13 14 15 17 19 20 24 28 30= 3), gen(business_sector)

gen nonagriprofit = i_08_ - i_07_ //monthly
replace nonagriprofit=0 if nonagriprofit<0

bysort hhcode business_sector: egen hh_nonagri_profit=total(nonagriprofit)

keep hhcode business_sector hh_nonagri_profit HH_has_nonagriactivity
drop if business_sector==.
duplicates drop

ren hhcode hhid

reshape wide hh_nonagri_profit , i(hhid) j(business_sector)

keep hhid hh_nonagri_profit1 hh_nonagri_profit3 hh_nonagri_profit999 HH_has_nonagriactivity

egen HH_nonagriprofit=rowtotal(hh_nonagri_profit1 hh_nonagri_profit3 hh_nonagri_profit999 )

save "$rundata/nonagri_profits.dta", replace

********************************************************************************
						//LABOR MARKET// 
********************************************************************************
use "$long\mod_h1_primary_employment.dta", clear
ren b0_04 age 
ren b0_03 relationship
ren hhcode hhid 

merge m:1 hhid using "$rundata/agri_profits.dta", nogen
merge m:1 hhid using "$rundata/nonagri_profits.dta", nogen

* Re-derive head_flag consistent with demographic section
gen head_flag = (relationship == 1)
bysort hhid: egen has_head = max(head_flag)
replace head_flag = (relationship == 2) if has_head == 0
bysort hhid: egen has_head2 = max(head_flag)
bysort hhid: egen max_age_hh = max(age) if has_head2 == 0
replace head_flag = (age == max_age_hh) if has_head2 == 0
drop has_head has_head2 max_age_hh

recode HH_nonagriprofit hh_nonagri_profit1 hh_nonagri_profit3 hh_nonagri_profit999 (.=0)
label define empstat 0 "Not employed" 1"Employee" 2 "Family Worker" 3 "Employer" 4 "Own account worker" 
recode h3_01_ (8 9 10 =0) (1 2 6=1) (5 7=2) (3=3) (4=4) if age>=15, gen (empstat)

label values empstat empstat 

gen employee			=empstat==1
gen ownaccount_employer =empstat>=3 & empstat!=.
gen familyworker        =empstat==2 
gen employer			=empstat==3
gen self_employed       =empstat==4

tab employee empstat 
tab ownaccoun empstat 
tab familyworker empstat 

foreach var in employee ownaccount_employer familyworker employer self_employed {
	gen hhh_`var' = `var' if head_flag==1
	bysort hhid: egen `var'_hhh = max(hhh_`var')
	bysort hhid: egen num_`var'_hh = sum(`var') 
}

*Industry
ren h3_02_ industry

recode industry (1 2 3 = 1) (4 6 8 12 = 2) (7 = 3) (5 9 10 11 13/15 17 18 = 4) (16 19=.) if age>=15, gen(broad_industry)
label define broad_ind 1 "Agri" 2 "Manufacturing (excl construction)" 3 "Construction" 4 "Services"
label values broad_industry broad_ind

tab broad_industry, gen(broad_ind_)

bysort hhid: egen have_agri_emp=max(broad_ind_1)
bysort hhid: egen have_ind_emp=max(broad_ind_2)
bysort hhid: egen have_constr_emp=max(broad_ind_3)
bysort hhid: egen have_serv_emp=max(broad_ind_4)
recode have_agri_emp have_ind_emp have_constr_emp have_serv_emp (.=0)

bysort hhid: egen num_agri_emp=total (broad_ind_1)  
bysort hhid: egen num_indexcons_emp=total (broad_ind_2)  
bysort hhid: egen num_cons_emp=total (broad_ind_3)  
egen num_ind_emp=rowtotal(num_indexcons_emp num_cons_emp)  
bysort hhid: egen num_serv_emp=total (broad_ind_4)  

egen most_emp=rowmax(num_agri_emp num_ind_emp num_serv_emp)

gen hh_main_agri=(most_emp==num_agri_emp)
gen hh_main_ind=(most_emp==num_ind_emp)
gen hh_main_serv=(most_emp==num_serv_emp)

replace hh_main_agri=0 if most_emp==. | most_emp==0
replace hh_main_ind=0 if most_emp==. | most_emp==0
replace hh_main_serv=0 if most_emp==. | most_emp==0

********************************************************************************
*Income from paid employment as an employee
********************************************************************************
gen monthly_income=primaryjob_income *30  if h3_08_==1 & empstat==1
replace monthly_income=primaryjob_income *30/7  if h3_08_==2  & empstat==1
replace monthly_income=primaryjob_income *30/14  if h3_08_==3 & empstat==1
replace monthly_income=primaryjob_income  if h3_08_==4 & empstat==1
replace monthly_income=primaryjob_income *3  if h3_08_==5 & empstat==1
replace monthly_income=primaryjob_income /1.5  if h3_08_==6 & empstat==1
replace monthly_income=primaryjob_income /2  if h3_08_==7 & empstat==1
replace monthly_income=primaryjob_income /3  if h3_08_==8 & empstat==1
replace monthly_income=primaryjob_income /4  if h3_08_==9 & empstat==1
replace monthly_income=primaryjob_income /6  if h3_08_==10 & empstat==1
replace monthly_income=primaryjob_income /12  if h3_08_==11 & empstat==1

egen ind_monthly_income = rowtotal(monthly_income h4_06_1_ h4_06_2_ h4_06_3_)

bysort hhid: egen HH_wages = total(ind_monthly_income)

********************************************************************************
*Income from employment 
********************************************************************************
egen HH_total_empincome=rowtotal(HH_wages HH_agritotal_profit HH_nonagriprofit)

********************************************************************************
*Dummies for income source  
********************************************************************************
gen has_wages    		= HH_wages>0 & HH_wages!=.
gen has_selfemp_inc 	= ((HH_nonagriprofit>0 & HH_nonagriprofit!=.) | (HH_agritotal_profit>0 & HH_agritotal_profit!=. ))

gen sh_wages 	= HH_wages /  HH_total_empincome
gen sh_selfemp  = 1-sh_wages

// Household head 
gen hhh_wages = ind_monthly_income if head_flag==1 
bysort hhid: egen wages_hhh=total(hhh_wages)

********************************************************************************
*Income by sector 
********************************************************************************
gen agriwage = ind_monthly_income if broad_industry==1
gen indwage = ind_monthly_income if broad_industry==2 | broad_industry==3
gen servicewage = ind_monthly_income if broad_industry==4

bysort hhid: egen HH_agriwage=total(agriwage)
bysort hhid: egen HH_indwage=total(indwage)
bysort hhid: egen HH_servwage=total(servicewage)

egen hh_agri_inc = rowtotal(HH_agriwage HH_agritotal_profit)
egen hh_industry_inc = rowtotal(HH_indwage hh_nonagri_profit1)
egen hh_services_inc = rowtotal(HH_servwage hh_nonagri_profit3)

egen HH_max_income=rowmax(hh_agri_inc hh_industry_inc hh_services_inc)

gen hh_maininc_agri=(HH_max_income==hh_agri_inc)
gen hh_maininc_ind=(HH_max_income==hh_industry_inc)
gen hh_maininc_serv=(HH_max_income==hh_services_inc)

replace hh_maininc_agri=0 if HH_max_income==. | HH_max_income==0
replace hh_maininc_ind=0 if HH_max_income==. | HH_max_income==0
replace hh_maininc_serv=0 if HH_max_income==. | HH_max_income==0

keep hhid employee_hhh num_employee_hh ownaccount_employer_hhh num_ownaccount_employer_hh familyworker_hhh num_familyworker_hh employer_hhh num_employer_hh self_employed_hhh num_self_employed_hh have_agri_emp-hh_main_serv HH_wages HH_total_empincome has_wages has_selfemp_inc sh_wages sh_selfemp wages_hhh hh_agri_inc-hh_maininc_serv HH_agritotal_profit HH_nonagriprofit

duplicates drop

save "$rundata\employment_income.dta", replace

********************************************************************************
*****************************COMBINE DATASETS*********************************** 
********************************************************************************
use "$rundata/demo_HH.dta", clear

merge 1:1 hhid using "$rundata/housing_characteristics.dta", nogen
gen bedroom_pc=bed_rooms/hhmem

merge 1:1 hhid using "$rundata/water_source.dta", nogen
merge 1:1 hhid using "$rundata/assets.dta", nogen
merge 1:1 hhid using "$rundata/agri_assets.dta", nogen
recode threshers waterpumps mechine (.=0)

merge 1:1 hhid using "$rundata/livestock.dta", nogen
recode chickens cattle_buffaloes goats_sheeps (.=0)

merge 1:1 hhid using "$rundata/fishing_boat.dta", nogen
recode boats (.=0)

merge 1:1 hhid using "$rundata/land.dta", nogen
recode have_housing_land have_agri_land have_business_land (.=0)

merge 1:1 hhid using "$rundata/cash_transfers.dta", nogen
recode hh_samurdhi_aswes hh_pensions hh_tb hh_elder hh_disability (.=0)
gen have_samurdhi_aswes=(hh_samurdhi_aswes>0)
gen have_pensions=(hh_pensions>0)
gen have_tb=(hh_tb>0)
gen have_elder=(hh_elder>0)
gen have_disability=(hh_disability>0)

foreach var of varlist hh_samurdhi_aswes hh_pensions hh_tb hh_elder hh_disability {
	gen `var'_pc = `var'/hhmem
}

merge 1:1 hhid using "$rundata/debt.dta", nogen
recode debt_banks debt_financecomp debt_pawn_moneylender debt_employer debt_retail debt_other debt_any (.=0)

merge 1:1 hhid using "$rundata/food_insecurity.dta", nogen
merge 1:1 hhid using "$rundata/employment_income.dta", nogen
gen HH_wages_pc=HH_wages/hhmem

xtile HH_wage_quintile = HH_wages [aw=weight], nq(5)
xtile HH_wage_pc_quintile = HH_wages_pc [aw=weight], nq(5)

gen HH_agritotal_profit_pc=HH_agritotal_profit/hhmem

xtile HH_agriinc_quintile=HH_agritotal_profit [aw=weight], nq(5)
xtile HH_agriinc_pc_quintile=HH_agritotal_profit_pc [aw=weight], nq(5)

gen HH_nonagriprofit_pc=HH_nonagriprofit/hhmem

xtile HH_nonagri_quintile=HH_nonagriprofit [aw=weight], nq(5)
xtile HH_nonagri_pc_quintile=HH_nonagriprofit_pc [aw=weight], nq(5)


gen HH_total_empincome_pc=HH_total_empincome/hhmem

xtile HH_totinc_quintile = HH_total_empincome [aw=weight], nq(5)
xtile HH_totinc_pc_quintile = HH_total_empincome_pc [aw=weight], nq(5)

xtile HHH_wage_quintile=wages_hhh [aw=weight], nq(5)

replace sh_wages=0 if sh_wages==.
replace sh_selfemp=0 if sh_selfemp==.

save "$rundata/harmonized_BRIGHT.dta", replace
