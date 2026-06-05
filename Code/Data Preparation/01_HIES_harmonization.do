***********************************************************************
*	Clean 2019 HIES 
***********************************************************************


************************************************************************
*Process land dataset first
************************************************************************
import delimited "$hies2019/RAW/csv/SEC_9_LAND_ANIMAL.csv", clear 

gen housing_land=s9_useofland==1
gen agri_land=s9_useofland==2 | s9_useofland==3
gen business_land=s9_useofland==5
gen otheruse_land=s9_useofland==4|s9_useofland==6|s9_useofland==7

bysort district sector psu snumber hhno nhh: egen have_housing_land=max(housing_land)
bysort district sector psu snumber hhno nhh: egen have_agri_land=max(agri_land)
bysort district sector psu snumber hhno nhh: egen have_business_land=max(business_land)

keep district sector psu snumber hhno nhh have_housing_land have_agri_land have_business_land
duplicates drop

save "$hies2019/RAW/rundata/land_forbright.dta", replace

use "$hies2019/RAW/rundata/consumption_ind.dta", clear

destring snumber hhno nhh, replace
merge m:1 district sector psu snumber hhno nhh using "$hies2019/RAW/Stata/sec_5_2_is_agri_income.dta"
drop _m

merge m:1 district sector psu snumber hhno nhh using "$hies2019/RAW/Stata/sec_5_3_is_other_agri_income.dta"
drop _m

merge m:1 district sector psu snumber hhno nhh using "$hies2019/RAW/Stata/sec_5_4_is_non_agri_income.dta"
drop _m

merge m:1 district sector psu snumber hhno nhh using "$hies2019/RAW/rundata/land_forbright.dta"
drop _m

merge m:1 district sector psu snumber hhno nhh using "$hies2019/RAW/rundata/land_forbright.dta"
drop _m

merge m:1 district sector psu snumber hhno nhh using "$hies2019/RAW/SEC_10_FOODEXP.dta"
drop _m

/*
//Tiloka's code for hhid does not match that in dlw:using dlw for consistency with lfs s2s//

tostring district sector month psu snumber hhno nhh person_serial_no, replace force
replace month="0"+month if strlen(month)==1
replace snumber="0"+snumber if strlen(snumber)==1
replace person_serial_no="0"+person_serial_no if strlen(person_serial_no)==1

gen hhid=district+sector+month+psu+snumber+hhno
gen inid=district+sector+month+psu+snumber+hhno+person_serial_no
*/
ren hhid hhid_tiloka 

preserve
use "$hies2019/SARMD/031226_sharefoodnfood_hhid.dta", clear
keep if pid == "1"
drop pid
tempfile shares
save `shares'
restore

preserve
use "$data/cleaned/hies2019_clean.dta", clear
merge 1:1 hhid using `shares'
drop _m
keep hhid welfare district sector psu snumber hhno nhh share_food share_nonfood
tempfile hiesclean
save `hiesclean'
restore

merge m:1 district sector psu snumber hhno nhh using `hiesclean' , keepusing (hhid welfare share_food share_nonfood)
drop _m

//br hhid hhid_tiloka district sector psu snumber hhno nhh month

********************************************************************************
//DEMOGRAPHIC//
********************************************************************************
*Provinces
tostring district, gen(district1)
gen province=substr(district1,1,1)

destring province, replace

label define province 1 "WP" 2 "CP" 3 "SP" 4 "NP" 5 "EP" 6 "NWP" 7 "NCP" 8 "UVA" 9 "SAB"
label values province province

*District dummies
tab district, gen(dist_)
tab province, gen(prov_)
tab sector, gen(sector_)

gen urban=(sector==1)
gen rural=(sector==2)

*ethnicity 
label define  ethn 1 "Sinhala" 2"Tamil" 3"Indian Tamil" 4"Moor/Muslim" 5"Burgher" 6"Malay" 9"Other"
label values ethnicity ethn 

gen sinhala=ethnicity==1

*religion 
label define rel 1"Buddhist" 2 "Hindu" 3"Islam" 4"Catholic" 5"Christian" 9"Other"
label values religion rel 
gen buddhist=religion==1 

*sex
recode sex (2=0), gen(male)

*marital_status
recode marital_status (1=1) (2/3=2) (4=3) (5=4) (6/7=5), gen(marstat)

label define marital 1 "Never married" 2"Married" 3"Widowed" 4"Divorced" 5"Separated"
//doesn't include living together without marrying (BRIGHT has this)
label values marstat marital

tab marital_status
gen married = marstat==2

********************************************************************************
*HH level Variables: district province sector dist_* prov_* sector_* 
********************************************************************************
 
********************************************************************************
*Education 
********************************************************************************

recode education (19 = 1) (0/4 = 1) (5/10 = 2) (11/12 = 3) (13/14 = 4) (15/18=5) if age>=5, g(educat5)
note educat5: We categorized "Special Education learning / learnt" as educat5 = 5 "Anything higher than secondary".

label define edu 1 "Less than primary" 2 "Completed primary" 3 "Completed O/L" 4 "Completed A/L" 5 "Anything more than A/L"
label values educat5 edu

gen noedu=(education==19 & age>=5)

gen atleast_sec=(educat5==3 | educat5==4 | educat5==5)

gen schoolage_noschool=(age>=5 & age<=16 & curr_educ==9)

bysort hhid: egen have_atleast_secedu=max(atleast_sec)

bysort hhid: egen have_schoolage_noschl=max(schoolage_noschool)

//Currently in education 
tab curr_educ

gen curr_school =  curr_educ<=2
gen curr_edu_other = curr_educ >3 & curr_educ<9

bys hhid : egen has_in_school = max(curr_school)
gen sh_in_school = has_in_school / hhmem 

*******************************************************************************
*Head of household Characteristics 
*******************************************************************************

gen femaleHHH=(relationship==1 & male==0)
bysort hhid: egen female_hhh=max(femaleHHH)

gen age_hhh = age if relationship==1
bysort hhid: egen hhh_age=max(femaleHHH)

gen hhh_married = (relationship==1 & married==1)
bysort hhid: egen married_hhh=max(hhh_married)

gen hhh_buddhist = (relationship==1 & buddhist==1)
bysort hhid: egen buddhist_hhh=max(hhh_buddhist)

gen hhh_sinhala = (relationship==1 & sinhala==1)
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

mdesc hhmem 
mdesc dep_ratio
mdesc share_*
tab hhmem if dep_ratio==.  
tab num_deps if dep_ratio==. 

	*Sex structure 
		g aux_fem 	= sex==2
		g aux_male	= sex==1
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
		
	g aux_edu_hhh_none	= education==19 & relationship==1
	g aux_edu_hhh_lessprim	= educat5==1 & relationship==1
	g aux_edu_hhh_primary	= educat5==2 & relationship==1
	g aux_edu_hhh_OL		= educat5==3 & relationship==1
	g aux_edu_hhh_AL		= educat5==4 & relationship==1
	g aux_edu_hhh_moreAL	= educat5==5 & relationship==1
	
	loc tomax edu_hhh_none edu_hhh_lessprim edu_hhh_prim edu_hhh_OL edu_hhh_AL edu_hhh_moreAL 
	foreach var of loc tomax {
	
	bys hhid: egen `var'=max(aux_`var')
	assert `var'!=.
	drop aux_`var'
	
	}
	
	g aux_edu_sh_5plus_none 	= education==19 & age>=5 & age<.
	g aux_edu_sh_1564_none 		= education==19 & age>=15 & age<65
		
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
	
********************************************************************************
*HH level Variables: have_atleast_secedu have_schoolage_noschl has_in_school sh_in_school female_hhh hhh_age married_hhh sinhala_hhh buddhist_hhh age_avg num_deps num_kids num_old num_labor dep_ratio share_dep share_kids aux_fem_tot aux_male_tot sex_ratio hh_mem_014 hh_mem_1564 hh_mem_0 hh_mem_1 hh_mem_2 hh_mem_3 hh_mem_4 hh_mem_male hh_mem_fem sh_mem_014 sh_mem_1564 sh_mem_0 sh_mem_1 sh_mem_2 sh_mem_3 sh_mem_4 sh_mem_male sh_mem_fem edu_hhh_none edu_hhh_lessprim edu_hhh_primary edu_hhh_OL edu_hhh_AL edu_hhh_moreAL edu_sh_5plus_none edu_sh_1564_none 
********************************************************************************


*******************************************************************************
//Chronic illness
*******************************************************************************
gen heart_disease 	= s3a_14==1
gen blood_pressure 	= s3a_14==2
gen diabetes		= s3a_14==3
gen asthma			= s3a_14==4
gen kidney_disease 	= s3a_14==8
gen arthritis		= s3a_14==11
gen mental_illness	= s3a_14==12
gen any_chronillness= s3a_13==1
gen no_chronicillness= s3a_13==2

foreach var in heart_disease blood_pressure diabetes asthma kidney_disease arthritis mental_illness any_chronillness no_chronicillness {
	bys hhid: egen num_`var' = sum(`var') //number of people in the HH with the disease
	bys hhid: egen have_`var' = max(`var') //someone in the HH has the disease
	}
	
********************************************************************************
*HH level Variables: num_heart_disease - num_no_chronicillness have_heart_disease - have_no_chronicillness
********************************************************************************

*******************************************************************************
// Housing conditions
*******************************************************************************
*Housing characteristics
//Variables to use as is: structure, bed_rooms, area, floor, roof
gen single_storey = structure==1
gen line_room = structure==8
gen shanty = structure==9

gen bedroom_pc=bed_rooms/hhmem

gen lessthan_500sqft = area<=2

recode walls (6 7 8 9 = 9), gen(wall_type)

recode walls (1 2 4=1) (3 5 6 7 8 9 =0), gen(brickwalls)

gen floortiles=(floor==2)

gen semiperm_roof=(roof==4 | roof==5 | roof==6)

recode ownership (2 3 4 = 2) (5 = 3) (7 8 = 4) (9 = 5) (99 6 = 99), gen(house_tenure)

gen house_owned = (house_tenure==1 | house_tenure==2)

*******************************************************************************
// Water/sanitation 
*******************************************************************************
recode toilet_type (1 2 3 4 = 1) (5 6 = 2) (7 9 =3), gen(type_toilet)

gen exclusive_toilet = tioilet_use==1

gen inside_toilet = (tioilet_use==1|tioilet_use==2)

recode garbage_dumping (3 4 = 2) (2 = 3) (5 = 4), gen(waste_disposal)
gen garbage_collected=(waste_disposal==1)
gen garbage_burned=(waste_disposal==3)

recode drinking_water (1 2 3 = 1) (4 5 6 7 = 2) (8 9 10 11 12 99 = 9), gen(water_source)

gen safe_water=s8_6c_water_safe
gen water_safe_boil=safe_water==1
gen water_safe_nothing = safe_water==8

*******************************************************************************
// Asset ownership
*******************************************************************************
recode tv computers washing_mechine cookers electric_fans s6a_aircon fridge  bus_lorry bicycle motor_car_van motor_bicycle three_wheeler  (2=0)
ren s6a_aircon airconditioners

*******************************************************************************
// Productive asset ownership
*******************************************************************************
recode threshers waterpumps mechine boats s9_cattle_buffaloes goats_sheeps chickens (2=0)
ren s9_cattle_buffaloes cattle_buffaloes

*******************************************************************************
// Land ownership
*******************************************************************************
gen HH_owns_land = is_agriland_owner==1

//Constructed variables - includes rented ones
recode have_housing_land have_agri_land have_business_land (.=0)

********************************************************************************
						//LABOR MARKET// 
********************************************************************************
ren employer employer_debt

*employment status
label define empstat 0 "Not employed" 1"Employee" 2 "Family Worker" 3 "Employer" 4 "Own account worker" 
recode employment_status (1 2 3=1) (6=2) (4=3) (5=4), gen (empstat)
replace empstat=0 if employment_status==.

label values empstat empstat 

gen employee			=empstat==1
gen ownaccount_employer=empstat>=3 & empstat!=.
gen familyworker=empstat==2 
gen employer				=empstat==3
gen self_employed           =empstat==4

tab employee empstat 
tab ownaccoun empstat 
tab familyworker empstat 

foreach var in employee ownaccount_employer familyworker employer self_employed {
	gen `var'_hhh = `var' if relationship==1 
	bysort hhid: egen num_`var'_hh = sum(`var')
}

*Industry
tostring industry, replace
replace industry="0"+industry if strlen(industry)==4

gen main_industry=substr(industry, 1, 2)
destring main_industry, replace

gen broad_industry=1 if main_industry<=3
replace broad_industry=2 if main_industry>=5 & main_industry<40
replace broad_industry=3 if main_industry>=41 & main_industry<=43
replace broad_industry=4 if broad_industry==. & main_industry!=.

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
gen ind_wage_allow=ind_wages+ind_allowances //exclude allowances

bysort hhid: egen HH_wages=sum(ind_wage_allow)
gen HH_wages_pc= HH_wages/hhmem

xtile HH_wage_quintile = HH_wages [aw=weight], nq(5)
xtile HH_wage_pc_quintile = HH_wages_pc [aw=weight], nq(5)

********************************************************************************
*Income from cultivation activities as employer/own account worker
********************************************************************************
gen HH_has_agriactivity = (is_agricultural_income==1 | is_other_agrri_income==1)

gen HH_agritotal_profit_pc=HH_agritotal_profit/hhmem

xtile HH_agriinc_quintile=HH_agritotal_profit [aw=weight], nq(5)
xtile HH_agriinc_pc_quintile=HH_agritotal_profit_pc [aw=weight], nq(5)

********************************************************************************
*Income from non-agri activities as employer/own account workers
********************************************************************************
gen HH_has_nonagriactivity = (is_non_agri_income==1)

gen HH_nonagriprofit_pc=HH_nonagriprofit/hhmem

xtile HH_nonagri_quintile=HH_nonagriprofit [aw=weight], nq(5)
xtile HH_nonagri_pc_quintile=HH_nonagriprofit_pc [aw=weight], nq(5)

********************************************************************************
*Income from employment 
********************************************************************************
egen HH_total_empincome=rowtotal(HH_wages HH_agritotal_profit HH_nonagriprofit)
gen HH_total_empincome_pc=HH_total_empincome/hhmem

xtile HH_totinc_quintile = HH_total_empincome [aw=weight], nq(5)
xtile HH_totinc_pc_quintile = HH_total_empincome_pc [aw=weight], nq(5)

********************************************************************************
*Dummies for income source  
********************************************************************************
gen has_wages    		= HH_wages>0 & HH_wages!=.
gen has_selfemp_inc 	= ((HH_nonagriprofit>0 & HH_nonagriprofit!=.) | (HH_agritotal_profit>0 & HH_agritotal_profit!=. ))

//Shares of labor income from primary occupation pc : check

gen sh_wages 	= HH_wages /  HH_total_empincome
gen sh_selfemp  = 1-sh_wages

// Household head 
gen wages_hhh 			= ind_wage_allow if relationship==1 

xtile HHH_wage_quintile=wages_hhh [aw=weight], nq(5)

********************************************************************************
*Income by sector 
********************************************************************************
gen agri_inc=ind_wage_allow + ind_agritotal_profit if broad_industry==1
replace agri_inc=ind_agritotal_profit if broad_industry!=1

gen construction_inc=ind_profit_construc+ind_wage_allow if broad_industry==3
replace construction_inc=ind_profit_construc if broad_industry!=3

gen manufac_inc=ind_profit_manufac+ind_wage_allow if broad_industry==2
replace manufac_inc=ind_profit_manufac if broad_industry!=2

gen services_inc=ind_profit_services+ind_wage_allow if broad_industry==4
replace services_inc=ind_profit_services if broad_industry!=4

gen earned_income=agri_inc+construction_inc+manufac_inc+services_inc

gen industry_inc=manufac_inc+construction_inc

foreach var of varlist agri_inc industry_inc services_inc {
	bysort hhid: egen hh_`var'=sum(`var')
}

egen hh_nonagri_inc=rowtotal(hh_industry_inc hh_services_inc)

egen HH_max_income=rowmax(hh_agri_inc hh_industry_inc hh_services_inc)

gen hh_maininc_agri=(HH_max_income==hh_agri_inc)
gen hh_maininc_ind=(HH_max_income==hh_industry_inc)
gen hh_maininc_serv=(HH_max_income==hh_services_inc)
gen hh_maininc_nonagri=(hh_maininc_ind==1 | hh_maininc_serv==1)

replace hh_maininc_agri=0 if HH_max_income==. | HH_max_income==0
replace hh_maininc_ind=0 if HH_max_income==. | HH_max_income==0
replace hh_maininc_serv=0 if HH_max_income==. | HH_max_income==0
replace hh_maininc_nonagri=0 if HH_max_income==. | HH_max_income==0

********************************************************************************
*SP Incomes   
********************************************************************************

bys hhid: egen hh_pensions			 = total(pension) 
bys hhid: egen hh_samurdhi_aswes	 = total(samurdhi) 
bys hhid: egen hh_elder		 		 = total(elder)  
bys hhid: egen hh_tb		 		 = total(tb) 
bys hhid: egen hh_disability 		 = total(disability) 

foreach var in hh_pensions hh_samurdhi_aswes hh_elder hh_tb hh_disability {
	gen `var'_pc = `var'/hhmem 
	gen have_`var' = (`var'>0 & `var'!=.)
}
 
********************************************************************************
*Indebtedness
********************************************************************************
gen debt_banks = (banks==1 | samurtdhi==1)
gen debt_financecomp = (finance==1)
gen debt_pawn_moneylender = (pawning==1 | lender==1)
gen debt_employer = (employer_debt==1)
gen debt_retail = (retail_shops==1 | instalment_goods==1)
gen debt_other = (other_debts==1 | credit_cards==1)

egen debt_any = rowmax(debt_banks debt_financecomp debt_pawn_moneylender debt_employer debt_retail debt_other)

********************************************************************************
*Food insecurity
********************************************************************************
gen worry_food = quesion_1==1 
gen nohealthy_food = quesion_2==1
gen limited_food = quesion_3==1
gen skipmeals_food = quesion_4==1
gen eatless_food = quesion_5==1
gen runout_food = quesion_6==1
gen hungry_food = quesion_7a==1
gen fulldaywo_food = quesion_8a==1

keep if relationship==1

//Keep only variables we need for BRIGHT imputations 

keep year month district sector psu snumber hhno nhh hhid* hhmem weight district province sector dist_1 dist_2 dist_3 dist_4 dist_5 dist_6 dist_7 dist_8 dist_9 dist_10 dist_11 dist_12 dist_13 dist_14 dist_15 dist_16 dist_17 dist_18 dist_19 dist_20 dist_21 dist_22 dist_23 dist_24 dist_25 prov_1 prov_2 prov_3 prov_4 prov_5 prov_6 prov_7 prov_8 prov_9 sector_1 sector_2 sector_3 have_atleast_secedu have_schoolage_noschl has_in_school sh_in_school female_hhh age_hhh married_hhh buddhist_hhh sinhala_hhh age_avg num_deps num_kids num_old num_labor dep_ratio share_dep share_kids sex_ratio hh_mem_014 sh_mem_014 hh_mem_1564 sh_mem_1564 hh_mem_0 sh_mem_0 hh_mem_1 sh_mem_1 hh_mem_2 sh_mem_2 hh_mem_3 sh_mem_3 hh_mem_4 sh_mem_4 hh_mem_male sh_mem_male hh_mem_fem sh_mem_fem edu_hhh_none edu_hhh_le~m edu_hhh_prim edu_hhh_OL edu_hhh_AL edu_hhh_moreAL edu_sh_5plus_none edu_sh_1564_none num_heart_disease have_heart_disease num_blood_pressure have_blood_pressure num_diabetes have_diabetes num_asthma have_asthma num_kidney_disease have_kidney_disease num_arthritis have_arthritis num_mental_illness have_mental_illness num_any_chronillness have_any_chronillness num_no_chronicillness have_no_chronicillness bed_rooms area structure single_storey line_room shanty lessthan_500sqft wall_type brickwalls floor floortiles roof semiperm_roof house_tenure house_owned type_toilet exclusive_toilet inside_toilet garbage_collected garbage_burned bedroom_pc water_source water_safe_boil water_safe_nothing tv computers washing_mechine fridge cookers electric_fans airconditioners bicycle motor_bicycle three_wheeler motor_car_van bus_lorry threshers waterpumps mechine chickens cattle_buffaloes goats_sheeps boats have_housing_land have_agri_land have_business_land hh_samurdhi_aswes hh_pensions hh_elder hh_tb hh_disability have_hh_pensions have_hh_samurdhi_aswes have_hh_elder have_hh_tb have_hh_disability  hh_samurdhi_aswes_pc hh_pensions_pc hh_tb_pc hh_elder_pc hh_disability_pc debt_banks debt_financecomp debt_pawn_moneylender debt_employer debt_retail debt_other debt_any worry_food nohealthy_food limited_food skipmeals_food eatless_food runout_food hungry_food fulldaywo_food HH_agritotal_profit HH_nonagriprofit employee_hhh num_employee_hh ownaccount_employer_hhh num_ownaccount_employer_hh familyworker_hhh num_familyworker_hh employer_hhh num_employer_hh self_employed_hhh num_self_employed_hh have_agri_emp have_ind_emp have_constr_emp have_serv_emp num_agri_emp num_indexcons_emp num_cons_emp num_ind_emp num_serv_emp hh_main_agri hh_main_ind hh_main_serv HH_wages HH_total_empincome has_wages has_selfemp_inc sh_wages sh_selfemp wages_hhh hh_agri_inc hh_industry_inc hh_services_inc hh_maininc_agri hh_maininc_ind hh_maininc_serv HH_wages_pc HH_wage_quintile HH_wage_pc_quintile HH_agritotal_profit_pc HH_agriinc_quintile HH_agriinc_pc_quintile HH_nonagriprofit_pc HH_nonagri_quintile HH_nonagri_pc_quintile HH_total_empincome_pc HH_totinc_quintile HH_totinc_pc_quintile HHH_wage_quintile welfare share_food share_nonfood

replace sh_wages=0 if sh_wages==.
replace sh_selfemp=0 if sh_selfemp==.
save "$hies2019/harmonized_HIES.dta", replace
/*
sum hhmem district province sector dist_1 dist_2 dist_3 dist_4 dist_5 dist_6 dist_7 dist_8 dist_9 dist_10 dist_11 dist_12 dist_13 dist_14 dist_15 dist_16 dist_17 dist_18 dist_19 dist_20 dist_21 dist_22 dist_23 dist_24 dist_25 prov_1 prov_2 prov_3 prov_4 prov_5 prov_6 prov_7 prov_8 prov_9 sector_1 sector_2 sector_3 have_atleast_secedu have_schoolage_noschl has_in_school sh_in_school female_hhh age_hhh married_hhh buddhist_hhh sinhala_hhh age_avg num_deps num_kids num_old num_labor dep_ratio share_dep share_kids sex_ratio hh_mem_014 sh_mem_014 hh_mem_1564 sh_mem_1564 hh_mem_0 sh_mem_0 hh_mem_1 sh_mem_1 hh_mem_2 sh_mem_2 hh_mem_3 sh_mem_3 hh_mem_4 sh_mem_4 hh_mem_male sh_mem_male hh_mem_fem sh_mem_fem edu_hhh_none edu_hhh_le~m edu_hhh_prim edu_hhh_OL edu_hhh_AL edu_hhh_moreAL edu_sh_5plus_none edu_sh_1564_none num_heart_disease have_heart_disease num_blood_pressure have_blood_pressure num_diabetes have_diabetes num_asthma have_asthma num_kidney_disease have_kidney_disease num_arthritis have_arthritis num_mental_illness have_mental_illness num_any_chronillness have_any_chronillness num_no_chronicillness have_no_chronicillness bed_rooms area structure single_storey line_room shanty lessthan_500sqft wall_type brickwalls floor floortiles roof semiperm_roof house_tenure house_owned type_toilet exclusive_toilet inside_toilet garbage_collected garbage_burned bedroom_pc water_source water_safe_boil water_safe_nothing tv computers washing_mechine fridge cookers electric_fans airconditioners bicycle motor_bicycle three_wheeler motor_car_van bus_lorry threshers waterpumps mechine chickens cattle_buffaloes goats_sheeps boats have_housing_land have_agri_land have_business_land hh_samurdhi_aswes hh_pensions hh_elder hh_tb hh_disability have_hh_pensions have_hh_samurdhi_aswes have_hh_elder have_hh_tb have_hh_disability  hh_samurdhi_aswes_pc hh_pensions_pc hh_tb_pc hh_elder_pc hh_disability_pc debt_banks debt_financecomp debt_pawn_moneylender debt_employer debt_retail debt_other debt_any worry_food nohealthy_food limited_food skipmeals_food eatless_food runout_food hungry_food fulldaywo_food HH_agritotal_profit HH_nonagriprofit employee_hhh num_employee_hh ownaccount_employer_hhh num_ownaccount_employer_hh familyworker_hhh num_familyworker_hh employer_hhh num_employer_hh self_employed_hhh num_self_employed_hh have_agri_emp have_ind_emp have_constr_emp have_serv_emp num_agri_emp num_indexcons_emp num_cons_emp num_ind_emp num_serv_emp hh_main_agri hh_main_ind hh_main_serv HH_wages HH_total_empincome has_wages has_selfemp_inc sh_wages sh_selfemp wages_hhh hh_agri_inc hh_industry_inc hh_services_inc hh_maininc_agri hh_maininc_ind hh_maininc_serv HH_wages_pc HH_wage_quintile HH_wage_pc_quintile HH_agritotal_profit_pc HH_agriinc_quintile HH_agriinc_pc_quintile HH_nonagriprofit_pc HH_nonagri_quintile HH_nonagri_pc_quintile HH_total_empincome_pc HH_totinc_quintile HH_totinc_pc_quintile HHH_wage_quintile welfare

 
/*
********************************************************************************
*Temporal and Spatial Deflation  
********************************************************************************
tab year 
tab month

merge m:1 year month using $data/NCPI_series, keepusing(cpi_base2013) 
keep if _merge==3 
drop _merge 

//merge m:1 district using "$data/HIES/RAW/spatial_priceindex.dta",  nogen
bys year: egen avg_cpi = mean(cpi_base2013)
tab avg_cpi 

********************************************************************************
* Income in real terms 
********************************************************************************
xtile decile = welfare [aw=finalweight] , nq(10)
tabstat welfare hh_inc_primary_nc_pc , by(decile)

gen rpcinc1  = (hh_inc_primary_nc_pc*avg_cpi)/cpi_base2013
gen rpcwage1 = (hh_wages_primary_pc*avg_cpi)/cpi_base2013
gen rpcself1 = (hh_selfemp_primary_pc*avg_cpi)/cpi_base2013

gen rpcinc2  = (hh_inc_sec_nc_pc*avg_cpi)/cpi_base2013
gen rpcwage2 = (hh_wages_sec_pc*avg_cpi)/cpi_base2013
gen rpcself2 = (hh_selfemp_sec_pc*avg_cpi)/cpi_base2013

//spatial 
foreach var in rpcinc1 rpcwage1 rpcself1 rpcinc2 rpcwage2 rpcself2 {
	replace `var' = `var'*lpindex1
}

foreach var in pensions capital remittances inocct_m {
	
	gen r`var'pc = ((hh_`var'_pc * avg_cpi)/cpi_base2013)*lpindex1
	su r`var'pc, d 
}

//Deflate Total income per capita: from SARMD
gen ripcfpc = ((ipcf * avg_cpi)/cpi_base2013)*lpindex1
su ripcfpc, d 

tabstat welfare hh_inc_primary_nc_pc rpcinc* cpi_base2013, by(month)

xtile quintile19 = welfare [aw=finalweight] , nq(5)

tabstat welfare hh_inc_primary_nc_pc rpcinc* , by(quintile19)
sum welfare hh_inc_primary_nc_pc rpcinc* 

tabstat welfare hh_inc_primary_nc_pc rpcinc1, by(decile)

tabstat welfare hh_inc_pc hh_inc_primary_nc_pc , by(decile)

*******************************************************************
//Replace negative incomes with zeros 
*******************************************************************
foreach var in rpcinc1 rpcwage1 rpcself1 ///
				hh_inc_primary_nc_pc hh_wages_primary_pc hh_selfemp_primary_pc ///
				hh_pensions hh_capital hh_remittances hh_inocct_m ///
				rpensionspc rcapitalpc rremittancespc rinocct_mpc ripcfpc {
				//winsorize all income variables 
				sum `var' if `var'>0, d 
				scalar p1_`var'=r(p1)
				scalar p99_`var' = r(p99)
				replace `var'=p1_`var' if `var'<p1_`var' & `var'>0 & `var'!=.
				replace `var'=p99_`var' if `var'>p99_`var' & `var'>0 & `var'!=.
				
				replace `var' = 0 if `var'<0
}

*******************************************************************
// Propensity to Consume 
*******************************************************************
gen theta = welfare/ ripcfpc 

tabstat theta , by(quintile19)
tabstat welfare ripcfpc , by(quintile19)

*******************************************************************
// Non - Labor Income Components as share of welfare (?) 
*******************************************************************
gen sh_pension = 	rpensionspc/welfare
gen sh_capital =  	rcapitalpc/welfare 
gen sh_remittances =  	rremittancespc/welfare 
gen sh_inocct_m =  	rinocct_mpc/welfare 

egen rnlincpc19 = rowtotal (rpensionspc rcapitalpc rremittancespc rinocct_mpc) , missing 
sum rnlincpc19 if rnlincpc19>0 ,d 
scalar p1 =r(p1)
scalar p99 = r(p99)
replace rnlincpc19=p1 if rnlincpc19<p1 & rnlincpc19>0 & rnlincpc19!=.
replace rnlincpc19=p99 if rnlincpc19>p99 & rnlincpc19>0  & rnlincpc19!=.
replace rnlincpc19 =0 if rnlincpc19<0 & rnlincpc19!=.

gen sh_nl19 = rnlincpc19 / welfare 
tabstat sh_nl19 , by(quintile19)
tabstat sh_pension , by(quintile19) 
tabstat sh_remittances , by(quintile19) 
tabstat sh_inocct_m , by(quintile19) 

*******************************************************************************
// As a share of labor income for methodological ease in s2s 
*******************************************************************************
gen sh_ynyl19 		= rnlincpc19 /rpcinc1
gen sh_pensionyl19 	= rpensionspc /rpcinc1
gen sh_remittancesyl19 = rremittancespc /rpcinc1
gen sh_inocct_myl19 = rinocct_mpc /rpcinc1
gen sh_icapyl19 = rcapitalpc /rpcinc1

xtile quintileyl19 = rpcinc1 [aw=finalweigh] , nq(5)

tabstat sh_ynyl19 if rpcinc1>0 , by(quintileyl19)
tabstat sh_pensionyl19 if rpcinc1>0, by(quintileyl19) 
tabstat sh_remittancesyl19 if rpcinc1>0, by(quintileyl19) 
tabstat sh_inocct_myl19  if rpcinc1>0, by(quintileyl19) 
tabstat sh_icapyl19 if rpcinc1>0, by(quintileyl19)

*******************************************************************************
//2023 : already in 2019 prices, only need to spatially deflate 
*******************************************************************************

merge 1:1 district sector month psu snumber hhno nhh result person_serial_no  using $data/inc_microsim23.dta, nogen

bys hhid: egen hh_pensions23	 	= total(ijubi23) , missing
bys hhid: egen hh_capital23	 		= total(icap23) , missing
bys hhid: egen hh_remittances23 	= total(itranext_m23) , missing
bys hhid: egen hh_inocct_m23		= total(inocct_m23) , missing
bys hhid: egen hh_laborinc23        = total(labor_income23_real), missing 

foreach var in pensions capital remittances inocct_m laborinc {
	gen 	r`var'pc23 = hh_`var'23	/	hhmem 
	replace r`var'pc23 = r`var'pc23 *	lpindex1
}

gen sh_pension23 	= 	rpensionspc23/welfare23
gen sh_capital23 	=  	rcapitalpc23/welfare23
gen sh_remittances23=  	rremittancespc23/welfare23 
gen sh_inocct_m23   =  	rinocct_mpc23/welfare23 

egen rnlincpc23 = rowtotal (rpensionspc23 rcapitalpc23 rremittancespc23 rinocct_mpc23) , missing 
sum rnlincpc23 if rnlincpc23>0,d 
scalar p1 =r(p1)
scalar p99 = r(p99)
replace rnlincpc23=p1 if rnlincpc23<p1 & rnlincpc23>0 & rnlincpc23!=.
replace rnlincpc23=p99 if rnlincpc23>p99 & rnlincpc23>0 & rnlincpc23!=.
replace rnlincpc23 =0 if rnlincpc23<0 & rnlincpc23!=.

su *23 , d 
gen sh_nl23 = rnlincpc23 / welfare 

xtile quintile23 = welfare23 [aw=finalweight] , nq(5)
tabstat sh_nl23 , by(quintile23)
tabstat sh_pension23 , by(quintile23) 
tabstat sh_remittances23 , by(quintile23) 
tabstat sh_inocct_m23 , by(quintile23) 

tabstat sh_pension if rpensionspc	>0 , by(quintile19) 
tabstat sh_pension23 if rcapitalpc23 >0 , by(quintile23) 

tabstat sh_capital if rcapitalpc    >0 , by(quintile19) 
tabstat sh_capital23 if rcapitalpc23 >0 , by(quintile23) 

tabstat sh_remittances if rremittancespc>0 , by(quintile19) 
tabstat sh_remittances23 if rremittancespc23 >0 , by(quintile23) 

tabstat sh_inocct_m if inocct_m>0 , by(quintile19) 
tabstat sh_inocct_m23 if inocct_m23 >0 , by(quintile23) 


*******************************************************************************
// As a share of labor income for methodological ease in s2s 
*******************************************************************************

gen sh_pensionyl23 	= 	rpensionspc23/rlaborincpc23
gen sh_icapyl23 	=  	rcapitalpc23/rlaborincpc23
gen sh_remittancesyl23=  	rremittancespc23/rlaborincpc23 
gen sh_inocct_myl23   =  	rinocct_mpc23/rlaborincpc23 

gen sh_ynyl23 = rnlincpc23 / rlaborincpc23 

xtile quintileyl23 = rlaborincpc23 [aw=finalweigh] , nq(5)

tabstat sh_ynyl23 		if labor_income23_real>0 , by(quintileyl23)
tabstat sh_pensionyl23 	if labor_income23_real>0, by(quintileyl23) 
tabstat sh_remittancesyl23 if labor_income23_real>0, by(quintileyl23) 
tabstat sh_inocct_myl23  if labor_income23_real>0, by(quintileyl23) 
tabstat sh_icapyl23		 if labor_income23_real>0, by(quintileyl23)

*******************************************************************************
//MPC 
*******************************************************************************

gen sh_yl19 = rpcinc1 / welfare 
tabstat sh_yl19  if rpcinc1>0 , by(quintile19)

*******************************************************************
*Income by sector
*******************************************************************


bysort hhid: egen have_skilled_worker=max(skill_4)
bysort hhid: egen have_semiskilled_worker=max(skill_3)

//bysort hhid: egen have_member_disab=max(have_disabilities)
mdesc empstat 
bysort hhid: egen num_public_emp  		=total (empstat_1) , missing  
bysort hhid: egen num_pvt_emp     		=total (empstat_2)  , missing 
bysort hhid: egen num_family_worker 	=total (empstat_3) , missing  
bysort hhid: egen num_employer			=total (empstat_4) , missing 
bysort hhid: egen num_self_emp			=total (empstat_5) , missing 

bysort hhid: egen have_public_emp  		=max(empstat_1)
bysort hhid: egen have_pvt_emp     		=max(empstat_2)
bysort hhid: egen have_family_worker 	=max(empstat_3)
bysort hhid: egen have_employer			=max(empstat_4)
bysort hhid: egen have_self_emp			=max(empstat_5) 

bysort hhid: egen num_ecactive=total (lstat_active) , missing 

gen hh_lfpr=num_ecactive/(hhmem-num_kids)

gen sh_employee =  (num_public_emp+num_pvt_emp) / num_ecactive
gen sh_selfempl =  num_self_emp / num_ecactive
gen sh_ecactive = num_ecactive / hhmem 

*******************************************************************************
// Summary stats relevant variables 
*******************************************************************************

tab district 
tab ethnicity
tab religion
tab sex

tab educat5
tab educat7

tab eye_dsablty
tab hear_dsablty
tab conc_dsord
tab slfcre_dsablty
tab comm_dsablty

tab empstat 
tab lstat_active
tab broad_industry
tab skill_level
ren finalweight weight

mean dist_* sector_* sin buddhist male age married hhmem  noedu atleast_sec schoolage_noschool no_* cellphone computer [aw=weight]

mean lstat_active empstat_* broad_ind_* skill_*  [aw=weight]

mean inc_paidemp_mon [aw=weight] if inc_paidemp_mon!=0
mean inc_selfemp_mon [aw=weight] if inc_selfemp_mon!=0
mean labor_income_total [aw=weight] if labor_income_total!=0
mean inc_emp_excl_bonus [aw=weight] if inc_emp_excl_bonus!=0

*******************************************************************************
*******************************************************************************

*keep hhid relationship person weight dist* sector urban sector_* hhmem district ethnicity sin age sex male religion buddhist educat5 educat7 noedu atleast_sec schoolage_noschool marstat married empstat* lstat_active inc* rpccons hhexppm cellphone computer eye_dsablty hear_dsablty conc_dsord slfcre_dsablty comm_dsablty broad_industry skill_level broad_ind_* skill_* no_* major_*  inc_emp_excl_bonus

//ren person pid

gen data="HIES2019"

//only keep head of household 
keep if relationship==1

//recode have_public_emp have_pvt_emp have_family_worker have_employer have_self_emp have_skilled_worker have_semiskilled_worker (.=0)

*Household level variables
mean dist_* sector_* female_hh sin buddhist age married hhmem cellphone computer have_atleast_secedu have_schoolage_noschl share_dep share_kids num_kids have_agri_emp have_ind_emp have_constr_emp have_serv_emp hh_main_agri hh_main_ind hh_main_serv hh_maininc_agri hh_maininc_ind hh_maininc_serv  have_skilled_worker have_semiskilled_worker have_public_emp  have_pvt_emp have_family_worker have_employer have_self_emp hh_lfpr have_emp_inc hh_inc_pc [aw=weight]

mean hh_inc_pc if hh_inc_pc!=0 [aw=weight]
mean hh_inc_paidemp if hh_inc_paidemp!=0 [aw=weight]
mean hh_inc_selfemp if hh_inc_selfemp!=0 [aw=weight]

sum hh_inc_pc if hh_inc_pc!=0 [aw=weight]
sum hh_inc_paidemp if hh_inc_paidemp!=0 [aw=weight]
sum hh_inc_selfemp if hh_inc_selfemp!=0 [aw=weight]

//Per-capita values 
sum hh_inc_pc if hh_inc_pc!=0 [aw=weight]
sum hh_paidemp_pc if hh_paidemp_pc!=0 [aw=weight]
sum hh_selfemp_pc if hh_selfemp_pc!=0 [aw=weight]

gen ln_welfare=ln(welfare)

reg ln_welfare dist_* sector_* female_hh sin buddhist age married hhmem cellphone computer have_atleast_secedu have_schoolage_noschl share_dep have_agri_emp have_ind_emp have_constr_emp have_serv_emp have_skilled_worker have_semiskilled_worker have_public_emp  have_pvt_emp have_family_worker have_employer have_self_emp have_emp_inc hh_inc_pc

gen popwt=hhmem*weight
svyset [pw=popwt] , psu(psu)
svy: total popwt 

foreach var in married sinhala buddhist {
	rename `var' `var'_hhh
}

//Percentiles of the income distribution by type of income 
pctile ptile_selfemp 		= hh_selfemp_primary_pc [pw=weight] , nq(100)
pctile ptile_wages 			= hh_wages_primary_pc [pw=weight] , nq(100)
pctile ptile_hh_inc_prim_nc = hh_inc_primary_nc_pc [pw=weight], nq(100)

keep hhid psu weight popwt province district dist_* sector* urban rural age_avg hhmem share_dep num_dep num_kids num_old share_kids dep_ratio cellphone computer have_atleast_secedu have_schoolage_noschl have_agri_emp have_ind_emp have_constr_emp have_serv_emp hh_main_agri hh_main_ind hh_main_serv hh_maininc_agri hh_maininc_ind hh_maininc_serv  have_skilled_worker have_semiskilled_worker have_public_emp  have_pvt_emp have_family_worker have_employer have_self_emp have_emp_inc ln_welfare welfare data hh_lfpr sex_ratio *mem* edu* ///
 hh_wages have_* wages_hhh *_pc ptile* rpcinc1 rpcwage1 rpcself1 rpcinc2 rpcwage2 rpcself2 /// 
 labor_income* collects_* sh_in_school have_schoolage_noschl has_in_school has_*_disab *hhh ///
 sh_selfempl sh_employee sh_ecactive sh_pensionyl* sh_icapyl* sh_inocct_myl* sh_remittancesyl* sh_ynyl* welfare23 rlaborincpc23 rnlincpc19 rnlincpc23
 
save "$data/hies2019_clean" , replace
sum sh_ynyl19 , d
sum rnlincpc19 , d 
mdesc * 
sum *
