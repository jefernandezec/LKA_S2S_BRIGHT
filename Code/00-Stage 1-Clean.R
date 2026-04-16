# Data cleaning and construction

####Prepare receiver survey#####
data.rec=read_dta(paste(datapath,"BRIGHT_Comp_202425.dta",sep=""))

#create sequential IDs
data.rec$hidseq=seq(1:nrow(data.rec))

#Additional clean and construction
# Missing values report
missing_report.rec <- data.rec %>%
  summarise(across(everything(), ~ mean(is.na(.)) * 100)) %>%
  pivot_longer(cols = everything(), 
               names_to = "Variable", values_to = "PercentMissing")
subset(missing_report.rec,PercentMissing>0)
#based on report, these variables are excluded only when using linear models
data.rec=subset(data.rec,sel=-c(dep_ratio,sex_ratio))
data.rec <- data.rec %>% 
  select(-starts_with("dist_"))  %>% 
  select(-starts_with("prov_"))

data.rec=na.omit(data.rec)
vars.to.factor <- c("sector", "province", "floor", "roof",
                    "HH_totinc_pc_quintile", "wall_type", "type_toilet",
                    "water_source", "HH_agriinc_pc_quintile",
                    "HH_nonagri_pc_quintile", "HH_wage_pc_quintile")

data.rec[vars.to.factor] <- lapply(data.rec[vars.to.factor], factor)
data.rec$b_year=2024-data.rec$age_hhh
#hh birth year squared
data.rec$b_year_sq = with(data.rec,b_year^2)
#shares of foond and non-food of comparable expenditure
data.rec = data.rec %>%
  mutate(sh_rpcexpfood = rpcexpfood/rpcexptot,
         sh_rpcexpnfood =rpcexpnfood/ rpcexptot)

##############################
#####Prepare donor survey#####
data.don=read_dta(paste(datapath,"HIES_Comp_2019.dta",sep=""))

#create sequential Ids
data.don$hidseq=seq(1:nrow(data.don))

#Additional clean and construction
data.don <- data.don %>% 
  filter(!is.na(welfare))
# Missing values report
missing_report.don <- data.don %>%
  summarise(across(everything(), ~ mean(is.na(.)) * 100)) %>%
  pivot_longer(cols = everything(), 
               names_to = "Variable", values_to = "PercentMissing")
subset(missing_report.don,PercentMissing>1)

data.don <- data.don %>% 
  select(-starts_with("dist_"))  %>% 
  select(-starts_with("prov_"))  %>%
  select(-c(dep_ratio,sex_ratio))

data.don=na.omit(data.don)
data.don[vars.to.factor] <- lapply(data.don[vars.to.factor], factor)
data.don$b_year=2019-data.don$age_hhh
#hh birth year squared
data.don$b_year_sq = with(data.don,b_year^2)
#shares of foond and non-food of comparable expenditure
data.don = data.don %>%
  mutate(sh_rpcexpfood = rpcexpfood/rpcexptot,
         sh_rpcexpnfood =rpcexpnfood/ rpcexptot)

data.don$ratio=with(data.don,welfare/rpcexptot)

