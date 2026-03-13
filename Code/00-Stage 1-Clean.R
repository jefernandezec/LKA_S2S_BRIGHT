# Data cleaning and construction

####Prepare receiver survey#####
data.rec=read_dta(paste(datapath,"BRIGHT_Comp_202425.dta",sep=""))

#create sequential IDs
data.rec$hidseq=seq(1:nrow(data.rec))

#Additional clean and construction
#create population weights
#data.rec$pop_wgt=with(data.rec,hh_size*hhwt)
#hh age squared
data.rec$hh_head_age_sq = with(data.rec,age_hhh^2)

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

#####Prepare donor survey#####
data.don=read_dta(paste(datapath,"HIES_Comp_2019.dta",sep=""))

#create sequential Ids
data.don$hidseq=seq(1:nrow(data.don))

#Additional clean and construction
data.don <- data.don %>% 
    filter(!is.na(welfare))
#hh age squared
data.don$hh_head_age_sq = with(data.don,age_hhh^2)
# Missing values report
missing_report.don <- data.don %>%
    summarise(across(everything(), ~ mean(is.na(.)) * 100)) %>%
    pivot_longer(cols = everything(), 
                 names_to = "Variable", values_to = "PercentMissing")
subset(missing_report.don,PercentMissing>0)

data.don <- data.don %>% 
  select(-starts_with("dist_"))  %>% 
  select(-starts_with("prov_"))

data.don=na.omit(data.don)
data.don[vars.to.factor] <- lapply(data.don[vars.to.factor], factor)
data.don$b_year=2019-data.don$age_hhh
data.don$ratio=with(data.don,welfare/rpcexpcomp)

