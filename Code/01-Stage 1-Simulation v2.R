### This code requires the harmonized HIES 2019 and BRIGHT 2024 data sets. Implements
### S2S using Lasso-based PMM constrained by district and income quintile
### and uses Mahalanobis distance to find the nearest neighbor.


#number of simulations
sim = nsim1

X.mtc1=c("ymatch","hhmem","b_year")

X.mtc2=c("ymatch","sh_rpcexpnfood","hhmem","b_year")

data.don$ratio.f=with(data.don,welfare/rpcexpfood)

#Define variables to be used in the models
vars1=setdiff(names(data.don),names(data.rec))
#vars1=setdiff(vars1,c("welfare","ratio_tot"))
vars2=setdiff(names(data.rec),names(data.don))

#common_vars <- intersect(names(data.don), names(data.rec))

var2excl=c(vars1,vars2,"district_name","district","month", "psu","snumber",
           "hhno", "nhh","hhid_tiloka","structure","area","weight","age_hhh",
           "HH_nonagriprofit","HH_agritotal_profit","sector_1","sector_2",                  
           "sector_3","year","hhid","welfare","hh_mem_0" , "sh_mem_0",
           "hh_mem_1","sh_mem_1","hh_mem_2","sh_mem_2", "hh_mem_3",
           "sh_mem_3", "hh_mem_4", "sh_mem_4","hh_mem_male", "hh_mem_fem",
           "have_heart_disease","have_blood_pressure" ,"have_diabetes",
           "have_asthma","have_kidney_disease","have_arthritis" ,
           "have_mental_illness","have_any_chronillness","have_no_chronicillness",
           "house_tenure", "HH_wages","HH_wages","HH_wages_pc","HH_wage_quintile",
           "HH_agritotal_profit_pc","HH_agriinc_quintile",            
           "HH_nonagriprofit_pc","HH_nonagri_quintile","HH_total_empincome",
           "HH_total_empincome_pc","HH_totinc_quintile",
           "wages_hhh","HHH_wage_quintile","hh_agri_inc","hh_industry_inc" ,          
           "hh_services_inc","hh_pensions","hh_samurdhi_aswes","hh_elder" ,
           "hh_tb","hh_disability","hh_pensions_pc",
           "hh_samurdhi_aswes_pc","hh_elder_pc","hh_tb_pc","hh_disability_pc",
           "HH_monthly_exp_comp","HH_monthly_exp_comp1","HH_monthly_exp_comp2",      
           "HH_monthly_exp_comp_pc","HH_monthly_exp_comp1_pc",
           "HH_monthly_exp_comp2_pc","popwt","lpindex1",
           "avg_cpi","rpcexptot","quintiles","cpi_base2013",
           "cpi_base2013_food","sh_rpcexpfood","sh_rpcexpnfood",
           "cpi_base2013_nonfood","avg2019","avg2019food","avg2019nonfood",
           "rpcexpfood","rpcexpnfood")

covariates=setdiff(names(data.don),var2excl)

# Create the formula 
formula.mod.a <- as.formula(paste("welfare ~", 
                                  paste(covariates, collapse = " + ")))
formula.mod.b <- as.formula(paste("rpcexptot ~", 
                                  paste(covariates, collapse = " + ")))

#empty objects to save results
#matching
simcons_match=subset(data.rec,sel=c(hhid)) # here we save welfare
#prediction
simcons_pred=subset(data.rec,sel=c(hhid))# here we save welfare

#R squared
r2=c()
md=c()

#Design matrices for applying LASSO model
X.samp.b =build.x(formula = formula.mod.b, 
                  data.rec,
                  contrasts = FALSE)
X.samp.a =build.x(formula = formula.mod.a, 
                  data.don,
                  contrasts = FALSE)
#Simulation loop
set.seed(seed)
foreach (j=1:sim) %do% { #replace to dopar later
    print(paste("simulation number ",j,
                sep=""))
    
    #Sample (preserves state participation in sample)
    train.a <- data.don %>%
        group_by(province) %>%
        sample_frac(n.a)
    
    #Make sure all donation classes have sufficient data
    if (min(table(train.a$district,train.a$HH_totinc_pc_quintile))>0){
      group.v <- c("district","HH_totinc_pc_quintile")  # donation classes
    }  else {
    group.v <- c("province")  # donation classes
    }
    train.a.orig=train.a
    #subset to model covariates
    train.a = train.a[,unique(c("welfare",covariates))] 
    #remove NAs for training
    train.a=na.omit(train.a)
    #Design matrix for training model
    mod.a=lm(welfare~.,data=train.a)
    X.a = model.matrix(mod.a)
    common_vars.X <- intersect(colnames(X.a), colnames(X.samp.b))
    X.a <- X.a[, common_vars.X, drop = FALSE]
    #X.samp.b <- X.samp.b[, common_vars.X, drop = FALSE]
    
    sd_cols <- apply(X.a, 2, function(z) sd(as.numeric(z), na.rm = TRUE))
    X.a     <- X.a[, is.finite(sd_cols) & sd_cols > 0, drop = FALSE]
    y.a = as.matrix(train.a$welfare)
    y.a= log(y.a+1)
    
    #Estimation
    #AdapLASSO
    cv.ridge1 = cv.glmnet(X.a, y=y.a,alpha=0)
    ridge_coefs1 <- coef(cv.ridge1, s = "lambda.min")
    # Compute adaptive weights (inverse of absolute Ridge coefficients)
    # We exclude the intercept (ridge_coefs[1])
    # Adding small value to avoid division by zero
    adapt_wgts1 <- 1 / (abs(ridge_coefs1[-1]) + 1e-6)
    #Fit Lasso through CV using adaptive weights
    cv.lasso1 = cv.glmnet(X.a, y=y.a,
                          penalty.factor = adapt_wgts1)
    best.lambda1 = cv.lasso1$lambda.min
    lasso_best1 <- glmnet(X.a, y=y.a, alpha = 1,
                          lambda = best.lambda1,
                          penalty.factor = adapt_wgts1)
    
    #  #Predict log consumption using Adaplasso
    Ya.al<-predict(lasso_best1, newx=X.a, s=best.lambda1)
    r2_al <- compute_r_squared(y.a, Ya.al)
    
    #LASSO
    cv.lasso2 = cv.glmnet(X.a, y=y.a, alpha=1)
    best.lambda2 = cv.lasso2$lambda.min
    lasso_best2 <- glmnet(X.a, y=y.a, alpha = 1, 
                          lambda = best.lambda2)
    #Predict log consumption using lasso
    Ya.l<-predict(lasso_best2, newx=X.a, s=best.lambda2)
    r2_l <- compute_r_squared(y.a, Ya.l)
    #Keep only regular Lasso for computational ease
    r2[j]=max(r2_al,r2_l)
    md[j]=ifelse(r2_l<=r2_al,"AdapLasso","Lasso")
    
    #Subset common variables
    X.newb =X.samp.b[,colnames(X.a)]
    X.a =X.a[,colnames(X.newb)]  #Donor data is train.a
    
    if (r2_l<=r2_al) {
        Yb<-predict(lasso_best1, newx=X.newb, s=best.lambda1)
        coef_temp = data.frame(as.matrix(coefficients(lasso_best1)))
        Ya=predict(lasso_best1, newx=X.a, s=best.lambda1)
    } else {
        Yb<-predict(lasso_best2, newx=X.newb, s=best.lambda2)
        coef_temp = data.frame(as.matrix(coefficients(lasso_best2)))
        Ya=predict(lasso_best2, newx=X.a, s=best.lambda2)
    }
    
    #Best model coefficients  
    names(coef_temp)=paste("coef_",j,sep="") 
    if (j==1){ coefs = coef_temp } else {coefs=cbind(coefs,coef_temp)}
    
    #save predictions from best model on PLFS
    Pred_Yb=data.table(cbind(data.rec[,"hhid"], exp(Yb)-1))
    names(Pred_Yb)=c("hhid",paste("welfare_",j,sep=""))
    simcons_pred=merge(simcons_pred,Pred_Yb,by="hhid")
    
    #Calculate consumption predictions on both surveys
    X.samp.b.pred=data.table(cbind(X.samp.b[,"hidseq"], Yb))
    X.samp.a.pred=data.table(cbind(X.a[,"hidseq"], Ya))
    rm(Yb,Ya,Ya.l,Ya.al)
    colnames(X.samp.b.pred)=c("hidseq","ymatch")
    colnames(X.samp.a.pred)=c("hidseq","ymatch")
    
    #Merge predictions with original base
    samp.btemp=merge.data.table(data.rec,X.samp.b.pred,
                                by="hidseq",all=TRUE,sort=TRUE)
    samp.atemp=merge.data.table(train.a.orig,X.samp.a.pred,
                                by="hidseq",all=TRUE,sort=TRUE)
    rm(train.a,train.a.orig,y.a,X.samp.b.pred,X.samp.a.pred,X.a,X.newb,
       mod.a,cv.lasso1,best.lambda1,
       lasso_best1,adapt_wgts1,
       ridge_coefs1,cv.ridge1, coef_temp,Pred_Yb,
       r2_l,r2_al,cv.lasso2,lasso_best2,best.lambda2)
    samp.btemp=data.frame(samp.btemp)
    samp.atemp=data.frame(samp.atemp)
    row.names(samp.btemp)=as.character(seq(1:nrow(samp.btemp)))
    row.names(samp.atemp)=as.character(seq(1:nrow(samp.atemp)))
    #standardize variables before calculating distance
    # samp.atemp[X.mtc1] <- lapply(samp.atemp[X.mtc1], 
    #                              function(x) as.numeric(scale(x)))
    # samp.btemp[X.mtc1] <- lapply(samp.btemp[X.mtc1], 
    #                              function(x) as.numeric(scale(x)))
    
    #########
    ##FOOD###
    #########
    
    #Matching using lasso predictions and random nearest neighbor distance hot deck (D'Orazio, 2017)
    rnd.f <- RANDwNND.hotdeck(data.rec=samp.btemp, data.don=samp.atemp,
                              match.vars=X.mtc1, don.class=group.v,
                              dist.fun="Mahalanobis",
                              cut.don="min")
    
    #Create fused dataset
    fA.wrnd.f <- create.fused(data.rec=samp.btemp, data.don=samp.atemp,
                              mtc.ids=rnd.f$mtc.ids,
                              z.vars=don.vars1)  
    fA.wrnd.f$welfare = with(fA.wrnd.f,ratio.f*rpcexpfood)
    fA.wrnd.f = fA.wrnd.f[,c("hhid","welfare")]
    
    fA.wrnd = fA.wrnd.f
    names(fA.wrnd)=c("hhid",paste("welfare_",j,sep=""))
    
    simcons_match=merge(simcons_match,fA.wrnd,by="hhid")
    rm(samp.atemp,samp.btemp,fA.wrnd,fA.wrnd.f,rnd.f)
}



#save simulations results
#R-squared
write.csv(r2,file=paste(path,
                        "Outputs/Intermediate/Simulations_R2_",sim,".csv",sep=""),
          row.names = FALSE)
#Model used
write.csv(md,file=paste(path,
                        "Outputs/Intermediate/Simulations_model_used_",sim,".csv",sep=""),
          row.names = FALSE)


#Ensembles match consumption
simcons_match$welfare_mean=apply(simcons_match[,-1],
                                 1,mean,na.rm=TRUE)
simcons_match$welfare_median=apply(simcons_match[,-1],
                                   1,median,na.rm=TRUE)
#simcons_match$welfare_geom=apply(simcons_match[,-1],
#                                 1,geometric_mean,na.rm=TRUE)
write.csv(simcons_match,file=paste(datapath,
                                   "Simulations_match_",sim,".csv",sep=""),
          row.names = FALSE)
saveRDS(simcons_match,file=paste(datapath,
                                 "Simulations_match_",sim,".rds",sep=""))


# #Ensembles pred
simcons_pred$welfare_mean=apply(simcons_pred[,-1],
                                1,mean,na.rm=TRUE)
simcons_pred$welfare_median=apply(simcons_pred[,-1],
                                  1,median,na.rm=TRUE)
#simcons_pred$welfare_geom=apply(simcons_pred[,-1],
#                                1,geometric_mean,na.rm=TRUE)

write.csv(simcons_pred,file=paste(datapath,
                                  "Simulations_pred_",sim,".csv",sep=""),
          row.names = FALSE)
saveRDS(simcons_pred,file=paste(datapath,
                                "Simulations_pred_",sim,".rds",sep=""))

#Ensemble coefficients
coefs$coef=apply(coefs, 1,mean,na.rm=TRUE)

write.csv(coefs,file=paste(path,
                           "Outputs/Intermediate/Simulations_coefficients_",sim,".csv",
                           sep=""),
          row.names = TRUE)


