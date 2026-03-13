final_pred_df = subset(simcons_pred,select=c(hhid,welfare_mean,
            welfare_median,welfare_geom))
#Keep prediction-based imputation
data.rec2=merge(data.rec,final_pred_df,by="hhid",all.x = TRUE)
write_dta(data.rec2,paste(datapath,
     "Imputed_Bright_pred.dta",sep=""))

final_match_df = subset(simcons_match,select=c(hhid,welfare_mean,
                        welfare_median,welfare_geom))

#Keep matching-based imputation using distributional distance
data.rec2=merge(data.rec,final_match_df,by="hhid",all.x = TRUE)
write_dta(data.rec2,paste(datapath,
      "Imputed_Bright_match.dta",sep=""))

