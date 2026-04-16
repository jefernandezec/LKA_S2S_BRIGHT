ifpripath <-"C:/Users/wb553773/WBG/Marta Schoch - Analysis/Data/IFPRI/World Bank BRIGHT/"

bright_imp=read_dta(paste(ifpripath,
                      "Output/Imputed_Bright_match.dta",sep=""))
bright_cons_orig=read_dta(paste(ifpripath,
                    "Data/consumption_aggregate.dta",sep=""))
bright_m=merge(bright_imp,bright_cons_orig,by.x="hhid",by.y="hhcode")

bright_m$welfare=bright_m$welfare_median

bright_m$decile_imp=xtile(bright_m$welfare,n=10,wt=bright_m$popweight)
bright_m$decile_orig=xtile(bright_m$total_exp,n=10,wt=bright_m$popweight)

des <- svydesign(ids = ~1, weights = ~popweight, data = bright_m)

#Cross-tabulate weighted counts
tab <- svytable(~decile_imp + decile_orig, design = des)
heatmap_data <- as.data.frame(tab)

heatmap_data <- heatmap_data %>%
    mutate(rel_freq = Freq / sum(Freq))

#Plot heatmap
ggplot(heatmap_data, aes(x = decile_imp, y = decile_orig, fill = rel_freq)) +
    geom_tile(color = "white") +
    #geom_text(aes(label = percent(rel_freq, accuracy = .1)), color = "white", size = 3) +
    scale_fill_viridis_c(
        option = "mako",
        labels = percent_format(accuracy = 1)
    ) +
    labs(
        title = "Cross-Decile Heatmap",
        x = "Deciles of imputed consumption",
        y = "Deciles of BRIGHT reported consumption",
        fill = "Proportion of population"
    ) +
    theme_minimal()
ggsave(paste(path,
             "/Outputs/Main/Figures/Heatmap deciles.png",sep=""),
       width = 30, height = 20, units = "cm")
