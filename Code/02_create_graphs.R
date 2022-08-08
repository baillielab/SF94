###################################################################### 

## Code author: Maaike Swets, Steven Kerr

## Description: 
# Creates graphs for the sf94 paper

###################################################################### 

library(tidyverse)
library(DescTools)
library(egg)
library(gtsummary)
library(broom)

#source("~/Git/SF94/Code/01_analysis.R")

set.seed(1234)

## Code for split violin plots
GeomSplitViolin <- ggproto("GeomSplitViolin", GeomViolin, 
                           draw_group = function(self, data, ..., draw_quantiles = NULL) {
                             data <- transform(data, xminv = x - violinwidth * (x - xmin), xmaxv = x + violinwidth * (xmax - x))
                             grp <- data[1, "group"]
                             newdata <- plyr::arrange(transform(data, x = if (grp %% 2 == 1) xminv else xmaxv), if (grp %% 2 == 1) y else -y)
                             newdata <- rbind(newdata[1, ], newdata, newdata[nrow(newdata), ], newdata[1, ])
                             newdata[c(1, nrow(newdata) - 1, nrow(newdata)), "x"] <- round(newdata[1, "x"])
                             
                             if (length(draw_quantiles) > 0 & !scales::zero_range(range(data$y))) {
                               stopifnot(all(draw_quantiles >= 0), all(draw_quantiles <=
                                                                         1))
                               quantiles <- ggplot2:::create_quantile_segment_frame(data, draw_quantiles)
                               aesthetics <- data[rep(1, nrow(quantiles)), setdiff(names(data), c("x", "y")), drop = FALSE]
                               aesthetics$alpha <- rep(1, nrow(quantiles))
                               both <- cbind(quantiles, aesthetics)
                               quantile_grob <- GeomPath$draw_panel(both, ...)
                               ggplot2:::ggname("geom_split_violin", grid::grobTree(GeomPolygon$draw_panel(newdata, ...), quantile_grob))
                             }
                             else {
                               ggplot2:::ggname("geom_split_violin", GeomPolygon$draw_panel(newdata, ...))
                             }
                           })

geom_split_violin <- function(mapping = NULL, data = NULL, stat = "ydensity", position = "identity", ..., 
                              draw_quantiles = NULL, trim = TRUE, scale = "area", na.rm = FALSE, 
                              show.legend = NA, inherit.aes = TRUE) {
  layer(data = data, mapping = mapping, stat = stat, geom = GeomSplitViolin, 
        position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
        params = list(trim = trim, scale = scale, draw_quantiles = draw_quantiles, na.rm = na.rm, ...))
}


###################################### Sample size graph  #################################################

#### Mortality
mean_death = mean(as.numeric(subset1$mortality_28))
sd_death = sd(as.numeric(subset1$mortality_28))

death_ucl <- mean_death + 1.96 *sd_death / sqrt( sum( !is.na(subset1$mortality_28) ))
death_lcl <- mean_death - 1.96 *sd_death / sqrt( sum( !is.na(subset1$mortality_28) ))

# Vectorise sample size calculation
calculate_sample_size_mort_vec = Vectorize(calculate_sample_size_mort)

sample_size_mort = calculate_sample_size_mort_vec(mean_death, c(0.85, 0.8, 0.75, 0.7)) %>%
  t() %>%
  data.frame() %>%
  mutate(outcome_measure = "28-day mortality",
         treatment_effect = c(0.85, 0.8, 0.75, 0.7)) %>%
  dplyr::select(outcome_measure, treatment_effect, continuity_corrected_sample_size) %>%
  dplyr::rename(sample_size = continuity_corrected_sample_size)
  
sample_size_mort$sample_size_lcl = calculate_sample_size_mort_vec(death_ucl, c(0.85, 0.8, 0.75, 0.7))[5,] 
sample_size_mort$sample_size_ucl = calculate_sample_size_mort_vec(death_lcl, c(0.85, 0.8, 0.75, 0.7))[5,] 





#### Opportunistic sf94
sd_sf94 = df_sf94_sd["subset1", "day5_P"]
corr_sf94 = df_sf94_corr["subset1", "day5_P"]

calculate_sample_size_sf94_vec = Vectorize(calculate_sample_size_sf94)

# Effect sizes and their upper and lower confidence intervals
sf94_effect_size = c()
sf94_effect_size_ucl = c()
sf94_effect_size_lcl = c()

for (treatment in c(0.85, 0.8, 0.75, 0.7)){
  
  boot_result = boot(data = subset1, statistic = effect_size_boot_sf94, R=1000, day = 5, treatment = treatment)
  boot_ci = boot.ci(boot_result, conf = 0.95, type="basic")
  
  sf94_effect_size = c(sf94_effect_size, boot_result$t0)
  sf94_effect_size_ucl = c(sf94_effect_size_ucl, boot_ci$basic[,5])
  sf94_effect_size_lcl = c(sf94_effect_size_lcl, boot_ci$basic[,4])
}

effect_size_sf94 = data.frame(outcome_measure = rep("S/F94 day 5",4),
                              treatment_effect = c(0.85, 0.8, 0.75, 0.7),
                              effect_size = sf94_effect_size,
                              effect_size_lcl = sf94_effect_size_lcl,
                              effect_size_ucl = sf94_effect_size_ucl)

sf94_sample_size = calculate_sample_size_sf94_vec(0.05 ,0.8 , sf94_effect_size, sd_sf94, corr_sf94)
sf94_sample_size_lcl = calculate_sample_size_sf94_vec(0.05 ,0.8 , sf94_effect_size_ucl, sd_sf94, corr_sf94)
sf94_sample_size_ucl = calculate_sample_size_sf94_vec(0.05 ,0.8 , sf94_effect_size_lcl, sd_sf94, corr_sf94)

sample_size_sf94 = data.frame(outcome_measure = rep("S/F94 day 5",4),
                              treatment_effect = c(0.85, 0.8, 0.75, 0.7),
                              sample_size = sf94_sample_size,
                              sample_size_lcl = sf94_sample_size_lcl,
                              sample_size_ucl = sf94_sample_size_ucl)


#### WHO

# Effect sizes and their upper and lower confidence intervals
who_effect_size = c()
who_effect_size_ucl = c()
who_effect_size_lcl = c()

for (treatment in c(0.85, 0.8, 0.75, 0.7)){
  
  boot_result = boot(data = subset1, statistic = effect_size_boot_who, R=1000, day = 5, treatment = treatment)
  boot_ci = boot.ci(boot_result, conf = 0.95, type="basic")
  
  who_effect_size = c(who_effect_size, boot_result$t0)
  who_effect_size_ucl = c(who_effect_size_ucl, boot_ci$basic[,5])
  who_effect_size_lcl = c(who_effect_size_lcl, boot_ci$basic[,4])
}

effect_size_who = data.frame(outcome_measure = rep("WHO day 5",4),
                             treatment_effect = c(0.85, 0.8, 0.75, 0.7),
                             effect_size = who_effect_size,
                             effect_size_lcl = who_effect_size_lcl,
                             effect_size_ucl = who_effect_size_ucl)

who_sample_size = c( calculate_sample_size_who(0.05 ,0.8 , who_effect_size[1], who_prop_table["subset1_day5_P", ]),
                         calculate_sample_size_who(0.05 ,0.8 , who_effect_size[2], who_prop_table["subset1_day5_P", ]),
                         calculate_sample_size_who(0.05 ,0.8 , who_effect_size[3], who_prop_table["subset1_day5_P", ]),
                         calculate_sample_size_who(0.05 ,0.8 , who_effect_size[4], who_prop_table["subset1_day5_P", ]) )

who_sample_size_ucl = c( calculate_sample_size_who(0.05 ,0.8 , who_effect_size_ucl[1], who_prop_table["subset1_day5_P", ]),
                              calculate_sample_size_who(0.05 ,0.8 , who_effect_size_ucl[2], who_prop_table["subset1_day5_P", ]),
                              calculate_sample_size_who(0.05 ,0.8 , who_effect_size_ucl[3], who_prop_table["subset1_day5_P", ]),
                              calculate_sample_size_who(0.05 ,0.8 , who_effect_size_ucl[4], who_prop_table["subset1_day5_P", ]) )

who_sample_size_lcl = c( calculate_sample_size_who(0.05 ,0.8 , who_effect_size_lcl[1], who_prop_table["subset1_day5_P", ]),
                             calculate_sample_size_who(0.05 ,0.8 , who_effect_size_lcl[2], who_prop_table["subset1_day5_P", ]),
                             calculate_sample_size_who(0.05 ,0.8 , who_effect_size_lcl[3], who_prop_table["subset1_day5_P", ]),
                             calculate_sample_size_who(0.05 ,0.8 , who_effect_size_lcl[4], who_prop_table["subset1_day5_P", ]) )


sample_size_who = data.frame(outcome_measure = rep("WHO day 5",4),
                              treatment_effect = c(0.85, 0.8, 0.75, 0.7),
                              sample_size = who_sample_size,
                              sample_size_lcl = who_sample_size_lcl,
                              sample_size_ucl = who_sample_size_ucl)



#### Sustained 1 level improvement

calculate_sample_size_susimp_vec = Vectorize(calculate_sample_size_susimp)

# Effect sizes and their upper and lower confidence intervals
susimp_effect_size = c()
susimp_effect_size_ucl = c()
susimp_effect_size_lcl = c()

for (treatment in c(0.85, 0.8, 0.75, 0.7)){
  
  boot_result = boot(data = subset1, statistic = effect_size_boot_susimp, R=1000, improvement = 1, treatment = treatment)
  boot_ci = boot.ci(boot_result, conf = 0.95, type="basic")
  
  susimp_effect_size = c(susimp_effect_size, boot_result$t0)
  susimp_effect_size_ucl = c(susimp_effect_size_ucl, boot_ci$basic[,5])
  susimp_effect_size_lcl = c(susimp_effect_size_lcl, boot_ci$basic[,4])
}

effect_size_susimp = data.frame(outcome_measure = rep("Sustained 1 level improvement",4),
                             treatment_effect = c(0.85, 0.8, 0.75, 0.7),
                             effect_size = susimp_effect_size,
                             effect_size_lcl = susimp_effect_size_lcl,
                             effect_size_ucl = susimp_effect_size_ucl)

susimp_sample_size = calculate_sample_size_susimp_vec(0.8 , susimp_effect_size, susimp_prop_table["subset1", "sustained_1L_improvement"])
susimp_sample_size_lcl = calculate_sample_size_susimp_vec(0.8 , susimp_effect_size_ucl, susimp_prop_table["subset1", "sustained_1L_improvement"])
susimp_sample_size_ucl = calculate_sample_size_susimp_vec(0.8 , susimp_effect_size_lcl, susimp_prop_table["subset1", "sustained_1L_improvement"])

sample_size_susimp = data.frame(outcome_measure = rep("Sustained 1 level improvement",4),
                              treatment_effect = c(0.85, 0.8, 0.75, 0.7),
                              sample_size = susimp_sample_size,
                              sample_size_lcl = susimp_sample_size_lcl,
                              sample_size_ucl = susimp_sample_size_ucl)




#### Protocolised sf94

# Assumed parameters of measurement error model
sd_mult = 0.8
rho_opp_prot = 0.7

# Effect sizes and their upper and lower confidence intervals
sf94_prot_effect_size = c()
sf94_prot_effect_size_ucl = c()
sf94_prot_effect_size_lcl = c()

for (treatment in c(0.85, 0.8, 0.75, 0.7)){
  
  boot_result = boot(data = subset1, statistic = effect_size_boot_sf94_prot, R=1000, treatment = treatment, sd_mult = sd_mult, rho_opp_prot = rho_opp_prot)
  boot_ci = boot.ci(boot_result, conf = 0.95, type="basic")
  
  sf94_prot_effect_size = c(sf94_prot_effect_size, boot_result$t0)
  sf94_prot_effect_size_ucl = c(sf94_prot_effect_size_ucl, boot_ci$basic[,5])
  sf94_prot_effect_size_lcl = c(sf94_prot_effect_size_lcl, boot_ci$basic[,4])
}

effect_size_sf94_prot = data.frame(outcome_measure = rep("Protocolised S/F94 day 5",4),
                                treatment_effect = c(0.85, 0.8, 0.75, 0.7),
                                effect_size = sf94_prot_effect_size,
                                effect_size_lcl = sf94_prot_effect_size_lcl,
                                effect_size_ucl = sf94_prot_effect_size_ucl)

# Mean and standard deviation of opportunistic measurements
mean_opp = mean(subset1$sf94_day5_P, na.rm = TRUE)
sd_opp = sd(subset1$sf94_day5_P, na.rm = TRUE)

# Calculate correlation between sf94 day 5 and sf94 day 0.
rho = cor(subset1$sf94_day5_P,  subset1$sf94_day0, use = "complete.obs")

# Assume that protocolised measurements have 0.8* standard deviation of opportunistic measurements,
# and there is 0.7 correlation between them
mean_prot = mean_opp
sd_prot = sd_mult * sd_opp

sf94_prot_sample_size = calculate_sample_size_sf94_vec(0.05 , 0.8, sf94_prot_effect_size, sd_prot, rho/rho_opp_prot^2) 
sf94_prot_sample_size_lcl = calculate_sample_size_sf94_vec(0.05 , 0.8, sf94_prot_effect_size_ucl, sd_prot, rho/rho_opp_prot^2) 
sf94_prot_sample_size_ucl = calculate_sample_size_sf94_vec(0.05 , 0.8, sf94_prot_effect_size_lcl, sd_prot, rho/rho_opp_prot^2) 

sample_size_sf94_prot = data.frame(outcome_measure = rep("Protocolised S/F94 day 5",4),
                                treatment_effect = c(0.85, 0.8, 0.75, 0.7),
                                sample_size = sf94_prot_sample_size,
                                sample_size_lcl = sf94_prot_sample_size_lcl,
                                sample_size_ucl = sf94_prot_sample_size_ucl)




#### Combine all and create plot

df_effect_size = rbind(effect_size_sf94, effect_size_who, effect_size_susimp, effect_size_sf94_prot)
write.csv(df_effect_size, paste0("/home/skerr/Git/SF94/Outputs/", time_stamp, "/effect_sizes_all_outcomes.csv"))

df_sample_size = rbind(sample_size_mort, sample_size_sf94, sample_size_who, sample_size_susimp, sample_size_sf94_prot)
write.csv(df_sample_size, paste0("/home/skerr/Git/SF94/Outputs/", time_stamp, "/sample_sizes_all_outcomes.csv"))

ggplot(df_sample_size , 
  aes(x= treatment_effect,
  y = sample_size,
  group= outcome_measure, colour = outcome_measure, fill = outcome_measure)) +
  geom_line() +
  geom_ribbon(aes(ymin = sample_size_lcl, ymax = sample_size_ucl), linetype=1, alpha=0.2, colour = NA) +
  xlab("Treatment effect (predicted 28-day mortality relative risk ratio)")+ 
  ylab("Sample size") +
  scale_x_reverse() +
  theme_bw()+
  guides(fill=guide_legend(title="Outcome measure"))

ggsave(dpi=300, path = paste0("/home/skerr/Git/SF94/Outputs/", time_stamp), filename="samplesize_graph.pdf")





#################################### sf94 violin plots ####################################################

# sf94 violin plots over 12 days
df = dplyr::select(data, subjid, days_since_start, sf94, mortality_28) %>%
  filter(!is.na(sf94), days_since_start <= 12, subjid %in% pull(data_1, subjid) ) %>%
  mutate(days_since_start = factor(as.character((days_since_start)), levels = 0:12),
         mortality_28 = as.factor(mortality_28))

n_12 = length(unique(df$subjid))

# Split violin plot
ggplot(df, 
       aes(x=days_since_start, y=sf94, fill=mortality_28)) +
  geom_split_violin(width=1.5)+
  xlab("Day")+
  ylab("S/F94")+
  ggtitle(paste0("S/F94 in the first 12 days since admission (N=", n_12, ")") )+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_manual(values =c("#f60000", "#0000f6"),
                    name="28-day outcome", labels=c("Discharged alive", "Death"))

ggsave(width=13, dpi=300, path = paste0("/home/skerr/Git/SF94/Outputs/", time_stamp), filename="sf94_violin_plot_12_days.pdf")

#Respiratory rate and SF9/4 function, including regression line (Sup figure 5)
pearson_value = as.character(round(cor(x=data_1$sf94, y=data_1$rr_vsorres, method = "pearson", use = "complete.obs"), 2))

ggplot(data_1, aes(x=sf94, y=rr_vsorres)) +
  geom_jitter(size=1.2, shape=16, stroke=0, alpha=0.5)+
  geom_smooth(method=lm, colour="darkblue")+
  xlab("S/F94")+
  ylab("Respiratory Rate")+
  theme_bw()+
  annotate("text", x=c(3.7,4), y=68, label= c("R=", pearson_value))

ggsave(dpi=300, path = paste0("/home/skerr/Git/SF94/Outputs/", time_stamp), filename="sf94_rr_plot.pdf")

write.csv(data.frame(unique_subjids_subset1 = length(unique(subset1$subjid)),
                     unique_subjids_before_day12 = n_12),
          paste0("/home/skerr/Git/SF94/Outputs/", time_stamp, "/number_unique_subjids.csv"))


#time series SF94 for each day split by outcome
ggplot(df, aes(x=days_since_start, y=sf94,
               group=mortality_28, colour=mortality_28)) +
  stat_summary(geom="line", fun=mean)+
  ggtitle("Mean S/F94 over time, by outcome")+
  xlab("Day")+
  ylab("S/F94")+
  theme_bw()+
  scale_color_manual(values =c("#f60000", "#0000f6"),
                     name= "Outcome", labels=c("Discharged alive", "Death"))

ggsave(dpi=300, path = paste0("/home/skerr/Git/SF94/Outputs/", time_stamp), filename="timeseries_sf94.pdf")

# sf94 violin plots over 28 days
df = dplyr::select(data, subjid, days_since_start, sf94, mortality_28) %>%
  filter(!is.na(sf94), days_since_start <= 28, subjid %in% pull(data_1, subjid) ) %>%
  mutate(days_since_start = factor(as.character((days_since_start)), levels = 0:28),
         mortality_28 = as.factor(mortality_28))

n_28 = length(unique(df$subjid))

# Split violin plot
ggplot(df, 
       aes(x=days_since_start, y=sf94, fill=mortality_28)) +
  geom_split_violin(width=1.5)+
  xlab("Day")+
  ylab("S/F94")+
  ggtitle(paste0("S/F94 in the first 12 days since admission (N=", n_28, ")") )+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_fill_manual(values =c("#f60000", "#0000f6"),
                    name="28-day outcome", labels=c("Discharged alive", "Death"))

ggsave(width=13, dpi=300, path = paste0("/home/skerr/Git/SF94/Outputs/", time_stamp), filename="sf94_violin_plot_28_days.pdf")


# sf94 day 5 violin plot for everyone in subjids_1 who has a severity scale
who_day5 = filter(data_1, days_since_start == 5, !is.na(severity_scale_ordinal)) %>%
                mutate(severity_scale_ordinal = paste0("WHO level ", severity_scale_ordinal)) %>%
                mutate(severity_scale_ordinal = factor(severity_scale_ordinal,
                                                      levels=c("WHO level 4","WHO level 5",
                                                               "WHO level 6","WHO level 7",
                                                               "WHO level 8","WHO level 9","WHO level 10")) )
n_day5 = length(unique(who_day5$subjid))
title = paste0("WHO ordinal severity scale and S/F94 (N=", n_day5, ")")

ggplot(who_day5,
       aes(x=severity_scale_ordinal, y=sf94, fill=severity_scale_ordinal)) +
  geom_violin()+ #remove outliers
  theme_bw()+
  ggtitle(title)+ 
  ylab("S/F94 day5") +
  scale_fill_manual(values=c("#ff0000", "#d7001b", "#a5002b","#7e0055",
                             "#530073","#350087", "#0000aa"))+
  xlab("") +
  theme(legend.position = "none",
        plot.title = element_text (hjust = 0.5))+ #remove legend + center title
  scale_x_discrete(labels=c("4 Hosp", "5 Ox", "6 CPAP/HFNO", "7 IMV", "8 IMV S/F<2", "9 MOF", "10 Dead"))

ggsave(dpi=300, path = paste0("/home/skerr/Git/SF94/Outputs/", time_stamp), filename="who_sf94_day5_violin_plot.pdf")


# sf94 day 0 violin plot
day0 = filter(data_1, days_since_start == 0)
n_day0 = length(unique(day0$subjid))
title = paste0("Selected patients (N=", n_day0, ")")

ggplot(day0,
  aes(x=as.factor(mortality_28), y=sf94, fill=as.factor(mortality_28) )) +
  geom_violin() + 
  theme_bw()+
  ggtitle(title)+
  scale_fill_manual(values =c("#f60000", "#0000f6"))+
  scale_x_discrete(labels = c("Discharge","Death"))+
  ylab("S/F94 day0")+
  xlab("") +
  theme(legend.position = "none",
        plot.title = element_text (hjust = 0.5)) #remove legend + center title

ggsave(dpi=300, path = paste0("/home/skerr/Git/SF94/Outputs/", time_stamp), filename="mort_sf94_day0_violin_plot.pdf")

# sf94 day 5 violin plot
day5 = filter(data_1, days_since_start == 5)
nday5 = length(unique(day5$subjid))
title = paste0("Selected patients (N=", nday5, ")")

ggplot(day5,
       aes(x=as.factor(mortality_28), y=sf94, fill=as.factor(mortality_28) )) +
  geom_violin()+ 
  theme_bw()+
  ggtitle(title)+
  scale_fill_manual(values =c("#f60000", "#0000f6"))+
  scale_x_discrete(labels = c("Discharge","Death"))+
  ylab("S/F94 day5")+
  xlab("") +
  theme(legend.position = "none",
        plot.title = element_text (hjust = 0.5)) #remove legend + center title

ggsave(dpi=300, path = paste0("/home/skerr/Git/SF94/Outputs/", time_stamp), filename="mort_sf94_day5_violin_plot.pdf")




# sf94 day 5 violin plot for all rows with a severity scale measurement
who_day5_all = filter(data, days_since_start == 5, !is.na(severity_scale_ordinal)) %>%
  mutate(severity_scale_ordinal = paste0("WHO level ", severity_scale_ordinal)) %>%
  mutate(severity_scale_ordinal = factor(severity_scale_ordinal,
                                         levels=c("WHO level 4","WHO level 5",
                                                  "WHO level 6","WHO level 7",
                                                  "WHO level 8","WHO level 9","WHO level 10")) )
n_who5all = length(unique(who_day5_all$subjid))
title = paste0("WHO ordinal severity scale for unselected patients (N=", n_who5all, ")", sep = "")

ggplot(who_day5_all,
       aes(x=severity_scale_ordinal, y=sf94, fill=severity_scale_ordinal)) +
  geom_violin()+ #remove outliers
  theme_bw()+
  scale_fill_manual(values=c("#ff0000", "#d7001b", "#a5002b","#7e0055",
                             "#530073","#350087", "#0000aa"))+
  ggtitle(title)+ 
  ylab("S/F94 day5") +
  xlab("") +
  theme(legend.position = "none",
        plot.title = element_text (hjust = 0.5))+ #remove legend + center title
  scale_x_discrete(labels=c("4 Hosp", "5 Ox", "6 CPAP/HFNO", "7 IMV", "8 IMV S/F<2", "9 MOF", "10 Dead"))

ggsave(dpi=300, path = paste0("/home/skerr/Git/SF94/Outputs/", time_stamp), filename="who_sf94_day5_violin_plot_all.pdf")




# sf94 day 0 violin plot for everyone
day0_all = subset(data, days_since_start == 0)
n_day0all = length(unique(day0_all$subjid))
title = paste0("Unselected patients (N=", n_day0all, ")", sep = "")

ggplot(day0_all,
  aes(x=as.factor(mortality_28), y=sf94, fill=as.factor(mortality_28) )) +
  geom_violin()+ 
  theme_bw()+
  ggtitle(title)+ 
  scale_fill_manual(values =c("#f60000", "#0000f6"))+
  ylab("S/F94 day0")+
  xlab("")+
  scale_x_discrete(labels = c("Discharge","Death"))+
  theme(legend.position = "none",
        plot.title = element_text (hjust = 0.5)) #remove legend + center title

ggsave(dpi=300, path = paste0("/home/skerr/Git/SF94/Outputs/", time_stamp), filename="mort_sf94_day0_violin_plot_all.pdf")


# sf94 day 5 violin plot for everyone
day5_all = subset(data, days_since_start == 5)
n_day5all = length(unique(day5_all$subjid))
title = paste0("Unselected patients (N=", n_day5all, ")", sep = "")

ggplot(day5_all,
       aes(x=as.factor(mortality_28), y=sf94, fill=as.factor(mortality_28) )) +
  geom_violin()+ 
  theme_bw()+
  ggtitle(title)+ 
  scale_fill_manual(values =c("#f60000", "#0000f6"))+
  ylab("S/F94 day5")+
  xlab("")+
  scale_x_discrete(labels = c("Discharge","Death"))+
  theme(legend.position = "none",
        plot.title = element_text (hjust = 0.5)) #remove legend + center title

ggsave(dpi=300, path = paste0("/home/skerr/Git/SF94/Outputs/", time_stamp), filename="mort_sf94_day5_violin_plot_all.pdf")





############################# Model summary and predictions #############################################

# tbl_regression appears only to work with glm and not lrm
multivariate_model <- glm(mortality_28 ~ sf94_day0 + sf94_day5_P, subset1, family = "binomial")

results = tidy(multivariate_model) %>%
  mutate(ucl = estimate + 1.96 * std.error,
         lcl = estimate - 1.96 * std.error,
         term = gsub("period", "", term)) %>%
  select(term, estimate, lcl, ucl) %>%
  mutate_if(is.numeric, ~formatC(round(exp(.), 2), format = "f", big.mark = ",", drop0trailing = TRUE)) %>%
  mutate(estimate = paste0(estimate, " (", lcl, " - ", ucl, ")" )) %>%
  select(-lcl, -ucl) 

write.csv(results, paste0("/home/skerr/Git/SF94/Outputs/", time_stamp, "/day0_day5_P_multivariate_model_summary.csv"))

# Predict function only appears to work with lrm, not glm

#First need to set data distribution for rms functions
attach(subset1)
ddist <- datadist(sf94_day0, sf94_day5_P, mortality_28)
options(datadist="ddist")
detach(subset1)


multivariate_model <- lrm(mortality_28 ~ sf94_day0 + sf94_day5_P, subset1, x=TRUE, y=TRUE)
not_na = sum(!is.na(subset1$mortality_28) & !is.na(subset1$sf94_day0) & !is.na(subset1$sf94_day5_P) )


title_d5mult<-paste("S/F94 day 5 (N=", not_na, ")", sep = "")
title_d0mult<-paste("S/F94 day 0 (N=", not_na, ")", sep = "")

### Predicted mortality risk plots

## Multivariate
# Day 0
plot_d0_multi = ggplot(Predict(multivariate_model, fun=plogis), sepdiscrete="vertical",
                       ylab= "Risk of 28-day mortality", ylim=c(0,0.8)) + theme_bw()

# Remove column day 5 (column 2) and final 200 rows
plot_d0_multi$data<-plot_d0_multi$data[-c(201:400),-c(2)]

# Change label name
plot_d0_multi$data$.predictor.<-factor(plot_d0_multi$data$.predictor.,
                                       labels= paste0("S/F94 day 0 (N=", not_na, ")")) 
plot_d0_multi<-plot_d0_multi + facet_grid(. ~ .predictor., labeller = label_value)


# Day 5
plot_d5_multi<- ggplot(Predict(multivariate_model, fun=plogis),sepdiscrete="vertical",
                       ylab= "Risk of 28-day mortality") + theme_bw()

# Change label name
plot_d5_multi$data<-plot_d5_multi$data[-c(1:200),-c(1)] #change label names
plot_d5_multi$data$.predictor.<-factor(plot_d5_multi$data$.predictor.,
                                       labels=title_d5mult)

plot_d5_multi<-plot_d5_multi + facet_grid(. ~ .predictor., labeller = label_value)



## Univariate
univariate_model <- lrm(mortality_28 ~ sf94_day0, subset1, x=TRUE, y=TRUE)

not_na = sum(!is.na(subset1$mortality_28) & !is.na(subset1$sf94_day0))

plot_uni<-ggplot(Predict(univariate_model, fun=plogis),
                       ylab= "Risk of 28-day mortality", ylim=c(0,0.8), sepdiscrete="vertical")+ theme_bw()
plot_uni$data$.predictor. <- factor(plot_uni$data$.predictor., 
                                          labels = paste0("S/F94 day 0 (N=", not_na, ")"))
#  this "label_value" labeller() call alters the facet labels
plot_uni<-plot_uni + facet_grid(. ~ .predictor., labeller = label_value)



# Combine the two day 0 plots into 1 figure
day_0_plots = ggarrange(plot_d0_multi, 
                        plot_uni+ theme(axis.text.y=element_blank(), #remove Y axis labels and text
                                       axis.ticks.y=element_blank(),
                                       axis.title.y=element_blank()), ncol=2, widths=c(1,1)) #make same size


ggsave(plot=day_0_plots, dpi=300, path = paste0("/home/skerr/Git/SF94/Outputs/", time_stamp), filename="day_0_predicted_mortality_plots.pdf")
ggsave(plot=plot_d5_multi, dpi=300, path = paste0("/home/skerr/Git/SF94/Outputs/", time_stamp), filename="day_5_predicted_mortality_multivariate_model_plot.pdf",
       width = 4, height=7, units = "cm")
ggsave(plot=plot_uni, dpi=300, path = paste0("/home/skerr/Git/SF94/Outputs/", time_stamp), filename="day_0_predicted_mortality_univariate_model_plot.pdf")
ggsave(plot=plot_d0_multi, dpi=300, path = paste0("/home/skerr/Git/SF94/Outputs/", time_stamp), filename="day_0_predicted_mortality_multivariate_model_plot.pdf")


