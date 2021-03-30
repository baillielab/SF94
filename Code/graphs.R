library(dplyr)
library(ggplot2)
library(data.table)
library(tidyr)


#----------------------------------- GRAPHS --------------------------------
#split violin plot
#data
subjects_to_include <- filter(df_1, ( fio2 >=0.22 & days_since_start %in% c(0,1,2)  & age_estimateyears >19 & age_estimateyears <76 ) )['subjid']
subset_violin<-df_1[df_1$subjid %in% subjects_to_include$subjid,]
subset_violin <- as.data.frame(subset_violin)
base_sf94_12<-createDF(subset_violin, "base", "sf94", 12)
base_sf94_12<-distinct(base_sf94_12)
base_sf94_12<-setDT(base_sf94_12)[, lapply(.SD, na.omit), by=subjid] #keep 1 entry/subject
library(tidyr)
#transform to long format 
long_dfsf94_12<-base_sf94_12%>%
  pivot_longer(!subjid, names_to= "days_since_start", values_to= "sf94")
#removing rows without SF94 value
long_dfsf94_12 <- subset(long_dfsf94_12, !is.na(sf94))
#add 28 day mortality data
mortality<-df_1[,c("subjid","mortality_28")]
mortality<-mortality%>%
  group_by(subjid)%>%
  slice(which.min(mortality_28))
long_dfsf94_12<-left_join(long_dfsf94_12, mortality, by="subjid")
#change to character and set correct order
long_dfsf94_12$mortality_28<-as.character(long_dfsf94_12$mortality_28)
long_dfsf94_12$mortality_28<-factor(long_dfsf94_12$mortality_28,
                                    levels=c("0","1"))
long_dfsf94_12$days_since_start<-as.character(long_dfsf94_12$days_since_start)
long_dfsf94_12$days_since_start<-factor(long_dfsf94_12$days_since_start,
                                        levels=c("0","1","2","3","4","5",
                                                 "6","7","8","9","10", "11", "12"))
#function code for split violin plots
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

#split violin plot
p1 <- ggplot(long_dfsf94_12, aes(x=days_since_start, y=sf94, fill=mortality_28)) +
  geom_split_violin(width=1.5)+
  xlab("Day")+
  ylab("S/F94")+
  ggtitle("")+
  theme_bw()+
  scale_fill_discrete(name="28-day outcome", labels=c("Discharged alive", "Death"))


show(p1)

ggsave(plot=p1, width=10, dpi=300, path = '/home/skerr/Git/SF94/Outputs/', filename="12_days.pdf")

base_sf94_28<-createDF(subset_violin, "base", "sf94", 28)
#transform to long format 
long_dfsf94_28<-base_sf94_28%>%
  pivot_longer(!subjid, names_to= "days_since_start", values_to= "sf94")
#removing rows without SF94 value
long_dfsf94_28 <- subset(long_dfsf94_28, !is.na(sf94))
#add 28 day mortality data
mortality<-df_1[,c("subjid","mortality_28")]
mortality<-mortality%>%
  group_by(subjid)%>%
  slice(which.min(mortality_28))
long_dfsf94_28<-left_join(long_dfsf94_28, mortality, by="subjid")
#change to character and set correct order
long_dfsf94_28$mortality_28<-as.character(long_dfsf94_28$mortality_28)
long_dfsf94_28$mortality_28<-factor(long_dfsf94_28$mortality_28,
                                    levels=c("0","1"))
long_dfsf94_28$days_since_start<-as.character(long_dfsf94_28$days_since_start)
long_dfsf94_28$days_since_start<-factor(long_dfsf94_28$days_since_start,
                                        levels=c("0","1","2","3","4","5",
                                                 "6","7","8","9","10", "11", "12",
                                                 "13", "14", "15", "16", "17", "18",
                                                 "19", "20", "21", "22", "23", "24", 
                                                 "25", "26", "27", "28"))
violin_28days <- ggplot(long_dfsf94_28, aes(x=days_since_start, y=sf94, fill=mortality_28)) +
  geom_split_violin(width=1.5)+
  xlab("Day")+
  ylab("S/F94")+
  ggtitle("")+
  theme_bw()+
  scale_fill_discrete(name="28-day outcome", labels=c("Discharged alive", "Death"))+
  scale_x_discrete(expand = c(0,0))

ggsave(plot=violin_28days, width=12, dpi=300, path = '/home/skerr/Git/SF94/Outputs/', filename="28-days.pdf")

#time series SF94 for each day split by outcome
timeseries_sf94<-ggplot(long_dfsf94_12, aes(x=days_since_start, y=sf94,
                                            group=mortality_28, colour=mortality_28)) +
  stat_summary(geom="line", fun=mean)+
  ggtitle("Change in S/F94 over time, split by outcome")+
  xlab("Day")+
  ylab("S/F94")+
  theme_bw()+
  scale_color_discrete(name= "Outcome", labels=c("Discharged alive", "Death"))
timeseries_sf94

#example final graph
#mortality
mort_a_0.80<-sample_function(subset1,0.80)
mort_0.80<-cont_cor_function(mort_a_0.80)
mort_a_0.85<-sample_function(subset1,0.85)
mort_0.85<-cont_cor_function(mort_a_0.85)
mort_a_0.90<-sample_function(subset1,0.90)
mort_0.90<-cont_cor_function(mort_a_0.90)
mort_a_0.95<-sample_function(subset1,0.95)
mort_0.95<-cont_cor_function(mort_a_0.95)

mort_values<-rbind(mort_0.80,mort_0.85,mort_0.90,mort_0.95)

#SF94

sf94_ES_0.80<-sf94_regression(subset1,0.80)
sf94_ES_0.85<-sf94_regression(subset1,0.85)
sf94_ES_0.90<-sf94_regression(subset1,0.90)
sf94_ES_0.95<-sf94_regression(subset1,0.95)
sf94_0.80<-power_sf94(0.05,0.8,sf94_ES_0.80[,1], meanSD_subset1[2,1], cor_subset1[[1]])
sf94_0.85<-power_sf94(0.05,0.8,sf94_ES_0.85[,1], meanSD_subset1[2,1], cor_subset1[[1]])
sf94_0.90<-power_sf94(0.05,0.8,sf94_ES_0.90[,1], meanSD_subset1[2,1], cor_subset1[[1]])
sf94_0.95<-power_sf94(0.05,0.8,sf94_ES_0.95[,1], meanSD_subset1[2,1], cor_subset1[[1]])

sf94_values<-rbind(sf94_0.80,sf94_0.85,sf94_0.90,sf94_0.95)

#WHO
who_a_0.80<-who_function(subset1, 0.80)
who_0.80<-who_effectsize_function_ss(subset1, who_a_0.80)
who_a_0.85<-who_function(subset1, 0.85)
who_0.85<-who_effectsize_function_ss(subset1, who_a_0.85)
who_a_0.90<-who_function(subset1, 0.90)
who_0.90<-who_effectsize_function_ss(subset1, who_a_0.90)
who_a_0.95<-who_function(subset1, 0.95)
who_0.95<-who_effectsize_function_ss(subset1, who_a_0.95)
who_values<- rbind(who_0.80[1,1], who_0.85[1,1], who_0.90[1,1], who_0.95[1,1])

#sus imp
susimp_a_0.80<-susimpfunc(subset1,0.80)
susimp_0.80<-susimp_pwr_func(subset1, susimp_a_0.80[,1],susimp_a_0.80[,2])
susimp_a_0.85<-susimpfunc(subset1,0.85)
susimp_0.85<-susimp_pwr_func(subset1, susimp_a_0.85[,1],susimp_a_0.85[,2])
susimp_a_0.90<-susimpfunc(subset1,0.90)
susimp_0.90<-susimp_pwr_func(subset1, susimp_a_0.90[,1],susimp_a_0.90[,2])
susimp_a_0.95<-susimpfunc(subset1,0.95)
susimp_0.95<-susimp_pwr_func(subset1, susimp_a_0.95[,1],susimp_a_0.95[,2])

susimp_value<-rbind(susimp_0.80[1,1],susimp_0.85[1,1],susimp_0.90[1,1],susimp_0.95[1,1])

#bind values
ss_values<-rbind(mort_values, sf94_values, who_values, susimp_value)
ss_outcomemeasure<-c(rep("28-day mortality",4),rep("S/F94 day 5",4),rep("WHO day 5",4),rep("Sustained 1 level improvement",4) )
ss_treatmenteffect<-c("0.80", "0.85", "0.90", "0.95",
                      "0.80", "0.85", "0.90", "0.95",
                      "0.80", "0.85", "0.90", "0.95",
                      "0.80", "0.85", "0.90", "0.95")
samplesize_dataframe<-cbind(ss_values, ss_outcomemeasure, ss_treatmenteffect)
samplesize_dataframe<-data.frame(samplesize_dataframe)
colnames(samplesize_dataframe)<-c("values", "outcome_measure", "treatment_effect")
samplesize_dataframe$values<-as.numeric(as.character(samplesize_dataframe$values))
samplesize_graph<-ggplot(samplesize_dataframe, aes(x=treatment_effect, y=values,
                                  group= outcome_measure, colour=outcome_measure))
samplesize_graph + geom_path() + xlab("Treatment effect (predicited 28-day mortality relative risk ratio)")+ 
  ylab("Sample size") +
  ggtitle("")+
  scale_color_discrete(name="Outcome measure")+ theme_bw()


subset_graph<-subset_violin
#Respiratory rate and SF9/4 function, including regression line (Sup figure 5)
library(ggplot2)
rr_graph<-ggplot(subset_graph, aes(x=sf94, y=rr_vsorres, colour=sao2)) +
  geom_point(size=1.5, shape=16, stroke=0)+
  geom_smooth(method=lm, colour="black")+
  xlab("S/F94")+
  ylab("Respiratory Rate")+
  theme_bw()+
  scale_colour_gradient2(midpoint = 0.75,
                         low = "blue",
                         mid = "black",
                         high = "red",
                         limits=c(min(0.5),
                                  max(1.0)),
                         name="SaO2")
rr_graph
#title: rr_sf94_regression

# WHO and SF94 
#data = subset 1 + filters
# take day 5 from WHO and SF data 
who_sf_5<-subset(subset_graph, days_since_start==5)
who_sf_5<-subset(who_sf_5, !is.na(severity_scale_ordinal))
who_sf_5$severity_scale_ordinal<- paste("WHO level", who_sf_5$severity_scale_ordinal, sep = " ")
who_sf_5$severity_scale_ordinal<-factor(who_sf_5$severity_scale_ordinal,
                                        levels=c("WHO level 4","WHO level 5",
                                                 "WHO level 6","WHO level 7",
                                                 "WHO level 8","WHO level 9","WHO level 10"))
title_who5<-as.character(sum(!is.na(who_sf_5$sf94))) 
title_who5<-paste("WHO ordinal severity scale and S/F94 (N=", title_who5, ")", sep = "")
who_sf_5plot<-ggplot(who_sf_5,
                     aes(x=severity_scale_ordinal, y=sf94, fill=severity_scale_ordinal ))
who_sf_5plot+ geom_violin()+ #remove outliers
  theme_bw()+
  ggtitle(title_who5)+ 
  scale_fill_brewer(palette = "Spectral")+
  xlab("")+
  ylab("S/F94 day5")+
  theme(legend.position = "none",
        plot.title = element_text (hjust = 0.5))+ #remove legend + center title
  scale_x_discrete(labels=c("4 Hosp", "5 Ox", "6 CPAP", "7 IMV", "8 IMV S/F<2", "9 MOF", "10 Dead"))

library(RColorBrewer)
#mortality and SF94 on day 0 and day 5
subset_graph$mortality_28<-factor(subset_graph$mortality_28,
                                     levels=c("0","1"))
#for day 0 
sfmort_day0<-subset(subset_graph, (days_since_admission == 0))
#violin plots
#distribution of SF94 values on day 0 for unselected population

sfmort_day0plot<-ggplot(sfmort_day0,
                        aes(x=as.factor(mortality_28), y=sf94, fill=mortality_28 ))
title_0<-as.character(sum(!is.na(sfmort_day0$sf94))) 
title_0<-paste("(N=", title_0, ")", sep = "")
sfmort_day0plot + geom_violin()+ 
  theme_bw()+
  ggtitle(title_0)+
  scale_fill_brewer(palette = "Paired")+
  xlab("")+
  ylab("S/F94 day0")+
  theme(legend.position = "none",
        plot.title = element_text (hjust = 0.5)) #remove legend + center title


#for day 5 
sfmort_day5<-subset(subset_graph, (days_since_admission == 5))
#violin plots
#distribution of SF94 values on day 5 for unselected population
sfmort_day5plot<-ggplot(sfmort_day5,
                        aes(x=as.factor(mortality_28), y=sf94, fill=mortality_28 ))
title_5<-as.character(sum(!is.na(sfmort_day5$sf94))) 
title_5<-paste("(N=", title_5, ")", sep = "")
sfmort_day5plot + geom_violin()+ 
  theme_bw()+
  ggtitle(title_5)+ 
  scale_fill_brewer(palette = "Paired")+
  xlab("")+
  ylab("S/F94 day5")+
  theme(legend.position = "none",
        plot.title = element_text (hjust = 0.5)) #remove legend + center title

# figure 7A: effect size with alternative outcome in an unselected population
# data = df_1
# no filters used
#take day 5 from who and sf data
unselectedwho_day5<-subset(df_1, days_since_start == 5)
unselectedwho_day5<-subset(unselectedwho_day5, (!is.na(severity_scale_ordinal)))
unselectedwho_day5$severity_scale_ordinal<- paste("WHO level", unselectedwho_day5$severity_scale_ordinal, sep = " ")
unselectedwho_day5$severity_scale_ordinal<-factor(unselectedwho_day5$severity_scale_ordinal,
                                                  levels=c("WHO level 4","WHO level 5",
                                                           "WHO level 6","WHO level 7",
                                                           "WHO level 8","WHO level 9","WHO level 10"))
title_un_who5<-as.character(sum(!is.na(unselectedwho_day5$sf94))) 
title_un_who5<-paste("WHO ordinal severity scale for unselected subjects (N=", title_un_who5, ")", sep = "")
#violin plots (figure 7a)
unselect_who5<-ggplot(unselectedwho_day5,
                      aes(x=severity_scale_ordinal, y=sf94, fill=severity_scale_ordinal ))
unselect_who5+ geom_violin()+ #remove outliers
  theme_bw()+
  ggtitle(title_un_who5)+ 
  scale_fill_brewer(palette = "Spectral")+
  xlab("")+
  ylab("S/F94 day5")+
  theme(legend.position = "none",
        plot.title = element_text (hjust = 0.5))+ #remove legend + center title
  scale_x_discrete(labels=c("4 Hosp", "5 Ox", "6 CPAP", "7 IMV", "8 IMV S/F<2", "9 MOF", "10 Dead"))

#make dataframe with SF94 day 0 and day 5 + outcome for violin plots (figure 7b+c)
#for day 0 (unselected)
unselected_day0<-subset(df_1, (days_since_admission == 0))
#violin plots
#distribution of SF94 values on day 0 for unselected population
unselected_outcome_0<-ggplot(unselected_day0,
                             aes(x=as.factor(mortality_28), y=sf94, fill=mortality_28 ))
title_un0<-as.character(sum(!is.na(unselected_day0$sf94))) 
title_un0<-paste("Unselected subjects (N=", title_un0, ")", sep = "")
unselected_outcome_0 + geom_violin()+ 
  theme_bw()+
  ggtitle(title_un0)+ 
  scale_fill_brewer(palette = "Paired")+
  xlab("")+
  ylab("S/F94 day0")+
  theme(legend.position = "none",
        plot.title = element_text (hjust = 0.5)) #remove legend + center title

#Same for day 5 (figure 7c)
unselected_day5<-subset(df_1, (days_since_admission == 5))
#violin plots
unselected_outcome_5<-ggplot(unselected_day5,
                             aes(x=as.factor(mortality_28), y=sf94, fill=mortality_28 ))
title_un5<-as.character(sum(!is.na(unselected_day5$sf94))) 
title_un5<-paste("Unselected subjects (N=", title_un5, ")", sep = "")
unselected_outcome_5 + geom_violin()+ 
  theme_bw()+
  ggtitle(title_un5)+ 
  scale_fill_brewer(palette = "Paired")+
  xlab("")+
  ylab("S/F94 day5")+
  theme(legend.position = "none",
        plot.title = element_text (hjust = 0.5)) #remove legend + center title




