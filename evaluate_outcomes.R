length(unique(subset1$subjid))


#Mean and SD for different days since admission (0-10) for different variables (sf94, sf94_dd, sf94_dd_day0)
mean(subset1$sf94_dd_day0[subset1$days_since_admission == 0], na.rm = T)
mean(subset1$sf94_dd_day0[subset1$days_since_admission == 1], na.rm = T)
mean(subset1$sf94_dd_day0[subset1$days_since_admission == 2], na.rm = T)
mean(subset1$sf94_dd_day0[subset1$days_since_admission == 3], na.rm = T)
mean(subset1$sf94_dd_day0[subset1$days_since_admission == 4], na.rm = T)
mean(subset1$sf94_dd_day0[subset1$days_since_admission == 5], na.rm = T)
mean(subset1$sf94_dd_day0[subset1$days_since_admission == 6], na.rm = T)
mean(subset1$sf94_dd_day0[subset1$days_since_admission == 7], na.rm = T)
mean(subset1$sf94_dd_day0[subset1$days_since_admission == 8], na.rm = T)
mean(subset1$sf94_dd_day0[subset1$days_since_admission == 9], na.rm = T)
mean(subset1$sf94_dd_day0[subset1$days_since_admission == 10], na.rm = T)
#SD
sd(subset1$sf94_dd_day0[subset1$days_since_admission == 0], na.rm = T)
sd(subset1$sf94_dd_day0[subset1$days_since_admission == 1], na.rm = T)
sd(subset1$sf94_dd_day0[subset1$days_since_admission == 2], na.rm = T)
sd(subset1$sf94_dd_day0[subset1$days_since_admission == 3], na.rm = T)
sd(subset1$sf94_dd_day0[subset1$days_since_admission == 4], na.rm = T)
sd(subset1$sf94_dd_day0[subset1$days_since_admission == 5], na.rm = T)
sd(subset1$sf94_dd_day0[subset1$days_since_admission == 6], na.rm = T)
sd(subset1$sf94_dd_day0[subset1$days_since_admission == 7], na.rm = T)
sd(subset1$sf94_dd_day0[subset1$days_since_admission == 8], na.rm = T)
sd(subset1$sf94_dd_day0[subset1$days_since_admission == 9], na.rm = T)
sd(subset1$sf94_dd_day0[subset1$days_since_admission == 10], na.rm = T)

#correlation
library(data.table)
#Correlation subsets: same subjects for different days,
# eg all subjects with a day-1 and day 5 measurement
#only relevant columns (days_since_admission and SF94)
correlation_subset<-subset1[,c(1,2,50)]
#remove after day 10 to make easier to work with
correlation_subset<-subset(correlation_subset, days_since_admission<11)
correlation_subset<-subset(correlation_subset, days_since_admission>=0)
correlation_subset<-data.frame(correlation_subset)
#add day for more sensible variable name in wide format
correlation_subset$days_since_admission<- paste("day", correlation_subset$days_since_admission, sep = "_")
head(correlation_subset)
#from long to wide format
correlation_subset<- correlation_subset %>%
  mutate(rn= row_number()) %>%
  spread (days_since_admission, sf94) %>%
  select(-rn)
#keep only 1 row for each subject
f <- function(x) {
  x <- na.omit(x)
  if (length(x) > 0) unique(x) else NA
}
correlation_subset<-correlation_subset %>% 
  group_by(subjid) %>% 
  summarise_all(funs(f))
#make subsets of data in which all subjects have SF94 available for those 2 days
correlation_subset_07<-subset(correlation_subset, (!is.na(day_0)&!is.na(day_7)))
correlation_subset_05<-subset(correlation_subset, (!is.na(day_0)&!is.na(day_5)))
correlation_subset15<-subset(correlation_subset, (!is.na(day_1)&!is.na(day_5)))
correlation_subset17<-subset(correlation_subset, (!is.na(day_1)&!is.na(day_7)))
length(correlation_subset_17$subjid)
# correlation DAY 0 DAY 7
x <-  correlation_subset_07$day_0
y <-  correlation_subset_07$day_7
cor(x,y)
# DAY 0-5
x <-  correlation_subset_05$day_0
y <-  correlation_subset_05$day_5
cor(x,y)
# DAY 1- 5
x <-  correlation_subset15$day_1
y <-  correlation_subset15$day_5
cor(x,y)
# DAY 1-7
x <-  correlation_subset17$day_1
y <-  correlation_subset17$day_7
cor(x,y)

#compare day 0-5 correlation group to the complete population


#compare key clinical variables between 2 groups (SaO2<0.94 |fio2=0.21) and SaO2>0.94
df_1$sf94_group<-if_else((df_1$sao2 <=0.94 | df_1$fio2_corrected == 0.21), "SF94<", "SF94>")
tapply(df_1$age_estimateyears, df_1$sf94_group, summary, na.rm=T)
tapply(df_1$daily_temp_vsorres, df_1$sf94_group, summary, na.rm=T)
tapply(df_1$systolic_vsorres, df_1$sf94_group, summary, na.rm=T)
tapply(df_1$diastolic_vsorres, df_1$sf94_group, summary, na.rm=T)
tapply(df_1$onset2admission, df_1$sf94_group, summary, na.rm=T)
tapply(df_1$hodur, df_1$sf94_group, summary, na.rm=T)
table(df_1$sex, df_1$sf94_group)
table(df_1$infiltrates_faorres, df_1$sf94_group)
table(df_1$outcome, df_1$sf94_group)
table(df_1$severity_scale_ordinal, df_1$sf94_group)
#check distribution in fio2 when excluding fio2=0.21
high_fio2<-subset(df_1, fio2_corrected >0.22)
tapply(high_fio2$fio2_corrected, high_fio2$sf94_group, summary, na.rm=T)

#barchar mean/ variance on different days
library(ggplot2)
library(cowplot)
days_meanvar<-df_1[,c(117,121)]
#removing missing admission day values
days_meanvar<-subset(days_meanvar, !is.na(days_since_admission))
#remove days >10, easier to work with
days_meanvar<-subset(days_meanvar, (days_since_admission <11))
days_meanvar<-subset(days_meanvar, (days_since_admission >=0))
#change to character and set correct order
days_meanvar$days_since_admission<-as.character(days_meanvar$days_since_admission)
days_meanvar$days_since_admission<-factor(days_meanvar$days_since_admission,
                                          levels=c("0","1","2","3","4","5","6","7","8","9","10"))
#function to calculate summary stats
min.mean.sd.max <- function(x) {
  r <- c(min(x), mean(x) - sd(x), mean(x), mean(x) + sd(x), max(x))
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}
p <- ggplot(days_meanvar, aes(x=days_since_admission, y=sfr_value, fill=days_since_admission)) +
  stat_summary(fun.data = min.mean.sd.max, geom = "boxplot") +
  theme(legend.position="none")+
  xlab("Day")+
  ylab("S/F94 value")+
  ggtitle("S/F94 value for each day- mean±SD (data=S/F94")
p



#plot trajectories based on worst severity score value
#make variable for highest severity score for each subject
df_score<-df_1
#keep only worst score for each subjects
df_score<- df_score%>%
  group_by(subjid)%>%
  slice(which.max(severity_scale_ordinal))
#rename variable
df_score<-df_score %>%
  dplyr::rename(worst_score= severity_scale_ordinal)
#only keep subject ID and worst score
df_score<-df_score[,c(1,53)]
#add to original dataframe 
df_score2<-df_1
df_score<-left_join(df_score2,df_score, by="subjid" )
#keep subjects with at least 5 entries
df_score<- df_score %>%
  group_by(subjid)%>%
  filter(n()>=5)

#plot trajectory for each severity score subgroup
levels_plot<-ggplot(data=df_score,
                    aes(x=days_since_admission, y=sfr_value)) + 
  geom_line(aes(group=subjid), alpha=0.1)+
  xlab("Measurement day")+
  ylab("S/F value")+
  xlim(c(0,60))
L10<-levels_plot %+% subset(df_score, worst_score %in% c(10)) %+% ggtitle("Score 10")
L9<-levels_plot %+% subset(df_score, worst_score %in% c(9)) %+% ggtitle("Score 9")
L8<-levels_plot %+% subset(df_score, worst_score %in% c(8)) %+% ggtitle("Score 8 ")
L7<-levels_plot %+% subset(df_score, worst_score %in% c(7)) %+% ggtitle("Score 7")
L6<-levels_plot %+% subset(df_score, worst_score %in% c(6)) %+% ggtitle("Score 6")
L5<-levels_plot %+% subset(df_score, worst_score %in% c(5)) %+% ggtitle("Score 5")
L4<-levels_plot %+% subset(df_score, worst_score %in% c(4))%+% ggtitle("Score 4")
#cowplot
plot_grid(L10,L9,L8,L7,L6,L5,L4)

#Histograms for several days showing the spread in extreme SF94 values (sf94_dd)
#data used: subset extremes
# SF value used: SF94_dd
hist(subset1$sf94_dd[subset1$days_since_admission == 0], 
     breaks = 50, main="Day 0",ylim = c(0,15000), xlab="S/F94_dd", xlim = c(0,5))
hist(subset1$sf94_dd[subset1$days_since_admission == 1], 
     breaks = 50, main="Day 1",ylim = c(0,15000), xlab="S/F94_dd", xlim = c(0,5))
hist(subset1$sf94_dd[subset1$days_since_admission == 2], 
     breaks = 50, main="Day 2",ylim = c(0,15000), xlab="S/F94_dd", xlim = c(0,5))
hist(subset1$sf94_dd[subset1$days_since_admission == 3], 
     breaks = 50, main="Day 3",ylim = c(0,15000), xlab="S/F94_dd", xlim = c(0,5))
hist(subset1$sf94_dd[subset1$days_since_admission == 5], 
     breaks = 50, main="Day 5",ylim = c(0,15000), xlab="S/F94_dd", xlim = c(0,5))
hist(subset1$sf94_dd[subset1$days_since_admission == 7], 
     breaks = 50, main="Day 7",ylim = c(0,15000), xlab="S/F94_dd", xlim = c(0,5))
hist(subset1$sf94_dd[subset1$days_since_admission == 10], 
     breaks = 50, main="Day 10", xlab="S/F94_dd", ylim = c(0,15000), xlim = c(0,5))
hist(subset1$sf94_dd[subset1$days_since_admission == 20], 
     breaks = 50, main="Day 20", xlab="S/F94_dd", ylim = c(0,15000), xlim = c(0,5))
hist(subset1$sf94_dd[subset1$days_since_admission == 40], 
     breaks = 50, main="Day 40", xlab="S/F94_dd", ylim = c(0,15000), xlim = c(0,5))
rpng.off()


