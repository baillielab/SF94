#sorry this file is still a mess.. will clean it up!! And work on efficiency

#Mean
#####CHANGE ########### CHANGE ##################################
colnames(subset1)
colnames(subset_extremes)
mean(subset_extremes$sf94_dd_day0[subset_extremes$days_since_admission == 0], na.rm = T)
mean(subset_extremes$sf94_dd_day0[subset_extremes$days_since_admission == 1], na.rm = T)
mean(subset_extremes$sf94_dd_day0[subset_extremes$days_since_admission == 2], na.rm = T)
mean(subset_extremes$sf94_dd_day0[subset_extremes$days_since_admission == 3], na.rm = T)
mean(subset_extremes$sf94_dd_day0[subset_extremes$days_since_admission == 4], na.rm = T)
mean(subset_extremes$sf94_dd_day0[subset_extremes$days_since_admission == 5], na.rm = T)
mean(subset_extremes$sf94_dd_day0[subset_extremes$days_since_admission == 6], na.rm = T)
mean(subset_extremes$sf94_dd_day0[subset_extremes$days_since_admission == 7], na.rm = T)
mean(subset_extremes$sf94_dd_day0[subset_extremes$days_since_admission == 8], na.rm = T)
mean(subset_extremes$sf94_dd_day0[subset_extremes$days_since_admission == 9], na.rm = T)
mean(subset_extremes$sf94_dd_day0[subset_extremes$days_since_admission == 10], na.rm = T)
#SD
sd(subset_extremes$sf94_dd_day0[subset1$days_since_admission == 0], na.rm = T)
sd(subset_extremes$sf94_dd_day0[subset1$days_since_admission == 1], na.rm = T)
sd(subset_extremes$sf94_dd_day0[subset1$days_since_admission == 2], na.rm = T)
sd(subset_extremes$sf94_dd_day0[subset1$days_since_admission == 3], na.rm = T)
sd(subset_extremes$sf94_dd_day0[subset1$days_since_admission == 4], na.rm = T)
sd(subset_extremes$sf94_dd_day0[subset1$days_since_admission == 5], na.rm = T)
sd(subset_extremes$sf94_dd_day0[subset1$days_since_admission == 6], na.rm = T)
sd(subset_extremes$sf94_dd_day0[subset1$days_since_admission == 7], na.rm = T)
sd(subset_extremes$sf94_dd_day0[subset1$days_since_admission == 8], na.rm = T)
sd(subset_extremes$sf94_dd_day0[subset1$days_since_admission == 9], na.rm = T)
sd(subset_extremes$sf94_dd_day0[subset1$days_since_admission == 10], na.rm = T)

length(na.omit(subset1$sf94[subset1$days_since_admission == 0]))
length(na.omit(subset1$sf94[subset1$days_since_admission == 1]))
length(na.omit(subset1$sf94[subset1$days_since_admission == 2]))
length(na.omit(subset1$sf94[subset1$days_since_admission == 3]))
length(na.omit(subset1$sf94[subset1$days_since_admission == 4]))
length(na.omit(subset1$sf94[subset1$days_since_admission == 5]))
length(na.omit(subset1$sf94[subset1$days_since_admission == 6]))
length(na.omit(subset1$sf94[subset1$days_since_admission == 7]))
length(na.omit(subset1$sf94[subset1$days_since_admission == 8]))
length(na.omit(subset1$sf94[subset1$days_since_admission == 9]))
length(na.omit(subset1$sf94[subset1$days_since_admission == 10]))


#correlation
library(data.table)
#Correlation subsets: same subjects for different days,
# eg all subjects with a day-1 and day 5 measurement
#only relevant columns (days_since_admission and SF94)
correlation_subset<-subset1[,c(1,41,48)]
colnames(subset1)
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
#make subset with data available for 2 days (0/7, 1/7, 0/5, 1/5, 1/6)
correlation_subset_0_7<-subset(correlation_subset, !is.na(day_0))
correlation_subset_0_7<-subset(correlation_subset_0_7, !is.na(day_7))
nrow(correlation_subset_1_5)
correlation_subset_0_5<-subset(correlation_subset, !is.na(day_0))
correlation_subset_0_5<-subset(correlation_subset_0_5, !is.na(day_5))
correlation_subset_1_5<-subset(correlation_subset, !is.na(day_1))
correlation_subset_1_5<-subset(correlation_subset_1_5, !is.na(day_5))
correlation_subset_1_7<-subset(correlation_subset, !is.na(day_1))
correlation_subset_1_7<-subset(correlation_subset_1_7, !is.na(day_7))
correlation_subset_1_6<-subset(correlation_subset, !is.na(day_1))
correlation_subset_1_6<-subset(correlation_subset_1_6, !is.na(day_6))
# correlation DAY 0 DAY 7
x <-  correlation_subset_0_7$day_0
y <-  correlation_subset_0_7$day_7
cor(x,y)
# DAY 0-5
x <-  correlation_subset_0_5$day_0
y <-  correlation_subset_0_5$day_5
cor(x,y)
# DAY 1- 5
x <-  correlation_subset_1_5$day_1
y <-  correlation_subset_1_5$day_5
cor(x,y)
# DAY 1-7
x <-  correlation_subset_1_7$day_1
y <-  correlation_subset_1_7$day_7
cor(x,y)
# DAY 1-6
x <-  correlation_subset_1_6$day_1
y <-  correlation_subset_1_6$day_6
cor(x,y)

#### RELATIONSHIP WHO SCALE AND SFR VALUES
#been done using subset3 and dfsfr94 data limited to day 0 and day 1
summary(dfsfr94_day1$sfr_value[dfsfr94_day1$severity_scale_ordinal == 4])
summary(dfsfr94_day1$sfr_value[dfsfr94_day1$severity_scale_ordinal == 5])
summary(dfsfr94_day1$sfr_value[dfsfr94_day1$severity_scale_ordinal == 6])
summary(dfsfr94_day1$sfr_value[dfsfr94_day1$severity_scale_ordinal == 7])
summary(dfsfr94_day1$sfr_value[dfsfr94_day1$severity_scale_ordinal == 8])
summary(dfsfr94_day1$sfr_value[dfsfr94_day1$severity_scale_ordinal == 9])
summary(dfsfr94_day1$sfr_value[dfsfr94_day1$severity_scale_ordinal == 10])
sd(dfsfr94_day1$sfr_value[dfsfr94_day1$severity_scale_ordinal == 4], na.rm = T)
sd(dfsfr94_day1$sfr_value[dfsfr94_day1$severity_scale_ordinal == 5], na.rm = T)
sd(dfsfr94_day1$sfr_value[dfsfr94_day1$severity_scale_ordinal == 6], na.rm = T)
sd(dfsfr94_day1$sfr_value[dfsfr94_day1$severity_scale_ordinal == 7], na.rm = T)
sd(dfsfr94_day1$sfr_value[dfsfr94_day1$severity_scale_ordinal == 8], na.rm = T)
sd(dfsfr94_day1$sfr_value[dfsfr94_day1$severity_scale_ordinal == 9], na.rm = T)
sd(dfsfr94_day1$sfr_value[dfsfr94_day1$severity_scale_ordinal == 10], na.rm = T)
table(dfsfr94_day1$severity_scale_ordinal)
table(subset3$severity_scale_ordinal)

#barchar mean/ variance on different days
library(ggplot2)
library(cowplot)
#smaller dataset easier to work with
days_meanvar<-dfsfr94[,c(117,121)]
#removing missing enrolment day values
days_meanvar<-subset(days_meanvar, !is.na(days_since_enrolment))
#remove days >10, easier to work with
days_meanvar<-subset(days_meanvar, (days_since_enrolment <11))
days_meanvar<-subset(days_meanvar, (days_since_enrolment >=0))
#change to character and set correct order
days_meanvar$days_since_enrolment<-as.character(days_meanvar$days_since_enrolment)
days_meanvar$days_since_enrolment<-factor(days_meanvar$days_since_enrolment,
                                          levels=c("0","1","2","3","4","5","6","7","8","9","10"))
#function to calculate summary stats
min.mean.sd.max <- function(x) {
  r <- c(min(x), mean(x) - sd(x), mean(x), mean(x) + sd(x), max(x))
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}
p <- ggplot(days_meanvar, aes(x=days_since_enrolment, y=sfr_value, fill=days_since_enrolment)) +
  stat_summary(fun.data = min.mean.sd.max, geom = "boxplot") +
  theme(legend.position="none")+
  xlab("Day")+
  ylab("S/F94 value")+
  ggtitle("S/F94 value for each day- mean±SD (data=S/F94")
p

#WHO summary stats boxplot
colnames(dfsfr94_day1)
#select SF value and severity scale 
WHO_boxplot<-dfsfr94_day1[,c(121,122)]
#remove entries with missing score
WHO_boxplot<-subset(WHO_boxplot, !is.na(severity_scale_ordinal))
WHO_boxplot$severity_scale_ordinal<-as.character(WHO_boxplot$severity_scale_ordinal)
#add "WHO" before number for more sensible X-axis titles
WHO_boxplot$severity_scale_ordinal<-paste("WHO", WHO_boxplot$severity_scale_ordinal, sep=" ")
table(WHO_boxplot$severity_scale_ordinal)
WHO_boxplot$severity_scale_ordinal<-factor(WHO_boxplot$severity_scale_ordinal,
                                           levels=c("WHO 4","WHO 5","WHO 6","WHO 7",
                                                    "WHO 8"," WHO 9"," WHO 10"))

W1 <- ggplot(WHO_boxplot, aes(x=severity_scale_ordinal, y=sfr_value, fill=severity_scale_ordinal)) +
  stat_summary(fun.data = min.mean.sd.max, geom = "boxplot") +
  theme(legend.position="none")+
  xlab("WHO level")+
  ylab("S/F94 value")+
  ggtitle("Boxplot for each WHO level")
W1

#RR values and difference between SF/SF94 values
library(dplyr)

#get rr from toplinefile (no daily rr measurements)
resp_data<-toplinefile[,c(1,54)]
#subset sfr data for day 1 (matched to RR)
sfr_day1<-subset(dfsfr,days_since_admission ==1)
sfr94_day1<-subset(dfsfr94,days_since_admission ==1)
#link data
sfr_day1<-left_join(sfr_day1, resp_data, by="subjid")
sfr94_day1<-left_join(sfr94_day1, resp_data, by="subjid")
#remove RR<4 or >60
sfr_day1$rr_vsorres[sfr_day1$rr_vsorres<4]<-NA
sfr_day1$rr_vsorres[sfr_day1$rr_vsorres>60]<-NA
sfr94_day1$rr_vsorres[sfr94_day1$rr_vsorres<4]<-NA
sfr94_day1$rr_vsorres[sfr94_day1$rr_vsorres>60]<-NA
plot(x=sfr_day1$sfr_value, y=sfr_day1$rr_vsorres)
rpng.off()
summary(sfr94_day1$sfr_value)
ggplot(sfr_day1, aes(x=sfr_value, y=rr_vsorres)) +
  geom_point()+
  geom_smooth(method=lm)+
  xlab("SF value")+
  ylab("Respiratory Rate")

#severity scale analysis
#time to n levels increase/decrease
library(plyr)

#subset dataframe
severity_difference<-dfsfr[,c(1,115,118)]
#only select subjects with >1 measurement, remove rows with missing days and seveiry score
severity_difference<-severity_difference %>%
  group_by(subjid)%>%
  filter(n()>=2)
severity_difference<-subset(severity_difference, !is.na(days_since_admission))
severity_difference<-subset(severity_difference, !is.na(severity_scale_ordinal))
severity_difference<-severity_difference %>%
  left_join(severity_difference, "subjid") %>%
  filter(abs(severity_scale_ordinal.x - severity_scale_ordinal.y) >= 2) %>%
  mutate(Days = abs(days_since_admission.x - days_since_admission.y)) %>%
  group_by(subjid) %>%
  slice_min(days_since_admission.y) %>%
  ungroup %>%
  right_join(distinct(severity_difference["subjid"]), "subjid")%>%
  select(subjid, Days, severity_scale_ordinal.x,severity_scale_ordinal.y)

severity_difference$score_difference<-abs(severity_difference$severity_scale_ordinal.y-
                                            severity_difference$severity_scale_ordinal.x)
severity_difference<-data.frame(severity_difference)
severity_difference<-severity_difference[,c(1,2,5)]
head(severity_difference)

#difference for each measurement
severity_difference<-severity_difference %>%
  group_by(subjid) %>%
  mutate(diff_prev = severity_scale_ordinal - lag(severity_scale_ordinal))

#plot trajectories
#select subjects with at least 5 measurements
dfsfr_random100<-dfsfr
dfsfr_random100<-dfsfr_random100 %>%
  group_by(subjid)%>%
  filter(n()>=5)
#random sample of 100 trajectories
dfsfr_random100<-subset(dfsfr_random100, subjid %in% sample(unique(dfsfr_random100$subjid),100))
#transparent graphs
SF_random100<- ggplot(data=dfsfr_random100, aes(x=days_since_admission, y=sfr_value))
sf_r100<-SF_random100 + 
  geom_line(aes(group=subjid), alpha=0.1)+
  xlab("Measurement day")+
  ylab("SFR value")+
  ggtitle("100 random trajectories from the S/F group")+
  xlim(c(0,25))
sf_r100

#plot trajectories based on worst severity score value
#make variable for highest severity score for each subject
dfsfr_score_levels<-dfsfr94
dfsfr_score_levels<- dfsfr_score_levels %>%
  group_by(subjid)%>%
  slice(which.max(severity_scale_ordinal))
dfsfr_score_levels<-dfsfr_score_levels %>%
  dplyr::rename(worst_score= severity_scale_ordinal)
dfsfr_score_levels<-data.frame(dfsfr_score_levels)
dfsfr_score_levels<-dfsfr_score_levels[,c(1,118)]
dfsfr_score_levels2<-dfsfr94
dfsfr_score_levels<-left_join(dfsfr_score_levels2,dfsfr_score_levels, by="subjid" )
dfsfr_score_levels<- dfsfr_score_levels %>%
  group_by(subjid)%>%
  filter(n()>=5)
dfsfr_score_levels<-subset(dfsfr_score_levels, !is.na(days_since_admission))

#plot trajectory for each severity score subgroup
levels_plot<-ggplot(data=dfsfr_score_levels,
                    aes(x=days_since_admission, y=sfr_value)) + 
  geom_line(aes(group=subjid), alpha=0.1)+
  xlab("Measurement day")+
  ylab("S/F value")+
  xlim(c(0,60))
L10<-levels_plot %+% subset(dfsfr_score_levels, worst_score %in% c(10)) %+% ggtitle("Score 10")
L9<-levels_plot %+% subset(dfsfr_score_levels, worst_score %in% c(9)) %+% ggtitle("Score 9")
L8<-levels_plot %+% subset(dfsfr_score_levels, worst_score %in% c(8)) %+% ggtitle("Score 8 ")
L7<-levels_plot %+% subset(dfsfr_score_levels, worst_score %in% c(7)) %+% ggtitle("Score 7")
L6<-levels_plot %+% subset(dfsfr_score_levels, worst_score %in% c(6)) %+% ggtitle("Score 6")
L5<-levels_plot %+% subset(dfsfr_score_levels, worst_score %in% c(5)) %+% ggtitle("Score 5")
L4<-levels_plot %+% subset(dfsfr_score_levels, worst_score %in% c(4))%+% ggtitle("Score 4")
#cowplot
plot_grid(L10,L9,L8,L7,L6,L5,L4)

#plot trajectory for each severity score subgroup
levels_plot<-ggplot(data=dfsfr_score_levels,
                    aes(x=days_since_admission, y=sfr_value)) + 
  geom_line(aes(group=subjid), alpha=0.1)+
  xlab("Measurement day")+
  ylab("S/F94 value")+
  xlim(c(0,60))
L10<-levels_plot %+% subset(dfsfr_score_levels, worst_score %in% c(10)) %+% ggtitle("Score 10")
L9<-levels_plot %+% subset(dfsfr_score_levels, worst_score %in% c(9)) %+% ggtitle("Score 9")
L8<-levels_plot %+% subset(dfsfr_score_levels, worst_score %in% c(8)) %+% ggtitle("Score 8 ")
L7<-levels_plot %+% subset(dfsfr_score_levels, worst_score %in% c(7)) %+% ggtitle("Score 7")
L6<-levels_plot %+% subset(dfsfr_score_levels, worst_score %in% c(6)) %+% ggtitle("Score 6")
L5<-levels_plot %+% subset(dfsfr_score_levels, worst_score %in% c(5)) %+% ggtitle("Score 5")
L4<-levels_plot %+% subset(dfsfr_score_levels, worst_score %in% c(4))%+% ggtitle("Score 4")
#cowplot
plot_grid(L10,L9,L8,L7,L6,L5,L4)

#plot trajectories excluding severe frailty & comorbidity & no oxygen at admission
head(dfsfr_score_levels)
colnames(dfsfr_score_levels)
frailty_variable<-ccp_data[,c(1,128)]
dfsfr_score_levels_subset1<-data.frame(dfsfr_score_levels)
dfsfr_score_levels_subset1<-left
dfsfr_score_levels_subset1<- dfsfr_score_levels_subset1 %>%
  group_by(subjid)%>%
  filter(frail)

#### EXTREME VALUES MEAN AND SD CALCULATION
#Mean
########## CHANGE ##### CHANGE ##################################3
sd(subset3_EXTREME$sfr_value[subset3_EXTREME$days_since_enrolment == 0], na.rm = T)
sd(subset3_EXTREME$sfr_value[subset3_EXTREME$days_since_enrolment == 1], na.rm = T)
sd(subset3_EXTREME$sfr_value[subset3_EXTREME$days_since_enrolment == 2], na.rm = T)
sd(subset3_EXTREME$sfr_value[subset3_EXTREME$days_since_enrolment == 3], na.rm = T)
sd(subset3_EXTREME$sfr_value[subset3_EXTREME$days_since_enrolment == 4], na.rm = T)
sd(subset3_EXTREME$sfr_value[subset3_EXTREME$days_since_enrolment == 5], na.rm = T)
sd(subset3_EXTREME$sfr_value[subset3_EXTREME$days_since_enrolment == 6], na.rm = T)
sd(subset3_EXTREME$sfr_value[subset3_EXTREME$days_since_enrolment == 7], na.rm = T)
sd(subset3_EXTREME$sfr_value[subset3_EXTREME$days_since_enrolment == 8], na.rm = T)
sd(subset3_EXTREME$sfr_value[subset3_EXTREME$days_since_enrolment == 9], na.rm = T)
sd(subset3_EXTREME$sfr_value[subset3_EXTREME$days_since_enrolment == 10], na.rm = T)
#SD
sd(subset1$sfr_value[subset1$days_since_enrolment == 0], na.rm = T)
sd(subset1$sfr_value[subset1$days_since_enrolment == 1], na.rm = T)
sd(subset1$sfr_value[subset1$days_since_enrolment == 2], na.rm = T)
sd(subset1$sfr_value[subset1$days_since_enrolment == 3], na.rm = T)
sd(subset1$sfr_value[subset1$days_since_enrolment == 4], na.rm = T)
sd(subset1$sfr_value[subset1$days_since_enrolment == 5], na.rm = T)
sd(subset1$sfr_value[subset1$days_since_enrolment == 6], na.rm = T)
sd(subset1$sfr_value[subset1$days_since_enrolment == 7], na.rm = T)
sd(subset1$sfr_value[subset1$days_since_enrolment == 8], na.rm = T)
sd(subset1$sfr_value[subset1$days_since_enrolment == 9], na.rm = T)
sd(subset1$sfr_value[subset1$days_since_enrolment == 10], na.rm = T)

#histogram WHO and SF
hist(dfsfr94$sfr_value[dfsfr94$severity_scale_ordinal == 4], breaks = 50, main="Level 4", xlab="SF", ylim = c(0,1000), xlim = c(0,5))
hist(dfsfr94$sfr_value[dfsfr94$severity_scale_ordinal == 5], breaks = 50, main="Level 5", xlab="SF", ylim = c(0,1000), xlim = c(0,5))
hist(dfsfr94$sfr_value[dfsfr94$severity_scale_ordinal == 6], breaks = 50, main="Level 6", xlab="SF", ylim = c(0,1000), xlim = c(0,5))
hist(dfsfr94$sfr_value[dfsfr94$severity_scale_ordinal == 7], breaks = 50, main="Level 7", xlab="SF", ylim = c(0,1000), xlim = c(0,5))
hist(dfsfr94$sfr_value[dfsfr94$severity_scale_ordinal == 8], breaks = 50, main="Level 8", xlab="SF", ylim = c(0,1000), xlim = c(0,5))
hist(dfsfr94$sfr_value[dfsfr94$severity_scale_ordinal == 9], breaks = 50, main="Level 9", xlab="SF", ylim = c(0,1000), xlim = c(0,5))
hist(dfsfr94$sfr_value[dfsfr94$severity_scale_ordinal == 10], breaks = 50, main="Level 10", xlab="SF", ylim = c(0,1000), xlim = c(0,5))
par(mfrow= c(3,3))
rpng.off()
###################################### EXTREME VALUES ANALYSIS ######################################

#Histograms for several days showing the spread in extreme SF94 values (sf94_dd)
#data used: subset extremes
# SF value used: SF94_dd
hist(subset_extremes$sf94_dd[subset_extremes$days_since_admission == 0], 
     breaks = 50, main="Day 0",ylim = c(0,15000), xlab="S/F94_dd", xlim = c(0,5))
hist(subset_extremes$sf94_dd[subset_extremes$days_since_admission == 1], 
     breaks = 50, main="Day 1",ylim = c(0,15000), xlab="S/F94_dd", xlim = c(0,5))
hist(subset_extremes$sf94_dd[subset_extremes$days_since_admission == 2], 
     breaks = 50, main="Day 2",ylim = c(0,15000), xlab="S/F94_dd", xlim = c(0,5))
hist(subset_extremes$sf94_dd[subset_extremes$days_since_admission == 3], 
     breaks = 50, main="Day 3",ylim = c(0,15000), xlab="S/F94_dd", xlim = c(0,5))
hist(subset_extremes$sf94_dd[subset_extremes$days_since_admission == 5], 
     breaks = 50, main="Day 5",ylim = c(0,15000), xlab="S/F94_dd", xlim = c(0,5))
hist(subset_extremes$sf94_dd[subset_extremes$days_since_admission == 7], 
     breaks = 50, main="Day 7",ylim = c(0,15000), xlab="S/F94_dd", xlim = c(0,5))
hist(subset_extremes$sf94_dd[subset_extremes$days_since_admission == 10], 
     breaks = 50, main="Day 10", xlab="S/F94_dd", ylim = c(0,15000), xlim = c(0,5))
hist(subset_extremes$sf94_dd[subset_extremes$days_since_admission == 20], 
     breaks = 50, main="Day 20", xlab="S/F94_dd", ylim = c(0,15000), xlim = c(0,5))
hist(subset_extremes$sf94_dd[subset_extremes$days_since_admission == 40], 
     breaks = 50, main="Day 40", xlab="S/F94_dd", ylim = c(0,15000), xlim = c(0,5))
rpng.off()


sum(df_extremes$fio2==0.21,na.rm=T)
sum(df_extremes$fio2_corrected==0.21,na.rm=T)
#correlation
library(data.table)
#Correlation subsets: same subjects for different days,
# eg all subjects with a day-1 and day 5 measurement
#only relevant columns (days_since_admission and SF94)
correlation_subset_dd_1<-subset_extremes[,c(2,3,11)]
colnames(subset_extremes)
table(correlation_subset_dd_1$days_since_admission)

#remove after day 10 to make easier to work with
correlation_subset_dd_1<-subset(correlation_subset_dd_1, days_since_admission<11)
correlation_subset_dd_1<-subset(correlation_subset_dd_1, days_since_admission>=0)

correlation_subset_dd_1<-data.frame(correlation_subset_dd_1)
#add day for more sensible variable name in wide format
correlation_subset_dd_1$days_since_admission<- paste("day", correlation_subset_dd_1$days_since_admission, sep = "_")
head(correlation_subset_dd_1)
#from long to wide format
correlation_subset_dd_1<- correlation_subset_dd_1 %>%
  mutate(rn= row_number()) %>%
  spread (days_since_admission, sf94_dd_day0) %>%
  select(-rn)
#keep only 1 row for each subject
f <- function(x) {
  x <- na.omit(x)
  if (length(x) > 0) unique(x) else NA
}
correlation_subset_dd_1<-correlation_subset_dd_1 %>% 
  group_by(subjid) %>% 
  summarise_all(funs(f))
head(correlation_subset_dd_1)
#make subset with data available for 2 days (0/7, 1/7, 0/5, 1/5, 1/6)
dd_cor_07<-subset(correlation_subset_dd_1, !is.na(day_0))
dd_cor_07<-subset(dd_cor_07, !is.na(day_7))
dd_cor_05<-subset(correlation_subset_dd_1, !is.na(day_0))
dd_cor_05<-subset(dd_cor_05, !is.na(day_5))
dd_cor_15<-subset(correlation_subset_dd, !is.na(day_1))
dd_cor_15<-subset(dd_cor_15, !is.na(day_5))
dd_cor_17<-subset(correlation_subset_dd, !is.na(day_1))
dd_cor_17<-subset(dd_cor_17, !is.na(day_7))
dd_cor_16<-subset(correlation_subset_dd, !is.na(day_1))
dd_cor_16<-subset(dd_cor_16, !is.na(day_6))
# correlation DAY 0 DAY 7
x <-  dd_cor_07$day_0
y <-  dd_cor_07$day_7
cor(x,y)
nrow(dd_cor_07)
# DAY 0-5
x <-  dd_cor_05$day_0
y <-  dd_cor_05$day_5
cor(x,y)
nrow(dd_cor_05)
# DAY 1- 5
x <-  dd_cor_15$day_1
y <-  dd_cor_15$day_5
cor(x,y)
nrow(dd_cor_15)
# DAY 1-7
x <-  dd_cor_17$day_1
y <-  dd_cor_17$day_7
cor(x,y)
length(dd_cor_17$subjid)
# DAY 1-6
x <-  dd_cor_16$day_1
y <-  dd_cor_16$day_6
cor(x,y)
nrow(dd_cor_16)

