library(dplyr)

library(data.table)


df_1<-fread("/home/u034/mcswets/df_1_20210402.csv", data.table = FALSE)


# start population (df_1) = 79843
# after applying age limits = 38919
# only subjects with an sao2 on day 0,1,2 or 3 = 31245 unique subjects

subjects_to_include <- filter(df_1, ( !is.na(sao2) & days_since_admission %in% c(0,1,2,3)  & age_estimateyears >19 & age_estimateyears <76 ) )['subjid']
subset1<-df_1[df_1$subjid %in% subjects_to_include$subjid,] 
subset1 <- as.data.frame(subset1)

# variable should be either  'sf94' or 'severity_scale_ordinal'
# group should be 'base' if you want to include everyone except those who died or were discharged
# 'basedd' if you want to include those who died or were discharged
# 'day0' if you want to exclude people who died or were discharged on day_since_admission == 0
# Output is a table that you can query for summary statistics.

createDF <- function(group, variable, time){
  
  if( group == 'base' |  group == 'basedd' ) { 
    df <- filter( subset1[ c('subjid', 'days_since_admission', variable ) ] , days_since_admission <= time  )
  } else if (group == 'day0') {
    dropID <- subset1[   (subset1[ 'day_of_death' ] == 0 |  subset1['day_of_discharge'] == 0) %in% TRUE, 'subjid']
    
    df <- filter(subset1[ c('subjid', 'days_since_admission', variable ) ], days_since_admission <= time & !(subjid %in% dropID)  ) 
  } 
  
  
  derived <-  df  %>%  mutate(rn= row_number()) %>% spread (days_since_admission, variable) %>% select(-rn)
  
  newCols <- as.character( setdiff( 0:time,  setdiff( colnames(derived), 'subjid' ) ) )
  
  derived[newCols] <- NA
  
  derived <- derived[ c( 'subjid', as.character(0:time))  ]
  
  
  if(group == 'basedd' | group == 'day0' ){
    
    censorDeath <- filter( subset1,  death== TRUE  & day_of_death < time    )[c('subjid', 'day_of_death')]
    
    censorDischarge <- filter( subset1,  discharge== TRUE  & day_of_discharge < time    )[c('subjid', 'day_of_discharge')]
    
    
    if(variable == 'sf94'){
      deathValue <- 0.5
      dischargeValue <- 4.76
    } else if(variable == 'severity_scale_ordinal'){
      deathValue <- 10
      dischargeValue <- 4
    }
    
    for( i in as.numeric(rownames(censorDeath))   ){
      
      row <- which( derived['subjid']  ==  censorDeath[i, 'subjid']  )
      
      cols <- as.character( (censorDeath[i, 'day_of_death'] +1):time  )
      
      derived[ row ,  cols  ] <- deathValue
    }
    
    for( i in as.numeric(rownames(censorDischarge))    ){
      
      row <- which( derived['subjid']  ==  censorDischarge[i, 'subjid']  )
      
      cols <- as.character( (censorDischarge[i, 'day_of_discharge']+1):time  )
      
      derived[ row ,  cols  ] <- dischargeValue
     
    }
    
  }
  return(derived)
  
  
  
}


#(group,variable, time)
# variable = sf94, who
# group= base, basedd, day0
# time =days_since_admission

base_sf94_10<-createDF("base", "sf94", 10)
base_sf94_12<-createDF("base", "sf94", 12)
basedd_sf94_10<-createDF("basedd", "sf94", 10)
basedd_sf94_12<-createDF("basedd", "sf94", 12)
basedd_sf94day0_10<-createDF("day0", "sf94", 10)
base_who_5<-createDF("base", "severity_scale_ordinal", 5)
base_who_8<-createDF("base", "severity_scale_ordinal", 8)
basedd_who_8<-createDF("basedd", "severity_scale_ordinal",8)
basedd0_who_8<-createDF("day0", "severity_scale_ordinal", 8)


summary(base_sf94_10)
summary(basedd_sf94_10)
summary(basedd_sf94day0_10)
sapply(base_sf94_10, mean, na.rm=T)
sapply(basedd_sf94_10, mean, na.rm=T)
sapply(basedd_sf94day0_10, mean, na.rm=T)


#correlation
library(data.table)
#Correlation subsets: same subjects for different days
#keep only 1 row for each subject
f <- function(x) {
  x <- na.omit(x)
  if (length(x) > 0) unique(x) else NA
}
correlation_subset<-basedd_sf94day0_10
correlation_subset<-correlation_subset %>% 
  group_by(subjid) %>% 
  summarise_all(funs(f))
#make subsets of data in which all subjects have SF94 available for those 2 days
correlation_subset_05<-subset(correlation_subset, ((!is.na(correlation_subset[,2])&(!is.na(correlation_subset[,7])))))
correlation_subset_07<-subset(correlation_subset, ((!is.na(correlation_subset[,2])&(!is.na(correlation_subset[,9])))))
correlation_subset_08<-subset(correlation_subset, ((!is.na(correlation_subset[,2])&(!is.na(correlation_subset[,10])))))
length(correlation_subset_08$subjid)
head(correlation_subset_07)
# DAY 0/5
x <-  correlation_subset_05[,2]
y <-  correlation_subset_05[,7]
cor(x,y)
# Day 0/7
x <-  correlation_subset_07[,2]
y <-  correlation_subset_07[,9]
cor(x,y)
# DAY 0/8
x <-  correlation_subset_08[,2]
y <-  correlation_subset_08[,10]
cor(x,y)

#violin plots
#take day 5 from who and sf data
day5who<-base_who_5[,c(1,7)]
day5who<-day5who%>%
  group_by(subjid)%>%
  summarise_all(funs(f))
day5who<-day5who%>%
  dplyr::rename(who_day5= "5")
day5sf94<-base_sf94_10[,c(1,7)]
day5sf94<-day5sf94%>%
  group_by(subjid)%>%
  summarise_all(funs(f))
day5sf94<-day5sf94%>%
  dplyr::rename(sf94_day5= "5")
#join together
day5<-left_join(day5who, day5sf94, by="subjid")
day5<-data.frame(day5)
sum(!is.na(day5$sf94_day5))
day5<-subset(day5, (!is.na(who_day5)))
day5$who_day5<- paste("WHO level", day5$who_day5, sep = " ")
day5$who_day5<-factor(day5$who_day5,
                      levels=c("WHO level 4","WHO level 5",
                               "WHO level 6","WHO level 7",
                               "WHO level 8","WHO level 9","WHO level 10"))
sum(!is.na(day5$sf94_day5))
#violin plots (figure 4a)
library(ggplot2)
who_day5<-ggplot(day5,
                 aes(x=who_day5, y=sf94_day5, fill=who_day5 ))

who_day5+ geom_violin()+ #remove outliers
  theme_light()+
  ggtitle("WHO ordinal severity scale (N=5215)")+
  scale_fill_brewer(palette = "Spectral")+
  xlab("")+
  ylab("S/F94 day5")+
  theme(legend.position = "none",
        plot.title = element_text (hjust = 0.5)) #remove legend + center title

#make dataframe with SF94 day 0 and day 5 + outcome for violin plots (figure 4b+c)
#day 0
day0sf94<-base_sf94_10[,c(1,2)]
head(day0sf94)
day0sf94<-day0sf94%>%
  group_by(subjid)%>%
  summarise_all(funs(f))
day0sf94<-day0sf94%>%
  dplyr::rename(sf94_day0= "0")
#merge with day 5, cleaned before
day_05<-merge(day0sf94, day5sf94, by="subjid", all=T)

#outcome
outcome_df<-subset1[,c(2,60,61)]
outcome_df<-outcome_df %>% 
  mutate(
    outcome = case_when(
      death==TRUE ~ "Death",
      discharge==TRUE ~ "Discharge"))
outcome_df<-outcome_df[,c(1,4)]
#only 1 entry for each subject
outcome_df<-outcome_df%>%
  group_by(subjid)%>%
  summarise_all(funs(f))

#combine with day 0 and day 5 data
day_05_outcome<-left_join(day_05, outcome_df, by="subjid")
day_05_outcome<-subset(day_05_outcome, !is.na(outcome))
head(day_05_outcome)

#violin plots
#distribution of SF94 values on day 0
outcome_day0<-ggplot(day_05_outcome,
                 aes(x=outcome, y=sf94_day0, fill=outcome ))
sum(!is.na(day_05_outcome$sf94_day0))
outcome_day0 + geom_violin()+ #remove outliers
  theme_light()+
  ggtitle("N=21223")+
  scale_fill_brewer(palette = "Spectral")+
  xlab("")+
  ylab("S/F94 day0")+
  theme(legend.position = "none",
        plot.title = element_text (hjust = 0.5)) #remove legend + center title
#distribution of SF94 values on day 5
outcome_day5<-ggplot(day_05_outcome,
                     aes(x=outcome, y=sf94_day5, fill=outcome ))
sum(!is.na(day_05_outcome$sf94_day5))
outcome_day5 + geom_violin()+ #remove outliers
  theme_light()+
  ggtitle("N=5243")+
  scale_fill_brewer(palette = "Spectral")+
  xlab("")+
  ylab("S/F94 day5")+
  theme(legend.position = "none",
        plot.title = element_text (hjust = 0.5))

#WHO summary stats for day 5 and day 8
day5_wide<-reshape(day5, idvar="subjid", timevar="who_day5", direction="wide")
#reorder columns
day5_wide<-day5_wide[,c(1,8,4,3,6,2,5,7)]
summary(day5_wide)
sapply(day5_wide, sd, na.rm=T)

#barchar mean/ variance on different days
library(ggplot2)
library(cowplot)
#transform to long format 
long_dfsf94_12<-gather(base_sf94_12, days_since_admission, sf94, 2:14, factor_key=T)
#change to character and set correct order
long_dfsf94_12$days_since_admission<-as.character(long_dfsf94_12$days_since_admission)
long_dfsf94_12$days_since_admission<-factor(long_dfsf94_12$days_since_admission,
                                          levels=c("0","1","2","3","4","5",
                                                   "6","7","8","9","10", "11", "12"))
head(long_dfsf94_12)
#function to calculate summary stats
min.mean.sd.max <- function(x) {
  r <- c(min(x), mean(x) - sd(x), mean(x), mean(x) + sd(x), max(x))
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}
p <- ggplot(long_dfsf94_12, aes(x=days_since_admission, y=sf94, fill=days_since_admission)) +
  stat_summary(fun.data = min.mean.sd.max, geom = "boxplot") +
  theme(legend.position="none")+
  scale_fill_brewer(palette = "Spectral")+
  xlab("Day")+
  ylab("S/F94")+
  ggtitle("")
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


