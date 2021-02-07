library(dplyr)

library(data.table)


df_1<-fread("/home/u034/mcswets/df_1_20210402.csv", data.table = FALSE)

length(unique(subset1$subjid))
# start population (df_1) = 79843
# after applying age limits = 38919
# only subjects with supplemental oxygen on day 0,1 or 2 = 14049 unique subjects

subjects_to_include <- filter(df_1, ( fio2 >=0.22 & days_since_start %in% c(0,1,2)  & age_estimateyears >19 & age_estimateyears <76 ) )['subjid']
subset1<-df_1[df_1$subjid %in% subjects_to_include$subjid,] 
subset1 <- as.data.frame(subset1)
head(subset1)
# variable should be either  'sf94' or 'severity_scale_ordinal'
# group should be 'base' if you want to include everyone except those who died or were discharged
# 'basedd' if you want to include those who died or were discharged
# 'day0' if you want to exclude people who died or were discharged on day_since_admission == 0
# Output is a table that you can query for summary statistics.

createDF <- function(group, variable, time){
  
  if( group == 'base' |  group == 'basedd' ) { 
    df <- filter( subset1[ c('subjid', 'days_since_start', variable ) ] , days_since_start <= time  )
  } else if (group == 'day0') {
    dropID <- subset1[   (subset1[ 'day_of_death' ] == 0 |  subset1['day_of_discharge'] == 0) %in% TRUE, 'subjid']
    
    df <- filter(subset1[ c('subjid', 'days_since_start', variable ) ], days_since_start <= time & !(subjid %in% dropID)  ) 
  } 
  
  
  derived <-  df  %>%  mutate(rn= row_number()) %>% spread (days_since_start, variable) %>% select(-rn)
  
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
# time =days_since_start

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

#WHO summary stats for day 5 and day 8
day5_wide<-reshape(day5, idvar="subjid", timevar="who_day5", direction="wide")
#reorder columns
day5_wide<-day5_wide[,c(1,8,4,3,6,2,5,7)]
length(day5_wide$subjid)
summary(day5_wide)
sapply(day5_wide, sd, na.rm=T)

#day 8 WHO and SF dataframe
#WHO day 8
day8_who<-base_who_8[,c(1,10)]
day8_who<-day8_who%>%
  group_by(subjid)%>%
  summarise_all(funs(f))
day8_who<-day8_who%>%
  dplyr::rename(who_day8= "8")
#SF94 day 8
day8sf94<-base_sf94_10[,c(1,10)]
day8sf94<-day8sf94%>%
  group_by(subjid)%>%
  summarise_all(funs(f))
day8sf94<-day8sf94%>%
  dplyr::rename(sf94_day8= "8")
#join together
day8<-left_join(day8_who, day8sf94, by="subjid")
day8<-data.frame(day8)
day8<-subset(day8, (!is.na(who_day8)))
#make wide
day8_wide<-reshape(day8, idvar="subjid", timevar="who_day8", direction="wide")
#reorder columns
day8_wide<-day8_wide[,c(1,2,3,4,5,8,7,6)]
length(day8_wide$subjid)
summary(day8_wide)
sapply(day8_wide, sd, na.rm=T)




