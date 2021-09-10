library(plyr)
library(dplyr)
library(ggplot2)
library(data.table)
library(tidyr)
library(rms)
library(DescTools)

df_1<-readRDS("/home/skerr/Data/ccp_subset_derived_2021-05-26_1941.rds")

#df_1<-fread("/home/u034/mcswets/df_20211402.csv", data.table = FALSE)
#df_1<-df_1[,c(2:89)]

#OUTPUT
#number of subjects before and after filters

numberSubs <- data.frame( before_filter =  length(unique(df_1$subjid)) ,
                          after_filter = length(unique(subset1$subjid))   ) #26420
write.csv(numberSubs,"/home/skerr/Git/SF94/Outputs/numberSubs.csv")


########################## FUNCTIONS ############################

# variable should be either  'sf94' or 'severity_scale_ordinal'
# group should be 'base' if you want to include everyone except those who died or were discharged
# 'basedd' if you want to include those who died or were discharged
# 'day0' if you want to exclude people who died or were discharged on day_since_admission == 0
# Output is a table that you can query for summary statistics.

createDF <- function(df, group, variable, time){
  
  if( group == 'base' |  group == 'basedd' ) { 
    df2 <- filter( df[ c('subjid', 'days_since_start', variable ) ] , days_since_start <= time  )
  } else if (group == 'day0') {
    dropID <- df[   (df[ 'day_of_death' ] == 0 |  subset1['day_of_discharge'] == 0) %in% TRUE, 'subjid']
    
    df2 <- filter(df[ c('subjid', 'days_since_start', variable ) ], days_since_start <= time & !(subjid %in% dropID)  ) 
  } 
  
  derived <-  df2  %>%  mutate(rn= row_number()) %>% spread (days_since_start, variable) %>% dplyr::select(-rn)
  
  newCols <- as.character( setdiff( 0:time,  setdiff( colnames(derived), 'subjid' ) ) )
  
  derived[newCols] <- NA
  
  derived <- derived[ c( 'subjid', as.character(0:time))  ]
  
  if(group == 'basedd' | group == 'day0' ){
    censorDeath <- filter( df,  death== "YES"  & day_of_death < time    )[c('subjid', 'day_of_death')]
    
    censorDischarge <- filter( df,  discharge== "YES"  & day_of_discharge < time    )[c('subjid', 'day_of_discharge')]
    
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


#Correlation subsets: same subjects for different days
#keep only 1 row for each subject
f <- function(x) {
  x <- na.omit(x)
  if (length(x) > 0) unique(x) else NA
}

####################################################################################

#if error message try detaching plyr:
#detach("package:plyr", unload=T)

#summary 
basedd_sf94_10<-createDF(subset1, "basedd", "sf94", 10)

#mortality at 28 days
mort<-subset1 %>%
  group_by(subjid)%>%
  count(mortality_28)

mort28 <- sum(mort$mortality_28 == 1, na.rm = T)/ sum(!is.na(mort$mortality_28))

#OUTPUT
#percentage of subjects who died in the first 28 days

write.csv(mort28,"/home/skerr/Git/SF94/Outputs/mort28.csv")




#difference in SF94 between different WHO scale steps
medians <- subset1 %>%
  group_by(severity_scale_ordinal)%>%
  summarise_at("sf94", median, na.rm=T)

#OUTPUT
# 7 median values
write.csv(medians,"/home/skerr/Git/SF94/Outputs/medians.csv")

#additional numbers:

notMissingV <- c(  sapply(basedd_sf94_10[c('5', '8')], function(x) sum(!is.na(x))),       
                   sapply(day8_who[c('who_day5', 'who_day8')], function(x) sum(!is.na(x))),
                   sum(!is.na( subset1Dist$who_days_to_improve1 )), 
                   sum(!is.na( subset1Dist$who_days_to_improve2 )),
                   sum(!is.na(mort$mortality_28))
)

notMissing <- data.frame(  variable =   c('sf94_day5', 'sf94_day8', 'who_day5', 'who_day8', 'severity_dif_1level',
                                          'severity_dif_2level', 'mortality_28'),
                           value = notMissingV)

write.csv(notMissing,"/home/skerr/Git/SF94/Outputs/notMissing.csv")


missingV <- c(  sapply(basedd_sf94_10[c('5', '8')], function(x) sum(is.na(x))),       
                sapply(day8_who[c('who_day5', 'who_day8')], function(x) sum(is.na(x))),
                sum(is.na( subset1Dist$who_days_to_improve1 )), 
                sum(is.na( subset1Dist$who_days_to_improve2 )),
                sum(is.na(mort$mortality_28))
)

missing <- data.frame(  variable =   c('sf94_day5', 'sf94_day8', 'who_day5', 'who_day8', 'who_days_to_improve1',
                                       'who_days_to_improve2', 'mortality_28'),
                        value = missingV)

write.csv(missing,"/home/skerr/Git/SF94/Outputs/missing.csv")




write.csv(table(mort$mortality_28),"/home/skerr/Git/SF94/Outputs/mort28Table.csv")


#make small dataframe for 28-day mortality and outcome
mortality<-df_1[,c("subjid","mortality_28")]
mortality<-mortality%>%
  group_by(subjid)%>%
  slice(which.min(mortality_28))


#=============================================================================================#

#input data = df_1, filters are applied at a later point in time
df_1_base_sf94<-createDF(df_1, "base", "sf94", 16)

#regression model D5 SF94 and mortality
#with sf94 values regression
day05<-df_1_base_sf94[,c("subjid","0","5","8","10","11","12","13","14","15","16")] #select subjid, day 0 and day 5 and day 8
day05<-day05%>%
  dplyr::rename(sf94_day0= "0",sf94_day5= "5", sf94_day8= "8",
                sf94_day10= "10",sf94_day11= "11", sf94_day12= "12",
                sf94_day13= "13",sf94_day14= "14", sf94_day15= "15",sf94_day16= "16" )
day05<-setDT(day05)[, lapply(.SD, na.omit), by=subjid] #keep 1 entry/subject

#calculate what number of subjects needs to be added to get correct proportion
dead_alive_to_add<-function(day_of_interest){
  dead<-(length(unique(df_1$subjid[(df_1$day_of_death < day_of_interest)])))/length(unique(df_1$subjid))
  alive<-(length(unique(df_1$subjid[(df_1$day_of_discharge < day_of_interest)])))/length(unique(df_1$subjid))
  in_study<-sum((!is.na(df_1$sf94) & df_1$days_since_start == day_of_interest), na.rm=T)
  percentage_in_study<- 1-dead-alive
  dead_to_add<- (dead*in_study)/percentage_in_study
  alive_to_add<-(alive*in_study)/percentage_in_study
  summary_dead_alive<-c("% dead" = dead,"% alive"= alive,"% in study" =percentage_in_study,
                        "dead to add" =dead_to_add,"alive to add" =alive_to_add)
  return(summary_dead_alive)
}
sum_d5<-dead_alive_to_add(5)
sum_d8<-dead_alive_to_add(8)
sum_d10<-dead_alive_to_add(10)
sum_d11<-dead_alive_to_add(11)
sum_d12<-dead_alive_to_add(12)
sum_d13<-dead_alive_to_add(13)
sum_d14<-dead_alive_to_add(14)
sum_d15<-dead_alive_to_add(15)
sum_d16<-dead_alive_to_add(16)

proportional_numbers<-cbind(sum_d5[c(1,2)], sum_d8[c(1,2)],sum_d10[c(1,2)],sum_d11[c(1,2)],sum_d12[c(1,2)],sum_d13[c(1,2)], 
                            sum_d14[c(1,2)],sum_d15[c(1,2)],sum_d16[c(1,2)])
write.csv(proportional_numbers,"/home/skerr/Git/SF94/Outputs/proportional_numbers.csv")

day05_P<-day05 #for proportional deaths
mortality<-df_1[,c("subjid","mortality_28", "day_of_death", "day_of_discharge")]
mortality<-mortality%>%
  group_by(subjid)%>%
  slice(which.min(mortality_28))
day05_P<-left_join(day05_P, mortality, by="subjid") #add mortality
head(day05_P)
prop_added<- function(sf_day_to_replace, dead_to_add_fun, alive_to_add_fun, day_of_interest){
  set.seed(1234)
  rows_to_replace<-which(is.na(day05_P[[sf_day_to_replace]]) &day05_P$day_of_death < day_of_interest ) #find rows with missing values and who died
  day05_P[[sf_day_to_replace]][sample(rows_to_replace, dead_to_add_fun)]<- 0.5 #and add correct number of dead patients
  rows_to_replace<-which(is.na(day05_P[[sf_day_to_replace]]) & day05_P$day_of_discharge < day_of_interest) #same for patients who lived
  day05_P[[sf_day_to_replace]][sample(rows_to_replace, alive_to_add_fun)]<- 4.76
  day05_P<-data.frame(day05_P)
  day05_P<-day05_P[c("subjid", sf_day_to_replace)]
  return(day05_P)
}

sf94_day5_P<-prop_added("sf94_day5", sum_d5[("dead to add")], sum_d5[("alive to add")], 5)
sf94_day8_P<-prop_added("sf94_day8", sum_d8[("dead to add")], sum_d8[("alive to add")], 8)

#merge dataframes together
#library(plyr)
sf94_D5_D8<-join_all(list(sf94_day5_P, sf94_day8_P), by="subjid", type="full") #if we want to add additional days to the analysis
sf94_D5_D8<-sf94_D5_D8%>%
  dplyr::rename(sf94_day5_P= sf94_day5, sf94_day8_P=sf94_day8) #change names to differentiate from un-edited values
#change this variable to include another day ^
#detach("package:plyr", unload=T)
#Missing data
library(naniar)
day5_P<-sf94_D5_D8[,c("subjid","sf94_day5_P", "sf94_day8_P")]
prop_original<-left_join(day5_P, day05, by="subjid") #merge with daily values
missing_add_prop<-miss_var_summary(prop_original)
miss_var_summary(subset1)
write.csv(missing_add_prop,"/home/skerr/Git/SF94/Outputs/missing_add_prop.csv")
day_of_outcome<-df_1[,c("subjid", "day_of_death", "day_of_discharge")] #add these to calculate deaths/discharges in cohort D5+D8
day_of_outcome<-day_of_outcome%>% #1 value for each subject
  group_by(subjid) %>% 
  summarise_all(funs(f))
day_of_outcome<-data.frame(day_of_outcome)
prop_original<-left_join(prop_original, day_of_outcome, by="subjid") #merge 2 dataframes
prop_original<-prop_original%>% #1 value for each subject
  group_by(subjid) %>% 
  summarise_all(funs(f))
prop_original<-data.frame(prop_original)


#missing values complete dataset
#df_1_basedd_who<-createDF(df_1, "basedd", "severity_scale_ordinal", 16)
#dd_dataset<-df_1_basedd_who
#dd_dataset<-dd_dataset%>%
#  group_by(subjid) %>% 
#  summarise_all(funs(f))
#dd_dataset<-data.frame(dd_dataset)
#miss_dd<-miss_var_summary(dd_dataset)
#write.csv(miss_dd,"/home/skerr/Git/SF94/Outputs/miss_dd.csv")
#cohort_size_total<-nrow(dd_dataset)
#cohort D5
day5_prop<-subset(prop_original, !is.na(sf94_day5_P)) #starting set: D5 is known
dead_alive_function_D5<-function(day_of_interest){
  dead<-(length(unique(day5_prop$subjid[(day5_prop$day_of_death < day_of_interest)])))/length(unique(day5_prop$subjid))
  alive<-(length(unique(day5_prop$subjid[(day5_prop$day_of_discharge < day_of_interest)])))/length(unique(day5_prop$subjid))
  summary_dead_alive_cohort<-c("% dead d5" = dead,"% alive d5"= alive)
  return(summary_dead_alive_cohort)
}
d5_cohort_sumD5<-dead_alive_function_D5(5)
d5_cohort_sumD8<-dead_alive_function_D5(8)
d5_cohort_sumD10<-dead_alive_function_D5(10)
d5_cohort_sumD11<-dead_alive_function_D5(11)
d5_cohort_sumD12<-dead_alive_function_D5(12)
d5_cohort_sumD13<-dead_alive_function_D5(13)
d5_cohort_sumD14<-dead_alive_function_D5(14)
d5_cohort_sumD15<-dead_alive_function_D5(15)
d5_cohort_sumD16<-dead_alive_function_D5(16)
cohort_size_d5<-nrow(day5_prop)

miss_day5<-miss_var_summary(day5_prop)
write.csv(miss_day5,"/home/skerr/Git/SF94/Outputs/miss_day5.csv")

dead_alive_d5<-cbind(d5_cohort_sumD5, d5_cohort_sumD8, d5_cohort_sumD10, d5_cohort_sumD11, d5_cohort_sumD12, 
                     d5_cohort_sumD13, d5_cohort_sumD14, d5_cohort_sumD15, d5_cohort_sumD16)
write.csv(dead_alive_d5,"/home/skerr/Git/SF94/Outputs/dead_alive_d5.csv")

#cohort D8
day8_prop<-subset(prop_original, !is.na(sf94_day8_P)) #starting set: D8 is known
dead_alive_function_D8<-function(day_of_interest){
  dead<-(length(unique(day8_prop$subjid[(day8_prop$day_of_death < day_of_interest)])))/length(unique(day8_prop$subjid))
  alive<-(length(unique(day8_prop$subjid[(day8_prop$day_of_discharge < day_of_interest)])))/length(unique(day8_prop$subjid))
  summary_dead_alive_cohort<-c("% dead d8" = dead,"% alive d8"= alive)
  return(summary_dead_alive_cohort)
}
d8_cohort_sumD5<-dead_alive_function_D8(5)
d8_cohort_sumD8<-dead_alive_function_D8(8)
d8_cohort_sumD10<-dead_alive_function_D8(10)
d8_cohort_sumD11<-dead_alive_function_D8(11)
d8_cohort_sumD12<-dead_alive_function_D8(12)
d8_cohort_sumD13<-dead_alive_function_D8(13)
d8_cohort_sumD14<-dead_alive_function_D8(14)
d8_cohort_sumD15<-dead_alive_function_D8(15)
d8_cohort_sumD16<-dead_alive_function_D8(16)
cohort_size_d8<-nrow(day8_prop)

miss_day8<-miss_var_summary(day8_prop)
write.csv(miss_day8,"/home/skerr/Git/SF94/Outputs/miss_day8.csv")

#cohort_sizes<-cbind(cohort_size_total, cohort_size_d5, cohort_size_d8)
#write.csv(cohort_sizes,"/home/skerr/Git/SF94/Outputs/cohort_sizes.csv")

dead_alive_d8<-cbind(d8_cohort_sumD5, d8_cohort_sumD8, d8_cohort_sumD10, d8_cohort_sumD11, d8_cohort_sumD12, 
                     d8_cohort_sumD13, d8_cohort_sumD14, d8_cohort_sumD15, d8_cohort_sumD16)
write.csv(dead_alive_d8,"/home/skerr/Git/SF94/Outputs/dead_alive_d8.csv")



#add proportional D5 and D8 to D0
sf94_D0<-day05[,c("subjid", "sf94_day0")] #only take necessary columns
#library(plyr)
sf94_D5_D8<-merge(sf94_D5_D8, sf94_D0, by="subjid") #combine 2 dataframes by subjid
#detach("package:plyr", unload=T)
#calculate delta variables
sf94_D5_D8$delta_SF94_05<-sf94_D5_D8$sf94_day5_P - sf94_D5_D8$sf94_day0 #calculate difference from day 0 to day 5
sf94_D5_D8$delta_SF94_08<-sf94_D5_D8$sf94_day8_P - sf94_D5_D8$sf94_day0 #same for D0-D8


#create small df with the independent predictor variables (except sf94) and outcome variable
predictor_variables<-df_1[,c("subjid", "sex", "age_estimateyears", "mortality_28")]
predictor_variables<-predictor_variables %>% group_by(subjid)%>%slice(which.max(mortality_28))

#join both together
regresson_df_P<-left_join(sf94_D5_D8,predictor_variables, by="subjid")
#add proportional WHO D5 and D8 to regresson DF
df_1_base_who_8<-createDF(df_1, "base", "severity_scale_ordinal",8)
who_5_8<-df_1_base_who_8[,c(1,2,7,10)] #select subjid, day 0 and day 5 and day 8
who_5_8<-who_5_8%>%
  dplyr::rename(WHOD5= "5", WHOD0= "0", WHOD8= "8") #change to easier names
who_5_8<-setDT(who_5_8)[, lapply(.SD, na.omit), by=subjid] #keep 1 entry/subject
regresson_df_P<-left_join(regresson_df_P, who_5_8, by="subjid")
regresson_df_P$WHOD5_P<-regresson_df_P$WHOD5
regresson_df_P$WHOD8_P<-regresson_df_P$WHOD8
#match the proportionally added SF94 values to the WHO scores
#this is to avoid that someone without a known value gets randomly set to dead on day 5 for SF94 (0.5)
#but then randomly gets a value 4 (no oxygen sup) on day 5 or 8 for WHO
regresson_df_P<-regresson_df_P %>% #change mortality to match proportionally added values
  mutate(
    WHOD5_P = case_when(
      sf94_day5_P == 4.760  ~ 4, sf94_day5_P == 0.5 ~ 10,TRUE ~ as.numeric(WHOD5_P)))
regresson_df_P<-regresson_df_P %>% #change mortality to match proportionally added values
  mutate(
    WHOD8_P = case_when(
      sf94_day8_P == 4.760  ~ 4, sf94_day8_P == 0.5 ~ 10,TRUE ~ as.numeric(WHOD8_P)))

#add days to improvement variable
time_to_improvement<-df_1[,c("subjid", "who_days_to_improve2", "who_days_to_improve1", "severity_scale_ordinal")]
time_to_improvement<- time_to_improvement %>% group_by(subjid)%>%add_count(number=!is.na(severity_scale_ordinal))
time_to_improvement<-time_to_improvement %>% #keep 1 value per subject
  group_by(subjid)%>%
  slice(which.min(severity_scale_ordinal))
time_to_improvement<-time_to_improvement %>%
  mutate( #if zero or 1 observation, change to NA. if there is a number, there is sustained improvement, so change to 1. 
    sustained_1L_improvement= case_when( #if >1 who value but no improvement > 0
      n<2 ~ NA_real_, !is.na(who_days_to_improve1) ~ 1, is.na(who_days_to_improve1) ~ 0 ))
time_to_improvement<-time_to_improvement %>%
  mutate( #if there is a number, there is sustained improvement, so change to 1. If not, change to 0
    sustained_2L_improvement= case_when(n<2 ~ NA_real_,
                                        !is.na(who_days_to_improve2) ~ 1, is.na(who_days_to_improve2) ~ 0 ))
time_to_improvement<-time_to_improvement[,c("subjid", "sustained_1L_improvement", "sustained_2L_improvement")]
time_to_improvement<-data.frame(time_to_improvement)
table(time_to_improvement$sustained_1L_improvement)

#bind to rest of data
regresson_df_P<-left_join(regresson_df_P, time_to_improvement, by="subjid")

#write.csv(regresson_df_P,"regresson_df_P.csv")
#regresson_df_P<-read.csv("/home/u034/mcswets/regresson_df_P.csv")
#attach(regresson_df_P)
#ddist <- datadist(sf94_day5_P, sf94_day8_P, sf94_day0, sex, age_estimateyears, mortality_28, WHOD5_P, WHOD8_P,
#                  sustained_1L_improvement, sustained_2L_improvement)
#options(datadist='ddist')
#detach(regresson_df_P)
regresson_df_P$sustained_1L_improvement <- sapply(regresson_df_P$sustained_1L_improvement, as.factor)
regresson_df_P$sustained_2L_improvement <- sapply(regresson_df_P$sustained_2L_improvement, as.factor)


df_1 <- mutate_at(df_1, 'sex',  as.character)

#apply age filter and supp oxygen filter
subjects_to_include <- filter(df_1, ( fio2 >=0.22 & days_since_start %in% c(0,1,2)  & age_estimateyears >19 & age_estimateyears <76 ) )['subjid']
subset1<-regresson_df_P[regresson_df_P$subjid %in% subjects_to_include$subjid,] 
subset1 <- as.data.frame(subset1)

#apply age filter and need for oxygen at day 0
subjects_to_include <- filter(df_1, ( fio2 >=0.22 & days_since_start %in% c(0)  & age_estimateyears >19 & age_estimateyears <76 ) )['subjid']
subset2<-regresson_df_P[regresson_df_P$subjid %in% subjects_to_include$subjid,] 
subset2<- as.data.frame(subset2)

#only age filter
subjects_to_include <- filter(df_1, (age_estimateyears >19 & age_estimateyears <76 ) )['subjid']
subset3<-regresson_df_P[regresson_df_P$subjid %in% subjects_to_include$subjid,] 
subset3 <- as.data.frame(subset3)

#Missing data
missing_df<-df_1[,c("subjid","sf94", "days_since_start")]
# only keep data up to day 14
missing_df<-subset(missing_df, days_since_start<=14)
# reshape into wide to have a variable for each day in 14 days for each subject
missing_df<-reshape(missing_df, idvar="subjid", timevar="days_since_start", direction="wide")
# back to long to make code easier
missing_df<-reshape(missing_df, direction = "long", varying= list(names(missing_df)[2:16]),
                    v.names="SF94",
                    idvar="subjid",
                    timevar="days_since_start",
                    times=0:14)
#create smalle dataset with just day of outcome
day_of_outcome<-df_1[,c("subjid","day_of_death", "day_of_discharge")]
day_of_outcome<-distinct(day_of_outcome)
#merge two dataframes
missing_df<-merge(x=missing_df, y=day_of_outcome, by="subjid", all.x=T)
#add values to sf94 variable
missing_df<-missing_df%>%mutate(
  SF94= case_when(
    !is.na(SF94) ~ SF94,
    day_of_death <= days_since_start ~ 0.5,
    day_of_discharge<= days_since_start ~ 4.76
  )
)

table_function<-function(day){
dead<-sum(missing_df$days_since_start== day & missing_df$SF94 == 0.5, na.rm=T)/ sum(missing_df$days_since_start== day)
home<-sum(missing_df$days_since_start== day & missing_df$SF94 == 4.76, na.rm=T)/ sum(missing_df$days_since_start== day)
study<-sum(missing_df$days_since_start== day & !is.na(missing_df$SF94) & missing_df$SF94!=4.76 & missing_df$SF94!=0.5,
    na.rm=T)/ sum(missing_df$days_since_start== day)
missing<-sum(missing_df$days_since_start== day & is.na(missing_df$SF94))/ sum(missing_df$days_since_start== day)
output_missing<-rbind(dead,home,study,missing)
return(output_missing)
}

days_input<-c(0:14)
table_missingdays<- lapply(days_input, table_function)
table_missingdays<-do.call(rbind.data.frame, table_missingdays)
write.csv(table_missingdays,"/home/skerr/Git/SF94/Outputs/table_missingdays.csv")

# C statistic
# use df_1, so there are no proportionally added values on any days
# compare C statistic for different days, with and without day 0
#select relevant columns
df_cstat<-df_1[,c("subjid", "sf94", "days_since_start")]
#remove later days
df_cstat<-subset(df_cstat, days_since_start <11)
#switch to wide format
df_cstat<-reshape(df_cstat, idvar="subjid", timevar="days_since_start", direction="wide")
#add mortality_28
df_cstat<-merge(x=df_cstat, y=df_1[,c("subjid", "mortality_28")], by="subjid", x.all=T)
#remove double entries
df_cstat<-distinct(df_cstat)
#function to calculate C-statistic for different days
cstat_function<-function(day_sf94, day_number){
  cstat_uni<-glm(mortality_28 ~ day_sf94, data=df_cstat, family=binomial)
  cstat_uni<-Cstat(cstat_uni)
  cstat_uni<-cbind(cstat_uni, day_number, "Univariate")
  cstat_d0<-glm(mortality_28 ~ day_sf94 + sf94.0, data=df_cstat, family=binomial)
  cstat_d0<-Cstat(cstat_d0)
  cstat_d0<-cbind(cstat_d0, day_number, "Multivariate")
  cstat_out<-rbind(cstat_uni, cstat_d0)
  return(cstat_out)
}
#call function for 10 days
cstat_1<-cstat_function(df_cstat$sf94.1, 1)
cstat_2<-cstat_function(df_cstat$sf94.2, 2)
cstat_3<-cstat_function(df_cstat$sf94.3, 3)
cstat_4<-cstat_function(df_cstat$sf94.4, 4)
cstat_5<-cstat_function(df_cstat$sf94.5, 5)
cstat_6<-cstat_function(df_cstat$sf94.6, 6)
cstat_7<-cstat_function(df_cstat$sf94.7, 7)
cstat_8<-cstat_function(df_cstat$sf94.8, 8)
cstat_9<-cstat_function(df_cstat$sf94.9, 9)
cstat_10<-cstat_function(df_cstat$sf94.10, 10)
cstat_df<-rbind(cstat_1, cstat_2, cstat_3, cstat_4, cstat_5, cstat_6, cstat_7,
                cstat_8, cstat_9, cstat_10)
cstat_df<-data.frame(cstat_df)
#change column names
colnames(cstat_df)<-c("c_statistic", "day_since_start", "model_group")
cstat_df$day_since_start<-as.factor(cstat_df$day_since_start)
cstat_df$c_statistic<-as.numeric(cstat_df$c_statistic)
#make graph
library(ggplot2)
cstat_graph<-ggplot(cstat_df, aes(x=day_since_start, y=c_statistic, colour= model_group, group=model_group))
cstat_graphic<-cstat_graph+geom_path()

ggsave(plot=cstat_graphic, width=13, dpi=300, path = '/home/skerr/Git/SF94/Outputs/', filename="cstat_graphic.pdf")


#######################################Mortality##################################################
## INPUTS REQUIRED ##
# alpha - significance level 
# power - specified power
# p1 - proportion experiencing event within specified time frame in control arm
# p2 - proportion experiencing event within specified time frame in active arm

riskratio_function<-function(subset_df,mort_dif){
  p1 <- sum(subset_df$mortality_28 == 1, na.rm = T)/ sum(!is.na(subset_df$mortality_28))
  p2=p1*mort_dif
  riskratio_mort<-p1/p2
  riskratio_output<-cbind(p1,p2,riskratio_mort)
  return(riskratio_output)
}
rr_subset1<-riskratio_function(subset1, 0.85)
rr_subset2<-riskratio_function(subset2, 0.85)
rr_subset3<-riskratio_function(subset3, 0.85)
rr_mort<-rbind(rr_subset1, rr_subset2, rr_subset3)
write.csv(rr_mort,"/home/skerr/Git/SF94/Outputs/rr_mort.csv")

sample_function<-function(subset_df, mort_dif){
  p1 <- sum(subset_df$mortality_28 == 1, na.rm = T)/ sum(!is.na(subset_df$mortality_28))
  p2=p1*mort_dif
  sample <- power.prop.test(n=NULL,p1=p1,p2=p2,power=0.8)
  return(sample)
}
sample_subset1<-sample_function(subset1,0.85)
sample_subset2<-sample_function(subset2,0.85)
sample_subset3<-sample_function(subset3,0.85)

cont_cor_function<-function(samplefunc){
  ncorrect <- function(n,p1,p2){
    round(2 * (n/4)*(1+sqrt(1+(4/(n*(p1-p2)))))^2)
  } 
  ncorrect(samplefunc$n, samplefunc$p1, samplefunc$p2)
}
sample_cont_correction_1<-cont_cor_function(sample_subset1)
sample_cont_correction_2<-cont_cor_function(sample_subset2)
sample_cont_correction_3<-cont_cor_function(sample_subset3)
samplesize_mortality<-cbind(sample_cont_correction_1,sample_cont_correction_2, sample_cont_correction_3)
write.csv(samplesize_mortality,"/home/skerr/Git/SF94/Outputs/samplesize_mortality.csv")

mort_table_function<-function(subset_df){
  mortTable <- table(subset_df[["mortality_28"]])
  return(mortTable)
}

#proportion who died/ who lived
mort_table_1<-mort_table_function(subset1)
mort_table_2<-mort_table_function(subset2)
mort_table_3<-mort_table_function(subset3)
mort_table_output<-rbind(mort_table_1, mort_table_2, mort_table_3)
write.csv(mort_table_output,"/home/skerr/Git/SF94/Outputs/mort_table_output.csv")


##############################################################
## Sample size formulae for analyses using log-rank methods ##
##############################################################

# taken from formulae described in Chapter 10 of Modelling Survival Data in Medical Research, 2nd Edition, Collett

## INPUTS REQUIRED ##
# alpha - significance level 
# power - specified power
# HR - assumed rate ratio for treatment effect
# hazc - baseline hazard in control arm (this is equal to the proportion that would be expected to experience the event during ONE unit of follow-up)
# a - length of recruitment period (defaults to 0 here as patients are to be followed for a set period of time, e.g. 28 days, which means the length of the recruitment period will not affect the median duration of follow-up
# f - length of follow-up (defaults to 28)

lrsamplesize <- function(subset_df,alpha,power,HR,a,f){
  # this function calculates the TOTAL sample size needed for a study that is to be analysed using log-rank methods
  p1 <- sum(subset_df$mortality_28 == 1, na.rm = T)/ sum(!is.na(subset_df$mortality_28))
  hazc = (1-exp(log(1-p1)/f)) 
  hazt <- hazc*HR
  d <- (4*((qnorm(1-alpha/2)+qnorm(power))^2))/((log(HR))^2)
  P_event <- 1-(1/6)*(avsurv(f,hazc,hazt)+4*avsurv(0.5*a+f,hazc,hazt)+avsurv(a+f,hazc,hazt))
  n <- d/P_event
  return(n)
}

avsurv <- function(time,haz1,haz2){
  (survfunc(time,haz1)+survfunc(time,haz2))/2
}

survfunc <- function(time,haz){
  (1-haz)^time
}
# calculate the rate ratio that is equivalent to the risk ratio assumed in the code above for sample size calculations based on difference in proportions
# p1 and p2 are proportions experiencing the event within specified time frame in control and active arms as defined above
HR_func<-function(subset_df,mort_dif, f=28){
  p1 <- sum(subset_df$mortality_28 == 1, na.rm = T)/ sum(!is.na(subset_df$mortality_28))
  p2=p1*mort_dif
  HR = (1-exp(log(1-p2)/f))/(1-exp(log(1-p1)/f))
  return(HR)
}

HR_ss_1<-HR_func(subset1, 0.85)
HR_ss_2<-HR_func(subset2, 0.85)
HR_ss_3<-HR_func(subset3, 0.85)
HR_mort<-cbind(HR_ss_1,HR_ss_2,HR_ss_3)
write.csv(HR_mort,"/home/skerr/Git/SF94/Outputs/HR_mort.csv")
ss_logrank_ss1<-round(lrsamplesize(subset1,0.05,0.8,HR_ss_1,0,28))
ss_logrank_ss2<-round(lrsamplesize(subset2,0.05,0.8,HR_ss_2,0,28))
ss_logrank_ss3<-round(lrsamplesize(subset3,0.05,0.8,HR_ss_3,0,28))
samplesize_logrank_mort<-cbind(ss_logrank_ss1,ss_logrank_ss2,ss_logrank_ss3)
write.csv(samplesize_logrank_mort,"/home/skerr/Git/SF94/Outputs/samplesize_logrank_mort.csv")

#############################################################################################################
library(Rmisc)
library(boot)
library(rms)

#Summary stats
function_mean_sd<-function(subset_df){
  meanSD <- as.data.frame( rbind(sapply(subset_df[c('sf94_day5_P','sf94_day8_P')], mean, na.rm=T),  
                                 sapply(subset_df[c('sf94_day5_P','sf94_day8_P')], sd, na.rm=T)) )
  rownames(meanSD) <- c('mean', 'SD')
  return(meanSD)
}
meanSD_subset1<-function_mean_sd(subset1)
meanSD_subset2<-function_mean_sd(subset2)
meanSD_subset3<-function_mean_sd(subset3)
meanSD_output<-cbind(meanSD_subset1,meanSD_subset2, meanSD_subset3)
#correlation
correlation_function<-function(subset_df) {
  correlation_subset_05<-subset(subset_df, ((!is.na(subset_df[,"sf94_day0"])&(!is.na(subset_df[,"sf94_day5_P"])))))
  correlation_subset_08<-subset(subset_df, ((!is.na(subset_df[,"sf94_day0"])&(!is.na(subset_df[,"sf94_day8_P"])))))
  # DAY 0/5
  w <-  correlation_subset_05[,"sf94_day0"]
  x <-  correlation_subset_05[,"sf94_day5_P"]
  day05_cor<-cor(w,x)
  # DAY 0/8
  y <-  correlation_subset_08[,"sf94_day0"]
  z <-  correlation_subset_08[,"sf94_day8_P"]
  day08_cor<-cor(y,z)
  cor_output<-list(day05_cor,day08_cor)
  return(cor_output)
}
cor_subset1<-correlation_function(subset1)
cor_subset2<-correlation_function(subset2)
cor_subset3<-correlation_function(subset3)
correlation_output<-cbind(cor_subset1, cor_subset2, cor_subset3)

#bootstrapped 95% CI for upper and lower limit of effectsize
effect_size_calc <- function(prob_pred, treatment, coef){
  return( mean (log( (treatment*(1-prob_pred)) / (1- treatment * prob_pred)) , na.rm = TRUE ) / coef  )
}
effect_size_boot <- function(data, indices){
  sf94_d5<-lrm(mortality_28 ~ sf94_day5_P+sf94_day0+ age_estimateyears+ sex, data = data[indices,])
  sf94_predictD5<-predict(sf94_d5, type = 'fitted'  )
  coef_d5<-sf94_d5$coef[2] #is sf94 day 5 coefficient
  effect_size_D5 <- effect_size_calc(sf94_predictD5, treatment, coef_d5)
  return(effect_size_D5)
} 

# Regression throws error if this isn't done
subset1 <- mutate_at(subset1, 'sex', as.character)
subset2 <- mutate_at(subset2, 'sex', as.character)
subset3 <- mutate_at(subset3, 'sex', as.character)

treatment <- 0.85
boot_result_0.85 <- boot(data = subset1, statistic = effect_size_boot, R=1000)
sf94_d5_boot_0.85<-boot.ci(boot_result_0.85, conf = 0.95, type="basic")
treatment <- 0.80
boot_result_0.80 <- boot(data = subset1, statistic = effect_size_boot, R=1000)
sf94_d5_boot_0.80<-boot.ci(boot_result_0.80, conf = 0.95, type="basic")
treatment <- 0.75
boot_result_0.75 <- boot(data = subset1, statistic = effect_size_boot, R=1000)
sf94_d5_boot_0.75<-boot.ci(boot_result_0.75, conf = 0.95, type="basic")
treatment <- 0.70
boot_result_0.70 <- boot(data = subset1, statistic = effect_size_boot, R=1000)
sf94_d5_boot_0.70<-boot.ci(boot_result_0.70, conf = 0.95, type="basic")

# non bootstrapped mean (still calculated non bootstrapped 95% CI, but those numbers are not used)
sf94_regression<-function(subset_df, mort_difference){
  sf94_d5<-lrm(mortality_28 ~ sf94_day5_P+sf94_day0+ age_estimateyears+ sex, data = subset_df)
  sf94_d8<-lrm(mortality_28 ~ sf94_day8_P+sf94_day0+ age_estimateyears+ sex, data = subset_df)
  sf94_predictD5<-predict(sf94_d5, type = 'fitted'  )
  sf94_predictD8<-predict(sf94_d8, type = 'fitted'  )
  coef_d5<-sf94_d5$coef[2] #is sf94 day 5 coefficient
  coef_d8<-sf94_d8$coef[2] #is sf94 day 8 coefficient
  
  d5_sf94_effectsize<-effect_size_calc(sf94_predictD5, mort_difference, coef_d5)
  d8_sf94_effectsize<-effect_size_calc(sf94_predictD8, mort_difference, coef_d8)
  d5_d8_effectsize<-cbind(d5_sf94_effectsize, d8_sf94_effectsize)
  return(d5_d8_effectsize)
}
sf94_regression_subset1<-sf94_regression(subset1,0.85)
sf94_regression_subset2<-sf94_regression(subset2,0.85)
sf94_regression_subset3<-sf94_regression(subset3,0.85)
sf94_effectsize<-rbind(sf94_regression_subset1, sf94_regression_subset2,sf94_regression_subset3)

## INPUTS REQUIRED ##
# alpha - significance level =0.05
# power - specified power =0.8
# delta - difference between mean change scores
# sd - standard deviation of the outcome measure
# rho - correlation between the outcome measured at baseline and at follow-up

power_sf94<-function(alpha, power, delta, sd, rho){
  
  power1 <- power.t.test(n=NULL, delta=delta, sd=sd, power=power, sig.level = alpha) # calculate sample size for a t test
  
  ntotal <- 2*round(((1-(rho^2))*power1$n)) # apply ANCOVA correction
  return(ntotal)
}

subset1_D5_SS<-power_sf94(0.05,0.8,sf94_regression_subset1[1], meanSD_subset1[2,1], cor_subset1[[1]])
subset1_D8_SS<-power_sf94(0.05,0.8,sf94_regression_subset1[2], meanSD_subset1[2,2], cor_subset1[[2]])
subset2_D5_SS<-power_sf94(0.05,0.8,sf94_regression_subset2[1], meanSD_subset2[2,1], cor_subset2[[1]])
subset2_D8_SS<-power_sf94(0.05,0.8,sf94_regression_subset2[2], meanSD_subset2[2,2], cor_subset2[[2]])
subset3_D5_SS<-power_sf94(0.05,0.8,sf94_regression_subset3[1], meanSD_subset3[2,1], cor_subset3[[1]])
subset3_D8_SS<-power_sf94(0.05,0.8,sf94_regression_subset3[2], meanSD_subset3[2,2], cor_subset3[[2]])
sf94_samplesize<-cbind(subset1_D5_SS,subset1_D8_SS,subset2_D5_SS,subset2_D8_SS,subset3_D5_SS,subset3_D8_SS)

#write to github
write.csv(meanSD_output,"/home/skerr/Git/SF94/Outputs/meanSD_output.csv")
write.csv(correlation_output,"/home/skerr/Git/SF94/Outputs/correlation_output.csv")
write.csv(sf94_effectsize,"/home/skerr/Git/SF94/Outputs/sf94_effectsize.csv")
write.csv(sf94_samplesize,"/home/skerr/Git/SF94/Outputs/sf94_samplesize.csv")


###########################################################################################################################
#WHO
library(MASS)

# OR effect size calculator
effect_size_calc_OR <- function(prob_pred, treatment){
  mean_prob <- mean(prob_pred, na.rm = TRUE)
  return(  (treatment*(1-mean_prob)) / (1- treatment * mean_prob) )  
}

#non bootstrapped effect size
who_function<-function(subset_df, mortality_diff){
  WHOD5_model_S<-polr(as.factor(WHOD5_P) ~ age_estimateyears+ sex, data = subset_df, Hess=T)
  WHOD8_model_S<-polr(as.factor(WHOD8_P) ~ age_estimateyears+ sex, data = subset_df, Hess=T)
  
  pred_D5 <- predict(WHOD5_model_S, newdata = subset_df, type = 'probs')
  pred_D8 <- predict(WHOD8_model_S, newdata = subset_df, type = 'probs')
  
  d5_effectsize_who<-effect_size_calc_OR(pred_D5[,"10"], mortality_diff)
  d8_effectsize_who<-effect_size_calc_OR(pred_D8[,"10"], mortality_diff)
  who_effectsize<-cbind(d5_effectsize_who, d8_effectsize_who)
  return(who_effectsize)
}
who_subset1<-who_function(subset1, 0.85)
who_subset2<-who_function(subset2, 0.85)
who_subset3<-who_function(subset3, 0.85)
who_effectsize<-rbind(who_subset1, who_subset2, who_subset3)

#summary numbers
who_table_function<-function(subset_df){
  whoTable <- rbind( table(subset_df[["WHOD5_P"]]), table(subset_df[["WHOD8_P"]])   )
  return(whoTable)
}

who_table_1<-who_table_function(subset1)
who_table_2<-who_table_function(subset2)
who_table_3<-who_table_function(subset3)
who_table_output<-rbind(who_table_1, who_table_2, who_table_3)
write.csv(who_table_output,"/home/skerr/Git/SF94/Outputs/who_table_output.csv")

#bootstrapped effect size
effect_size_boot_who <- function(data, indices){
  WHOD5_model_S<-polr(as.factor(WHOD5_P) ~ age_estimateyears + sex, data = data[indices, ], Hess=T)
  pred_D5 <- predict(WHOD5_model_S,newdata =data, type = 'probs')
  effect_size <- effect_size_calc_OR(pred_D5[,"10"], treatment)
  return(effect_size)
} 
treatment <- 0.85
boot_result_who_0.85 <- boot(data = subset1, statistic = effect_size_boot_who, R=1000)
who_d5_booted_0.85<-boot.ci(boot_result_who_0.85, conf = 0.95, type="basic")
treatment <- 0.80
boot_result_who_0.80 <- boot(data = subset1, statistic = effect_size_boot_who, R=1000)
who_d5_booted_0.80<-boot.ci(boot_result_who_0.80, conf = 0.95, type="basic")
treatment <- 0.75
boot_result_who_0.75 <- boot(data = subset1, statistic = effect_size_boot_who, R=1000)
who_d5_booted_0.75<-boot.ci(boot_result_who_0.75, conf = 0.95, type="basic")
treatment <- 0.70
boot_result_who_0.70 <- boot(data = subset1, statistic = effect_size_boot_who, R=1000)
who_d5_booted_0.70<-boot.ci(boot_result_who_0.70, conf = 0.95, type="basic")


# use function posamsize in package Hmisc
library(Hmisc)

## INPUTS REQUIRED ##
# alpha - significance level (=0.05)
# power - specified power (=0.8)
# OR - the odds ratio to be able to detect
# pavg  - a vector of probabilities of being in each category of the ordinal scale (averaged over the two treatment groups), needs to sum to one
# OR= this will need to be estimated from the models once finalised, but I have assumed a 15% reduction in odds for the moment 
# n= number of patients with non-missing values of WHO day 5 in the info I have
# We can only estimate the vector of probabilities for control group, denoted by p1 (the numbers in each WHO category at day 5 have been taken from the info I have)


who_prop<-function(subset_df){
  p1_D5 <-table(subset_df[,"WHOD5_P"])/sum(!is.na(subset_df[,"WHOD5_P"]))
  p1_D8 <-table(subset_df[,"WHOD8_P"])/sum(!is.na(subset_df[,"WHOD8_P"]))
  prop_who<-cbind(p1_D5,p1_D8)
  return(prop_who)
}
who_prop_ss1<-who_prop(subset1)
who_prop_ss2<-who_prop(subset2)
who_prop_ss3<-who_prop(subset3)
who_prop_output<-cbind(who_prop_ss1, who_prop_ss2, who_prop_ss3)
write.csv(who_prop_output,"/home/skerr/Git/SF94/Outputs/who_prop_output.csv")



who_effectsize_function_ss<-function(subset_df, who_subset_df_1, who_subset_df_2){
  p1_D5 <-table(subset_df[,"WHOD5_P"])/sum(!is.na(subset_df[,"WHOD5_P"]))
  p1_D8 = table(subset_df[,"WHOD8_P"])/sum(!is.na(subset_df[,"WHOD8_P"]))
  
  who_effectsize_function<-function(alpha, power, OR, p1){ #here's how to compute the average over the two groups from p1 and OR
    p2   <- pomodm(p=p1, odds.ratio=OR)
    pavg <- (p1 + p2) / 2
    posamsize(p=pavg, odds.ratio=OR, alpha=alpha, power=power)
  }
  
  who_samplesize_d5<-who_effectsize_function( 0.05, 0.8, who_subset_df_1, p1_D5)
  who_samplesize_d8<-who_effectsize_function( 0.05, 0.8, who_subset_df_2, p1_D8)
  who_samplesize<-cbind(who_samplesize_d5, who_samplesize_d8)
  return(who_samplesize)
}
who_samplesize_SS1<-who_effectsize_function_ss(subset1, who_subset1[1], who_subset1[2])
who_samplesize_SS2<-who_effectsize_function_ss(subset2, who_subset2[1], who_subset2[2])
who_samplesize_SS3<-who_effectsize_function_ss(subset3, who_subset3[1], who_subset3[2])

who_samplesize<-cbind(who_samplesize_SS1,who_samplesize_SS2,who_samplesize_SS3)

write.csv(who_effectsize,"/home/skerr/Git/SF94/Outputs/who_effectsize.csv")
write.csv(who_samplesize,"/home/skerr/Git/SF94/Outputs/who_samplesize.csv")


####################################################################################################################################
## Sample size formulae for analyses using difference in proportions ##


susimpfunc<-function(subset_df, mortdif){
  susimp_1L<-lrm(mortality_28 ~ sustained_1L_improvement + age_estimateyears+ sex, data = subset_df,maxit=1000)
  susimp_2L<-lrm(mortality_28 ~ sustained_2L_improvement + age_estimateyears+ sex, data = subset_df,maxit=1000)
  predict_susimp_1L<-predict(susimp_1L, type = 'fitted'  )
  predict_susimp_2L<-predict(susimp_2L, type = 'fitted'  )
  coef_1L<-susimp_1L$coef[2] 
  coef_2L<-susimp_2L$coef[2] 
  
  susimp_1L_effectsize<-effect_size_calc(predict_susimp_1L, mortdif, coef_1L)
  susimp_2L_effectsize<-effect_size_calc(predict_susimp_2L, mortdif, coef_2L)
  susimp_effectsize<-cbind(susimp_1L_effectsize, susimp_2L_effectsize)
  return(susimp_effectsize)
}


effectsize_susump1<-susimpfunc(subset1,0.85)
effectsize_susump2<-susimpfunc(subset2,0.85)
effectsize_susump3<-susimpfunc(subset3,0.85)
effectsize_sus_improvement<-rbind(effectsize_susump1,effectsize_susump2,effectsize_susump3)

#bootstrapped
effect_size_boot_sus <- function(data, indices){
  susimp_1L<-lrm(mortality_28 ~ sustained_1L_improvement + age_estimateyears+ sex,
                 data[indices, ],maxit=1000)
  predict_susimp_1L<-predict(susimp_1L, type = 'fitted'  )
  coef_1L<-susimp_1L$coef[2]
  effect_size <- effect_size_calc(predict_susimp_1L, treatment, coef_1L)
  return(effect_size)
} 
treatment <- 0.85
boot_result_sus_0.85 <- boot(data = subset1, statistic = effect_size_boot_sus, R=1000)
sus1_booted_0.85<-boot.ci(boot_result_sus_0.85, conf = 0.95, type="basic")
treatment <- 0.80
boot_result_sus_0.80 <- boot(data = subset1, statistic = effect_size_boot_sus, R=1000)
sus1_booted_0.80<-boot.ci(boot_result_sus_0.80, conf = 0.95, type="basic")
treatment <- 0.75
boot_result_sus_0.75 <- boot(data = subset1, statistic = effect_size_boot_sus, R=1000)
sus1_booted_0.75<-boot.ci(boot_result_sus_0.75, conf = 0.95, type="basic")
treatment <- 0.70
boot_result_sus_0.70 <- boot(data = subset1, statistic = effect_size_boot_sus, R=1000)
sus1_booted_0.70<-boot.ci(boot_result_sus_0.70, conf = 0.95, type="basic")

#numbers for Natalie/ for table 1 (Estimated treatment effect sustained 1/2 L improvement)
write.csv(effectsize_susump1,"/home/skerr/Git/SF94/Outputs/effectsize_susump1.csv")
write.csv(effectsize_susump2,"/home/skerr/Git/SF94/Outputs/effectsize_susump2.csv")
write.csv(effectsize_susump3,"/home/skerr/Git/SF94/Outputs/effectsize_susump3.csv")
p1_1L_number <- sum(subset1$sustained_1L_improvement == 1, na.rm = T)/ sum(!is.na(subset1$sustained_1L_improvement))
p1_2L_number<- sum(subset1$sustained_2L_improvement == 1, na.rm = T)/ sum(!is.na(subset1$sustained_2L_improvement))
p1_1L_numbers2 <- sum(subset2$sustained_1L_improvement == 1, na.rm = T)/ sum(!is.na(subset2$sustained_1L_improvement))
p1_2L_numbers2<- sum(subset2$sustained_2L_improvement == 1, na.rm = T)/ sum(!is.na(subset2$sustained_2L_improvement))
p1_1L_numbers3 <- sum(subset3$sustained_1L_improvement == 1, na.rm = T)/ sum(!is.na(subset3$sustained_1L_improvement))
p1_2L_numbers3<- sum(subset3$sustained_2L_improvement == 1, na.rm = T)/ sum(!is.na(subset3$sustained_2L_improvement))
p1p2_output<-rbind(p1_1L_number, p1_2L_number, p1_1L_numbers2, p1_2L_numbers2,p1_1L_numbers3, p1_2L_numbers3)
write.csv(p1p2_output,"/home/skerr/Git/SF94/Outputs/p1p2_output.csv")

#calculate the estimated treatment effect
# subset 1
es<-0.0273862022747544
p1<-0.425296796834167
p2<-p1+es
(p1-p2)/p1
es<-0.0290504940102538
p1<-0.164750242664078
p2<-p1+es
(p1-p2)/p1
# subset 2
es<-0.0276468243632414
p1<-0.422364729458918
p2<-p1+es
(p1-p2)/p1
es<-0.0293247524568871
p1<-0.164288577154309
p2<-p1+es
(p1-p2)/p1
# subset 3
es<-0.0270478403905907
p1<-0.396216482974173
p2<-p1+es
(p1-p2)/p1
es<-0.0287785239336453
p1<-0.152854087843395
p2<-p1+es
(p1-p2)/p1

#calculate sample size
susimp_pwr_func<-function(subset_df, effectsize_1L, effectsize_2L){
  p1_1L <- sum(subset_df$sustained_1L_improvement == 1, na.rm = T)/ sum(!is.na(subset_df$sustained_1L_improvement))
  p2_1L=p1_1L+effectsize_1L
  ss_sustained_improvement_1L<-power.prop.test(power=0.8, p1=p1_1L, p2=p2_1L)
  p1_2L <- sum(subset_df$sustained_2L_improvement == 1, na.rm = T)/ sum(!is.na(subset_df$sustained_2L_improvement))
  p2_2L=p1_2L+effectsize_2L
  ss_sustained_improvement_2L<-power.prop.test(power=0.8, p1=p1_2L, p2=p2_2L)
  ss_sustained_improvement<-cbind(ss_sustained_improvement_1L, ss_sustained_improvement_2L)
  return(ss_sustained_improvement)
}
samplesize_susimp_pwr1<-susimp_pwr_func(subset1, effectsize_susump1[,1],effectsize_susump1[,2])
samplesize_susimp_pwr2<-susimp_pwr_func(subset2, effectsize_susump2[,1],effectsize_susump2[,2])
samplesize_susimp_pwr3<-susimp_pwr_func(subset3, effectsize_susump3[,1],effectsize_susump3[,2])

saveRDS(samplesize_susimp_pwr1,"/home/skerr/Git/SF94/Outputs/samplesize_susimp_pwr1.rds")
saveRDS(samplesize_susimp_pwr2,"/home/skerr/Git/SF94/Outputs/samplesize_susimp_pwr2.rds")
saveRDS(samplesize_susimp_pwr3,"/home/skerr/Git/SF94/Outputs/samplesize_susimp_pwr3.rds")
readRDS("/Users/Maaike/Downloads/samplesize_susimp_pwr3.rds")

table_sus_imp<-function(subset_df){
  susimp_1L<-table(subset_df$sustained_1L_improvement)
  susimp_2L<-table(subset_df$sustained_2L_improvement)
  sus_imp<-cbind(susimp_1L, susimp_2L)
  return(sus_imp)
}
sus_imp_ss1<-table_sus_imp(subset1)
sus_imp_ss2<-table_sus_imp(subset2)
sus_imp_ss3<-table_sus_imp(subset3)
sus_imp_output<-cbind(sus_imp_ss1, sus_imp_ss2, sus_imp_ss3)
write.csv(sus_imp_output,"/home/skerr/Git/SF94/Outputs/sus_imp_output.csv")

## INPUTS REQUIRED ##
# alpha - significance level 
# power - specified power
# HR - assumed rate ratio for treatment effect
# hazc - baseline hazard in control arm (this is equal to the proportion that would be expected to experience the event during ONE unit of follow-up)
# a - length of recruitment period (defaults to 0 here as patients are to be followed for a set period of time, e.g. 28 days, which means the length of the recruitment period will not affect the median duration of follow-up
# f - length of follow-up (defaults to 28)

lrsamplesize_susimp <- function(subset_df,level_imp,alpha,power,HR,a,f){
  # this function calculates the TOTAL sample size needed for a study that is to be analysed using log-rank methods
  p1 <- sum(subset_df[[level_imp]] == 1, na.rm = T)/ sum(!is.na(subset_df[[level_imp]]))
  hazc = (1-exp(log(1-p1)/f)) 
  hazt <- hazc*HR
  d <- (4*((qnorm(1-alpha/2)+qnorm(power))^2))/((log(HR))^2)
  P_event <- 1-(1/6)*(avsurv(f,hazc,hazt)+4*avsurv(0.5*a+f,hazc,hazt)+avsurv(a+f,hazc,hazt))
  n <- d/P_event
  return(n)
}

avsurv <- function(time,haz1,haz2){
  (survfunc(time,haz1)+survfunc(time,haz2))/2
}

survfunc <- function(time,haz){
  (1-haz)^time
}
# calculate the rate ratio that is equivalent to the risk ratio assumed in the code above for sample size calculations based on difference in proportions
# p1 and p2 are proportions experiencing the event within specified time frame in control and active arms as defined above
susimp_HR_func<-function(subset_df,level_imp,effect_size, f=28){
  p1 <- sum(subset_df[[level_imp]] == 1, na.rm = T)/ sum(!is.na(subset_df[[level_imp]]))
  p2=p1+effect_size
  HR = (1-exp(log(1-p2)/f))/(1-exp(log(1-p1)/f))
  return(HR)
}

susimp_HR_1_1L<-susimp_HR_func(subset1, "sustained_1L_improvement",effectsize_susump1[,1] )
susimp_HR_2_1L<-susimp_HR_func(subset2, "sustained_1L_improvement",effectsize_susump2[,1] )
susimp_HR_3_1L<-susimp_HR_func(subset3, "sustained_1L_improvement",effectsize_susump3[,1]  )
susimp_HR_1_2L<-susimp_HR_func(subset1, "sustained_2L_improvement",effectsize_susump1[,2]  )
susimp_HR_2_2L<-susimp_HR_func(subset2, "sustained_2L_improvement",effectsize_susump2[,2]  )
susimp_HR_3_2L<-susimp_HR_func(subset3, "sustained_2L_improvement",effectsize_susump3[,2]  )

HR_mort_susimp<-cbind(susimp_HR_1_1L,susimp_HR_2_1L,susimp_HR_3_1L,susimp_HR_1_2L,susimp_HR_2_2L,susimp_HR_3_2L)
write.csv(HR_mort_susimp,"/home/skerr/Git/SF94/Outputs/HR_mort_susimp.csv")
#                                       subset_df,level_imp,                alpha,power,HR,         a,f
ss_logrank_1_susimp1L<-round(lrsamplesize_susimp(subset1,"sustained_1L_improvement",0.05,0.8,susimp_HR_1_1L,0,28))
ss_logrank_2_susimp1L<-round(lrsamplesize_susimp(subset2,"sustained_1L_improvement",0.05,0.8,susimp_HR_2_1L,0,28))
ss_logrank_3_susimp1L<-round(lrsamplesize_susimp(subset3,"sustained_1L_improvement",0.05,0.8,susimp_HR_3_1L,0,28))
ss_logrank_1_susimp2L<-round(lrsamplesize_susimp(subset1,"sustained_2L_improvement",0.05,0.8,susimp_HR_1_2L,0,28))
ss_logrank_2_susimp2L<-round(lrsamplesize_susimp(subset2,"sustained_2L_improvement",0.05,0.8,susimp_HR_2_2L,0,28))
ss_logrank_3_susimp2L<-round(lrsamplesize_susimp(subset3,"sustained_2L_improvement",0.05,0.8,susimp_HR_3_2L,0,28))
samplesize_logrank_susimp<-cbind(ss_logrank_1_susimp1L,ss_logrank_2_susimp1L,ss_logrank_3_susimp1L,
                                 ss_logrank_1_susimp2L,ss_logrank_2_susimp2L,ss_logrank_3_susimp2L)
write.csv(samplesize_logrank_susimp,"/home/skerr/Git/SF94/Outputs/samplesize_logrank_susimp.csv")

################################# protocolised measurement #############################################
# Just running the model to get coefficients etc
prot_model <- lrm(mortality_28 ~ sf94_day5_P+sf94_day0+ age_estimateyears+ sex, data = subset1)

# Mean and standard deviation of opportunistic measurements
mean_opp <- mean( subset1$sf94_day5_P, na.rm = TRUE)
SD_opp <- sd( subset1$sf94_day5_P, na.rm = TRUE)

# Example numbers for mean_prot, SD_prot, rho. Johnny will hopefully give us better estimates somehow
# rho_opp_prot =correlation between opportunistic and protocolised sf94 measurement
mean_prot <- mean_opp
SD_prot <- 0.8 * SD_opp
rho_opp_prot <- 0.7

# rho is the correlation between sf94 day 5 and sf94 day 0.
# I think you calculate it a different way in evaluate outcomes. 
# Ive done it this way just as an example. Feel free to amend/delete.
rho <- cor(subset1$sf94_day5_P,  subset1$sf94_day0, use = 'complete.obs')

# This creates coefficients for protocolised measurement.
create_coef_prot <- function(mean_prot, SD_prot, rho_opp_prot, coef_opp){
  alpha0 <- mean_opp - rho_opp_prot * SD_opp * mean_prot / SD_prot
  
  # Note this is a global assignment! alpha1 is needed elsewhere.
  alpha1 <<- rho_opp_prot * SD_opp / SD_prot
  
  coef_prot <- coef_opp
  
  coef_prot[1] <- coef_opp[1] + alpha0 * coef_opp[1]
  
  coef_prot[2] <- alpha1 * coef_opp[2]
  
  return(coef_prot)
}

# Logistic function
logistic <- function(x, coef){
  return(   1/(1 + exp( -coef[1]  - as.matrix(x) %*% coef[-1] ) ))
}  

# Calculate protocolised coefficients
coef_prot_05 <- create_coef_prot(mean_prot, SD_prot, 0.5, prot_model$coef)
coef_prot_06 <- create_coef_prot(mean_prot, SD_prot, 0.6, prot_model$coef)
coef_prot_07 <- create_coef_prot(mean_prot, SD_prot, 0.7, prot_model$coef)
coef_prot_08 <- create_coef_prot(mean_prot, SD_prot, 0.8, prot_model$coef)
coef_prot_09 <- create_coef_prot(mean_prot, SD_prot, 0.9, prot_model$coef)

# Turn sex into binary numeric
subset1$sex <- as.numeric(as.factor(subset1$sex))-1

# Get protocolised predictions
prot_pred_05 <- logistic( subset1[c('sf94_day5_P', 'sf94_day0', 'age_estimateyears', 'sex')], coef_prot_05 )
prot_pred_06 <- logistic( subset1[c('sf94_day5_P', 'sf94_day0', 'age_estimateyears', 'sex')], coef_prot_06 )
prot_pred_07 <- logistic( subset1[c('sf94_day5_P', 'sf94_day0', 'age_estimateyears', 'sex')], coef_prot_07 )
prot_pred_08 <- logistic( subset1[c('sf94_day5_P', 'sf94_day0', 'age_estimateyears', 'sex')], coef_prot_08 )
prot_pred_09 <- logistic( subset1[c('sf94_day5_P', 'sf94_day0', 'age_estimateyears', 'sex')], coef_prot_09 )

# Calculate protocolised effect size
effect_size_prot_05 <- effect_size_calc(prot_pred_05, 0.85, coef_prot_05[2])
effect_size_prot_06 <- effect_size_calc(prot_pred_06, 0.85, coef_prot_06[2])
effect_size_prot_07 <- effect_size_calc(prot_pred_07, 0.85, coef_prot_07[2])
effect_size_prot_08 <- effect_size_calc(prot_pred_08, 0.85, coef_prot_08[2])
effect_size_prot_09 <- effect_size_calc(prot_pred_09, 0.85, coef_prot_09[2])

# rho_prot_05 is the correlation between the protocolised sf94 measurements on day 0 and day 5
# This is to be used in the sample size calcuation
# rho_prot_opp is to be varied from 0.5-0.9
# alpha1 <<- rho_opp_prot * SD_opp / SD_prot
rho_prot_05 <- rho / rho_opp_prot**2

rho_prot_05_05 = rho/0.5**2
rho_prot_05_06 = rho/0.6**2
rho_prot_05_07 = rho/0.7**2
rho_prot_05_08 = rho/0.8**2
rho_prot_05_09 = rho/0.9**2

# Calculate protocolised sample size
sample_size_prot_05 <- power_sf94(0.05, 0.8, effect_size_prot_05, SD_prot, rho_prot_05_05)
sample_size_prot_06 <- power_sf94(0.05, 0.8, effect_size_prot_06, SD_prot, rho_prot_05_06)
sample_size_prot_07 <- power_sf94(0.05, 0.8, effect_size_prot_07, SD_prot, rho_prot_05_07)
sample_size_prot_08 <- power_sf94(0.05, 0.8, effect_size_prot_08, SD_prot, rho_prot_05_08)
sample_size_prot_09 <- power_sf94(0.05, 0.8, effect_size_prot_09, SD_prot, rho_prot_05_09)

samplesize_protocolised<-cbind(sample_size_prot_05,sample_size_prot_06,sample_size_prot_07,
                               sample_size_prot_08,sample_size_prot_09)
write.csv(samplesize_protocolised,"/home/skerr/Git/SF94/Outputs/samplesize_protocolised.csv")
