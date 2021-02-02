#load data from ultra
ccp_data<-read.csv("/home/u034/mcswets/ccp_data.csv")
#load packages
library(dplyr)
library(tidyr)
#smaller data frame of necessary data 
colnames(ccp_data)
df_1<-ccp_data[,c(1,11,26,28,42,43,63,64,128,140,142,143,146,147,148,150,169,171,173,175,
                  176,177,178,179,180,181,293,392,
                  393,394,395,396,447,448,449,450,452,598,602,603,606)]

####################################### DATA PREPARATION #######################################
#clean up SaO2
#make data frame (don't remove NA's yet)
sao2_clean<- (df_1$daily_sao2_lborres)
sao2_clean<-data.frame(sao2_clean)
#values <1: multiply by 100
sao2_clean <- with(sao2_clean, ifelse(sao2_clean<1, sao2_clean*100, sao2_clean))
#values >100: divide by 100
sao2_clean <- with(sao2_clean, ifelse(sao2_clean>100, sao2_clean/100, sao2_clean))
#remove values under 50, as they are unlikely to be true
sao2_clean[sao2_clean <50]<- NA
#check if makes sense
summary(sao2_clean) 

#sat on admission- clean up
sao_admission<- df_1$oxy_vsorres
sao_admission<-data.frame(sao_admission)
# make negative numbers positive, multiple between 0-1 by 100
sao_admission<- with(sao_admission, ifelse (sao_admission<0, abs(sao_admission), 
                                            sao_admission))
sao_admission<-data.frame(sao_admission)
sao_admission<- with(sao_admission, ifelse (sao_admission>=0 & sao_admission <=1,
                                            sao_admission*100, sao_admission))
sao_admission<-data.frame(sao_admission)
sao_admission<- with(sao_admission, ifelse (sao_admission>100 & sao_admission <1000,
                                            sao_admission*0.1, sao_admission))
sao_admission<-data.frame(sao_admission)
sao_admission<- with(sao_admission, ifelse (sao_admission>1000 & sao_admission <10000,
                                            sao_admission*0.01, sao_admission))
sao_admission<-data.frame(sao_admission)
sao_admission<- with(sao_admission, ifelse (sao_admission>10000 & sao_admission <100000,
                                            sao_admission*0.001, sao_admission))
#delete <50
sao_admission[sao_admission <50]<- NA
summary(sao_admission)

sao2 = ifelse(
  is.na(sao_admission) & !is.na(sao2_clean), sao2_clean, sao_admission)
#make into a fraction
sao2<-sao2/100
summary(sao2)
#add to dataframe
df_1$sao2<- sao2

#FiO2 fraction /100 if over 1
fio2_a_clean<- (df_1$daily_fio2_lborres)
fio2_a_clean<- data.frame(fio2_a_clean)
fio2_a_clean<- with(fio2_a_clean, ifelse(fio2_a_clean>1, fio2_a_clean/100, fio2_a_clean))
#if <0.21 > 0.21
fio2_a_clean[fio2_a_clean< 0.21]<- 0.21
#check if sensible
summary(fio2_a_clean)

#fio2_b cleanup- make dataframe
fio2_clean_b<-(df_1$daily_fio2b_lborres)
fio2_clean_b<- data.frame(fio2_clean_b)
#values /100
fio2_clean_b <- with(fio2_clean_b, ifelse(fio2_clean_b>1, fio2_clean_b/100, fio2_clean_b))
#< 0.21 >0.21
fio2_clean_b[fio2_clean_b <0.21]<- 0.21
#check if sensible
summary(fio2_clean_b)

#FiO2 C cleanup
#FiO2 LPM conversion to fraction
fio2_clean_c<- (df_1$daily_fio2c_lborres)
fio2_clean_c= ifelse(fio2_clean_c >=20,
                     ((fio2_clean_c - 20)/4), fio2_clean_c)
fio2_clean_c<- (fio2_clean_c*0.04 +0.2)
fio2_clean_c[fio2_clean_c>1]<- 1
fio2_clean_c[fio2_clean_c <0.21]<- 0.21
summary(fio2_clean_c)

# Combine FiO2 variables
#merge fio_a and fio_b
fio_ab = ifelse(
  is.na(fio2_a_clean) & !is.na(fio2_clean_b), fio2_clean_b, fio2_a_clean)
fio_abc = ifelse(
  is.na(fio_ab) & !is.na(fio2_clean_c), fio2_clean_c, fio_ab)
#if FiO2 unknown but respiratory support == roomair >> FiO2 =0.21
fio_abc = if_else(is.na(fio_abc) & (df_1$oxy_vsorresu=="Room air"), 0.21, fio_abc)
fio_abc = if_else(is.na(fio_abc) & (df_1$oxygen_cmoccur=="NO"), 0.21, fio_abc)
summary(fio_abc) 
# add to dataframe
df_1$fio2<- fio_abc

#correct fio2: roomair >fio2=0.21
df_1$fio2_corrected<-df_1$fio2
df_1$fio2_corrected<- ifelse(df_1$oxy_vsorresu == "Oxygen therapy" |is.na(df_1$oxy_vsorresu),
                             df_1$fio2_corrected, 0.21 )

write.csv(df_1,"cleaned_sao2_fio2.csv")

#backup
df_1backup<-df_1
df_1<-df_1backup

#days since study enrollment
#repeat enrollment date
df_1<-df_1 %>%
  group_by(subjid)%>%
  fill(dsstdat, .direction = "down")
#day since hospital admission
#repeat hostdat
df_1<-df_1 %>%
  group_by(subjid)%>%
  fill(hostdat, .direction = "down")
#subtract the data of measurement from the data of hospital admission
df_1$days_since_admission<-as.Date(as.character(df_1$daily_dsstdat), format="%Y-%m-%d")-
  as.Date(as.character(df_1$hostdat), format="%Y-%m-%d")
df_1$days_since_admission<-as.numeric(df_1$days_since_admission)
#days since start symptoms
df_1<-df_1 %>%
  group_by(subjid)%>%
  fill(cestdat, .direction = "down")
df_1$days_since_symptoms<-as.Date(as.character(df_1$daily_dsstdat), format="%Y-%m-%d")-
  as.Date(as.character(df_1$cestdat), format="%Y-%m-%d")
df_1$days_since_symptoms<-as.numeric(df_1$days_since_symptoms)
#repeat age for each subject so sub-setting won't leave lines out
df_1<-df_1 %>%
  group_by(subjid)%>%
  fill(age_estimateyears, .direction = "down")

#clean days since ... variables
df_1$days_since_admission[df_1$days_since_admission< (-30)]<-NA
df_1$days_since_admission[df_1$days_since_admission>200]<-NA
df_1$days_since_symptoms[df_1$days_since_symptoms< (-30)]<-NA
df_1$days_since_symptoms[df_1$days_since_symptoms>200]<-NA

#make day of death variable
df_1$day_of_death<-as.Date(as.character(df_1$dsstdtc), format="%Y-%m-%d")-
  as.Date(as.character(df_1$hostdat), format="%Y-%m-%d")
df_1$day_of_death<-as.numeric(df_1$day_of_death)
#remove weird values
summary(df_1$day_of_death,na.rm=T) #min = -368866 days, max=9000 days
df_1$day_of_death[df_1$day_of_death<0]<-NA #±200 subjects with date of outcome before date of admission
df_1$day_of_death[df_1$day_of_death>250]<-NA#also ±200 subjects with day of death >200 days after admission
#if days_since_admission is not known, use the days since death value
df_1$days_since_admission<-ifelse(is.na(df_1$days_since_admission),
                                  df_1$day_of_death,
                                  df_1$days_since_admission)
#check if there are duplicates in days since admission
df_1%>%
  na.omit()%>%
  group_by(subjid)%>%
  count(days_since_admission)%>%
  filter(n>1)
#make a new variable, if day of death matches days since admission, copy outcome 
df_1$outcome<-ifelse((df_1$day_of_death == df_1$days_since_admission), df_1$dsterm, NA)
#make new rows 
df_1<-expand(df_1, subjid, days_since_admission = -30:200) %>%
  left_join(., df_1)
#repeat day of death value 
df_1<-df_1 %>%
  group_by(subjid)%>%
  fill(day_of_death, .direction = "up")
df_1<-df_1 %>%
  group_by(subjid)%>%
  fill(day_of_death, .direction = "down")
#fill gaps
df_1$outcome<-ifelse((df_1$day_of_death == df_1$days_since_admission), df_1$dsterm, NA)

#backup pause
df_1backup2

#Create SF value and SF94 value
df_1$sfr_value<-df_1$sao2/df_1$fio2_corrected
df_1$sf94<-df_1$sfr_value
df_1$sf94<-ifelse((df_1$sao2 <0.94 | df_1$fio2_corrected ==0.21), df_1$sf94, NA)


#for the extremes, change the sf to 0.5/4.76 for the day after the outcome day
#make new variable with sf values
df_1$sf_dd<-df_1$sfr_value
#change next row to extreme value if patients went home or died
alive <- which(df_1$outcome == "Discharged alive") + 1
df_1$sf_dd[alive] <- 4.76
transfer <- which(df_1$outcome == "Transfer to other facility") + 1
df_1$sf_dd[transfer] <- 4.76
death <- which(df_1$outcome == "Death") + 1
df_1$sf_dd[death] <- 0.5

#make a SF94 dd variable
df_1$sf94_dd<-df_1$sf_dd
df_1$sf94_dd<- ifelse((df_1$sao2<0.94 | df_1$fio2_corrected == 0.21 | df_1$sf_dd == 4.76| df_1$sf_dd ==0.5),
                      df_1$sf94_dd, NA)

# backup
df_1<-df_1backup3

#make a new variable based on the ordinal scale levels from the WHO
df_1<-df_1 %>% 
  mutate(
    severity_scale_ordinal = case_when(
      outcome == "Death" ~ "10",
      daily_invasive_prtrt == "YES" & sfr_value <=2.0 & 
        (daily_inotrope_cmyn == "YES"|daily_ecmo_prtrt == "YES" |daily_rrt_cmtrt == "YES") ~ "9",
      daily_invasive_prtrt == "YES" & (sfr_value <=2.0|daily_inotrope_cmyn == "YES" ) ~ "8",
      daily_invasive_prtrt == "YES" & sfr_value >2.0 ~ "7",
      daily_noninvasive_prtrt == "YES" ~ "6",
      daily_nasaloxy_cmtrt == "YES" ~ "6",
      oxygen_cmoccur == "YES" ~ "5",
      oxy_vsorresu == "Oxygen therapy" ~ "5",
      oxygen_cmoccur == "NO" ~ "4",
      oxy_vsorresu == "Room air" ~ "4"))
#make a new variable with  severity score variable for day 1 and day 0
df_1$WHO_day1<-df_1$severity_scale_ordinal
#only keep value if day 0 is known
df_1$WHO_day1<-ifelse((df_1$days_since_admission == 0 | df_1$days_since_admission== 1),
                       df_1$WHO_day1,NA )
#who day1 SF94 value
df_1$WHO_day1_SF94<-df_1$WHO_day1
df_1$WHO_day1_SF94<-ifelse(is.na(df_1$sf94), NA, df_1$WHO_day1_SF94)



write.csv(df_1,"df_1_20210202.csv")


#repeat some values
df_1<-df_1 %>%
  group_by(subjid)%>%
  fill(age_estimateyears, .direction = "down")%>%
  fill(age_estimateyears, .direction = "up")

