#load data from ultra
ccp_data<-read.csv("/home/u034/mcswets/ccp_data.csv")
#load packages
library(dplyr)
library(tidyr)
#smaller data frame of necessary data 
df_1<-ccp_data[,c(1,11,26,28,42,43,63,64,128,140,142,143,146,147,148,150,169,171,173,175,
                  176,177,178,179,180,181,195,198,199,202,203,206,207,210,211,213,214,
                  217,218,221,224,226,229,232,233,236,239,240,243,246,247,250,251,254,255,
                  258,261,264,265,268,269,272,273,276,279,280,283,289,290,293,392,
                  393,394,395,396,415:444,447,448,449,450,452,598,602,603,606)]

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
sao_admission<- with(sao_admission, ifelse (sao_admission>=0 & sao_admission <=1,
                                            sao_admission*100, sao_admission))
sao_admission<- with(sao_admission, ifelse (sao_admission>100 & sao_admission <1000,
                                            sao_admission*0.1, sao_admission))
sao_admission<- with(sao_admission, ifelse (sao_admission>1000 & sao_admission <10000,
                                            sao_admission*0.01, sao_admission))
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

df_1_backup<-df_1

#days since study enrollment
#repeat enrollment date
df_1<-df_1 %>%
  fill(dsstdat, .direction = "down")
#subtract the data of measurement from the data of enrollment 
df_1$days_since_enrolment<-as.Date(as.character(df_1$daily_dsstdat), format="%Y-%m-%d")-
  as.Date(as.character(df_1$dsstdat), format="%Y-%m-%d")
df_1$days_since_enrolment<-as.numeric(df_1$days_since_enrolment)
#day since hospital admission
#repeat hostdat
df_1<-df_1 %>%
  fill(hostdat, .direction = "down")
#subtract the data of measurement from the data of hospital admission
df_1$days_since_admission<-as.Date(as.character(df_1$daily_dsstdat), format="%Y-%m-%d")-
  as.Date(as.character(df_1$hostdat), format="%Y-%m-%d")
df_1$days_since_admission<-as.numeric(df_1$days_since_admission)
#days since start symptoms
#repeat o2a
df_1<-df_1 %>%
  fill(cestdat, .direction = "down")
df_1$days_since_symptoms<-as.Date(as.character(df_1$daily_dsstdat), format="%Y-%m-%d")-
  as.Date(as.character(df_1$cestdat), format="%Y-%m-%d")
df_1$days_since_symptoms<-as.numeric(df_1$days_since_symptoms)
#repeat age for each subject so sub-setting won't leave lines out
df_1<-df_1 %>%
  fill(age_estimateyears, .direction = "down")
#difference between hospital and recruitment date
df_1$diff_hosp_recruit<-as.Date(as.character(df_1$hostdat), format="%Y-%m-%d")-
  as.Date(as.character(df_1$dsstdat), format="%Y-%m-%d")
df_1$diff_hosp_recruit<-as.numeric(df_1$diff_hosp_recruit)
summary(df_1$diff_hosp_recruit)

head(df_1)
summary(df_1$days_since_enrolment)
#clean days since ... variables
df_1$days_since_enrolment[df_1$days_since_enrolment< (-30)]<-NA
df_1$days_since_enrolment[df_1$days_since_enrolment>200]<-NA
df_1$days_since_admission[df_1$days_since_admission< (-30)]<-NA
df_1$days_since_admission[df_1$days_since_admission>200]<-NA
df_1$days_since_symptoms[df_1$days_since_symptoms< (-30)]<-NA
df_1$days_since_symptoms[df_1$days_since_symptoms>200]<-NA

#backup- write to ultra
write.csv(df_1, "SF94_df_1.csv")
df_1<-read.csv("/home/u034/mcswets/SF94_df_1.csv")

#make extremes variable
#for the extremes, change the sf to 0.5/4.76 for death/alive day
df_1_extremes<-df_1
df_1_extremes$sao2[df_1_extremes$dsterm == "Death"]<-0.5
df_1_extremes$sao2[df_1_extremes$dsterm == "Discharged alive"]<-1
df_1_extremes$sao2[df_1_extremes$dsterm == "Transfer to other facility"]<-1
df_1_extremes$sao2[df_1_extremes$dsterm == "Hospitalization"]<-1
df_1_extremes$fio2[df_1_extremes$dsterm == "Death"]<-1
df_1_extremes$fio2[df_1_extremes$dsterm == "Discharged alive"]<-0.21
df_1_extremes$fio2[df_1_extremes$dsterm == "Transfer to other facility"]<-0.21
df_1_extremes$fio2[df_1_extremes$dsterm == "Hospitalization"]<-0.21

#remove lines with missing subjid, sao2 or fio2
df_1_extremes<-subset(df_1_extremes, !is.na(subjid))
df_1_extremes<-subset(df_1_extremes, !is.na(sao2))
df_1_extremes<-subset(df_1_extremes, !is.na(fio2)) 
df_1_extremes$sfr_value<-df_1_extremes$sao2/df_1_extremes$fio2
df_1_extremes<-df_1_extremes %>%
  dplyr::rename(sf_dd= sfr_value)
df_1_extremes$days_since_admission[df_1_extremes$days_since_admission <0]<-NA
df_1_extremes$days_since_admission[df_1_extremes$days_since_admission >60]<-NA
colnames(df_1_extremes)
#select essential columns
df_1_extremes<-df_1_extremes[,c(2,5,107,116,117,119,122)]

#add rows for day 0-60 and order
df_1_extremes<-expand(df_1_extremes, subjid, days_since_admission = 0:60) %>%
  left_join(., df_1_extremes)
df_1_extremes<-data.frame(df_1_extremes)
#repeat outcome
df_1_extremes<-df_1_extremes%>%
  group_by(subjid)%>%
  fill(dsterm, .direction="down")
#change SF values
df_1_extremes$sf_dd[df_1_extremes$dsterm == "Death"]<-0.5
df_1_extremes$sf_dd[df_1_extremes$dsterm == "Discharged alive"]<-4.76
df_1_extremes$sf_dd[df_1_extremes$dsterm == "Transfer to other facility"]<-4.76
df_1_extremes$sf_dd[df_1_extremes$dsterm == "Hospitalization"]<-4.76

#add SF94 dd
df_1_extremes$sf94_dd<-df_1_extremes$sf_dd
df_1_extremes$sf94_dd<- ifelse((df_1_extremes$sao2<0.94 | df_1_extremes$fio2 == 0.21),
                               df_1_extremes$sf94_dd, NA)


write.csv(df_1_extremes, "df_1_extremes_20210130-2.csv")



#join to dfsfr
dfsfr<-left_join(dfsfr, df_1_extremes, by="subjid")
colnames(dfsfr)
#copy dataframe
dfsfr<-df_1[,c(2:20,22:27,72:76,107:112,115:121)]
#remove lines with missing subjid, sao2 or fio2
dfsfr<-subset(dfsfr, !is.na(subjid))
dfsfr<-subset(dfsfr, !is.na(sao2))
dfsfr<-subset(dfsfr, !is.na(fio2)) #147565 lines left 
dfsfr$sfr_value<-dfsfr$sao2/dfsfr$fio2

#make a new variable based on the ordinal scale levels from the WHO
dfsfr<-dfsfr %>% 
  mutate(
    severity_scale_ordinal = case_when(
      dsterm == "Death" ~ "10",
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
dfsfr$WHO_day1<-dfsfr$severity_scale_ordinal
#only keep value if day 0 is known
dfsfr$WHO_day1<-ifelse((dfsfr$days_since_admission == 0 | dfsfr$days_since_admission== 1),
                       dfsfr$WHO_day1,NA )
#make a new variable for measurements of SaO2 <94% 
dfsfr$sf94<-dfsfr$sfr_value
dfsfr$sf94<- ifelse((dfsfr$sao2<0.94 | dfsfr$fio2 == 0.21),dfsfr$sf94, NA)
#who day1 SF94 value
dfsfr$WHO_day1_SF94<-dfsfr$WHO_day1
dfsfr$WHO_day1_SF94<-ifelse(is.na(dfsfr$sf94), NA, dfsfr$WHO_day1_SF94)
head(dfsfr)
#correct fio2: roomair >fio2=0.21
dfsfr$fio2_corrected<-dfsfr$fio2
dfsfr$fio2_corrected<- ifelse(dfsfr$oxy_vsorresu == "Oxygen therapy" |is.na(dfsfr$oxy_vsorresu),
                              dfsfr$fio2_corrected, 0.21 )

write.csv(dfsfr,"dfsfr_20200130-2.csv")
