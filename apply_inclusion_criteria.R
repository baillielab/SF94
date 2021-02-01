#data used:
dfsfr<-read.csv("/home/u034/mcswets/dfsfr_20200130-3.csv")

df_extremes<-read.csv("/home/u034/mcswets/df_1_extremes_20210131-5.csv")

#select subjid of patients with a sats measurement on day 0,1,2 or 3
subjects_to_include<-subset(dfsfr,((!is.na(sao2) & days_since_admission == 0)|
                                     (!is.na(sao2) & days_since_admission == 1)|
                                     (!is.na(sao2) & days_since_admission == 2)|
                                     (!is.na(sao2) & days_since_admission == 3)))
#only keep unique values
subjects_to_include<-unique(subjects_to_include)
head(subjects_to_include)

#form subset inclusion criteria
subset1<-subset(dfsfr[dfsfr$subjid %in% subjects_to_include$subjid,], age_estimateyears >19 &
                  age_estimateyears <76)

subset_extremes<-subset(df_extremes[df_extremes$subjid %in% subjects_to_include$subjid,], age_estimateyears >19 &
                          age_estimateyears <76)

#Make a new variable with SF values for subjects who didn't die or went home on day 0
subset_extremes$sf94_dd_day0<-subset_extremes$sf94_dd
subset_extremes$sf94_dd_day0<-ifelse((!is.na(subset_extremes$dsterm) &
                                        subset_extremes$days_since_admission == 0),
                                     NA,subset_extremes$sf94_dd)
#sanity check
subset_extremes$sf94_dd_day0[subset_extremes$dsterm == "Death" & 
                               subset_extremes$days_since_admission == 0,],na.rm = T)

subset_extremes[subset_extremes$days_since_admission==0,]



sum(is.na(df_extremes$sf_dd))
test<-subset(df_extremes, (days_since_admission ==5 & !is.na(sf_dd)))
test<-subset(test, (sao2<0.94 | fio2_corrected ==0.21))

test1<-subset(test, (sf94_dd >=0.50 & sf94_dd<=4.77))
test2<-subset(test, (sf_dd >=0.50 & sf_dd<=4.77))
length(unique(test2$subjid))
length(na.omit(test$sf94_dd))
summary(test$sf_dd)


colnames(test)
table(test$days_since_admission)
summary(test$sao2)
test<-(df_extremes[(!is.na(df_extremes$sf_dd)) & df_extremes$days_since_admission ==5 )
test
test<-test[test$sao2<0.94 | test$fio2_corrected == 0.21)]

$sao2<0.94 | df_1_extremes$fio2_corrected == 0.21