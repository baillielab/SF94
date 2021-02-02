#data used:
df_1<-read.csv("/home/u034/mcswets/df_1_20210202.csv")

#select subjid of patients with a sats measurement on day 0,1,2 or 3
subjects_to_include<-subset(df_1,((!is.na(sao2) & days_since_admission == 0)|
                                     (!is.na(sao2) & days_since_admission == 1)|
                                     (!is.na(sao2) & days_since_admission == 2)|
                                     (!is.na(sao2) & days_since_admission == 3)))
#only keep unique values
subjects_to_include<-unique(subjects_to_include)

#form subset inclusion criteria
subset1<-subset(df_1[df_1$subjid %in% subjects_to_include$subjid,], age_estimateyears >19 &
                  age_estimateyears <76)


