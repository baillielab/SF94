library(dplyr)

library(data.table)

# Steven's totally awesome alternate code suggestion
# I'd also recommend deleting this file and putting the following code at the start of
# evaluate_outcomes

df_1<-fread("/home/u034/mcswets/df_1_20210202.csv", data.table = FALSE)

subjects_to_include <- filter(df, ( !is.na(sao2) & days_since_admission %in% c(0,1,2,3)  & age_estimateyears >19 & age_estimateyears <76 ) )$subjid

subjects_to_include <- unique(subjects_to_include)



#data used:
df_1<-read.csv("/home/u034/mcswets/df_1_20210202.csv")


df_1<-read.csv("Y:/mcswets/df_1_20210202.csv")

df_2 <- fread("Y:/mcswets/df_1_20210202.csv", data.table = FALSE)




#select subjid of patients with a sats measurement on day 0,1,2 or 3
subjects_to_include<-subset(df_1,((!is.na(sao2) & days_since_admission == 0)|
                                     (!is.na(sao2) & days_since_admission == 1)|
                                     (!is.na(sao2) & days_since_admission == 2)|
                                     (!is.na(sao2) & days_since_admission == 3)))
#only keep unique values
subjects_to_include<-unique(subjects_to_include)
# (median age =74 years old so losing ±half of the population)
#form subset inclusion criteria
subset1<-subset(df_1[df_1$subjid %in% subjects_to_include$subjid,], age_estimateyears >19 &
                  age_estimateyears <76)
