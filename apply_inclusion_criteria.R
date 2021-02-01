#data used:
dfsfr<-read.csv("/home/u034/mcswets/dfsfr_20200130-2.csv")
df_extremes<-read.csv("/home/u034/mcswets/df_1_extremes_20210130-2.csv")


#form subset inclusion criteria
subset1<-subset(dfsfr, age_estimateyears >19 &
                  age_estimateyears <76 &
                  ((!is.na(sao2) & days_since_admission == 0)|
                     (!is.na(sao2) & days_since_admission == 1)|
                     (!is.na(sao2) & days_since_admission == 2)|
                     (!is.na(sao2) & days_since_admission == 3)))
subset_extremes<-subset(df_extremes, age_estimateyears >19 &
                          age_estimateyears <76 &
                          ((!is.na(sao2) & days_since_admission == 0)|
                             (!is.na(sao2) & days_since_admission == 1)|
                             (!is.na(sao2) & days_since_admission == 2)|
                             (!is.na(sao2) & days_since_admission == 3)))
head(subset1)
head(dfsfr,25)
