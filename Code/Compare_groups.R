#We could add this to evaluate_outcome when you're no longer working on it :)
#it's inefficient but it works

#compare groups
#compare key clinical variables between 2 groups (SaO2<0.94 |fio2=0.21) and SaO2>0.94
df_1$sf94_group<-if_else((df_1$sao2 <=0.94 | df_1$fio2 == 0.21), "SF94<", "SF94>")
tapply(df_1$age_estimateyears, df_1$sf94_group, summary, na.rm=T)
tapply(df_1$daily_temp_vsorres, df_1$sf94_group, summary, na.rm=T)
tapply(df_1$systolic_vsorres, df_1$sf94_group, summary, na.rm=T)
tapply(df_1$diastolic_vsorres, df_1$sf94_group, summary, na.rm=T)
tapply(df_1$onset2admission, df_1$sf94_group, summary, na.rm=T)
tapply(df_1$clinical_frailty, df_1$sf94_group, summary, na.rm=T)
tapply(df_1$rr_vsorres, df_1$sf94_group, summary, na.rm=T)
tapply(df_1$daily_urine_lborres, df_1$sf94_group, summary, na.rm=T)
tapply(df_1$daily_bun_lborres, df_1$sf94_group, summary, na.rm=T)
tapply(df_1$daily_gcs_vsorres, df_1$sf94_group, summary, na.rm=T)
tapply(df_1$daily_creat_lborres, df_1$sf94_group, summary, na.rm=T)
tapply(df_1$daily_crp_lborres, df_1$sf94_group, summary, na.rm=T)
table(df_1$infiltrates_faorres, df_1$sf94_group)
table(df_1$severity_scale_ordinal, df_1$sf94_group)
table(df_1$sf94_group)
df_sex<-df_1%>%
  group_by(subjid, sf94_group)%>%
  count(sex, death, discharge)
table(df_sex$sf94_group, df_sex$sex)
table(df_sex$sf94_group, df_sex$death)
table(df_sex$sf94_group, df_sex$discharge)

#check distribution in fio2 when excluding fio2=0.21
high_fio2<-subset(df_day0, fio2>0.22)
tapply(high_fio2$fio2, high_fio2$sf94_group, summary, na.rm=T)


#compare groups based on SFgroup on day 0
df_day0<-subset(df_1, df_1$days_since_admission == 0)
table(df_day0$sf94_group)
table(df_day0$sf94_group, df_day0$sex)
tapply(df_day0$age_estimateyears, df_day0$sf94_group, summary, na.rm=T)
tapply(df_day0$daily_temp_vsorres, df_day0$sf94_group, summary, na.rm=T)
tapply(df_day0$systolic_vsorres, df_day0$sf94_group, summary, na.rm=T)
tapply(df_day0$diastolic_vsorres, df_day0$sf94_group, summary, na.rm=T)
tapply(df_day0$onset2admission, df_day0$sf94_group, summary, na.rm=T)
tapply(df_day0$rr_vsorres, df_day0$sf94_group, summary, na.rm=T)
tapply(df_day0$clinical_frailty, df_day0$sf94_group, summary, na.rm=T)
tapply(df_day0$daily_urine_lborres, df_day0$sf94_group, summary, na.rm=T)
tapply(df_day0$daily_bun_lborres, df_day0$sf94_group, summary, na.rm=T)
tapply(df_day0$daily_gcs_vsorres, df_day0$sf94_group, summary, na.rm=T)
tapply(df_day0$daily_creat_lborres, df_day0$sf94_group, summary, na.rm=T)
tapply(df_day0$daily_crp_lborres, df_day0$sf94_group, summary, na.rm=T)
table(df_day0$sex, df_day0$sf94_group)
table(df_day0$infiltrates_faorres, df_day0$sf94_group)
table(df_day0$death, df_day0$sf94_group)
sum(!is.na(df_day0$day_of_discharge) & df_day0$sf94_group == "SF94<", na.rm = T)
df_sex0<-df_day0%>%
  group_by(subjid, sf94_group)%>%
  count(sex, death, discharge)
table(df_sex0$sf94_group, df_sex0$sex)
table(df_sex0$sf94_group, df_sex0$death)
table(df_sex0$sf94_group, df_sex0$discharge)
table(df_day0$severity_scale_ordinal, df_day0$sf94_group)
