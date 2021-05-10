
#create 2 dataframes, 1 for values that are the same througout admission and 1 for values that change
compare_groups_subset1<-df_1[,c("subjid","daily_temp_vsorres", "systolic_vsorres", "diastolic_vsorres",
                                "daily_gcs_vsorres", "daily_crp_lborres","daily_bun_lborres",
                                "daily_creat_lborres", "daily_urine_lborres", )]

unique_values<-df_1[,c("subjid", "age_estimateyears", "sex","death", "discharge","onset2admission", 
                       "clinical_frailty","rr_vsorres", "mortality_28", "infiltrates_faorres")

colnames(df_1)
head(df_1)
df_1$clinical_frailty<-as.numeric(df_1$clinical_frailty)
table(df_1$clinical_frailty)
summary(df_1$clinical_frailty)
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

# compare day0/5 correlation group to complete population (both basedd_sf94_10)
# correlation_subset_05 is those with available measurements on day 0 and day 5
# comparing with complete basedd population (minus the subjects in correlation subset 05)
compare_correlation<-subset1
#if subject is in correlation day 0/day 5 list, add to 'group 05', if not add to other
compare_correlation$group<-ifelse(compare_correlation$subjid %in% correlation_subset_05$subjid, "group 05",
                                  "group other")
table(compare_correlation$group)
#compare groups
number_subjects_compare<-compare_correlation %>%
  group_by(subjid) %>%
  count(group)
table(number_subjects_compare$group)
#compare other characteristics
tapply(compare_correlation$age_estimateyears, compare_correlation$group, summary, na.rm=T)
tapply(compare_correlation$daily_temp_vsorres, compare_correlation$group, summary, na.rm=T)
tapply(compare_correlation$systolic_vsorres, compare_correlation$group, summary, na.rm=T)
tapply(compare_correlation$diastolic_vsorres, compare_correlation$group, summary, na.rm=T)
tapply(compare_correlation$onset2admission, compare_correlation$group, summary, na.rm=T)
tapply(compare_correlation$rr_vsorres, compare_correlation$group, summary, na.rm=T)
tapply(compare_correlation$clinical_frailty, compare_correlation$group, summary, na.rm=T)
tapply(compare_correlation$daily_urine_lborres, compare_correlation$group, summary, na.rm=T)
tapply(compare_correlation$daily_bun_lborres, compare_correlation$group, summary, na.rm=T)
tapply(compare_correlation$daily_gcs_vsorres, compare_correlation$group, summary, na.rm=T)
tapply(compare_correlation$daily_creat_lborres, compare_correlation$group, summary, na.rm=T)
tapply(compare_correlation$daily_crp_lborres, compare_correlation$group, summary, na.rm=T)
compare_group_cat<-compare_correlation%>%
  group_by(subjid, group)%>%
  count(infiltrates_faorres)
table(compare_group_cat$group, compare_group_cat$sex)
table(compare_group_cat$group, compare_group_cat$outcome)
table(compare_group_cat$group, compare_group_cat$infiltrates_faorres)
table(compare_correlation$severity_scale_ordinal, compare_correlation$group)

head(compare_correlation[,c(77,81,82)], 50)
colnames(compare_correlation)
sum(subset1$fio2 == 0.21, na.rm = T)
