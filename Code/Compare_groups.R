#compare groups
#make dataframe with variables of interest
df_comparegroups<-df_1[,c("subjid","age_estimateyears", "sex", "clinical_frailty", "infiltrates_faorres",
                          "daily_temp_vsorres", "systolic_vsorres", "diastolic_vsorres",
                          "onset2admission", "sf94", "sfr","rr_vsorres","daily_urine_lborres",
                          "daily_bun_lborres","daily_gcs_vsorres", "daily_creat_lborres",
                          "daily_crp_lborres","severity_scale_ordinal", "days_since_start", "death", "discharge", "sao2", "fio2")]
#create the 2 groups of interest
df_comparegroups<-df_comparegroups %>% 
  mutate(
    sf94_group = case_when(
      !is.na(sf94) ~ "S/F94",
      is.na(sf94) & !is.na(sfr) ~ "not_sf94"))
#remove rows with missing group (because of missing SF data)
df_comparegroups<-subset(df_comparegroups, !is.na(sf94_group))
#numerical variables
numerical_sf94<-summary(df_comparegroups[df_comparegroups$sf94_group == "S/F94",])
numerical_notsf94<-summary(df_comparegroups[df_comparegroups$sf94_group == "not_sf94",])
#non numerical variables
xray_sf94<-table(df_comparegroups$sf94_group, df_comparegroups$infiltrates_faorres)
who_sf94<-table(df_comparegroups$severity_scale_ordinal, df_comparegroups$sf94_group)
df_sex<-df_comparegroups%>%
  group_by(subjid, sf94_group)%>%
  count(sex, death, discharge)
sf94_sex<-table(df_sex$sf94_group, df_sex$sex)
sf94_death<-table(df_sex$sf94_group, df_sex$death)
sf94_discharge<-table(df_sex$sf94_group, df_sex$discharge)
#bind some output together
table_output<-cbind(xray_sf94, sf94_sex, sf94_death, sf94_discharge)
colnames(table_output)<-c("Xray_NO","Xray_YES","Female","Male", "Death_no", "Death_yes", "Discharge_no", "discharge_yes")

write.csv(table_output,"/home/skerr/Git/SF94/Outputs/table_output.csv")
write.csv(numerical_sf94,"/home/skerr/Git/SF94/Outputs/numerical_sf94.csv")
write.csv(numerical_notsf94,"/home/skerr/Git/SF94/Outputs/numerical_notsf94.csv")
write.csv(who_sf94,"/home/skerr/Git/SF94/Outputs/who_sf94.csv")

#compare groups based on SFgroup on day 0
df_day0<-subset(df_comparegroups, df_comparegroups$days_since_start == 0)
#numerical variables
day0numerical_sf94<-summary(df_day0[df_day0$sf94_group == "S/F94",])
day0numerical_notsf94<-summary(df_day0[df_day0$sf94_group == "not_sf94",])
#non numerical variables
day0xray_sf94<-table(df_day0$sf94_group, df_day0$infiltrates_faorres)
day0who_sf94<-table(df_day0$severity_scale_ordinal, df_day0$sf94_group)
day0df_sex<-df_day0%>%
  group_by(subjid, sf94_group)%>%
  count(sex, death, discharge)
day0sf94_sex<-table(day0df_sex$sf94_group, day0df_sex$sex)
day0sf94_death<-table(day0df_sex$sf94_group, day0df_sex$death)
day0sf94_discharge<-table(day0df_sex$sf94_group, day0df_sex$discharge)
#bind some output together
day0table_output<-cbind(day0xray_sf94, day0sf94_sex, day0sf94_death, day0sf94_discharge)
colnames(day0table_output)<-c("Xray_NO","Xray_YES","Female","Male", "Death_no", "Death_yes", "Discharge_no", "discharge_yes")

write.csv(day0table_output,"/home/skerr/Git/SF94/Outputs/day0table_output.csv")
write.csv(day0numerical_sf94,"/home/skerr/Git/SF94/Outputs/day0numerical_sf94.csv")
write.csv(day0numerical_notsf94,"/home/skerr/Git/SF94/Outputs/day0numerical_notsf94.csv")
write.csv(day0who_sf94,"/home/skerr/Git/SF94/Outputs/day0who_sf94.csv")

