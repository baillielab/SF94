# compare groups
# Supplementary table 1

library(finalfit)

# data and df_1 from the original code are slightly different.
# The reason appears to be that df_1 contains some duplicates, which we remove here
df_comparegroups = readRDS("/home/skerr/Data/ccp_subset_derived_2021-05-26_1941.rds") %>%
  dplyr::select(subjid, age_estimateyears, sex, days_since_start, day_of_death, day_of_discharge,
                fio2, sf94, sfr, severity_scale_ordinal, clinical_frailty, infiltrates_faorres,
                daily_temp_vsorres, systolic_vsorres, diastolic_vsorres, onset2admission,
                rr_vsorres, daily_urine_lborres, daily_bun_lborres, daily_gcs_vsorres,
                daily_creat_lborres, daily_crp_lborres, 
                outcome, mortality_28, sustained_1L_improvement, sustained_2L_improvement, 
                who_days_to_improve1, who_days_to_improve2, rr_vsorres) %>%
  mutate(days_since_start = as.numeric(days_since_start)) %>%
  distinct() %>%
  mutate(sf94_dd = case_when(days_since_start >= day_of_death ~ 0.5,
                             days_since_start >= day_of_discharge ~ 4.76,
                             TRUE ~ sf94),
         who_dd = case_when(days_since_start >= day_of_death ~ 10,
                            days_since_start >= day_of_discharge ~ 4,
                            TRUE ~ severity_scale_ordinal))


# -------------------------------------------------------------------------------------------------# 

#create the 2 groups of interest
df_comparegroups<-df_comparegroups %>% 
  mutate(
    sf94_group = case_when(
      !is.na(sf94) ~ "S/F94",
      is.na(sf94) & !is.na(sfr) & days_since_start != day_of_discharge ~ "not_sf94"))
#remove rows with missing group (because of missing SF data)
df_comparegroups<-subset(df_comparegroups, !is.na(sf94_group))

# set frailty to numeric
df_comparegroups$clinical_frailty<-as.numeric(df_comparegroups$clinical_frailty)
# and who to a factor
df_comparegroups$severity_scale_ordinal<-as.factor(df_comparegroups$severity_scale_ordinal)

# use finalfit to create summary tables
dependent<-"sf94_group"
explanatory<- c("age_estimateyears", "sex","rr_vsorres", "daily_temp_vsorres", "systolic_vsorres",
                "diastolic_vsorres", "daily_crp_lborres", "daily_bun_lborres", "daily_creat_lborres",
                "daily_urine_lborres", "daily_gcs_vsorres", "clinical_frailty", "onset2admission",
                "infiltrates_faorres", "severity_scale_ordinal","mortality_28")

comparegroups_table<-df_comparegroups%>%summary_factorlist(dependent, explanatory, cont="median")
write.csv(comparegroups_table, paste0("/home/skerr/Git/SF94/Outputs/", time_stamp, "/comparegroups_table.csv"))

# table with unique outcomes for supplementary table 1
mort_complete<-df_comparegroups%>%group_by(subjid)%>%slice(which.max(mortality_28))
mort_table_sup1<-table(mort_complete$mortality_28, mort_complete$sf94_group)

write.csv(mort_table_sup1, paste0("/home/skerr/Git/SF94/Outputs/", time_stamp, "/mort_table_sup1.csv"))

#compare groups based on SFgroup on day 0
compareday0<-subset(df_comparegroups, df_comparegroups$days_since_start == 0)
compareday0_table<-compareday0%>%summary_factorlist(dependent, explanatory, cont="median")
write.csv(compareday0_table, paste0("/home/skerr/Git/SF94/Outputs/", time_stamp, "/compareday0_table.csv"))

# number of patients

# all
uniquepatientssf94<-length(unique(df_comparegroups$subjid[df_comparegroups$sf94_group== "S/F94"]))
uniquepatientsnotsf94<-length(unique(df_comparegroups$subjid[df_comparegroups$sf94_group== "not_sf94"]))
measurementssf94<-nrow(df_comparegroups[df_comparegroups$sf94_group == "S/F94",])
measurementsnotsf94<-nrow(df_comparegroups[df_comparegroups$sf94_group == "not_sf94",])
#day 0
d0uniquepatientssf94<-length(unique(compareday0$subjid[compareday0$sf94_group== "S/F94"]))
d0uniquepatientsnotsf94<-length(unique(compareday0$subjid[compareday0$sf94_group== "not_sf94"]))
d0measurementssf94<-nrow(compareday0[compareday0$sf94_group == "S/F94",])
d0measurementsnotsf94<-nrow(compareday0[compareday0$sf94_group == "not_sf94",])



patientnumbers<-cbind(uniquepatientssf94, uniquepatientsnotsf94, measurementssf94, measurementsnotsf94,
                      d0uniquepatientssf94, d0uniquepatientsnotsf94, d0measurementssf94, d0measurementsnotsf94)
write.csv(patientnumbers, paste0("/home/skerr/Git/SF94/Outputs/", time_stamp, "/patientnumbers.csv"))

# mortality
df_mortality<-df_comparegroups[,c("subjid", "mortality_28", "sf94_group")]
df_mortality<-df_mortality%>%group_by(subjid)%>%slice(1)
unique_mortality<-table(df_mortality$mortality_28, df_mortality$sf94_group)

write.csv(unique_mortality, paste0("/home/skerr/Git/SF94/Outputs/", time_stamp, "/unique_mortality.csv"))

