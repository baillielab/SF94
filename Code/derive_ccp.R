###################################################################### 

## Code author: Maaike Swets, Steven Kerr

## Description: 
# This code adds derived variables to the clean ccp data, and then cleans them

###################################################################### 

library(dplyr)
library(tidyr)
library(data.table)
library(readr)

####################################### IMPORT DATA: #######################################

df<-fread("/home/skerr/Data/ccp_subset_clean.csv", data.table = FALSE, )

#read for maaike
df <-fread("/home/u034/mcswets/df_20211402.csv")
####################################### FUNCTIONS THAT WILL BE USED: #######################################

# This function squeezes variables.
# Values between hard limits and soft limits get set to the soft limit.
# Values outside hard limits get set to NA.
squeeze<- function(df, limits){
  for(var in colnames(limits) ){
    df[var][ df[var] > limits[1, var]  |   df[, var]< limits[4, var] ] <- NA
    df[var][ df[var] > limits[2, var]  &  df[, var] <= limits[1, var] ] <- limits[2, var] 
    df[var][ df[var] >= limits[4, var]  &   df[, var]< limits[3, var] ] <- limits[3, var]
  }
  
  return(df)  
}

####################################### ADD DERIVED VARIABLES: #######################################

# Add variable that is True if they died, False otherwise
# and similarly for if they were discharged

df <- mutate(df, death = case_when( dsterm == 'Death' ~ 'YES',
                                    !is.na(dsterm) ~ 'NO')   )

df <- mutate(df, discharge = case_when( dsterm == 'Discharged alive' | dsterm == 'Transfer to other facility'  ~ 'YES',
                                    !is.na(dsterm) ~ 'NO')   )

#change typos in year entry to 2020
### change this when using 177K subjects as they are also admitted in 2021!!
library(lubridate)
df$daily_dsstdat<-as.Date(df$daily_dsstdat)
year(df$daily_dsstdat)<-2020
table(year(df$daily_dsstdat))

# Add columns for days since admission, days since symptoms
df$dsstdtc<-(as.Date(as.character(df$dsstdtc), format="%Y-%m-%d"))
df$hostdat<-(as.Date(as.character(df$hostdat), format="%Y-%m-%d"))
df$cestdat<-(as.Date(as.character(df$cestdat), format="%Y-%m-%d"))

df$days_since_admission <-  df$daily_dsstdat - df$hostdat

df$days_since_symptoms <-  df$daily_dsstdat - df$cestdat

# Add column for date of first assessment
df<-df %>%
  group_by  (subjid) %>%
  arrange(daily_dsstdat) %>%
  mutate(first_study_day = first(daily_dsstdat))
#arrange by subjid
df<-df %>%
  arrange(subjid)

# Code throws errors if df is a tibble rather than a dataframe
df <- as.data.frame(df)

# add column for day since first assessment (= days_since_start)
df<-mutate(df, days_since_start =  daily_dsstdat- first_study_day)

# Create columns for day of death and day of discharge
df <- mutate(df, day_of_death = case_when( death == 'YES' ~  dsstdtc - first_study_day ))

df <- mutate(df, day_of_discharge = case_when( discharge == 'YES' ~  dsstdtc - first_study_day ))

# Add clean sao2, fio2 variables
df <- mutate(df, sao2 = case_when(  !is.na(daily_sao2_lborres) ~ daily_sao2_lborres/100))

df <- mutate(df, fio2 = case_when(  !is.na(daily_fio2_lborres) ~ daily_fio2_lborres,
                                    !is.na(daily_fio2b_lborres) ~ daily_fio2b_lborres,
                                    !is.na(daily_fio2c_lborres) ~ 0.04 * daily_fio2c_lborres + 0.21)  )

# Add diabetes and liver disease variable
df <- mutate(df, diabetes = case_when(diabetes_type_mhyn == 2 | diabetes_type_mhyn == 1 
                                          | diabetescom_mhyn == 'YES' | diabetes_mhyn == 'YES' ~ 'YES',
                                          diabetes_type_mhyn == 'NO' | diabetescom_mhyn == 'NO' | diabetes_mhyn == 'NO' ~ 'NO')   )

df <- mutate(df, liver = case_when( modliv == 'YES' | mildliver == 'YES' ~ 'YES',
                                        modliv == 'NO' | mildliver == 'NO' ~ 'NO')    )               

#Create SF value and SF94 value
df$sfr<- df$sao2/df$fio2

df <- mutate(df, sf94 = case_when(  sao2 <= 0.94  | fio2 == 0.21 ~ sfr))  

#make a new variable based on the ordinal scale levels from the WHO
df<-df %>% 
  mutate(
    severity_scale_ordinal = case_when(
      !is.na(day_of_death) ~ 10,
      daily_invasive_prtrt == "YES" & sfr <=2.0 & 
        (daily_inotrope_cmyn == "YES"|daily_ecmo_prtrt == "YES" |daily_rrt_cmtrt == "YES") ~ 9,
      daily_invasive_prtrt == "YES" & (sfr <=2.0|daily_inotrope_cmyn == "YES" ) ~ 8,
      daily_invasive_prtrt == "YES" & sfr >2.0 ~ 7,
      daily_noninvasive_prtrt == "YES" ~ 6,
      fio2 >= 0.41 ~ 6,
      fio2 >= 0.22 & fio2 <=0.40 ~ 5,
      daily_nasaloxy_cmtrt == "YES" ~ 5,
      fio2 == 0.21 ~ 4))

# Add death or discharage outcome variable
df<-df %>% 
  mutate(
    outcome = case_when(
      death=='YES' ~ "Death",
      discharge=='YES' ~ "Discharge"))

#28 day mortality variable
df<-df %>% 
  mutate(
    mortality_28 = case_when(
      day_of_death <29 ~ 1,
      day_of_discharge<29 ~ 0))

# 4 day mortality variable
df<-df %>% 
  mutate(
    mortality_4 = case_when(
       day_of_death <5 ~ 1,
       day_of_discharge<5 ~ 0))

############################# CLEAN DERIVED VARIABLES: ###################################

limits <- data.frame( 'days_since_start' = c(200,200,0,0),
                      'days_since_admission' = c(200, 200, -30, -30),  
                      'days_since_symptoms' = c(200, 200, -30, -30),
                      'day_of_death' = c(250,250,0,0),
                      'day_of_discharge' = c(250,250,0,0))

df <- squeeze(df, limits)
head(df)

####################################### WRITE DATA: #######################################

# Write on argosafe
write.csv(df,"/home/skerr/Data/ccp_subset_derived.csv", row.names = FALSE)

# Write for Maaike
write.csv(df,"df_20211402.csv")
