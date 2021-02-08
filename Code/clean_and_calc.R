#load packages
library(dplyr)
library(tidyr)
library(data.table)

cols <- c(1,11,26,28,42,43,54,63,64,96:114,128,140,142,143,146,147,148,150,167,169,171,173,175,
          176,177,178,179,180,181,293,250,251,264,265,279,280,392,
          393,394,395,396,447,448,449,450,452,598,602,603,606)

df_1 <-fread("Y:/mcswets/ccp_data.csv", select = cols, data.table = FALSE)
#read for maaike
df_1 <-fread("/home/u034/mcswets/ccp_data.csv", select = cols, data.table = FALSE)

# Take a sample
df_1 <- sample_n(ccp_data, 500)


####################################### DATA PREPARATION #######################################


# convert date columns to date objects

dateCols <- c('dsstdat', 'cestdat', 'hostdat', 'daily_dsstdat', 'dsstdtc')

df_1[ , dateCols] <- lapply(df_1[ , dateCols], as.Date)


# For each subjid, take dates of admission, symptoms, etc as the earliest recorded
# Take age to be the average of those recorded.

df_1 <- df_1 %>% group_by(subjid) %>% mutate(dsstdat = min(dsstdat, na.rm = TRUE))

df_1 <- df_1 %>% group_by(subjid) %>% mutate(hostdat = min(hostdat, na.rm = TRUE))

df_1 <- df_1 %>% group_by(subjid) %>% mutate(cestdat = mean(cestdat, na.rm = TRUE))

df_1 <- df_1 %>% group_by(subjid) %>% mutate(age_estimateyears =  ifelse(  any( !is.na(age_estimateyears  )  ),   min(age_estimateyears, na.rm = TRUE), NA    ) )


# Add variable that is True if they died, False otherwise
# and similary for if they were dicharged

df_1 <- df_1 %>% group_by(subjid) %>% mutate(death = any(dsterm == 'Death' ))

df_1 <- df_1 %>% group_by(subjid) %>% mutate(discharge = any(dsterm == 'Discharged alive' | dsterm == 'Transfer to other facility' ))


# Add columns for days since admission, days since symptoms

df_1$days_since_admission <-  ifelse(   is.finite(as.numeric( df_1$daily_dsstdat- df_1$hostdat )),  as.numeric( df_1$daily_dsstdat- df_1$hostdat ), NA)

df_1$days_since_symptoms <- ifelse(   is.finite(as.numeric( df_1$daily_dsstdat -  df_1$cestdat )),  as.numeric( df_1$daily_dsstdat -  df_1$cestdat ), NA)

# add column for day since first assessment (= days_since_start)
df_1<-df_1 %>%
  group_by  (subjid) %>%
  mutate(days_since_start = as.numeric(as.Date(daily_dsstdat) - first(as.Date(daily_dsstdat))))
#make a new variable, alternative to hostdat, with the first study date (=first daily_dsstdat)
df_1<-df_1 %>%
  group_by  (subjid) %>%
  mutate(first_study_day = first(as.Date(daily_dsstdat)))
# For reasons unkonwn, some columns get formatted as tibbles. Make everything a dataframe.

df_1 <- as.data.frame(df_1)


# Create columns for day of death and day of discharge

df_1['day_of_death'] <- ifelse(  df_1[,'death'] == TRUE & is.finite( df_1[, 'dsstdtc'] - df_1[, 'first_study_day']  ) , df_1[, 'dsstdtc'] - df_1[, 'first_study_day'],  NA  )

df_1['day_of_discharge'] <- ifelse(  df_1[,'discharge'] == TRUE & is.finite( df_1[, 'dsstdtc'] - df_1[, 'first_study_day']  ) , df_1[, 'dsstdtc'] - df_1[, 'first_study_day'],  NA  )
  
  


# 'daily_fio2c_lborres' is is oxygen received in litres per minute.
# We assume that if greater than 20, they actually wrote FiO2
# Each additional 4% FiO2 is equivalent to 1 L/min
# daily_fio2c_lborres needs to be converted to a number in [0,1]

df_1$daily_fio2c_lborres <- ifelse(df_1$daily_fio2c_lborres >= 20 , (df_1$daily_fio2c_lborres-20)/4, df_1$daily_fio2c_lborres)

#BUN: measured as mg/dL and mmol/L, convert mg/dL to mmol/L
#to convert from mg/dl to mmol/L for BUN: mg/dL x 0.357
df_1$daily_bun_lborres<-ifelse(df_1$daily_crp_lborresu == "mg/dL", 
                               (df_1$daily_bun_lborres * 0.357),
                               df_1$daily_bun_lborres)
#creatinin measured as mg/dl or umol/L 
#to convert from mg/dl to umol/L, multiply by 88.4
df_1$daily_creat_lborres<-ifelse(df_1$daily_creat_lborresu == "mg/dl", 
                                 (df_1$daily_creat_lborres * 88.4),
                                 df_1$daily_creat_lborres)
#crp measured in 3 ways: mg/dL, mg/L (majority) and ug/ml. mg/L=ug/ml
df_1$daily_crp_lborres<-ifelse(df_1$daily_crp_lborresu == "mg/dL",
                               (df_1$daily_crp_lborres * 10),
                               df_1$daily_crp_lborres)


# change negative values in cols to positive

negCols = c('daily_sao2_lborres', 'daily_fio2c_lborres', 'oxy_vsorres', 
            'daily_fio2b_lborres', 'daily_fio2_lborres','diastolic_vsorres',
            'daily_gcs_vsorres', 'daily_urine_lborres','daily_crp_lborres')
#additional columns to remove negative values

df_1[negCols] <- abs(df_1[negCols])


# Clean variables that are percentages
# If it has been recorded as in [0,1], multiply by 100
# if it has been recorded as in [100, 1000], divide by 10
# [1000,10000] divide by 100
# [10000, 100000] divide by 1000

cleanPercent <- function(df, cols){
  
     for(var in cols){
    
       indices <- which(df[, var] >0 & df[, var]<= 1)
       
       df[indices, var] <- df[indices, var] * 100
       
       indices <- which(df[, var] >100 & df[, var]<= 1000)
       
       df[indices, var] <- df[indices, var]/10
       
       indices <- which(df[, var] >1000 & df[, var]<= 10000)
       
       df[indices, var] <- df[indices, var]/100
       
       indices <- which(df[, var] >10000 & df[, var]<= 100000)
       
       df[indices, var] <- df[indices, var]/1000
}
return(df)  
}

# Clean variables that take values in unit interval

cleanInt1 <- function(df, cols){
  
  for(var in cols){
    
    indices <- which(df[, var] >1 & df[, var]<= 100)
    
    df[indices, var] <- df[indices, var]/100
    
  }
return(df)  
}


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


percentCols <- c( 'daily_sao2_lborres', 'oxy_vsorres')

df_1 <- cleanPercent(df_1, percentCols)

int1Cols <- c('daily_fio2_lborres', 'daily_fio2b_lborres')

df_1 <- cleanInt1(df_1, int1Cols)


# Limit for acceptable variable values
# In row order, the numbers are 1. Hard upper limit 2. Soft upper limit 3. Soft lower limit 4. Hard lower limit

limits <- data.frame( 'daily_sao2_lborres' = c( 100, 100, 50, 50),
                      'oxy_vsorres' = c(100, 100, 50, 50),
                      'daily_fio2_lborres'= c(1,1, 0.21, 0),
                      'daily_fio2b_lborres' = c(1, 1, 0.21, 0),
                      'daily_fio2c_lborres' = c(20, 20, 0, 0 ), 
                      'days_since_admission' = c(200, 200, -30, -30),  
                      'days_since_symptoms' = c(200, 200, -30, -30),
                      'day_of_death' = c(250,250,0,0),
                      'day_of_discharge' = c(250,250,0,0),
                      'daily_temp_vsorres'= c(44,44,34,34),
                      'daily_gcs_vsorres'= c(15,15,3,3),
                      'systolic_vsorres'= c(300,300,50,50),
                      'diastolic_vsorres'= c(200,200,20,20),
                      'daily_urine_lborres' = c(15000,15000,0,0),
                      'daily_creat_lborres' = c(1000,1000,50,50),
                      'daily_bun_lborres' = c(50,50,0,0),
                      'daily_crp_lborres'= c(700,700,5,5),
                      'rr_vsorres' = c(70,70,5,5),
                      'onset2admission' = c(100,100,-100,-100),
                      'hodur' = c(200,200,0,0),
                      'days_since_start' = c(200,200,0,0))

df_1 <- squeeze(df_1, limits)

summary(df_1)


# Add clean sao2, fio2 variables


df_1['sao2'] <- ifelse(is.na(df_1[,'daily_sao2_lborres']) & !is.na(df_1[,'oxy_vsorres'])  , df_1[,'oxy_vsorres'],   df_1[,'daily_sao2_lborres'])/100



df_1['fio2'] <- ifelse(is.na(df_1[,'daily_fio2_lborres']) & !is.na(df_1[,'daily_fio2b_lborres']), df_1[,'daily_fio2b_lborres'], df_1[,'daily_fio2_lborres'])

df_1['fio2'] <- ifelse(is.na(df_1[,'fio2']) & !is.na(df_1[,'daily_fio2c_lborres']), df_1[,'daily_fio2c_lborres']*0.04 + 0.21, df_1[,'fio2'])

df_1['fio2'] <- ifelse( is.na(df_1[,'fio2'])  &  df_1['oxy_vsorresu']=="Room air", 0.21, df_1[,'fio2']) 

df_1['fio2'] <- ifelse( is.na(df_1[,'fio2'])  &  df_1['oxygen_cmoccur']=="NO", 0.21, df_1[,'fio2']) 

df_1['fio2']<- ifelse( df_1$oxy_vsorresu == "Oxygen therapy" |is.na(df_1$oxy_vsorresu), df_1[,'fio2'], 0.21 )

summary(df_1$sa02)

# Drop columns that are no longer needed

dropCols <- c('oxy_vsorres', 'daily_sao2_lborres', 'daily_fio2_lborres', 'daily_fio2b_lborres',
              'daily_fio2c_lborres', 'oxy_vsorresu', 'oxygen_cmoccur', 'daily_crp_lborresu',
              'daily_bun_lborresu', 'daily_creat_lborresu')

df_1 <- df_1[, !(names(df_1) %in% dropCols)]



#Create SF value and SF94 value
df_1$sfr<- df_1$sao2/df_1$fio2

df_1$sf94<- ifelse((df_1$sao2 <= 0.94 | df_1$fio2 ==0.21), df_1$sfr, NA)


#make a new variable based on the ordinal scale levels from the WHO
df_1<-df_1 %>% 
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

#Maaike's less efficient code
#Clean up newly added variable
df_1$clinical_frailty[df_1$clinical_frailty == "N/K"]<-NA
df_1$clinical_frailty<-as.numeric(df_1$clinical_frailty)
#repeat sex for all rows for a subject
df_1<-df_1 %>%
  group_by(subjid)%>%
  fill(sex, .direction = "down") %>%
  fill(sex, .direction = "up")


#Complications: if any 'yes' >> repeat yes 

#1 outcome variable
library(tidyr)
df_1<-df_1 %>% 
  mutate(
    outcome = case_when(
      death==TRUE ~ "Death",
      discharge==TRUE ~ "Discharge"))
#30 day mortality variable
df_1<-df_1 %>% 
  mutate(
   mortality_30 = case_when(
      death==TRUE & day_of_death <31 ~ 1,
      discharge==TRUE & day_of_discharge<31 ~ 0))

df_1<-df_1 %>%
  group_by(subjid)%>%
  fill(mortality_30, .direction = "down") %>%
  fill(mortality_30, .direction = "up")

head(df_1)

write.csv(df_1,"df_1_20210402.csv")
