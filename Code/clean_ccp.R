###################################################################### 

## Code author: Maaike Swets, Steven Kerr

## Description: 
# This code cleans a large selection of variables from the CCP data.

###################################################################### 

library(dplyr)
library(tidyr)
library(data.table)
library(readr)
library(forcats)
library(lubridate)
library(stringr)

####################################### CURATE VARIABLE SELECTION: #######################################

# Note that the same variable can appear in multiple groups. They should be in all that apply

#Variables that are of type Date
dateVars <- c( "dsstdat", "cestdat", "hostdat", "daily_dsstdat", "dsstdtc")

#Variables that should be constant in the data
constVars <- c("sex", "age_estimateyears", "hypertension_mhyn", "chrincard", "chronicpul_mhyn", "asthma_mhyn", "renal_mhyn",             
               "modliv", "mildliver", "chronicneu_mhyn", "malignantneo_mhyn", "chronichaemo_mhyn", "obesity_mhyn",           
               "diabetes_type_mhyn", "diabetescom_mhyn", "diabetes_mhyn", "rheumatologic_mhyn" , "dementia_mhyn",           
               "malnutrition_mhyn", "smoking_mhyn", "other_mhyn", "ethnicity","clinical_frailty", "oxygen_cmoccur",
               "oxygen_proccur", "oxygenhf_cmoccur", "noninvasive_proccur", "invasive_proccur", "dsterm", "dsstdtcyn",
               "smoking_mhyn_2levels", "any_invasive", "onset2admission" )

# Variables that need not be constant in the data
nonConstVars <- c("rr_vsorres", "oxy_vsorres", "oxy_vsorresu","daily_temp_vsorres", "daily_temp_vsorresu", "daily_fio2_lborres", 
                  "daily_fio2b_lborres", "daily_fio2c_lborres", "daily_sao2_lborres", "daily_gcs_vsorres", "systolic_vsorres", 
                  "diastolic_vsorres", "daily_meanart_vsorres", "daily_urine_lborres", "daily_noninvasive_prtrt",
                  "daily_invasive_prtrt", "daily_nasaloxy_cmtrt", "daily_ecmo_prtrt", "daily_rrt_cmtrt",
                  "daily_inotrope_cmyn", "infiltrates_faorres", "daily_bun_lborres", "daily_bun_lborresu",    
                  "daily_creat_lborres", "daily_creat_lborresu", "daily_crp_lborres", "daily_crp_lborresu",     
                  "hodur")

# All variables
allVars <- c("subjid", dateVars, constVars, nonConstVars) 

# Binary variables - take values YES or NO
binaryVars <- c("hypertension_mhyn", "chrincard", "chronicpul_mhyn", "asthma_mhyn", "renal_mhyn",             
                "modliv", "mildliver", "chronicneu_mhyn", "malignantneo_mhyn", "chronichaemo_mhyn", "obesity_mhyn",
                "diabetescom_mhyn", "diabetes_mhyn", "rheumatologic_mhyn" , "dementia_mhyn",           
                "malnutrition_mhyn", "other_mhyn","oxygen_cmoccur", "oxygen_proccur", "oxygenhf_cmoccur", 
                "noninvasive_proccur", "invasive_proccur", "dsstdtcyn", "smoking_mhyn_2levels", "any_invasive",
                'daily_noninvasive_prtrt', 'daily_invasive_prtrt', 'daily_nasaloxy_cmtrt', 'daily_ecmo_prtrt',
                'daily_rrt_cmtrt', 'daily_inotrope_cmyn', 'infiltrates_faorres')

# constant, multi-l categorical variables - take values other than YES or NO, and are not measurement units
otherCatVars <-  c("sex", "diabetes_type_mhyn", "smoking_mhyn", "ethnicity", "clinical_frailty", "dsterm", 'oxy_vsorresu')

# Measurement unit variables
unitVars <- c('daily_temp_vsorresu', "daily_bun_lborresu", "daily_creat_lborresu", "daily_crp_lborresu" )

####################################### IMPORT DATA: #######################################

# You should run code in this section selectively, or add your own read path depending on your environment

#read for Steven
#df <-fread("Y:/mcswets/ccp_data.csv", select = allVars, data.table = FALSE)

#read for maaike
#df <-fread("/home/u034/mcswets/ccp_data.csv", select = allVars, data.table = FALSE)

# Read on argosafe
datadir = "/home/common/covid/cleaned/full/"
timestamp = "2021-02-05_1049"

ccp_data = read_rds(paste0(datadir, "ccp_data_", timestamp, "_full.rds"))

df <- ccp_data[allVars]

# Take a sample. 
#df <- sample_n(bob, 1000)

####################################### FUNCTIONS THAT WILL BE USED: #######################################

# Clean variables that are percentages
# If it has been recorded as in [0,1], multiply by 100
# if it has been recorded as in [100, 1000], divide by 10
# ...[1000,10000] divide by 100
# ...[10000, 100000] divide by 1000
cleanPercent <- function(df, cols){
  df <- df %>%  mutate_at(cols, ~case_when( . >0 & . <= 1   ~ 10 * . ,
                                            . >1 & . <= 100   ~ . ,
                                            . >100 & . <= 1000  ~ ./10,
                                            . >1000 & . <= 10000 ~ ./100,
                                            . >10000 & . <= 100000 ~ ./1000 )   )
  return(df)  
}

# Clean variables that take values in unit interval
cleanInt1 <- function(df, cols){
  df <- df %>%  mutate_at(cols, ~case_when( . >=0 & . <= 1   ~ . ,
                                            . >1 & . <= 100   ~ ./100 )   )
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

# Find the mode of a vector. If there are multiple, return the first that appears.
# If length is zero, return NA
Mode <- function(x) {
  x <- x[ !is.na(x)   ]
  
  if( length(x) == 0 ){
    return(NA)
  } else{
    ux <- unique(x)
    return( ux[which.max(tabulate(match(x, ux))) ] )
  }
}

####################################### GENERAL REPLACEMENTS: #######################################

# Variables of type date have to be excluded from this, otherwise an error will be thrown.
replaceVars <- setdiff( allVars, c('subjid', dateVars))

df[, replaceVars][ df[ replaceVars] == 'Unknown' ] <- NA
df[, replaceVars][ df[ , replaceVars] == 'N/K' ] <- NA
df[, replaceVars][ df[ , replaceVars] == 'N/A' ] <- NA
df[, replaceVars][ df[ , replaceVars] == 'Not specified' ] <- NA
df[, replaceVars][ df[ , replaceVars] == '' ] <- NA

# More columns and factor renaming can be included here by adding more variables/recodings to this command

df <- mutate_at(df, c('any_invasive', 'other_mhyn', 'dsstdtcyn'), ~ fct_recode(., YES = "Yes", NO = 'No'))

################################ REMOVE DUPLICATE ROWS: ###################################

# Worth doing this at the start to make the rest of the code run quicker.
df <- distinct(df)

##################################### CORRECT COLUMN TYPES: #######################################

# daily_temp_vsorres is recorded as a character. Take it to be numberic
df['daily_temp_vsorres'] <- sapply(df['daily_temp_vsorres'], as.numeric)

# Redundant for argosafe data, but in other data date variables might be formatted as characters
# rather than as dates
df[dateVars] <- lapply(df[dateVars], as.Date)

# Redundant for argosafe data, but in other data daily_fio2b_lborres might be formatted as npon-numeric
df['daily_fio2b_lborres'] <- lapply(df['daily_fio2b_lborres'], as.numeric )

####################################### UNIT CONVERSIONS: #######################################

#BUN: measured as mg/dL and mmol/L, convert mg/dL to mmol/L
#to convert from mg/dl to mmol/L for BUN: mg/dL x 0.357
df$daily_bun_lborres<-ifelse(df$daily_bun_lborresu == "mg/dL", 
                             (df$daily_bun_lborres * 0.357),
                             df$daily_bun_lborres)

df['daily_bun_lborresu'][ !is.na( df['daily_bun_lborresu'])  ] <- 'mmol/L'

#creatinin measured as mg/dl or umol/L 
#to convert from mg/dl to umol/L, multiply by 88.4
df$daily_creat_lborres<-ifelse(df$daily_creat_lborresu == "mg/dl", 
                               (df$daily_creat_lborres * 88.4),
                               df$daily_creat_lborres)

df['daily_creat_lborresu'][ !is.na( df['daily_creat_lborresu'])  ] <- 'umol/L'


#crp measured in 3 ways: mg/dL, mg/L (majority) and ug/ml. mg/L=ug/ml
df$daily_crp_lborres<-ifelse(df$daily_crp_lborresu == "mg/dL",
                             (df$daily_crp_lborres * 10), df$daily_crp_lborres)

df['daily_crp_lborresu'][ !is.na( df['daily_crp_lborresu'])  ] <- 'ug/ml'


# Celsius = (Farrenheit - 32) *5/9
df$daily_temp_vsorres<-ifelse(df$daily_temp_vsorresu == "°F",
                              (df$daily_temp_vsorres - 32) * 5/9, df$daily_temp_vsorres)

df['daily_temp_vsorresu'][ !is.na( df['daily_temp_vsorresu'])  ] <- '°C'

####################################### CLEANING: #######################################

# 'daily_fio2c_lborres' is is oxygen received in litres per minute.
# We assume that if greater than 20, they actually wrote FiO2
# Each additional 4% FiO2 is equivalent to 1 L/min
df$daily_fio2c_lborres <- ifelse(df$daily_fio2c_lborres >= 20 , (df$daily_fio2c_lborres-20)/4, df$daily_fio2c_lborres)

# change negative values in cols to positive

negCols = c('daily_sao2_lborres', 'daily_fio2c_lborres', 'oxy_vsorres', 
            'daily_fio2b_lborres', 'daily_fio2_lborres','diastolic_vsorres',
            'daily_gcs_vsorres', 'daily_urine_lborres','daily_crp_lborres')

df[negCols] <- abs(df[negCols])


percentCols <- c( 'daily_sao2_lborres', 'oxy_vsorres')

df <- cleanPercent(df, percentCols)

int1Cols <- c('daily_fio2_lborres', 'daily_fio2b_lborres')
#error: daily_fio2b_lborres is not numeric
df$daily_fio2b_lborres<-as.numeric(df$daily_fio2b_lborres)
df <- cleanInt1(df, int1Cols)

# Limit for acceptable variable values
# In row order, the numbers are 1. Hard upper limit 2. Soft upper limit 3. Soft lower limit 4. Hard lower limit

limits <- data.frame( 'daily_sao2_lborres' = c( 100, 100, 50, 50),
                      'oxy_vsorres' = c(100, 100, 50, 50),
                      'daily_fio2_lborres'= c(1,1, 0.21, 0),
                      'daily_fio2b_lborres' = c(1, 1, 0.21, 0),
                      'daily_fio2c_lborres' = c(20, 20, 0, 0 ), 
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
                      'hodur' = c(200,200,0,0))

df <- squeeze(df, limits)

####################################### TIME SERIES CLEANING: #######################################

# If the year of a date variable is in 2020 or 2021, leave it be. 
# If the year of a date variable is 2002, 2030 or 3030, we assume they meant 2020.
# Otherwise set to NA

df <- mutate_at(df, dateVars,  ~case_when(    year(.) %in% c(2020, 2021) ~ . ,
                                              year(.) %in% c(2002, 2030, 3030) ~ as.Date( paste( '2020', str_sub(. ,-6,-1), sep=""), format = '%Y-%m-%d') ))

# For each subjid, take dates of admission, symptoms, etc as the earliest recorded
# For binary variables that are constant, take them to be 'YES' if they are ever 'YES' for a given subjid.
# If they are never 'YES', but at least once 'NO', take it to be 'NO'.
# Take date enrolment (dsstdat), date of admission (hostdat), date of onset of symptoms (cestdat), outcome date (dsstdtc)
# to be the min of those recorded for each patient.
# Take age to be the average of those recorded.
# For other categorical variables, take the mode of those recorded

df <- df %>% group_by(subjid) %>% mutate_at( c('dsstdat', 'hostdat', 'cestdat', 'dsstdtc'), min, na.rm = TRUE )

df <- df %>% group_by(subjid) %>% mutate_at(intersect(binaryVars, constVars),  ~case_when( any(. == 'YES') ~ 'YES',
                                                                                           any(. == 'NO', na.rm = TRUE) ~ 'NO') )

df <- df %>% group_by(subjid) %>% mutate_at( setdiff( constVars, union(binaryVars, otherCatVars) ), mean, na.rm = TRUE )

df <- df %>% group_by(subjid) %>% mutate_at( intersect(otherCatVars, constVars), Mode)

# Some patients have multiple entries for the same day
# For categorical non-constant values, we take the mode
# For continuous non-constant values, we take the mean

df <- df %>% group_by(subjid, daily_dsstdat) %>% mutate_at( intersect(  union(binaryVars, otherCatVars), nonConstVars), Mode)

df <- df %>% group_by(subjid, daily_dsstdat) %>% mutate_at( setdiff(nonConstVars, union( union(binaryVars, otherCatVars), unitVars) ), mean)

# Taking means leaves NaN values. Make them NA. 
df[is.na(df)] <- NA

# NOTE: In future, may wish to add additional cleaning for time variables.
# In particular, it should be the case that ddstdtc, dsstdat, daily_dsstdat >= hostdat
# and dsstdat, daily_dsstdat >= dsstdat
# In our analysis this cleaning isn't required, but it may be needed for other analyses.

################################ REMOVE UNINFORMATIVE ROWS: ###################################

# Remove duplicate rows
df <- distinct(df)

# Remove any rows that have NA for daily_dsstdat & all non constant variables.
# They don't contain any information
df <- df %>% filter_at( c('daily_dsstdat', setdiff(nonConstVars, unitVars) ), any_vars(!is.na(.))) 

####################################### WRITE DATA: #######################################
df<-data.frame(df)
# Write for Maaike
#write.csv(df,"df_20211402.csv")
#write.csv(df,"df_20211402-backup.csv")

# Write on argosafe
write.csv(df,"/home/skerr/Data/ccp_subset_clean.csv", row.names = FALSE)
