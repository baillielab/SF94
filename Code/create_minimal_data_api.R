####################################################################

# Code authors: Steven Kerr

## Description: 
### Create minimal dataset for use in the API
####################################################################

library(data.table)
library(dplyr)

df<-fread('/home/skerr/Data/ccp_subset_simulated_post.csv', data.table = FALSE )

df <- mutate_at(df, 'sex', ~case_when(  sex == 'Male' ~ 1,
                                      sex == 'Female' ~ 0))

df[df == 'YES'] <- 1
df[df == 'NO'] <- 0

# Variables that will be used to take subsets of the data for power calculations. Age will also be used as selection
selectionVars <-  c('clinical_frailty', 'respiratory_support')

# Ppredictor variables in the various models
indVars <- c('sf94_day0', 'sf94_day5', 'sf94_day8', 'age_estimateyears', 'sex')

# Dependent variables in the various models
depVars <- c('day28_mortality', 'who_day5', 'who_day8', 'whoImprovement1', 'whoImprovement2')

# Variables that will be used in power calculations
utilityVars <- c('who_day0')

df <- df[ c( 'subjid', depVars, indVars, selectionVars, utilityVars) ]


sapply(df, class)

# Make data types correct
df <- mutate_at(df, c('age_estimateyears', 'sex', 'whoImprovement1', 'whoImprovement2'), as.numeric )
df <- mutate_at(df, c('day28_mortality', 'who_day5', 'who_day8', utilityVars), as.factor )

############################## FUNCTIONS #################################

# Logistic function
logistic <- function(x, coef){
  return(   1/(1 + exp( -coef[1]  - as.matrix(x) %*% coef[-1] ) ))
}  

############################ SET COEFFICIENTS ##########################
#These coefficients are from models fit on the real data.

# Model coefficients
#Intercept       sf94_day5_P         sf94_day0 age_estimateyears          sex=Male 
#-2.45762310       -1.53026304        0.03639850        0.06910471        0.12419596 
sf94_day5_model_coef <- c(-2.45762310  , -1.53026304, 0.03639850, 0.06910471, 0.12419596 )

#Intercept       sf94_day8_P         sf94_day0 age_estimateyears          sex=Male 
#-1.97214706       -1.83464770        0.03184873        0.07034787        0.05161162 
sf94_day8_model_coef <- c(-1.97214706 , -1.83464770 , 0.03184873, 0.07034787, 0.05161162)


# STILL NEEDS UPDATED
sus_1_model_coef <- c(-5.24650031, -13.49099253 , 0.06773265 , 0.19383108 )


sus_2_model_coef <- c(-2.80402241 , 0.11562657, 0.06837051, 0.15284334 ) 
  


# These are propotional odds models
# For these ones, the intercept (first component) must be set as the threshold value between who levels 9 and 10
# The rest should be MINUS the coefficients from the model.

#age_estimateyears           sexMale 
#0.01996716        0.27125033 
#4|5       5|6       6|7       7|8       8|9      9|10 
#0.2912119 1.3564190 2.4491752 2.6491326 3.4950492 4.0460459 
who_day5_model_coef <- c(4.0460459  , -0.01996716  , -0.27125033)

#age_estimateyears           sexMale 
#0.02575375        0.22636142 
#4|5      5|6      6|7      7|8      8|9     9|10 
#1.228147 1.950264 2.633149 2.880049 3.626227 4.050183 
who_day8_model_coef <- c(4.050183 , -0.02575375 , -0.22636142)

############################ GET PREDICTIONS #############################

# model predictions
df$sf94_day5_model_pred <- logistic(df[c('sf94_day5', 'sf94_day0', 'age_estimateyears', 'sex')], sf94_day5_model_coef)
df$sf94_day8_model_pred <- logistic(df[c('sf94_day8', 'sf94_day0', 'age_estimateyears', 'sex')], sf94_day8_model_coef)

df$who_day5_model_pred <- 1 - logistic(df[c('age_estimateyears', 'sex')], who_day5_model_coef)
df$who_day8_model_pred <- 1 - logistic(df[c('age_estimateyears', 'sex')], who_day8_model_coef)

df$sus_1_model_pred <- logistic(df[c('whoImprovement1', 'age_estimateyears', 'sex')], sus_1_model_coef)
df$sus_2_model_pred <- logistic(df[c('whoImprovement2', 'age_estimateyears', 'sex')], sus_2_model_coef)
  
##############################################################################################

# Choose only model prediction variables, selection variables, and utility variables
df <- select(df, c('day28_mortality', 'age_estimateyears', 'clinical_frailty', 'respiratory_support', 'sf94_day0', 'sf94_day5', 
                   'sf94_day8', 'who_day0', 'whoImprovement1', 'whoImprovement2') | ends_with('model_pred') )

# Save minimal dataset to be used for web API
write.csv(df ,"/home/skerr/Git/SF94_API/ccp_subset_simulated_api.csv", row.names = FALSE)

