####################################################################

# Code authors: Steven Kerr

## Description: 
### Create minimal dataset for use in the API
####################################################################

library(data.table)
library(dplyr)

df<-fread("/home/skerr/Data/ccp_subset_simulated_post.csv", data.table = FALSE )

df <- mutate_at(df, 'sex', ~case_when(  sex == 'Male' ~ 1,
                                      sex == 'Female' ~ 0))

df[df == 'YES'] <- 1
df[df == 'NO'] <- 0

# Variables that will be used to take subsets of the data for power calculations. Age will also be used as selection
selectionVars <-  c('clinical_frailty')

# Ppredictor variables in the various models
indVars <- c('sf94_day0', 'sf94_day5', 'sf94_day8', 'sf94_delta_05', 'sf94_delta_08', 'age_estimateyears', 'sex')

# Dependent variables in the various models
depVars <- c('day28_mortality', 'whoImprovement1', 'whoImprovement2', 'who_day5', 'who_day8')

# Variables that will be used in power calculations
utilityVars <- c('who_day0')

df <- df[ c( 'subjid', depVars, indVars, selectionVars, utilityVars) ]

# Make data types correct
df <- mutate_at(df, indVars, as.numeric )
df <- mutate_at(df, c(depVars, utilityVars), as.factor )
  

############################## FUNCTIONS #################################

# Logistic function
logistic <- function(x, coef){
  return(   1/(1 + exp( -coef[1]  - as.matrix(x) %*% coef[-1] ) ))
}  

###################################################################

# Model coefficients
sf94_delta_d5_model_coef <- c(1,2,3,4)
sf94_delta_d8_model_coef <- c(1,2,3,4)
sf94_d5_model_coef <- c(1,2,3,4,5)
sf94_d8_model_coef <- c(1,2,3,4,5)
sus_1L_d5_model_coef <- c(1,2,3,4)
sus_1L_d8_model_coef <- c(1,2,3,4)
sus_2L_d5_model_coef <- c(1,2,3,4)
sus_2L_d8_model_coef <- c(1,2,3,4)
  
# These are propotional odds models
# For these ones, the intercept (first component) must be set as the threshold value between who levels 9 and 10
# The rest should be MINUS the coefficients from the model.
who_d5_model_coef <- c(1,2,3,4)
who_d8_model_coef <- c(1,2,3,4)

# model predictions
df$sf94_delta_d5_model_pred <- logistic( df[c('sf94_delta_05', 'age_estimateyears', 'sex')], sf94_delta_d5_model_coef )
df$sf94_delta_d8_model_pred <- logistic( df[c('sf94_delta_08', 'age_estimateyears', 'sex')], sf94_delta_d8_model_coef )
df$sf94_d5_model_pred <- logistic(df[c('sf94_day5', 'sf94_day0', 'age_estimateyears', 'sex')], sf94_d5_model_coef)
df$sf94_d8_model_pred <- logistic(df[c('sf94_day8', 'sf94_day0', 'age_estimateyears', 'sex')], sf94_d8_model_coef)

df$sus_1L_d5_model_pred <- logistic(df[c('sf94_delta_05', 'age_estimateyears', 'sex')], sus_1L_d5_model_coef)
df$sus_1L_d8_model_pred <- logistic(df[c('sf94_delta_08', 'age_estimateyears', 'sex')], sus_1L_d8_model_coef)
df$sus_2L_d5_model_pred <- logistic(df[c('sf94_delta_05', 'age_estimateyears', 'sex')], sus_2L_d5_model_coef)
df$sus_2L_d8_model_pred <- logistic(df[c('sf94_delta_08', 'age_estimateyears', 'sex')], sus_2L_d8_model_coef)

df$who_d5_model_pred <- logistic(df[c('sf94_delta_05', 'age_estimateyears', 'sex')], who_d5_model_coef)
df$who_d8_model_pred <- logistic(df[c('sf94_delta_08', 'age_estimateyears', 'sex')], who_d8_model_coef)

##############################################################################################

# Choose only model prediction variables, selection variables, and utility variables
df <- select(df, c('age_estimateyears', selectionVars, utilityVars) | ends_with('model_pred') )

# Save minimal dataset to be used for web API
write.csv(df ,"/home/skerr/SF94_api/ccp_subset_simulated_api.csv", row.names = FALSE)

