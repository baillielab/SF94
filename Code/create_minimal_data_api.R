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

# Predictor variables in the various models
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
#-2.509152208      -1.506249130       0.008950016       0.070450115       0.134044117 
sf94_day5_model_coef <- c(-2.509152208  , -1.506249130 , 0.008950016, 0.070450115, 0.134044117 )

#Intercept       sf94_day8_P         sf94_day0 age_estimateyears          sex=Male 
#-1.9044854126     -1.8755228838      0.0004714726      0.0712604229      0.0596474283 
sf94_day8_model_coef <- c(-1.9044854126 , -1.8755228838 , 0.0004714726, 0.0712604229, 0.0596474283)


#Intercept       sf94_day5_P         sf94_day0 age_estimateyears               sex 
#-3.371696366      -1.317967988       0.008950016       0.070450115       0.134044117 
sf94_day5_prot_model_coef <- c(-3.371696366, -1.317967988, 0.008950016, 0.070450115, 0.134044117)


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

#Intercept sustained_1L_improvement=1          age_estimateyears                   sex=Male 
#-4.72998904                -8.72526280                 0.07047150                 0.08958965 
sus_1_model_coef <- c(-4.72998904 , -8.72526280 ,  0.07047150 , 0.08958965 )

#Intercept sustained_2L_improvement=1          age_estimateyears                   sex=Male 
#-5.33090060               -13.47858231                 0.06939885                 0.16183775
sus_2_model_coef <- c(-5.33090060  , -13.47858231, 0.06939885 , 0.16183775 ) 

############################ GET PREDICTIONS #############################

# model predictions
df$sf94_day5_model_pred <- logistic(df[c('sf94_day5', 'sf94_day0', 'age_estimateyears', 'sex')], sf94_day5_model_coef)
df$sf94_day8_model_pred <- logistic(df[c('sf94_day8', 'sf94_day0', 'age_estimateyears', 'sex')], sf94_day8_model_coef)

df$sf94_day5_prot_model_pred <- logistic(df[c('sf94_day5', 'sf94_day0', 'age_estimateyears', 'sex')], sf94_day5_prot_model_coef)


df$who_day5_model_pred <- 1 - logistic(df[c('age_estimateyears', 'sex')], who_day5_model_coef)
df$who_day8_model_pred <- 1 - logistic(df[c('age_estimateyears', 'sex')], who_day8_model_coef)

df$sus_1_model_pred <- logistic(df[c('whoImprovement1', 'age_estimateyears', 'sex')], sus_1_model_coef)
df$sus_2_model_pred <- logistic(df[c('whoImprovement2', 'age_estimateyears', 'sex')], sus_2_model_coef)
  
##############################################################################################

# Choose only model prediction variables, selection variables, and utility variables
df <- select(df, c('day28_mortality', 'age_estimateyears', 'clinical_frailty', 'respiratory_support', 'sf94_day0', 'sf94_day5', 
                   'sf94_day8', 'who_day0', 'whoImprovement1', 'whoImprovement2') | ends_with('model_pred') )

# Save minimal dataset to be used for web API
write.csv(df ,"/home/skerr/Git/SF94_API/src/ccp_subset_simulated_api.csv", row.names = FALSE)

