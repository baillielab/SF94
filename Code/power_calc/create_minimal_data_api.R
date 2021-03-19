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
selectionVars <-  c('clinical_frailty', 'respiratory_support')

# Ppredictor variables in the various models
indVars <- c('sf94_day0', 'sf94_day5', 'sf94_day8', 'sf94_delta_05', 'sf94_delta_08', 'age_estimateyears', 'sex')

# Dependent variables in the various models
depVars <- c('day28_mortality', 'whoImprovement1', 'whoImprovement2', 'who_day5', 'who_day8')

# Variables that will be used in power calculations
utilityVars <- c('who_day0', 'who_day5', 'who_day8')

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
#Intercept       sf94_day5_P         sf94_day0 age_estimateyears          sex=Male 
#-1.89117524       -1.64419961       -0.04653267        0.07267907        0.20714655 
sf94_d5_model_coef <- c(-1.89117524, -1.64419961, -0.04653267, 0.07267907, 0.20714655 )

#Intercept       sf94_day8_P         sf94_day0 age_estimateyears          sex=Male 
#-0.98185830       -2.04131763       -0.06151697        0.07250340        0.21240031 
sf94_d8_model_coef <- c(-0.98185830, -2.04131763, -0.06151697, 0.07250340, 0.21240031)

#Intercept       sf94_day5_P         sf94_day0 age_estimateyears          sex=Male 
#-0.088478842       0.460245766      -0.004033554      -0.019552229      -0.109547236 
sus_1L_d5_model_coef <- c(-0.088478842, 0.460245766, -0.004033554, -0.019552229, -0.109547236)

#Intercept       sf94_day8_P         sf94_day0 age_estimateyears          sex=Male 
#-1.006224691       0.537649175      -0.075947423      -0.007904097       0.03161860
sus_1L_d8_model_coef <- c(-1.006224691, 0.537649175, -0.075947423, -0.007904097, 0.03161860)

#Intercept       sf94_day5_P         sf94_day0 age_estimateyears          sex=Male 
#0.31970770        0.12559804       -0.40591675       -0.02084021        0.06568982 
sus_2L_d5_model_coef <- c(0.31970770, 0.12559804, -0.40591675, -0.02084021, 0.06568982)

#Intercept       sf94_day8_P         sf94_day0 age_estimateyears          sex=Male 
#-0.11560870        0.23233468       -0.47970481       -0.01755118        0.17794323 
sus_2L_d8_model_coef <- c(-0.11560870, 0.23233468, -0.47970481, -0.01755118, 0.17794323 )
  
# These are propotional odds models
# For these ones, the intercept (first component) must be set as the threshold value between who levels 9 and 10
# The rest should be MINUS the coefficients from the model.

#age_estimateyears           sexMale 
#0.01987002        0.23623191
#4|5       5|6       6|7       7|8       8|9      9|10 
#0.6017289 1.6012292 2.6964929 2.8625757 3.4732608 3.8456992
who_d5_model_coef <- c(1,2,3,4)


#age_estimateyears           sexMale 
#0.03000907        0.20834102
#4|5      5|6      6|7      7|8      8|9     9|10 
#1.751196 2.466708 3.134009 3.306187 3.788585 4.005230
who_d8_model_coef <- c(1,2,3,4)






# model predictions
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

