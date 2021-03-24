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

# Make data types correct
df <- mutate_at(df, indVars, as.numeric )
df <- mutate_at(df, c(depVars, utilityVars), as.factor )

############################## FUNCTIONS #################################

# Logistic function
logistic <- function(x, coef){
  return(   1/(1 + exp( -coef[1]  - as.matrix(x) %*% coef[-1] ) ))
}  

############################ SET COEFFICIENTS ##########################
#These coefficients are from models fit on the real data.

# Model coefficients
#Intercept       sf94_day5_P         sf94_day0 age_estimateyears          sex=Male 
#-2.80402241       -1.36874782        0.11562657        0.06837051        0.15284334 
sf94_day5_model_coef <- c(-2.80402241 , -1.36874782, 0.11562657, 0.06837051, 0.15284334 )

#Intercept       sf94_day8_P         sf94_day0 age_estimateyears          sex=Male 
#-2.39597844       -1.63192149        0.11583817        0.07057511        0.03101884 
sf94_day8_model_coef <- c(-2.39597844 , -1.63192149 , 0.11583817, 0.07057511, 0.03101884)


# These are propotional odds models
# For these ones, the intercept (first component) must be set as the threshold value between who levels 9 and 10
# The rest should be MINUS the coefficients from the model.

#age_estimateyears           sexMale 
#0.02220284        0.30985545 
#4|5       5|6       6|7       7|8       8|9      9|10 
#0.8992445 1.8169024 2.8521141 3.0405687 3.8400794 4.3587487 
who_day5_model_coef <- c(4.3587487 , -0.02220284 , -0.30985545)

# age_estimateyears           sexMale 
# 0.02646835        0.27379549
#4|5      5|6      6|7      7|8      8|9     9|10 
#1.571906 2.259101 2.925450 3.154584 3.853252 4.252926 
who_day8_model_coef <- c(4.252926, -0.02646835 , -0.27379549)

############################ GET PREDICTIONS #############################

# model predictions
df$sf94_day5_model_pred <- logistic(df[c('sf94_day5', 'sf94_day0', 'age_estimateyears', 'sex')], sf94_day5_model_coef)
df$sf94_day8_model_pred <- logistic(df[c('sf94_day8', 'sf94_day0', 'age_estimateyears', 'sex')], sf94_day8_model_coef)

df$who_day5_model_pred <- 1 - logistic(df[c('age_estimateyears', 'sex')], who_day5_model_coef)
df$who_day8_model_pred <- 1 - logistic(df[c('age_estimateyears', 'sex')], who_day8_model_coef)

##############################################################################################

# Choose only model prediction variables, selection variables, and utility variables
df <- select(df, c('day28_mortality', 'age_estimateyears', 'clinical_frailty', 'respiratory_support', 'sf94_day0', 'sf94_day5', 
                   'sf94_day8', 'who_day0') | ends_with('model_pred') )

# Save minimal dataset to be used for web API
write.csv(df ,"/home/skerr/Git/SF94_API/ccp_subset_simulated_api.csv", row.names = FALSE)

