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
selectionVars <-  'clinical_frailty'

# Ppredictor variables in the various models
indVars <- c('sf94_day0', 'sf94_day5', 'sf94_day8', 'sf94_delta_05', 'sf94_delta_08', 'age_estimateyears', 'sex')

# Dependent variables in the various models
depVars <- c('day28_mortality', 'whoImprovement1', 'whoImprovement2', 'who_day5', 'who_day8')

df <- df[ c( 'subjid', depVars, indVars, selectionVars) ]

# Make data types correct
df <- mutate_at(df, indVars, as.numeric )
df <- mutate_at(df, depVars, as.factor )
  

############################## FUNCTIONS #################################

# Logistic function
logistic <- function(x, coef){
  return(   1/(1 + exp( -coef[1]  -as.matrix(x) %*% coef[-1] ) ))
}  

###################################################################

# Model coefficients
sf94_d5_model_coef <- c(1,2,3,4)
sf94_d8_model_coef <- c(1,2,3,4)
sf94_d5_2_model_coef <- c(1,2,3,4,5)
sf94_d8_2_model_coef <- c(1,2,3,4,5)
sus_1L_D5_model_coef <- c(1,2,3,4)
sus_1L_D8_model_coef <- c(1,2,3,4)
sus_2L_D5_model_coef <- c(1,2,3,4)
sus_2L_D8_model_coef <- c(1,2,3,4)
  
# For these ones, the interecept must be set carefully
WHOD5_model_coef <- c(1,2,3,4)
WHOD8_model_coef <- c(1,2,3,4)


# model predictions
df$sf94_d5_model_pred <- logistic( df[c('sf94_delta_05', 'age_estimateyears', 'sex')], sf94_d5_model_coef )

df$sf94_d8_model_pred <- logistic( df[c('sf94_delta_08', 'age_estimateyears', 'sex')], sf94_d8_model_coef )

df$sf94_d5_2_model_pred <- logistic(df[c('sf94_day5', 'sf94_day0', 'age_estimateyears', 'sex')], sf94_d5_2_model_coef)

df$sf94_d8_2_model_pred <- logistic(df[c('sf94_day8', 'sf94_day0', 'age_estimateyears', 'sex')], sf94_d8_2_model_coef)


df$sus_1L_D5_model_pred <- logistic(df[c('sf94_delta_05', 'age_estimateyears', 'sex')], sus_1L_D5_model_coef)

df$sus_1L_D8_model_pred <- logistic(df[c('sf94_delta_08', 'age_estimateyears', 'sex')], sus_1L_D8_model_coef)


df$sus_2L_D5_model_pred <- logistic(df[c('sf94_delta_05', 'age_estimateyears', 'sex')], sus_2L_D5_model_coef)

df$sus_2L_D8_model_pred <- logistic(df[c('sf94_delta_08', 'age_estimateyears', 'sex')], sus_2L_D8_model_coef)


df$WHOD5_model_pred <- logistic(df[c('sf94_delta_05', 'age_estimateyears', 'sex')], WHOD5_model_coef)

df$WHOD8_model_pred <- logistic(df[c('sf94_delta_08', 'age_estimateyears', 'sex')], WHOD8_model_coef)



bob1 <- predict( sf94_d5<-lrm(day28_mortality ~ sf94_delta_05+ age_estimateyears+ sex, data = df), type = "fitted" )

df[c(1,2), 'day28_mortality'] <- 'YES'

bob2 <- predict( sf94_d5<-lrm(day28_mortality ~ sf94_delta_05+ age_estimateyears+ sex, data = df), type = "fitted" )

arse <- cbind(bob1, bob2)

bob <- factor(c('male','male','female'),
             levels = c("male", "female"))

levels(bob) <- c("female", "male")

bob

as.numeric(bob)


##############################################################################################

# Choose only model prediction and subset selection columns
df <- select(df, c('age_estimateyears', 'clinical_frailty') | ends_with('model_pred') )

# Save minimal dataset to be used for web API
write.csv(df ,"/home/skerr/SF94_api/ccp_subset_simulated_api.csv", row.names = FALSE)

