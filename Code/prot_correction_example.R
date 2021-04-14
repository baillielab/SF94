
# Just running the model to get coefficients etc
model <- lrm(mortality_28 ~ sf94_day5_P+sf94_day0+ age_estimateyears+ sex, data = subset1)

# Mean and standard deviation of opportunistic measurements
mean_opp <- mean( subset1$sf94_day5_P, na.rm = TRUE)
SD_opp <- sd( subset1$sf94_day5_P, na.rm = TRUE)

# Example numbers for mean_prot, SD_prot, rho. Johnny will hopefully give us better estimates somehow
# rho_opp_prot = (sd of opportunistic measurement) / (sd of protocolised measurement)
mean_prot <- mean_opp
SD_prot <- 0.8 * SD_opp
rho_opp_prot <- 0.7

# rho is the correlation between sf94 day 5 and sf94 day 0.
# I think you calculate it a different way in evaluate outcomes. 
# Ive done it this way just as an example. Feel free to amend/delete.
rho <- cor(subset1$sf94_day5_P,  subset1$sf94_day0, use = 'complete.obs')

# This creates coefficients for protocolised measurement.
create_coef_prot <- function(mean_prot, SD_prot, rho_opp_prot, coef_opp){
  alpha0 <- mean_opp - rho_opp_prot * SD_opp * mean_prot / SD_prot
  
  # Note this is a global assignment! alpha1 is needed elsewhere.
  alpha1 <<- rho_opp_prot * SD_opp / SD_prot
  
  coef_prot <- coef_opp
  
  coef_prot[1] <- coef_opp[1] + alpha0 * coef_opp[1]
  
  coef_prot[2] <- alpha1 * coef_opp[2]
  
  return(coef_prot)
}

# Logistic function
logistic <- function(x, coef){
  return(   1/(1 + exp( -coef[1]  - as.matrix(x) %*% coef[-1] ) ))
}  

# Calculate protocolised coefficients
coef_prot <- create_coef_prot(mean_prot, SD_prot, rho_opp_prot, model$coef)

# Turn sex into binary numeric
subset1$sex <- as.numeric(as.factor(subset1$sex))-1

# Get protocolised predictions
prot_pred <- logistic( subset1[c('sf94_day5_P', 'sf94_day0', 'age_estimateyears', 'sex')], coef_prot )

# Calculate protocolised effect size
effect_size_prot <- effect_size_calc(prot_pred, 0.85, coef_prot[2])

# rho_prot_05 is the correlation between the protocolised sf94 measurements on day 0 and day 5
# This is to be used in the sample size calcuation
rho_prot_05 = rho_opp_prot**2 / alpha1**2

# Calculate protocolised sample size
sample_size_prot <- power_sf94(0.05, 0.8, effect_size_prot, SD_prot, rho_prot_05)

