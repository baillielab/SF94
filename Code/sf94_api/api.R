####################################################################

# Code authors: Steven Kerr

## Description: 
### API for SF94 sample size calculations using synthetic data
####################################################################

library(plumber)
library(data.table)
library(stringr)
library(Hmisc)
library(dplyr)

####################################################################

df<-fread("/home/skerr/Git/SF94/Code/sf94_api/ccp_subset_simulated_api.csv", data.table = FALSE)

####################### NON-API FUNCTIONS ################################

apply_exclusion <- function(df, age_lb, age_up, frailty_lb, frailty_ub, resp_supp_include ){

  return(filter(df, age_estimateyears >= age_lb & age_estimateyears <= age_ub  
          &  clinical_frailty >= frailty_lb & clinical_frailty <= frailty_ub &
          respiratory_support %in% resp_supp_include))
}


# Effect size calculator for logistic regression
effect_size_calc <- function(prob_pred, treatment, coef){
  return( mean( log((treatment*(1-prob_pred)) / (1- treatment * prob_pred)) / coef , na.rm = TRUE) )
}

# Effect size calculator for odds ratio in proportional odds model
effect_size_calc_OR <- function(prob_pred, treatment){
  mean_prob <- mean(prob_pred, na.rm = TRUE)
  return( treatment*( 1- mean_prob) / (1 - treatment * mean_prob)  )
}

###########################################################################

resp_supp_levels <- c('None', 'HFNC', 'NIV', 'IMV')

logisticModels <- c('sf94_d5', 'sf94_d8', 'sus_1L_d5', 'sus_1L_d8', 'sus_2L_d5', 'sus_2L_d8')

propOddsModels <- c('who_d5', 'who_d8')

# Coefficients of interest in the logistic models
coefs <- data.frame( sf94_d5 = -1.64419961, 
                     sf94_d8 = -2.04131763,
                     sus_1L_d5 = 0.460245766,
                     sus_1L_d8 = 0.537649175, 
                     sus_2L_d5 = 0.12559804  ,
                     sus_2L_d8 = 0.23233468)

###################### API FUNCTION #######################################

#* Calculate required sample size
#* @param model What is the model?
#* @param treatment What is the target multiplier of day 28 mortality?
#* @param power What is the desired statistical power? 
#* @param confidence What is the desired confidence level? 
#* @param age_lb What is the lower bound for age?
#* @param age_ub What is the upper bound for age?
#* @param frailty_lb What is the lower bound for frailty?
#* @param frailty_ub What is the upper bound for frailty?
#* @param resp_None Include people who are not on repiratory support?
#* @param resp_HNFC Include people who are are on HNFC?
#* @param resp_NIV Include people who are are on NIV?
#* @param resp_IMV Include people who are are on IMV?
#* @get /sample_size_calc
function(model, treatment, power, confidence, age_lb, age_ub, frailty_lb, frailty_ub, resp_None, resp_HNFC, resp_NIV, resp_IMV) {
  
  treatment <- as.numeric(treatment)
  power <- as.numeric(power)
  confidence <- as.numeric(confidence)
  age_lb <- as.numeric(age_lb)
  age_ub <- as.numeric(age_ub)
  frailty_lb <- as.numeric(frailty_lb)
  frailty_ub <- as.numeric(frailty_ub)
  
  resp_supp_include <- resp_supp_levels[as.logical(as.numeric(c( resp_None, resp_HNFC, resp_NIV, resp_IMV)))]
  
  alpha <- 1 - confidence
  
  day <- as.numeric(str_sub(model,-1))
  
  col <- paste(model, '_model_pred', sep = '')
  
  subset <- apply_exclusion(df, age_lb, age_up, frailty_lb, frailty_ub, resp_supp_include)

  if(model %in%  logisticModels){
    
    if(  day == 5){
      SD <- sd(subset$who_day5, na.rm = TRUE) 
      rho <- cor(subset$who_day0, subset$who_day5, use = 'complete.obs')
    }else if(day==8){
      SD <- sd(subset$who_day8, na.rm = TRUE) 
      rho = cor(subset$who_day0, subset$who_day8, use = 'complete.obs')
    }
    
    coef <- pull(coefs, model)
    
    effect_size <- effect_size_calc( pull(subset, col), treatment, coef)
  
    # calculate sample size for a t test
    power1 <- power.t.test(n=NULL, delta=effect_size, sd=SD, power=power, sig.level = alpha)
    # apply ANCOVA correction
    ntotal <- 2*round(((1-(rho^2))*power1$n))
    
    }else if (model %in% propOddsModels  ){
    
    oddsRatio <- effect_size_calc_OR(pull(subset, col), treatment) 
        
    p1 <- table(df['who_day0']) / sum( table(df['who_day0']) )
    #here's how to compute the average over the two groups from p1 and OR
    p2   <- pomodm(p=p1, odds.ratio=oddsRatio)
    pavg <- (p1 + p2) / 2
      
    ntotal <- round( as.numeric(posamsize(p=pavg, odds.ratio=oddsRatio, alpha=alpha, power=power)[1] )  )
    }
  
  return(ntotal)
}



#model <- 'sf94_d5'
#treatment <- '0.85'
#power <- '0.8'
#age_lb <- '20'
#age_ub <- '80'
#frailty_lb <- '0'
#frailty_ub <- '10'
#resp_None <- '1'
#resp_HNFC <- '0'
#resp_NIV <- '1'
#resp_IMV <- '1'
#confidence <- '0.95'

#sample_size_calc(model, treatment, power, confidence, age_lb, age_ub, frailty_lb, frailty_ub, resp_None, resp_HNFC, resp_NIV, resp_IMV)


