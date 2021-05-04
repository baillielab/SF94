###################################################################### 

## Code author: Steven Kerr

## Description: 
# This code predicts fio2 as a function of sfr and other covariates
# The goal is to predict sfr for patients who initially have sao2 > 0.94  | fio2 > 0.21,
# but then have fio2 reduced until sao2 = 0.94

###################################################################### 

library(dplyr)
library(ggplot2)
library(data.table)


df<- fread("/home/skerr/Data/ccp_subset_derived.csv", data.table = FALSE)

######################### FUNCTIONS #################################

plotPred <- function(model){
  arse <- predict.lm(model, newdata = df)
  
  print(ggplot(df, aes(x=sao2)) + 
          geom_point(aes(y = fio2), color = "red") + 
          geom_point(aes(y = arse), color="blue", linetype="twodash") )
  
  print(ggplot(df, aes(x=arse, y=fio2)) + geom_point())
}

######################### MODELS #####################################

# Add respiratory support column
df <- mutate(df,  respiratory_support = case_when(
  daily_invasive_prtrt == 'YES'   ~ 'IMV',
  daily_noninvasive_prtrt == 'YES'    ~ 'NIV',
  daily_nasaloxy_cmtrt == 'YES'  ~ 'HFNC',
  TRUE ~ 'None') )

# Logistic function
logistic <- function(x, coef){
  return(   1/(1 + exp( -coef[1]  - as.matrix(x) %*% coef[-1] ) ))
}  

# Do logistic transform on fio2
df <- mutate(df, fio2_logodds = log(fio2 / (1-fio2)))

df$fio2_logodds[is.infinite(df$fio2_logodds)] <- NA


bob <- filter(df, respiratory_support == 'None' &  asthma_mhyn == 'NO' & chronicpul_mhyn == 'NO')

ggplot(bob, aes(x=fio2)) +  geom_point(aes(y = sao2), color = "red")
######################################################################

model <- lm(sfr ~ sex + sao2 , data = df)

summary(model)

plotPred(model)



model2 <- lm(sfr ~  sex + respiratory_support * (sao2 *  + I(sao2^2)) 
            + asthma_mhyn + chronicpul_mhyn + respiratory_support, data = df)

summary(model2)

plotPred(model2)


model3 <- lm(fio2 ~  sex + sao2 + I(sao2^2) +
             + asthma_mhyn + chronicpul_mhyn + respiratory_support, data = bob)


summary(model3)

plotPred(model3)




data <- filter(df, sao2 > 0.94  | fio2 > 0.21)


model4 <- lm( fio2_logodds ~ sex + sao2 + I(sao2^2) + chronicpul_mhyn + asthma_mhyn + clinical_frailty + 
                respiratory_support, data = data )

summary(model4)



predictions <-  logistic(  predict.lm(model4, newdata = data ), c(0,1) )


ggplot(data, aes(x=sao2)) + 
        geom_point(aes(y = fio2), color = "red") + 
        geom_point(aes(y = predictions), color="blue", linetype="twodash") 

ggplot(data, aes(x=predictions, y=fio2)) + geom_point()

###################################################################


arse <- mutate(bob, sex = as.numeric( as.character(sfr)) )


