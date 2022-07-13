###################################################################### 
## Code author: Maaike Swets, Steven Kerr

## Description: 
# This code calculates sample sizes required for clinical trials for COVID 
# treatments using different variables as surrogate endpoints

###################################################################### 

library(rms)
library(DescTools)
library(naniar)
library(Rmisc)
library(boot)
library(MASS)
library(Hmisc)
library(tidyverse)

######################################### Load data ###############################################################

# data and df_1 from the original code are slightly different.
# The reason appears to be that df_1 contains some duplicates, which we remove here
data = readRDS("/home/skerr/Data/ccp_subset_derived_2021-05-26_1941.rds") %>%
  dplyr::select(subjid, age_estimateyears, sex, days_since_start, day_of_death, day_of_discharge,
         fio2, sf94, severity_scale_ordinal,
         outcome, mortality_28, sustained_1L_improvement, sustained_2L_improvement, 
         who_days_to_improve1, who_days_to_improve2, rr_vsorres) %>%
  mutate(days_since_start = as.numeric(days_since_start)) %>%
  distinct() %>%
  mutate(sf94_dd = case_when(days_since_start >= day_of_death ~ 0.5,
                             days_since_start >= day_of_discharge ~ 4.76,
                             TRUE ~ sf94),
        who_dd = case_when(days_since_start >= day_of_death ~ 10,
                           days_since_start >= day_of_discharge ~ 4,
        TRUE ~ severity_scale_ordinal))

# Dataframes with each row containing all observations of sf94/who over time for each patient
#
# df_sf94 has some subjids that day05 in the original code doesn't.
# This leads to a difference of several thousand rows between day05 and df_sf94
# This is because day05 only includes subjids that have at least one non-NA sf94 measurements 
# on a certain subset of days
# day05 also contains a small number of duplicate entries, and this has been fixed for df_sf94

df_sf94 = dplyr::select(data, -sf94_dd, -severity_scale_ordinal, -who_dd, -fio2, -rr_vsorres) %>%
  filter(days_since_start <= 16, !is.na(sf94)) %>%
  distinct() %>%
  pivot_wider(names_from = (days_since_start), values_from = sf94, names_prefix = "sf94_day")
df_sf94[df_sf94 == 'NULL'] = NA

df_who =  dplyr::select(data, -sf94, -severity_scale_ordinal, -sf94_dd, -fio2, -rr_vsorres) %>%
  filter(days_since_start <= 16) %>%
  distinct() %>%
  pivot_wider(names_from = (days_since_start), values_from = who_dd, names_prefix = "who_day")
df_who[df_who == 'NULL'] = NA

# Same thing, but now with modified if they die/are discharged
df_sf94_dd = dplyr::select(data, -sf94, -severity_scale_ordinal, -who_dd, -fio2, -rr_vsorres) %>%
  filter(days_since_start <= 16) %>%
  distinct() %>%
  pivot_wider(names_from = (days_since_start), values_from = sf94_dd, names_prefix = "sf94_day")
df_sf94_dd[df_sf94_dd == 'NULL'] = NA

######################################### Functions ###############################################################

# Calculate what number of subjects needs to be added to get correct proportion
# in_study/percentage_in_study is the chances of sf94 being recorded amongst all those 
# who still could have had it recorded (not dead or discharged)
# We multiply the number of people who actually died by this probability.
dead_alive_to_add = function(df, days){
  
  output = c()
  
  for (day in days){
  
  dead=(length(unique(df$subjid[(df$day_of_death < day)])))/length(unique(df$subjid))
  
  alive=(length(unique(df$subjid[(df$day_of_discharge < day)])))/length(unique(df$subjid))
  
  in_study=sum((!is.na(df$sf94) & df$days_since_start == day), na.rm=T)
  
  percentage_in_study= 1-dead-alive
  
  dead_to_add= (dead*in_study)/percentage_in_study
  alive_to_add=(alive*in_study)/percentage_in_study
  row  = c("% dead" = dead,"% alive"= alive,"% in study" =percentage_in_study,
                        "dead to add" =dead_to_add,"alive to add" =alive_to_add)
  
  output = cbind(output, row)
  }
  output = data.frame(output)
  names(output) = days
  
  return(output)
}

# This function creates new columns with proportionally added sf94 values for dead/discharged so that 
# degree of missingness is maintained
prop_add = function(df, dead_discharge_proportions, day){

  col = paste0("sf94_day", day)
  new_col = paste0(col, '_P')
  
  dead_to_add = round( dead_discharge_proportions['dead to add', as.character(day)])
  alive_to_add = round( dead_discharge_proportions['alive to add', as.character(day)])
  
  df[new_col] = df[col]
  
  rows_to_replace=which(is.na(df[[new_col]]) &df$day_of_death < day ) #find rows with missing values and who died
  
  df[[new_col]][sample(rows_to_replace, dead_to_add)]= 0.5 #and add correct number of dead patients
  
  rows_to_replace=which(is.na(df[[new_col]]) & df$day_of_discharge < day) #same for patients who lived
  
  df[[new_col]][sample(rows_to_replace, alive_to_add)]= 4.76
  
  df = data.frame(df)
  
  return(df)
}


# This gives the proportion of all subjects who were dead/alive by a set of days
# This produces slightly different results compared to the original code
# The reason is the previously explained difference between day05 and df_sf94
dead_alive_summary_cum = function(df, days){
  
  output = c()
  
  for (day in days){
  
  dead=(length(unique(df$subjid[(df$day_of_death < day)])))/length(unique(df$subjid))
  alive=(length(unique(df$subjid[(df$day_of_discharge < day)])))/length(unique(df$subjid))
  
  output = cbind(output, c("% dead" = dead,"% alive"= alive) )
  
  }
  output = data.frame(output)
  names(output) = days
  
  return(output)
}


# This gives the proportion of all subjects who were dead/alive on a set of days
# This produces slightly different results compared to the original code
# The reason is the previously explained difference between day05 and df_sf94
dead_alive_summary = function(df, days){
  
  n_subjid = length(unique(df$subjid))
  
  df = dplyr::select(df, starts_with('sf94'))
  
  df_dead = df
  df_dead[df_dead != 0.5] = NA
  df_dead = colSums(df_dead, na.rm = TRUE)/0.5
  
  df_home = df
  df_home[df_home != 4.76] = NA
  df_home = colSums(df_home, na.rm = TRUE)/4.76
  
  df_study = df
  df_study[df_study == 4.76 | df_study == 0.5] = NA
  df_study[!is.na(df_study)] = 1
  df_study = colSums(df_study, na.rm = TRUE)
  
  df_out = rbind(df_dead, df_home, df_study) %>%
    data.frame()
  
  df_out = df_out/n_subjid
  
  rownames(df_out) = c('dead', 'home', 'study')
  names(df_out) = gsub('sf94_', '', names(df_out))
  
  return(df_out)
}


# Calculate C statistic for logistic model with outcome = day 28 mortality, 
# and predictor = sf94 on a given day
calculate_cstat_by_day = function(df, days){
  
  c_stat_out = c()
  
  for (day in days){

  formula = as.formula( paste0("mortality_28 ~ sf94_day", day) )
  model = glm( formula, data=df, family = binomial)
  cstat_uni = cbind("Univariate", day, Cstat(model))
  
  formula = as.formula( paste0("mortality_28 ~ sf94_day0 + sf94_day", day) )
  model = glm( formula, data=df, family=binomial)
  cstat_multi = cbind("Multivariate", day, Cstat(model))
  
  c_stat_out = rbind(c_stat_out, cstat_uni, cstat_multi)
  }
  
  c_stat_out = data.frame(c_stat_out)
  names(c_stat_out) = c('model', 'day', 'C_statistic')
  c_stat_out = mutate(c_stat_out, C_statistic = as.numeric(C_statistic),
                      day = as.numeric(day)) %>%
    arrange(model, day)
  
  return(c_stat_out)
}


########################## Proportionally add sf94 values for dead and discharged ###########################

dead_discharge_proportions = dead_alive_to_add(data, c(5, 8, 10:16))
write.csv(dead_discharge_proportions,"/home/skerr/Git/SF94/Outputs/proportional_numbers.csv")

# Add columns for proportionally add sf94 for those who died or where discharged
df_sf94 = prop_add(df_sf94, dead_discharge_proportions, 5)
df_sf94 = prop_add(df_sf94, dead_discharge_proportions, 8)

# Add columns for who day 0, 5, 8, with proportionally added deaths/discharged
df_sf94 = left_join(df_sf94, df_who %>% dplyr::select(subjid, who_day0, who_day5, who_day8)) %>%
  mutate(who_day5_P = case_when( sf94_day5_P == 4.76 ~ 10,
                                 sf94_day5_P == 0.5 ~ 4,
                                 TRUE ~ who_day5),
         who_day8_P = case_when( sf94_day8_P == 4.76 ~ 10,
                                 sf94_day8_P == 0.5 ~ 4,
                                 TRUE ~ who_day8))

####################### Create subsets that we will work with ###################################

# Apply age filter and supp oxygen filter
subjids_1 = filter(data, ( fio2 >=0.22 & days_since_start %in% c(0,1,2)  & age_estimateyears >19 & age_estimateyears <76 ) ) %>%
  pull(subjid)

data_1 = filter(data, subjid %in% subjids_1)
subset1 = filter(df_sf94, subjid %in% subjids_1) %>%
  data.frame()

#Apply age filter and need for oxygen at day 0
subjids_2 = filter(data, ( fio2 >=0.22 & days_since_start ==0  & age_estimateyears >19 & age_estimateyears <76 ) ) %>%
  pull(subjid)

data_2 = filter(data, subjid %in% subjids_2)
subset2 = filter(df_sf94, subjid %in% pull(data_2, subjid)) %>%
  data.frame()

# Only age filter
subjids_3 = filter(data, (age_estimateyears >19 & age_estimateyears <76 ) ) %>%
  pull(subjid)

data_3 = filter(data, subjid %in% subjids_3)
subset3 = filter(df_sf94, subjid %in% pull(data_3, subjid)) %>%
  data.frame()

################################## Summary statistics #####################################

# Day 5 dead and discharged cumulative summary
write.csv(dead_alive_summary_cum(df_sf94 %>% filter(!is.na(sf94_day5_P)), c(5, 8, 10:16)),
          "/home/skerr/Git/SF94/Outputs/dead_alive_d5.csv")

write.csv(miss_var_summary(df_sf94 %>% filter(!is.na(sf94_day5_P))) ,"/home/skerr/Git/SF94/Outputs/miss_day5.csv")

# Day 8 dead and discharged cumulative summary
write.csv(dead_alive_summary_cum(df_sf94 %>% filter(!is.na(sf94_day8_P)), c(5, 8, 10:16)),
          "/home/skerr/Git/SF94/Outputs/dead_alive_d8.csv")

write.csv(miss_var_summary(df_sf94 %>% filter(!is.na(sf94_day8_P))) ,"/home/skerr/Git/SF94/Outputs/miss_day8.csv")

# Dead and discharged day 0-14 summary, with proportionally added sf94 values for dead and discharged
#
# This produces slightly different results than the original code
# This is because the original misses people who have an sf94 on the day they died or were discharged,
# because of an unfortunate use of a case_when statement
write.csv(dead_alive_summary(df_sf94_dd, 0:14) ,"/home/skerr/Git/SF94/Outputs/table_missingdays.csv")

# Mortality rate at 28 days
mort28 = mean(df_sf94$mortality_28, na.rm = TRUE)
write.csv(mort28,"/home/skerr/Git/SF94/Outputs/mort28.csv")

# Median SF94 between by who severity scale
medians = data_1 %>%
  group_by(severity_scale_ordinal)%>%
  summarise_at("sf94", median, na.rm=T)

write.csv(medians,"/home/skerr/Git/SF94/Outputs/medians.csv")

# Summary of missing values for df_sf94_dd
missing_summary = sapply(df_sf94_dd, function(x) sum(!is.na(x))) %>%
                  rbind(sapply(df_sf94_dd, function(x) sum(is.na(x))) ) %>%
                  t() %>%
                  data.frame() 

write.csv(missing_summary,"/home/skerr/Git/SF94/Outputs/missing_summary.csv")

write.csv(table(df_sf94$mortality_28),"/home/skerr/Git/SF94/Outputs/mort28Table.csv")

# C statistic for day 28 mortality as predicted by sf94, days 1-14
df_cstat = calculate_cstat_by_day(df_sf94, 1:14)

ggplot(df_cstat, aes(x=day, y=C_statistic, colour= model, group=model)) +
  geom_path() +
  scale_x_continuous(breaks = 1:14) + 
  ylab('C statistic')

ggsave(width=13, dpi=300, path = '/home/skerr/Git/SF94/Outputs/', filename="cstat_graphic.pdf")





######################### Sample size calculation with mortality as endpoint ###############################
## INPUTS REQUIRED ##
# power - Desired power
# p1 - Proportion experiencing event within specified time frame in control arm
# p2 - Proportion experiencing event within specified time frame in active arm

# alpha - significance level is taken to be 0.05

calculate_sample_size_mort = function(mort, mort_multiplier){
  
  p1 = mort
  p2 = p1 * mort_multiplier
  
  model = power.prop.test(n=NULL, p1=p1, p2=p2, power=0.8)
  
  n = model$n
  
  sample_size = round(2 * (n/4)*( 1+sqrt( 1+(4/(n*(p1-p2))) ) )^2)
  
  out = c('p1' = p1, 'p2' = p2, 'mortality_multiplier' = mort_multiplier,
          'sample_size' = n, 'continuity_corrected_sample_size' = sample_size)
  
  return(out)
} 

sample_size_mort = rbind(calculate_sample_size_mort( mean(subset1$mortality_28, na.rm = TRUE), 0.85),
                         calculate_sample_size_mort( mean(subset2$mortality_28, na.rm = TRUE), 0.85),
                         calculate_sample_size_mort( mean(subset3$mortality_28, na.rm = TRUE), 0.85)) %>%
  data.frame()

rownames(sample_size_mort) = c('subset1', 'subset2', 'subset3')

write.csv(sample_size_mort,"/home/skerr/Git/SF94/Outputs/sample_size_mortality.csv")

mort_table_function=function(df){
  mortTable = table(df[["mortality_28"]])
  return(mortTable)
}

# Numbers who lived/died in each subset
mort_table = rbind( table(subset1[["mortality_28"]]),
                    table(subset2[["mortality_28"]]),
                    table(subset3[["mortality_28"]])) %>%
  data.frame()

rownames(mort_table) = c('subset1', 'subset2', 'subset3')

write.csv(mort_table, "/home/skerr/Git/SF94/Outputs/mort_table_output.csv")






########### Sample size calculation with mortality endpoint using survival and logrank methods ##########################
# Uses formulae described in Chapter 10 of Modelling Survival Data in Medical Research, 2nd Edition, Collett

## INPUTS REQUIRED ##
# alpha - significance level 
# power - specified power
# HR - Desired hazard ratio for treatment effect
# hazc - baseline hazard in control arm (this is equal to the proportion that would be expected to experience the event 
# during ONE unit of follow-up)
# a - length of recruitment period (defaults to 0 here as patients are to be followed for a set period of time, 
# e.g. 28 days, which means the length of the recruitment period will not affect the median duration of follow-up
# f - length of follow-up (defaults to 28)

calculate_sample_size_lr = function(df, endpoint_var, mort_multiplier, alpha, power, a , f){
  # Calculate the hazard ratio that is equivalent to the risk ratio assumed in the code above for sample size calculations 
  # based on difference in proportions
  p1 = mean(df[, endpoint_var], na.rm = T)
  p2 = p1 * mort_multiplier
  
  HR = (1-exp(log(1-p2)/f))/(1-exp(log(1-p1)/f))
  
  # Now calculate sample size required to detect this HR
  hazc = (1-exp(log(1-p1)/f)) 
  hazt = hazc*HR
  
  d = (4*((qnorm(1-alpha/2)+qnorm(power))^2))/((log(HR))^2)
  
  P_event = 1-(1/6)*(avsurv(f, hazc, hazt) + 4*avsurv(0.5 * a + f, hazc, hazt) + avsurv( a + f, hazc, hazt))
  
  sample_size = d/P_event
  
  out = c('p1' = p1, 'p2' = p2, 'mortality_multiplier' = mort_multiplier, 'hazard_ratio' = HR,
          'sample_size' = sample_size)
  
  return(out)
}

avsurv = function(time, haz1, haz2){
  ((1-haz1)^time + (1-haz2)^time)/2
}


df_mort_lr_sample_size = rbind(calculate_sample_size_lr(subset1, 'mortality_28', 0.85, 0.05, 0.8, 0, 28),
                         calculate_sample_size_lr(subset2, 'mortality_28', 0.85, 0.05, 0.8, 0, 28),
                         calculate_sample_size_lr(subset3, 'mortality_28', 0.85, 0.05, 0.8, 0, 28)) %>%
  data.frame()

rownames(df_mort_lr_sample_size) = c('subset1', 'subset2', 'subset3')

write.csv(df_mort_lr_sample_size, "/home/skerr/Git/SF94/Outputs/mort_logrank_sample_size.csv")





######################### Sample size calculation with opportunistic sf94 as endpoint ###############################
## INPUTS REQUIRED ##
# alpha - significance level =0.05
# power - specified power =0.8
# delta - change in mean sf94
# sd - standard deviation of the outcome measure
# rho - correlation between the outcome measured at baseline and at follow-up

##Summary stats. This is required for some corrections to the sample size calculation
# Mean and SD
calculate_mean_sf94 = function(df, subset_num){
  mean = as.data.frame( rbind(sapply(df[c('sf94_day5_P','sf94_day8_P')], mean, na.rm=T) ) )
  
  names(mean) = c('day5_P', 'day8_P')
  
  return(mean)
}

calculate_sd_sf94 = function(df, subset_num){
  sd = as.data.frame( rbind(sapply(df[c('sf94_day5_P','sf94_day8_P')], sd, na.rm=T)))
  
  names(sd) = c('day5_P', 'day8_P')

  return(sd)
}

df_sf94_mean = rbind( calculate_mean_sf94(subset1, 1),
                      calculate_mean_sf94(subset2, 2),
                      calculate_mean_sf94(subset3, 3)) %>%
  data.frame()

names(df_sf94_mean) = c('day5_P', 'day8_P')
rownames(df_sf94_mean) = c('subset1', 'subset2', 'subset3')

df_sf94_sd = rbind( calculate_sd_sf94(subset1, 1),
                      calculate_sd_sf94(subset2, 2),
                      calculate_sd_sf94(subset3, 3)) %>%
  data.frame()

names(df_sf94_sd) = c('day5_P', 'day8_P')
rownames(df_sf94_sd) = c('subset1', 'subset2', 'subset3')

# Correlation
calculate_correlation_sf94 = function(df) {
  
  correlation_subset_05=subset(df, (!is.na(df[,"sf94_day0"]) & (!is.na(df[,"sf94_day5_P"] ))))
  correlation_subset_08=subset(df, (!is.na(df[,"sf94_day0"]) & (!is.na(df[,"sf94_day8_P"] ))))
  # DAY 0/5
  
  w =  correlation_subset_05[,"sf94_day0"]
  x =  correlation_subset_05[,"sf94_day5_P"]
  day05_cor = cor(w,x)
  
  # DAY 0/8
  y =  correlation_subset_08[,"sf94_day0"]
  z =  correlation_subset_08[,"sf94_day8_P"]
  day08_cor = cor(y,z)
  
  cor_output = rbind(day05_cor, day08_cor)
  return(cor_output)
}

df_sf94_corr = cbind(calculate_correlation_sf94(subset1), 
                         calculate_correlation_sf94(subset2),
                         calculate_correlation_sf94(subset3)) %>%
  t() %>%
  data.frame() 

names(df_sf94_corr) = c('day5_P', 'day8_P')
rownames(df_sf94_corr) = c('subset1', 'subset2', 'subset3')


# Effect size calculator for logistic regression
calculate_effect_size = function(prob_pred, treatment, coef){
  return( mean (log( (treatment*(1-prob_pred)) / (1- treatment * prob_pred)) , na.rm = TRUE ) / coef  )
}

effect_size_boot_sf94 = function(data, indices, day, treatment){
  
  formula = as.formula( paste0('mortality_28 ~ sf94_day', day, '_P + sf94_day0 + age_estimateyears + sex'))
  
  model = glm(formula, data = data[indices,],
               family = binomial)
  
  prob_pred = predict(model, type = 'response')
  
  coef = model$coef[2] # is coefficient on sf94 on day of interest
  
  effect_size = calculate_effect_size(prob_pred, treatment, coef)
  return(effect_size)
} 


# Dataframe of effect sizes for different days and subsets
df_sf94_effect_size = 
  rbind( c(effect_size_boot_sf94(subset1, 1:nrow(subset1), 5, 0.85),  effect_size_boot_sf94(subset1, 1:nrow(subset1), 8, 0.85) ),
       c(effect_size_boot_sf94(subset2, 1:nrow(subset2), 5, 0.85), effect_size_boot_sf94(subset2, 1:nrow(subset2), 8, 0.85) ),
       c( effect_size_boot_sf94(subset3, 1:nrow(subset3), 5, 0.85), effect_size_boot_sf94(subset3, 1:nrow(subset3), 8, 0.85) ) ) %>%
  data.frame()

names(df_sf94_effect_size) = c('day5_P', 'day8_P')
rownames(df_sf94_effect_size) = c('subset1', 'subset2', 'subset3')


calculate_sample_size_sf94 = function(alpha, power, delta, sd, rho){
  
  # Calculate sample size for a t test
  power = power.t.test(n=NULL, delta = delta, sd = sd, power = power, sig.level = alpha) 
  
  # Apply ANCOVA correction
  sample_size = 2*round(((1-(rho^2))*power$n)) 
  return(sample_size)
}

df_sf94_sample_size = rbind(
                    # subset1 
                    c( calculate_sample_size_sf94(0.05 ,0.8 , df_sf94_effect_size['subset1', 'day5_P'],
                                                df_sf94_sd['subset1', 'day5_P'],
                                                df_sf94_corr['subset1', 'day5_P']),
                         
                         calculate_sample_size_sf94(0.05 ,0.8 , df_sf94_effect_size['subset1', 'day8_P'],
                                    df_sf94_sd['subset1', 'day8_P'],
                                    df_sf94_corr['subset1', 'day8_P']) ),
                    # subset2
                    c( calculate_sample_size_sf94(0.05 ,0.8 , df_sf94_effect_size['subset2', 'day5_P'],
                                    df_sf94_sd['subset2', 'day5_P'],
                                    df_sf94_corr['subset2', 'day5_P']),
                         
                         calculate_sample_size_sf94(0.05 ,0.8 , df_sf94_effect_size['subset2', 'day8_P'],
                                    df_sf94_sd['subset2', 'day8_P'],
                                    df_sf94_corr['subset2', 'day8_P']) ),
                    # subset2 
                    c( calculate_sample_size_sf94(0.05 ,0.8 , df_sf94_effect_size['subset3', 'day5_P'],
                                    df_sf94_sd['subset3', 'day5_P'],
                                    df_sf94_corr['subset3', 'day5_P']),
                         
                         calculate_sample_size_sf94(0.05 ,0.8 , df_sf94_effect_size['subset3', 'day8_P'],
                                    df_sf94_sd['subset3', 'day8_P'],
                                    df_sf94_corr['subset3', 'day8_P']) )   ) %>%
  data.frame()
                         
names(df_sf94_sample_size) = c('day5_P', 'day8_P')
rownames(df_sf94_sample_size) = c('subset1', 'subset2', 'subset3')                                       
                                    

# Write out
write.csv(df_sf94_mean,"/home/skerr/Git/SF94/Outputs/sf94_means.csv")
write.csv(df_sf94_sd,"/home/skerr/Git/SF94/Outputs/sf94_sd.csv")
write.csv(df_sf94_corr,"/home/skerr/Git/SF94/Outputs/sf94_correlation.csv")
write.csv(df_sf94_effect_size,"/home/skerr/Git/SF94/Outputs/sf94_effect_size.csv")
write.csv(df_sf94_sample_size,"/home/skerr/Git/SF94/Outputs/sf94_sample_size.csv")







######################### Sample size calculation with WHO severity scale as endpoint ###############################
## INPUTS REQUIRED ##
# alpha - significance level (=0.05)
# power - specified power (=0.8)
# OR - the odds ratio to be able to detect
# pavg  - a vector of probabilities of being in each category of the ordinal scale (averaged over the two treatment groups), needs to sum to one
# OR = this will need to be estimated from the models once finalised, but I have assumed a 15% reduction in odds for the moment 
# n = number of patients with non-missing values of WHO day 5 in the info I have
# We can only estimate the vector of probabilities for control group, denoted by p1 (the numbers in each WHO category at day 5 have been taken from the info I have)

# Effect size calculator for odds ratio associated with a reduction in mortality given by treatment
calculate_effect_size_who = function(prob_pred, treatment){
  mean_prob = mean(prob_pred, na.rm = TRUE)
  return(  (treatment*(1-mean_prob)) / (1- treatment * mean_prob) )  
}

effect_size_boot_who = function(df, indices, day, treatment){
  
  # Make who ordinal scale a factor
  var = paste0('who_day', day, '_P')
  df[ ,var]= factor(df[ , var])
  
  formula = as.formula( paste0(var, '~ age_estimateyears + sex') )
  
  model = polr(formula, data = df[indices, ], Hess=T)
  
  prob_pred = predict(model, type = 'probs')
  
  effect_size = calculate_effect_size_who(prob_pred[,"10"], treatment)
  return(effect_size)
} 

# Dataframe of effect sizes for different days and subsets
df_who_effect_size = 
  rbind( c(effect_size_boot_who(subset1, 1:nrow(subset1), 5, 0.85),  effect_size_boot_who(subset1, 1:nrow(subset1), 8, 0.85) ),
         c(effect_size_boot_who(subset2, 1:nrow(subset2), 5, 0.85), effect_size_boot_who(subset2, 1:nrow(subset2), 8, 0.85) ),
         c( effect_size_boot_who(subset3, 1:nrow(subset3), 5, 0.85), effect_size_boot_who(subset3, 1:nrow(subset3), 8, 0.85) ) ) %>%
  data.frame()

names(df_who_effect_size) = c('day5_P', 'day8_P')
rownames(df_who_effect_size) = c('subset1', 'subset2', 'subset3')    

# Number of people in each WHO category
create_who_table = function(df){
  whoTable = rbind( table(df[["who_day5_P"]]), table(df[["who_day8_P"]]))
  return(whoTable)
}

who_table =rbind(create_who_table(subset1), create_who_table(subset2), create_who_table(subset3)) %>%
  data.frame()

names(who_table) = 4:10
rownames(who_table) = c( 'subset1_day5_P', 'subset1_day8_P',
                         'subset2_day5_P', 'subset2_day8_P',
                         'subset3_day5_P', 'subset4_day8_P')


# Proportions of people in each WHO category
create_who_prop_table = function(df){
  
  p1_day5 =table(df[,"who_day5_P"])/sum(!is.na(df[,"who_day5_P"]))
  p1_day8 =table(df[,"who_day8_P"])/sum(!is.na(df[,"who_day8_P"]))

  return( rbind(p1_day5, p1_day8))
}

who_prop_table = rbind(create_who_prop_table(subset1),
                       create_who_prop_table(subset2),
                       create_who_prop_table(subset3)) %>%
  data.frame()

names(who_prop_table) = 4:10
rownames(who_prop_table) = c( 'subset1_day5_P', 'subset1_day8_P',
                         'subset2_day5_P', 'subset2_day8_P',
                         'subset3_day5_P', 'subset3_day8_P')


calculate_sample_size_who = function(alpha, power, OR, p1){
  # Documentation here: https://www.rdocumentation.org/packages/Hmisc/versions/4.7-0/topics/popower
  # pomodm creates a new vector of probabilities. The new probability of being in the highest category
  # is such that it has the desired odds ratio compared with the old probability of being in the highest category.
  # The other probabilities are adjusted, possibly according to some algorithm that tries to maintain
  # the proportional odds assumption
  p2 = pomodm(p= as.numeric(as.vector(p1)), odds.ratio=OR)
  
  # As it says in documentation, posamsize requires an average of probabilities of being in each
  # category over the two treatment groups
  pavg = (p1 + p2) / 2
  
  model = posamsize(p=pavg, odds.ratio=OR, alpha=alpha, power=power)

  return(model$n)
}

df_who_sample_size = rbind(
  # subset1 
  c( calculate_sample_size_who(0.05, 0.8, df_who_effect_size['subset1', 'day5_P'], who_prop_table['subset1_day5_P', ]),
     
     calculate_sample_size_who(0.05, 0.8, df_who_effect_size['subset1', 'day8_P'], who_prop_table['subset1_day8_P', ]) ),
     
  # subset2
  c( calculate_sample_size_who(0.05, 0.8, df_who_effect_size['subset2', 'day5_P'], who_prop_table['subset2_day5_P', ]),
     
     calculate_sample_size_who(0.05, 0.8, df_who_effect_size['subset2', 'day8_P'], who_prop_table['subset2_day8_P', ]) ),
     
  # subset3 
  c( calculate_sample_size_who(0.05, 0.8, df_who_effect_size['subset3', 'day5_P'], who_prop_table['subset3_day5_P', ]),
     
     calculate_sample_size_who(0.05, 0.8, df_who_effect_size['subset3', 'day8_P'], who_prop_table['subset3_day8_P', ]) )
  ) %>%
  data.frame()

names(df_who_sample_size) = c('day5_P', 'day8_P')
rownames(df_who_sample_size) = c('subset1', 'subset2', 'subset3')       

# Write out
write.csv(who_table,"/home/skerr/Git/SF94/Outputs/who_table_output.csv")
write.csv(who_prop_table,"/home/skerr/Git/SF94/Outputs/who_prop_output.csv")
write.csv(df_who_effect_size, "/home/skerr/Git/SF94/Outputs/who_effect_size.csv")
write.csv(df_who_sample_size,"/home/skerr/Git/SF94/Outputs/who_sample_size.csv")






############ Sample size calculation with sustained 1/2 level WHO improvement as endpoint ############################
## INPUTS REQUIRED ##
# alpha - significance level (=0.05)
# power - specified power (=0.8)
# p1 - proportion of people who had sustained 1/2 level improvement in control group
# effect_size - The number we must add to p1 in order to get the desired treatment effect

effect_size_boot_susimp = function(data, indices, improvement, treatment){
  
  formula = as.formula( paste0('mortality_28 ~ sustained_', improvement, 'L_improvement + age_estimateyears + sex'))
  
  model = glm(formula, data = data[indices,],
              family = binomial)

  prob_pred = predict(model, type = 'response')

  coef = model$coef[2] #is sustained improvement coefficient

  effect_size = calculate_effect_size(prob_pred, treatment, coef)

  return(effect_size)
} 

# Dataframe of effect sizes for different days and subsets
df_susimp_effect_size = 
  rbind( c(effect_size_boot_susimp(subset1, 1:nrow(subset1), 1, 0.85),  effect_size_boot_susimp(subset1, 1:nrow(subset1), 2, 0.85) ),
         c(effect_size_boot_susimp(subset2, 1:nrow(subset2), 1, 0.85), effect_size_boot_susimp(subset2, 1:nrow(subset2), 2, 0.85) ),
         c( effect_size_boot_susimp(subset3, 1:nrow(subset3), 1, 0.85), effect_size_boot_susimp(subset3, 1:nrow(subset3), 2, 0.85) ) ) %>%
  data.frame()

names(df_susimp_effect_size) = c('sustained_1L_improvement', 'sustained_2L_improvement')
rownames(df_susimp_effect_size) = c('subset1', 'subset2', 'subset3')

# Numbers of people who had sustained 1/2 level improvement
create_sus_imp_table = function(subset_df){
  susimp_1L=table(subset_df$sustained_1L_improvement)
  susimp_2L=table(subset_df$sustained_2L_improvement)

  return( cbind(susimp_1L, susimp_2L))
}

sus_imp_table = rbind(create_sus_imp_table(subset1), 
                      create_sus_imp_table(subset2),
                      create_sus_imp_table(subset3)) %>%
  data.frame()

names(sus_imp_table) = c('sustained_1L_improvement', 'sustained_2L_improvement')
rownames(sus_imp_table) = c('subset1_FALSE', 'subset1_TRUE',
                            'subset2_FALSE', 'subset2_TRUE',
                            'subset3_FALSE', 'subset3_TRUE')  

# Proportions of people who had sustained 1/2 level improvement
create_susimp_prop_table = function(df){
  
  p1 = mean(df$sustained_1L_improvement, na.rm = TRUE)
  p2 = mean(df$sustained_2L_improvement, na.rm = TRUE)
  
  return( cbind(p1, p2))
}

susimp_prop_table = rbind(create_susimp_prop_table(subset1),
                     create_susimp_prop_table(subset2),
                     create_susimp_prop_table(subset3)) %>%
  data.frame()

names(susimp_prop_table) = c('sustained_1L_improvement', 'sustained_2L_improvement')
rownames(susimp_prop_table) = c('subset1', 'subset2', 'subset3')


calculate_sample_size_susimp = function(power, effect_size, p1){
  
  # Note that the effect size gets added to p1
  # This is because in the case where the predictor is binary, like e.g. sustained_1L_improvement,
  # the effect size is the increase in the mean number of people who must have a sustained 1L improvement
  # in order to get the desired effect on mortality.
  p2 = p1 + effect_size
  
  # Calculate sample size for a t test
  sample_size = power.prop.test(power = power, p1 = p1, p2 = p2) 

  return(sample_size$n)
}


df_susimp_sample_size = rbind(
  # subset1 
  c( calculate_sample_size_susimp( 0.8, df_susimp_effect_size['subset1', 'sustained_1L_improvement'], 
                                   susimp_prop_table['subset1', 'sustained_1L_improvement']),
     
     calculate_sample_size_susimp( 0.8, df_susimp_effect_size['subset1', 'sustained_2L_improvement'], 
                                   susimp_prop_table['subset1', 'sustained_2L_improvement']) ),
    
  # subset2
  c( calculate_sample_size_susimp( 0.8, df_susimp_effect_size['subset2', 'sustained_1L_improvement'], 
                                susimp_prop_table['subset2', 'sustained_1L_improvement']),
  
  calculate_sample_size_susimp( 0.8, df_susimp_effect_size['subset2', 'sustained_2L_improvement'], 
                                susimp_prop_table['subset2', 'sustained_2L_improvement']) ),

  # subset3 
  c( calculate_sample_size_susimp( 0.8, df_susimp_effect_size['subset3', 'sustained_1L_improvement'], 
                                susimp_prop_table['subset3', 'sustained_1L_improvement']),
  
  calculate_sample_size_susimp( 0.8, df_susimp_effect_size['subset3', 'sustained_2L_improvement'], 
                                susimp_prop_table['subset3', 'sustained_2L_improvement']) )
  ) %>%
  data.frame()

names(df_susimp_sample_size) = c('sustained_1L_improvement', 'sustained_2L_improvement')
rownames(df_susimp_sample_size) = c('subset1', 'subset2', 'subset3')  


# Write out
write.csv(sus_imp_table,"/home/skerr/Git/SF94/Outputs/sus_imp_output.csv")
write.csv(susimp_prop_table,"/home/skerr/Git/SF94/Outputs/susimp_prop_output.csv")
write.csv(df_susimp_effect_size, "/home/skerr/Git/SF94/Outputs/susimp_effect_size.csv")
write.csv(df_susimp_sample_size, "/home/skerr/Git/SF94/Outputs/susimp_sample_size.csv")






#### Sample size calculation with sustained 1/2 level improvement endpoint using survival and logrank methods ##############
# This uses all the same functions as when mortality is the endpoint

## INPUTS REQUIRED ##
# alpha - significance level 
# power - specified power
# HR - assumed rate ratio for treatment effect
# hazc - baseline hazard in control arm (this is equal to the proportion that would be expected to experience the event during ONE unit of follow-up)
# a - length of recruitment period (defaults to 0 here as patients are to be followed for a set period of time, e.g. 28 days, which means the length of the recruitment period will not affect the median duration of follow-up
# f - length of follow-up (defaults to 28)

# 1 level sustained improvement
df_susimp1_lr_sample_size = rbind(calculate_sample_size_lr(subset1, 'sustained_1L_improvement', 0.85, 0.05, 0.8, 0, 28),
                          calculate_sample_size_lr(subset2, 'sustained_1L_improvement', 0.85, 0.05, 0.8, 0, 28),
                          calculate_sample_size_lr(subset3, 'sustained_1L_improvement', 0.85, 0.05, 0.8, 0, 28)) %>%
  data.frame()

rownames(df_susimp1_lr_sample_size) = c('subset1', 'subset2', 'subset3')

write.csv(df_susimp1_lr_sample_size, "/home/skerr/Git/SF94/Outputs/mort_logrank_susimp1_sample_size.csv")

# 2 level sustained improvement
df_susimp2_lr_sample_size = rbind(calculate_sample_size_lr(subset1, 'sustained_2L_improvement', 0.85, 0.05, 0.8, 0, 28),
                                  calculate_sample_size_lr(subset2, 'sustained_2L_improvement', 0.85, 0.05, 0.8, 0, 28),
                                  calculate_sample_size_lr(subset3, 'sustained_2L_improvement', 0.85, 0.05, 0.8, 0, 28)) %>%
  data.frame()

rownames(df_susimp2_lr_sample_size) = c('subset1', 'subset2', 'subset3')

write.csv(df_susimp2_lr_sample_size, "/home/skerr/Git/SF94/Outputs/mort_logrank_susimp2_sample_size.csv")










############ Sample size calculation with protocolised day 5 sf94 measurement as endpoint ############################

# Estimate model to get coefficients for opportunistic measurement
model = glm( mortality_28 ~ sf94_day5_P + sf94_day0 + age_estimateyears + sex, data=subset1, family = binomial)

# Mean and standard deviation of opportunistic measurements
mean_opp = df_sf94_mean['subset1', 'day5_P']
sd_opp = df_sf94_sd['subset1', 'day5_P']

# Calculate correlation between sf94 day 5 and sf94 day 0.
rho = cor(subset1$sf94_day5_P,  subset1$sf94_day0, use = 'complete.obs')

# Assume that protocolised measurements have 0.8* standard deviation of opportunistic measurements,
# and there is 0.7 correlation between them
mean_prot = mean_opp
sd_prot = 0.8 * sd_opp
rho_opp_prot = 0.7

# This creates coefficients for protocolised measurement.
create_coef_prot = function(mean_prot, sd_prot, rho_opp_prot, coef_opp){
  
  alpha0 = mean_opp - rho_opp_prot * sd_opp * mean_prot / sd_prot
  
  alpha1 = rho_opp_prot * sd_opp / sd_prot
  
  coef_prot = coef_opp
  
  coef_prot[1] = coef_opp[1] + alpha0 * coef_opp[1]
  
  coef_prot[2] = alpha1 * coef_opp[2]
  
  return(coef_prot)
}

# Logistic function
logistic = function(x, coef){
  
  return( 1/(1 + exp( - as.numeric(coef[1])  - as.matrix(x) %*% as.numeric(as.vector(coef[-1]))  ) ) )
}  

# Make dataframe of protocolised coefficients
df_coef_prot = rbind( create_coef_prot(mean_prot, sd_prot, 0.5, model$coef),
                      create_coef_prot(mean_prot, sd_prot, 0.6, model$coef),
                      create_coef_prot(mean_prot, sd_prot, 0.7, model$coef),
                      create_coef_prot(mean_prot, sd_prot, 0.8, model$coef),
                      create_coef_prot(mean_prot, sd_prot, 0.9, model$coef)) %>%
  data.frame() %>%
  dplyr::rename(intercept = X.Intercept., sex = sexFemale)

df_coef_prot['rho'] = seq(0.5, 0.9, 0.1)


effect_size_boot_sf94_prot = function(data, indices, treatment, sd_mult, rho_opp_prot){
  
  df = data[indices, ]
  
  model = glm( mortality_28 ~ sf94_day5_P + sf94_day0 + age_estimateyears + sex, data=df, family = binomial)
  
  # Mean and standard deviation of opportunistic measurements
  mean_opp = mean(df$sf94_day5_P, na.rm = TRUE)
  sd_opp = sd(df$sf94_day5_P, na.rm = TRUE)
  
  # Calculate correlation between sf94 day 5 and sf94 day 0.
  rho = cor(df$sf94_day5_P,  df$sf94_day0, use = 'complete.obs')
  
  # Assume that protocolised measurements have 0.8* standard deviation of opportunistic measurements,
  # and there is 0.7 correlation between them
  mean_prot = mean_opp
  sd_prot = sd_mult * sd_opp
  
  coef_prot = create_coef_prot(mean_prot, sd_prot, rho_opp_prot, model$coef)
  
  # Turn sex into binary with female = 1 for use in logistic function
  df$sex = as.numeric(as.factor(df$sex)) - 1
  
  predictor_cols = c('sf94_day5_P', 'sf94_day0', 'age_estimateyears', 'sex')
  
  prob_pred = logistic( df[predictor_cols], coef_prot)
  
  effect_size = calculate_effect_size(prob_pred, treatment, coef_prot[2])
  
  return(effect_size)
} 


# Calculate effect sizes
sf94_prot_effect_size = 
 c( effect_size_boot_sf94_prot(subset1, 1:nrow(subset1), treatment = 0.85, sd_mult = 0.8, rho_opp_prot = 0.5),
    effect_size_boot_sf94_prot(subset1, 1:nrow(subset1), treatment = 0.85, sd_mult = 0.8, rho_opp_prot = 0.6),
    effect_size_boot_sf94_prot(subset1, 1:nrow(subset1), treatment = 0.85, sd_mult = 0.8, rho_opp_prot = 0.7),
    effect_size_boot_sf94_prot(subset1, 1:nrow(subset1), treatment = 0.85, sd_mult = 0.8, rho_opp_prot = 0.8),
    effect_size_boot_sf94_prot(subset1, 1:nrow(subset1), treatment = 0.85, sd_mult = 0.8, rho_opp_prot = 0.9) )

# rho_prot is the correlation between the protocolised sf94 measurements on day 0 and day 5
# The proof of this follows from the assumptions that have been made, along with the
# measurement error model.
# This is explained with mathematical detail in the paper
rho_prot = rho / seq(0.5, 0.9, 0.1)**2

calculate_sample_size_sf94_vec = Vectorize(calculate_sample_size_sf94)

# Calculate sample sizes
sf94_prot_sample_size = calculate_sample_size_sf94_vec(0.05 , 0.8, sf94_prot_effect_size, sd_prot, rho_prot) 

# Dataframe of results
df_sf94_prot_effect_sample_size = data.frame(rho = seq(0.5, 0.9, 0.1),
                                        rho_prot = rho_prot,
                                        effect_size = sf94_prot_effect_size,
                                        sample_size = sf94_prot_sample_size )


# Write out
write.csv(df_coef_prot, "/home/skerr/Git/SF94/Outputs/sf94_prot_coef.csv")
write.csv(df_sf94_prot_effect_sample_size, "/home/skerr/Git/SF94/Outputs/sf94_prot_effect_sample_size.csv")


