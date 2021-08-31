###################################################################### 

## Code author: Steven Kerr

## Description: 
#   Post processing of simulated ccp data to create a minimal dataset
# that is efficient for use in the web API

###################################################################### 

library(data.table)
library(tidyr)
library(dplyr)


df<- fread("/home/skerr/Data/ccp_subset_simulated.csv", data.table = FALSE)

#Create SF value and SF94 value
df$sfr<- df$sao2/df$fio2

df <- mutate(df, sf94 = case_when(sao2 <= 0.94  | fio2 == 0.21 ~ sfr))  

# Add respiratory support column
df <- mutate(df,  respiratory_support = case_when(
  daily_invasive_prtrt == 'YES'   ~ 'IMV',
  daily_noninvasive_prtrt == 'YES'    ~ 'NIV',
  daily_nasaloxy_cmtrt == 'YES'  ~ 'HFNC',
  TRUE ~ 'None') )

# Add sf94 day 0,5,8 columns
df[which( df['days_since_start'] == 0 ), 'sf94_day0' ] <- df[ which( df['days_since_start'] == 0 ), 'sf94']

df[which( df['days_since_start'] == 5 ), 'sf94_day5' ] <- df[ which( df['days_since_start'] == 5 ), 'sf94']

df[which( df['days_since_start'] == 8 ), 'sf94_day8' ] <- df[ which( df['days_since_start'] == 8 ), 'sf94']


df <- df %>% group_by(subjid) %>% fill( c('sf94_day0', 'sf94_day5', 'sf94_day8'  ), .direction = 'downup')


# Add who day 0,5,8 columns
df[which( df['days_since_start'] == 0 ), 'who_day0' ] <- df[ which( df['days_since_start'] == 0 ), 'severity_scale_ordinal']

df[which( df['days_since_start'] == 5 ), 'who_day5' ] <- df[ which( df['days_since_start'] == 5 ), 'severity_scale_ordinal']

df[which( df['days_since_start'] == 8 ), 'who_day8' ] <- df[ which( df['days_since_start'] == 8 ), 'severity_scale_ordinal']


df <- df %>% group_by(subjid) %>% fill( c('who_day0', 'who_day5', 'who_day8'  ), .direction = 'downup')



df <- filter(df, days_since_start == 0)

# Add (day 5 - day 0) and (day 8 - day 0) difference variables for sf94
df$sf94_delta_05 <- df$sf94_day5 - df$sf94_day0

df$sf94_delta_08 <- df$sf94_day8 - df$sf94_day0

# Add mortality variable for day28
df <- mutate(df, day28_mortality = case_when(day_of_death <= 28 ~ "YES",
                                                                   TRUE ~ 'NO' ))

# Write on argosafe
write.csv(df ,"/home/skerr/Data/ccp_subset_simulated_post.csv", row.names = FALSE)

