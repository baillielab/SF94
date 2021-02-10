library(dplyr)
library(tidyr)
library(data.table)
library(readr) # loaded with tidyverse anyway
library(gridExtra)
library(ggplot2)
library(grid)
library(gtable)

df_1<-fread("/home/skerr/df_1_20210902.csv", data.table = FALSE)

cols <- c('daily_invasive_prtrt', 'daily_noninvasive_prtrt', 'daily_nasaloxy_cmtrt')

df_1[cols][  df_1[cols] == 'N/A'  ] <- NA

df_1['obesity_mhyn'][   df_1['obesity_mhyn'] == 'Unknown' ] <- NA

df_1 <- mutate(df_1,  respiratory_support = case_when(
  daily_invasive_prtrt == 'YES'   ~ 'IMV',
  daily_noninvasive_prtrt == 'YES'    ~ 'NIV',
  daily_nasaloxy_cmtrt == 'YES'  ~ 'Oxygen' ) )


# Criterion numbers:
# 1: daily_crp_lborres > 75
# 2:(sao2 <= 0.92 & fio2 == 0.21) OR respiratory_support = 'Oxygen'
# 3: CRP > 75 & (  (sao2 <= 0.92 & fio2 == 0.21)   OR   respiratory_support = 'Oxygen')
# 4: CRP > 75 & respiratory_support == 'NIV'
# 5: CRP > 75 & respiratory_support == 'IMV'


query <- function(df, time, condition){
  
  if(time == 'all'){
    Subset <- df
  } else {
    Subset <- filter( df, days_since_admission == time )
  }
  
  total_patients <-  length( unique(Subset$subjid)) 
  
  if(condition == 1){
    
    Subset <- mutate(Subset, condition = daily_crp_lborres > 75)
  } else if(condition ==2){
    
    Subset <- mutate(Subset, condition = (sao2 <= 0.92 & fio2 == 0.21) | respiratory_support == 'Oxygen')
  } else if(condition == 3){
    
    Subset <- mutate(Subset, condition = daily_crp_lborres > 75 & ( (sao2 <= 0.92 & fio2 == 0.21) | respiratory_support == 'Oxygen') ) 
  } else if(condition == 4){
    
    Subset <- mutate(Subset, condition = daily_crp_lborres > 75 & ( (sao2 <= 0.92 & fio2 == 0.21) | respiratory_support == 'NIV') ) 
  } else if(condition == 5){
    
    Subset <- mutate(Subset, condition = daily_crp_lborres > 75 & ( (sao2 <= 0.92 & fio2 == 0.21) | respiratory_support == 'IMV') ) 
  }
  
  number_present =   length( unique ( filter(Subset, !is.na(condition) )$subjid   ) )
  
  number_satifying_condition <- length( unique ( filter(Subset, condition == TRUE )$subjid   ) )
  
  number_obese <-  length( unique(  filter(Subset, condition==TRUE & obesity_mhyn == 'YES')$subjid  ) ) 
  
  meanCRP <- mean( filter(Subset, condition == TRUE )$daily_crp_lborres, na.rm = TRUE)
  
  sdCRP <- sd(  filter(Subset, condition == TRUE )$daily_crp_lborres, na.rm = TRUE )
  
  output <- c( total_patients, number_present, number_satifying_condition, number_obese, meanCRP, sdCRP)
  
  return(output)
  
}


createTable <- function(time){
  
  df <- data.frame(matrix(ncol = 6, nrow = 0))
  
  for(condition in 1:5 ){
    
    df[condition, ] <- query(df_1, time, condition)
    
  }
  
  colnames(df) <- c( '#Patients', '#NotNA', '#Satisfying condition', '#Obese', 'mean CRP', 'sd CRP')
  
  df['Condition'] <- c('CRP>75',
                       '(sao2 <= 0.92 & fio2 == 0.21) OR receiving Oxygen therapy)',
                       'CRP > 75 & (  (sao2 <= 0.92 & fio2 == 0.21)   OR receiving Oxygen therapy)',
                       'CRP > 75 & NIV',
                       'CRP > 75 & IMV')
  
  df <- df[, c(7,1,2,3,4,5,6)]
  
  return(df)  
}


# Create tables

table0 <- createTable(0)

tableall <- createTable('all')


#Create table plots

t1 <- tableGrob(table0)

title <- textGrob("Day 0",gp=gpar(fontsize=20))
padding <- unit(5,"mm")

table <- gtable_add_rows(
  t1, 
  heights = grobHeight(title) + padding,
  pos = 0)
table <- gtable_add_grob(
  table, 
  title, 
  1, 1, 1, ncol(table))

grid.newpage()
grid.draw(table)



t2 <- tableGrob(tableall)

title <- textGrob("All days",gp=gpar(fontsize=20))
padding <- unit(5,"mm")

table <- gtable_add_rows(
  t2, 
  heights = grobHeight(title) + padding,
  pos = 0)
table <- gtable_add_grob(
  table, 
  title, 
  1, 1, 1, ncol(table))

grid.newpage()
grid.draw(table)
