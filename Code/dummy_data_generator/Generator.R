###################################################################### 

## Code author: Steven Kerr

## Description: 

# This file generates a simulated dfset for the sf94 project
# The original df is CCP. The variables in the simulated dfset are in the variable cols below
# The basic strategy is to use multiple imputation by chained equations (MICE) in 2 steps. 
# I add n rows that are empty except for having a subject id, and days_since_admission = 8.
# I then use MICE to impute the values of variables that are constant in time for the period of the dfset, e.g. comorbidities
# Then each simulated subjid gets 2 news rows of df, corresponding to days_since_admission = 0, 5.
# A second use of MICE fills in values for sao2, severity_ordinal_scale and sf94.

###################################################################### 

library(dplyr)
library(ggplot2)
library(mice)
library(data.table)
library(stringr)

################################### CURATE VARIABLE SELECTION #################################################

# Varaibles that are constant over study period
constVars <- c('subjid', 'sex', 'age_estimateyears','day_of_death', 'death', 'chrincard',
               'hypertension_mhyn', 'chronicpul_mhyn', 'renal_mhyn', 'chronicneu_mhyn', 'malignantneo_mhyn', 
              'chronichaemo_mhyn', 'obesity_mhyn', 'dementia_mhyn', 'clinical_frailty', 'diabetes', 'liver')

#Variables that are not constant over study period
nonConstVars <- c('days_since_start', 'fio2', 'sao2', 'sfr','daily_invasive_prtrt','daily_inotrope_cmyn',
                  'daily_ecmo_prtrt', 'daily_rrt_cmtrt', 'daily_nasaloxy_cmtrt')

#
allVars <- c(constVars, nonConstVars)

# catVars is a list of variables that are categorical.
catVars <- c('sex', 'death', 'chrincard', 'hypertension_mhyn','chronicpul_mhyn', 'renal_mhyn',  'chronicneu_mhyn', 
             'malignantneo_mhyn', 'chronichaemo_mhyn', 'obesity_mhyn', 'dementia_mhyn', 'clinical_frailty', 'diabetes', 'liver',
             'daily_invasive_prtrt', 'daily_inotrope_cmyn', 'daily_ecmo_prtrt', 'daily_rrt_cmtrt', 'daily_nasaloxy_cmtrt')

##################################### IMPORT DATA: ######################################################

df<- fread("/home/skerr/Data/ccp_subset_derived.csv", select = allVars, data.table = FALSE)

# Take a sample
sample <- sample_n(df, 1000)              

############################# FUNCTIONS ######################################### 

# freqTab prints frequency tables for the imputations and the real df against each other for the categorical variables
# to allow comparison.
freqTab <- function(Imputation){
  
  for (var in catVars){
    real <- table(fulldf[var])
    
    nlevels <- length(real)
    
    freqTable <- data.frame( matrix(ncol = Imputation$m +1, nrow = nlevels) )
    
    colnames(freqTable) <- append( seq(1, Imputation$m, 1), 'real')
    
    rownames(freqTable) <- seq(0, nlevels-1, 1)
    
    for (m in 1:Imputation$m ){
      tab <- table(as.data.frame(Imputation$imp[var])[,m] )
      
      freqTable[, m] <- tab / sum(tab)
      
      freqTable[, 'real'] <- table(fulldf[var]) / sum( table(fulldf[var]) )
      
    }
    print(var)
    print(freqTable)
  }
}

# This function is used to set the method option in mice manually. 
createMethod <- function(impExclude){
  
  method <- as.data.frame( sapply(df, nlevels) )
  
  method[ method[,1] > 2, ] <- 'polyreg'
  
  method[ impExclude, ] <- ''
  
  method[ method[,1] ==0 , ] <- 'pmm'
  
  method[ method[,1] ==2 , ] <- 'logreg'
  
  method <- dplyr::pull(method, colnames(method) )
}

# This function adds rows for other values of days_since_start for any subjid that is 
# part of the simulated data
addRows <- function(df){
  
  rowNumbers <- which(  str_sub(df$subjid ,1, 9) == 'Simulated')
  
  for(i in rowNumbers   ){
    df[i, 'days_since_start'] <- 0
    
    row <-  df[i,   ] 
    if( is.na(row['day_of_death']) |  ( !is.na(row['day_of_death']) &   row['day_of_death'] >= 5)  ){
      newrow <- row
      
      newrow['days_since_start'] <- 5
      
      df <- rbind(df, newrow)
    }
    
    if( is.na(row['day_of_death']) |  ( !is.na(row['day_of_death']) &   row['day_of_death'] >= 8)   ){
      
      newrow <- row
      
      newrow['days_since_start'] <- 8
      
      df <- rbind(df, newrow)
    }
  }
  rownames(df) <- 1:nrow(df)
  return(df)  
}

##########################################################################################

# Create a dataframe for simulated df
# n is the size of the simulated dataset
n <- 100

simulated <- do.call(data.frame, replicate( length(df), rep(NA, n), simplify=FALSE))

colnames(simulated ) <- colnames(df)

simulated ['subjid'] <- paste( 'Simulated', rownames(simulated ), sep= ' ')

#Combine real and simulated df
fulldf <- rbind(sample, simulated)

# mice needs categorical varaiables to be factors
fulldf[catVars] <- lapply(fulldf[catVars], factor )

################################ IMPUTE CONSTANT VARIABLES: #######################################

# Variables that will not be imputed in first imputation
impExclude <- c(nonConstVars, 'subjid', 'day_of_death')

# Create a 'template' preditor matrix.
# I only use the first row of df, which will throw warnings. Just a quick way to get a matrix with the
# requisite rows and columns.
template <- quickpred(df[1,], mincor = -1)

template[,] <- 1

diag(template) <- 0

# Impute sex, age, death and comorbidities, 
method <- createMethod(impExclude)

predMat <- template
predMat[, impExclude] <- 0

Imputation <- mice(fulldf ,m=1, maxit=100, predictorMatrix = predMat, method = method)

# Look at results of Imputation

pdf('/home/skerr/Git/SF94/Code/dummy_data_generator/Imputation.pdf') 

densityplot(Imputation, fulldf ~ age_estimateyears)

dev.off()

freqTab(Imputation)

# Set fulldf equal to the complete imputation
fulldf1 <- complete(Imputation)

#Separate out those who have death == YES from those that don't
dead <- filter( fulldf1, death == 'YES' )

notDead <- filter( fulldf1, death != 'YES' )

############################### IMPUTE THE DEAD: ###############################################

# Impute day of death amongst those who died
impExclude2 <- setdiff(allVars, 'day_of_death')

method2 <- createMethod(impExclude2)

predMat2 <- template
predMat2[, nonConstVars] <- 0

Imputation2 <- mice(dead ,m=1, maxit=100, predictorMatrix = predMat2, method = method2)



# Look at results of Imputation

pdf('/home/skerr/Git/SF94/Code/dummy_data_generator/Imputation2.pdf') 

densityplot(Imputation2, dead ~ day_of_death)

dev.off()

# Set dead2 equal to complete result of Imputation2.
# Add rows for differetnt values of days_since_start
dead2 <- complete(Imputation2)

dead3 <- addRows(dead2)

dead3 <- mutate_at(dead3, 'severity_scale_ordinal', ~case_when(  day_of_death == days_since_start ~ as.integer(10),
                                                                       TRUE ~ severity_scale_ordinal))


# Impute non constant variables amongst those who died
impExclude3 <- c(constVars, 'days_since_start', 'sfr')

method3 <- createMethod(impExclude3)

predMat3 <- template
predMat3[, 'sfr'] <- 0

Imputation3 <- mice(dead3,m=1, maxit=100, predictorMatrix = predMat3, method = method3)



# Look at results of Imputation

pdf('/home/skerr/Git/SF94/Code/dummy_data_generator/Imputation2.pdf') 

densityplot(Imputation3, dead ~ fio2 + sao2)

dev.off()

freqTab(Imputation3)

dead4 <- complete(Imputation3)


############################ IMPUTE THE LIVING: ##########################################

notDead2 <- addRows(notDead)



impExclude4 <- c(constantVars, 'days_since_start', 'sfr')

method4 <- createMethod(impExclude4)

predMat4 <- quickpred(df, mincor = -1 )
predMat4['sfr'] <- 0

Imputation4 <- mice(notDead2 ,m=1,maxit=100, predictorMatrix = predMat4, method = method4)


pdf( paste( root, 'stevenkerr/Git/SF94/Code/dummy_df_generator/Imputation2.pdf', sep= "") )

densityplot(Imputation2, fulldf2 ~ severity_scale_ordinal + sao2 + sf94)

dev.off()



fulldf3 <- complete(Imputation2)

fulldf3[  fulldf3['death']=='NO',  ]['day_of_death'] <- NA

sapply(fulldf3['severity_scale_ordinal'], class)

fulldf3 <- mutate(fulldf3, severity_scale_ordinal = case_when(  day_of_death == days_since_start ~ 10,
                                                                TRUE ~ severity_scale_ordinal))













df<-df %>% 
  mutate(
    severity_scale_ordinal = case_when(
      day_of_death == days_since_start ~ 10,
      daily_invasive_prtrt == "YES" & sfr <=2.0 & 
        (daily_inotrope_cmyn == "YES"|daily_ecmo_prtrt == "YES" |daily_rrt_cmtrt == "YES") ~ 9,
      daily_invasive_prtrt == "YES" & (sfr <=2.0|daily_inotrope_cmyn == "YES" ) ~ 8,
      daily_invasive_prtrt == "YES" & sfr >2.0 ~ 7,
      daily_noninvasive_prtrt == "YES" ~ 6,
      fio2 >= 0.41 ~ 6,
      fio2 >= 0.22 & fio2 <=0.40 ~ 5,
      daily_nasaloxy_cmtrt == "YES" ~ 5,
      fio2 == 0.21 ~ 4))












