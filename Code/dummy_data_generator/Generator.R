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
library(tidyr)

################################### CURATE VARIABLE SELECTION #################################################

# Variables that are constant over study period
constVars <- c('subjid', 'sex', 'age_estimateyears','day_of_death', 'death', 'chrincard',
               'hypertension_mhyn', 'chronicpul_mhyn', 'renal_mhyn', 'chronicneu_mhyn', 'malignantneo_mhyn', 
              'chronichaemo_mhyn', 'obesity_mhyn', 'dementia_mhyn', 'clinical_frailty', 'diabetes', 'liver',
              'who_days_to_improve1', 'who_days_to_improve2')

#Variables that are not constant over study period
nonConstVars <- c('days_since_start', 'fio2', 'sao2','daily_invasive_prtrt', 'daily_noninvasive_prtrt',
                  'daily_inotrope_cmyn','daily_ecmo_prtrt', 'daily_rrt_cmtrt', 'daily_nasaloxy_cmtrt')

# All variables
allVars <- c(constVars, nonConstVars)

# catVars is a list of variables that are categorical.
catVars <- c('sex', 'death', 'chrincard', 'hypertension_mhyn','chronicpul_mhyn', 'renal_mhyn',  'chronicneu_mhyn', 
             'malignantneo_mhyn', 'chronichaemo_mhyn', 'obesity_mhyn', 'dementia_mhyn', 'clinical_frailty', 'diabetes', 'liver',
             'daily_invasive_prtrt', 'daily_noninvasive_prtrt', 'daily_inotrope_cmyn', 'daily_ecmo_prtrt', 'daily_rrt_cmtrt',
             'daily_nasaloxy_cmtrt')

##################################### IMPORT DATA: ######################################################

df<- fread("/home/skerr/Data/ccp_subset_derived.csv", select = allVars, data.table = FALSE)

# Take a sample
sample <- sample_n(df, 30000)              
#sample <- df

############################# FUNCTIONS ######################################### 

# freqTab prints frequency tables for the imputations and the real df against each other for the categorical variables
# to allow comparison.
freqTab <- function(df, Imputation, Vars){
  
  for (var in Vars){
    real <- table(df[var])
    
    nlevels <- length(real)
    
    freqTable <- data.frame( matrix(ncol = Imputation$m +1, nrow = nlevels) )
    
    colnames(freqTable) <- append( seq(1, Imputation$m, 1), 'real')
    
    rownames(freqTable) <- seq(0, nlevels-1, 1)
    
    for (m in 1:Imputation$m ){
      tab <- table(as.data.frame(Imputation$imp[var])[,m] )
      
      freqTable[, m] <- tab / sum(tab)
      
      freqTable[, 'real'] <- real / sum( real )
      }
    print(var)
    print(freqTable)
  }
}

# This function is used to set the method option in mice manually. 
createMethod <- function(df, impExclude){
  
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
  
  rowNumbers <- which(str_sub(df$subjid ,1, 9) == 'Simulated')
  
  for(i in rowNumbers   ){
    df[i, 'days_since_start'] <- 0
    
    row <-  df[i,   ] 
    if( is.na(row['day_of_death']) | (!is.na(row['day_of_death']) &   row['day_of_death'] >= 5)){
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
n <- 1000

simulated <- do.call(data.frame, replicate( length(df), rep(NA, n), simplify=FALSE))

colnames(simulated ) <- colnames(df)

simulated ['subjid'] <- paste( 'Simulated', rownames(simulated ), sep= ' ')

#Combine real and simulated df
fulldf <- rbind(sample, simulated)

# mice needs categorical varaiables to be factors
fulldf[catVars] <- lapply(fulldf[catVars], factor )

# maximum number of iterations to be done in MICE
iterations = 200

################################ IMPUTE CONSTANT VARIABLES: #######################################

# Variables that will not be imputed in first imputation
impExclude1 <- c(nonConstVars, 'subjid', 'day_of_death', 'who_days_to_improve1', 'who_days_to_improve2')

# Create a 'template' preditor matrix.
# I only use the first row of df, which will throw warnings. Just a quick way to get a matrix with the
# requisite rows and columns.
template <- quickpred(df[1,], mincor = -1)

template[,] <- 1
diag(template) <- 0
template[,'subjid'] <- 0

# Impute sex, age, death and comorbidities, 
method1 <- createMethod(fulldf, impExclude1)

predMat1 <- template
predMat1[, impExclude1] <- 0

Imputation1 <- mice(fulldf ,m=1, maxit=iterations, predictorMatrix = predMat1, method = method1)

# Look at results of Imputation

pdf('/home/skerr/Git/SF94/Code/dummy_data_generator/Imputation1.pdf') 

densityplot(Imputation1, ~age_estimateyears)

dev.off()

freqTab(fulldf, Imputation1, catVars)

# Set fulldf equal to the complete imputation
fulldf <- complete(Imputation1)

#Separate out those who have death == YES from those that don't
dead <- filter( fulldf, death == 'YES' )

notDead <- filter( fulldf, death != 'YES' )

############################### IMPUTE THE DEAD: ###############################################

# Impute day of death amongst those who died
impExclude2 <- setdiff(allVars, 'day_of_death')

method2 <- createMethod(dead, impExclude2)

predMat2 <- template
predMat2[, c(nonConstVars, 'who_days_to_improve1', 'who_days_to_improve2'  )] <- 0

Imputation2 <- mice(dead ,m=1, maxit=iterations, predictorMatrix = predMat2, method = method2)

# Look at results of Imputation

pdf('/home/skerr/Git/SF94/Code/dummy_data_generator/Imputation2.pdf') 

densityplot(Imputation2, ~ day_of_death)

dev.off()

# Set dead2 equal to complete result of Imputation2.
# Add rows for differetnt values of days_since_start
dead <- complete(Imputation2)

dead <- addRows(dead)


# Impute non constant variables amongst those who died
impExclude3 <- c(constVars, 'days_since_start')

method3 <- createMethod(dead, impExclude3)

predMat3 <- template

predMat3[, c('who_days_to_improve1', 'who_days_to_improve2')] <- 0

Imputation3 <- mice(dead,m=1, maxit=iterations, predictorMatrix = predMat3, method = method3)


# Look at results of Imputation

pdf('/home/skerr/Git/SF94/Code/dummy_data_generator/Imputation3.pdf') 

densityplot(Imputation3, dead ~ fio2 + sao2)

dev.off()

freqTab(dead, Imputation3, catVars)

dead <- complete(Imputation3)


############################ IMPUTE THE LIVING: ##########################################

notDead <- addRows(notDead)


impExclude4 <- c(constVars, 'days_since_start')

method4 <- createMethod(notDead, impExclude4)

predMat4 <- template

predMat4[, c('who_days_to_improve1', 'who_days_to_improve2')] <- 0

Imputation4 <- mice(notDead ,m=1,maxit=iterations, predictorMatrix = predMat4, method = method4)


pdf('/home/skerr/Git/SF94/Code/dummy_data_generator/Imputation4.pdf')

densityplot(Imputation4, notDead ~ fio2 + sao2)

dev.off()

freqTab(notDead, Imputation4, catVars)

notDead <- complete(Imputation4)


##################################### COMBINE AND ADD DERIVED VARIABLES ################################

fulldf <- rbind(dead, notDead)

# Add sfr
fulldf$sfr<- fulldf$sao2/fulldf$fio2

# Add who ordinal severity scale
fulldf<- mutate( fulldf,
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
      fio2 >=0.21 & fio2 < 0.22 ~ 4))

# Add whether they improved by 1 and 2 who levels
fulldf <- mutate(fulldf,  whoImprovement1 = case_when(  !is.na(who_days_to_improve1) ~ 'YES',
                                                          str_sub(subjid ,1, 9) != 'Simulated' ~ 'NO' ))    

fulldf <- mutate(fulldf,  whoImprovement2 = case_when(  !is.na(who_days_to_improve2) ~ 'YES',
                                                          str_sub(subjid ,1, 9) != 'Simulated' ~ 'NO' )) 

# mice needs categorical varaiables to be factors
fulldf[c('whoImprovement1', 'whoImprovement2' )] <- lapply(fulldf[c('whoImprovement1', 'whoImprovement2' )], factor)

# simDay0 includes all non-simulated entiries, and simulated day 0 entries
simDay0 <- filter(fulldf, (str_sub(subjid ,1, 9) == 'Simulated' & days_since_start == 0) 
                        | !(str_sub(subjid ,1, 9) == 'Simulated') )

# simRest contains the rest of fulldf2
simRest <- filter(fulldf, str_sub(subjid ,1, 9) == 'Simulated' & days_since_start != 0)

######################## IMPUTE whoImprovement1 and whoImprovement2 ############################

dead28 <- filter(simDay0, day_of_death <=28)

notDead28 <- filter(simDay0, is.na(day_of_death) | day_of_death >28 )

# Create a 'template' predictor matrix.
# I only use the first row of simDay0, which will throw warnings. Just a quick way to get a matrix with the
# requisite rows and columns.
template2 <- quickpred(notDead28[1,], mincor = -1)

template2[,] <- 1
diag(template2) <- 0
template2[,'subjid'] <- 0

# Impute whoImprovement2
impExclude5 <-  c(allVars, 'severity_scale_ordinal', 'sfr', 'whoImprovement1')

method5 <- createMethod(notDead28, impExclude5)

predMat5 <- template2

predMat5[, c('day_of_death', 'who_days_to_improve1', 'who_days_to_improve2', 'whoImprovement1')] <- 0

Imputation5 <- mice(notDead28 ,m=1,maxit=iterations, predictorMatrix = predMat5, method = method5)


freqTab(notDead28, Imputation5, 'whoImprovement2')



notDead28 <- complete(Imputation5)

# Whenever there is a 2 level improvement, there is a one level improvement
notDead28[ notDead28['whoImprovement2'] == 'YES', ]['whoImprovement1'] <- 'YES'

# Impute whoImprovement1
impExclude6 <-  c(allVars, 'severity_scale_ordinal', 'sfr', 'whoImprovement2')

method6 <- createMethod(fulldf, impExclude6)

predMat6 <- template2

predMat6[, c('day_of_death', 'who_days_to_improve1', 'who_days_to_improve2')] <- 0

Imputation6 <- mice(notDead28 ,m=1,maxit=iterations, predictorMatrix = predMat6, method = method6)


freqTab(notDead28, Imputation6, 'whoImprovement1')


notDead28 <- complete(Imputation6)

##################### IMPUTE who_days_to_improve2 and  who_days_to_improve1 #####################

# Split by who had a 1 level improvement and who didn't.
who2 <- filter(notDead28, whoImprovement2== 'YES')

who1 <- filter(notDead28, whoImprovement2== 'NO' & whoImprovement1== 'YES')

notwho <- filter(notDead28, whoImprovement1== 'NO')

# Impute who_days_to_improve1 for those whom whoImprovement1 is YES and  whoImprovement2 is NO
impExclude7 <-  setdiff(colnames(who1), 'who_days_to_improve1')

method7 <- createMethod(who1, impExclude7)

predMat7 <- template2

predMat7[, c('day_of_death', 'who_days_to_improve2')] <- 0

Imputation7 <- mice(who1 ,m=1,maxit=iterations, predictorMatrix = predMat7, method = method7)



pdf('/home/skerr/Git/SF94/Code/dummy_data_generator/Imputation7.pdf')

densityplot(Imputation7, who1 ~ who_days_to_improve1)

dev.off()


who1 <- complete(Imputation7)




who2['who12diff'] <- who2['who_days_to_improve2'] - who2['who_days_to_improve1']

# Create a 'template' predictor matrix.
template3 <- quickpred(who2[1,], mincor = -1)

template3[,] <- 1
diag(template3) <- 0
template3[,'subjid'] <- 0

# Impute who_days_to_improve1 and who12diff  for those whom whoImprovement2 is YES
impExclude8 <-  setdiff(colnames(who2), c('who_days_to_improve1', 'who12diff') )

method8 <- createMethod(who2, impExclude8)

predMat8 <- template3

predMat8[, c('day_of_death', 'who_days_to_improve2')] <- 0

Imputation8 <- mice(who2 ,m=1,maxit=iterations, predictorMatrix = predMat8, method = method8)



pdf('/home/skerr/Git/SF94/Code/dummy_data_generator/Imputation7.pdf')

densityplot(Imputation8, who2 ~ who_days_to_improve1 + who12diff)

dev.off()


who2 <- complete(Imputation8)

who2['who_days_to_improve2'] <- who2['who_days_to_improve1'] + who2['who12diff']

who2$who12diff <- NULL


# Set whoImprovement1, whoImprovement2 to No for those who died by day 28
dead28[c('whoImprovement1', 'whoImprovement2')] <- 'NO'



fulldf<- rbind(notwho, who1)
fulldf <- rbind(fulldf, who2)

fulldf <- rbind(fulldf, dead28)

fulldf <- rbind(fulldf, simRest)

#Separate out simulated and 'real' data
real_ccp <- filter(fulldf, str_sub(subjid ,1, 9) != 'Simulated')

simulated_ccp <- filter(fulldf, str_sub(subjid ,1, 9) == 'Simulated')

# Fill values for who_days_to_improve1 and who_days_to_improve2
simulated_ccp <- simulated_ccp %>% group_by(subjid) %>% fill( c('who_days_to_improve1','who_days_to_improve2'), .direction = 'downup')

simulated_ccp['age_estimateyears'] <- lapply( simulated_ccp['age_estimateyears'], as.integer)


####################################### WRITE DATA: #######################################

# Write on argosafe
write.csv(simulated_ccp ,"/home/skerr/Data/ccp_subset_simulated.csv", row.names = FALSE)

