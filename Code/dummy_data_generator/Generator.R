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

cols <- c('subjid', 'sex', 'age_estimateyears', 'days_since_start', 'day_of_death', 'death', 'severity_scale_ordinal', 'sao2', 
          'sf94', 'chrincard', 'hypertension_mhyn', 'chronicpul_mhyn', 'renal_mhyn', 'chronicneu_mhyn', 'malignantneo_mhyn', 
          'chronichaemo_mhyn', 'obesity_mhyn', 'dementia_mhyn', 'clinical_frailty', 'diabetes', 'liver')

df<- fread("/home/skerr/Data/ccp_subset_derived.csv", select = cols, data.table = FALSE)

# Take a sample
sample <- sample_n(df, 1000)              

# Create a dataframe for simulated df
# n is the size of the simulated dataset
n <- 100

simulated <- do.call(data.frame, replicate( length(df), rep(NA, n), simplify=FALSE))

colnames(simulated ) <- colnames(df)

simulated ['subjid'] <- paste( 'Simulated', rownames(simulated ), sep= ' ')

simulated ['days_since_start'] <- 8

#Combine real and simulated df
fulldf <- rbind(sample, simulated)

# catVars is a list of variables that are categorical.
catVars <- c('sex', 'death', 'chrincard', 'hypertension_mhyn','chronicpul_mhyn', 'renal_mhyn',  'chronicneu_mhyn', 
             'malignantneo_mhyn', 'chronichaemo_mhyn', 'obesity_mhyn', 'dementia_mhyn', 'clinical_frailty', 'diabetes', 'liver')

# mice needs categorical varaiables to be factors
fulldf[catVars] <- lapply(fulldf[catVars], factor )

# Variables that will not be imputed in first imputation
impExclude <- c('subjid','days_since_start', 'severity_scale_ordinal', 'sao2', 'sf94')

# Need to set the method option in mice manually
createMethod <- function(impExclude){
  
  method <- as.data.frame( sapply(fulldf, nlevels) )
  
  method[ method[,1] > 2, ] <- 'polyreg'
  
  method[ impExclude, ] <- ''
  
  method[ method[,1] ==0 , ] <- 'pmm'
  
  method[ method[,1] ==2 , ] <- 'logreg'
  
  method <- dplyr::pull(method, colnames(method) )
}

method <- createMethod(impExclude)


predMat <- quickpred(df, exclude = impExclude, mincor = -1)

Imputation <- mice(fulldf ,m=1, maxit=100, predictorMatrix = predMat, method = method)


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



# Look at results of Imputation

pdf( paste( root, 'stevenkerr/Git/SF94/Code/dummy_df_generator/Imputation.pdf', sep= "") )

densityplot(Imputation, fulldf ~ age_estimateyears + day_of_death)

dev.off()

freqTab(Imputation)


# Set fulldf equal to the complete imputation
fulldf1 <- complete(Imputation)

sapply(fulldf1['severity_scale_ordinal'], class)

fulldf1 <- fulldf1 %>% mutate_at('severity_scale_ordinal', ~case_when(  day_of_death == days_since_start ~ 10,
                                                                TRUE ~ severity_scale_ordinal))


# Add rows for day 0 and 5

addRows <- function(fulldf){
  for(i in (nrow(fulldf) - n +1):nrow(fulldf)    ){
    row <-  fulldf[i,   ] 
    if( row['death'] == 'NO' |  ( row['death'] == 'YES' &   row['day_of_death'] > 5)  ){
      newrow <- row
      
      newrow['days_since_start'] <- 5
      
      fulldf <- rbind(fulldf, newrow)
    }
    
    if( row['death'] == 'NO' |  ( row['death'] == 'YES' &   row['day_of_death'] > 0)   ){
      
      newrow <- row
      
      newrow['days_since_start'] <- 0
      
      fulldf <- rbind(fulldf, newrow)
    }
    }
  rownames(fulldf) <- 1:nrow(fulldf)
  return(fulldf)  
}

fulldf2 <- addRows(fulldf1)


impExclude2 <- setdiff(colnames(df), c('severity_scale_ordinal', 'sao2', 'sf94'))

method2 <- createMethod(impExclude2)

predMat2 <- quickpred(df, mincor = -1 )

predMat2['day_of_death'] <- 0

Imputation2 <- mice(fulldf2 ,m=1,maxit=100, predictorMatrix = quickpred(df), method = method2)


pdf( paste( root, 'stevenkerr/Git/SF94/Code/dummy_df_generator/Imputation2.pdf', sep= "") )

densityplot(Imputation2, fulldf2 ~ severity_scale_ordinal + sao2 + sf94)

dev.off()



fulldf3 <- complete(Imputation2)

fulldf3[  fulldf3['death']=='NO',  ]['day_of_death'] <- NA

sapply(fulldf3['severity_scale_ordinal'], class)

fulldf3 <- mutate(fulldf3, severity_scale_ordinal = case_when(  day_of_death == days_since_start ~ 10,
                                                                TRUE ~ severity_scale_ordinal))

