###################################################################### 

## Code author: Steven Kerr

## Description: 

# This file generates a simulated dataset for the sf94 project
# The original data is CCP. The variables in the simulated dataset are in the variable cols below
# The basic strategy is to use multiple imputation by chained equations (MICE) in 2 steps. 
# I add n rows that are empty except for having a subject id, and days_since_admission = 8.
# I then use MICE to impute the values of variables that are constant in time for the period of the dataset, e.g. comorbidities
# Then each simulated subjid gets 2 news rows of data, corresponding to days_since_admission = 0, 5.
# A second use of MICE fills in values for sao2, severity_ordinal_scale and sf94.

###################################################################### 

library(dplyr)

library(ggplot2)

library(mice)

library(data.table)


ultra <- FALSE

if (ultra == TRUE){
  
  root <- '/home/u034/'
} else {
  
  root <- 'Y:/'
}



cols <- c('subjid', 'sex', 'age_estimateyears', 'days_since_admission', 'day_of_death', 'death', 'severity_scale_ordinal', 'sao2', 'sf94', 
          'chrincard', 'hypertension_mhyn', 'chronicpul_mhyn', 'renal_mhyn', 'modliv', 'mildliver', 'chronicneu_mhyn', 'malignantneo_mhyn', 
          'chronichaemo_mhyn', 'obesity_mhyn','diabetes_type_mhyn', 'diabetescom_mhyn', 'diabetes_mhyn', 'dementia_mhyn', 'clinical_frailty')


data<- fread(  paste( root, "mcswets/df_1_20210402.csv", sep= "") , select = cols, data.table = FALSE)


# Create clean columns for diabetes and liver disease

data <- mutate(data, diabetes = case_when( diabetes_type_mhyn == 1 | diabetes_type_mhyn == 2 | diabetescom_mhyn == 'YES' |  diabetes_mhyn == 'YES' ~ 1,
                                           diabetes_type_mhyn == 'NO' |  diabetescom_mhyn == 'NO' |  diabetes_mhyn == 'NO' ~ 0) )


data <- mutate(data, liver = case_when( modliv == 'YES' | mildliver == 'YES' ~ 1,
                                        modliv == 'NO' | mildliver == 'NO' ~ 0)    )               

# Drop columns that aren't needed anymore

data[ c('diabetes_type_mhyn', 'diabetescom_mhyn', 'diabetes_mhyn', 'modliv', 'mildliver') ] <- NULL

# Replace some values
data[ data == 'Not Specified' ] <- NA
data[ data == 'Not specified' ] <- NA
data[ data == 'Unknown' ] <- NA
data[ data == 'N/K' ] <- NA
data[ is.na(data['death']), ]$death <- FALSE


# Take a sample
sample <- sample_n(data, 10000)              


# Create a dataframe for simulated data
# n is the size of the simulated dataset

n <- 1000

simulated <- do.call(data.frame, replicate( length(data), rep(NA, n), simplify=FALSE))

colnames(simulated ) <- colnames(data)

simulated ['subjid'] <- paste( 'Simulated', rownames(simulated ), sep= ' ')

simulated ['days_since_admission'] <- 8

#Combine real and simulated data

fullData <- rbind(sample, simulated)


# catVars is a list of variables that are categorical.

catVars <- c('sex', 'death', 'chrincard', 'hypertension_mhyn','chronicpul_mhyn', 'renal_mhyn',  'chronicneu_mhyn', 
             'malignantneo_mhyn', 'chronichaemo_mhyn', 'obesity_mhyn', 'dementia_mhyn', 'clinical_frailty',
             'diabetes', 'liver')

# mice needs categorical varaiables to be factors

fullData[catVars] <- lapply(fullData[catVars], factor )


# Variables that will not be imputed

impExclude <- c('subjid','days_since_admission', 'severity_scale_ordinal', 'sao2', 'sf94' )

# Need to set the method option in mice manually

createMethod <- function(impExclude){
  
  method <- as.data.frame( sapply(fullData, nlevels) )
  
  method[ method[,1] > 2, ] <- 'polyreg'
  
  method[ impExclude, ] <- ''
  
  method[ method[,1] ==0 , ] <- 'pmm'
  
  method[ method[,1] ==2 , ] <- 'logreg'
  
  method <- dplyr::pull(method, colnames(method) )
}

method <- createMethod(impExclude)


predMat <- quickpred(data, exclude = impExclude)

predMat[, 'day_of_death'] <- 0


Imputation <- mice(fullData ,m=1,maxit=100, predictorMatrix = predMat, method = method  )





# freqTab prints frequency tables for the imputations and the real data against each other for the categorical variables
# to allow comparison.


freqTab <- function(Imputation){
  
  for (var in catVars){
    
    real <- table(fullData[var])
    
    nlevels <- length(real)
    
    freqTable <- data.frame( matrix(ncol = Imputation$m +1, nrow = nlevels) )
    
    colnames(freqTable) <- append( seq(1, Imputation$m, 1), 'real')
    
    rownames(freqTable) <- seq(0, nlevels-1, 1)
    
    
    for (m in 1:Imputation$m ){
      
      tab <- table(as.data.frame(Imputation$imp[var])[,m] )
      
      freqTable[, m] <- tab / sum(tab)
      
      freqTable[, 'real'] <- table(fullData[var]) / sum( table(fullData[var]) )
      
    }
    print(var)
    
    print(freqTable)
    
  }
}



# Look at results of Imputation

pdf( paste( root, 'stevenkerr/Git/SF94/Code/dummy_data_generator/Imputation.pdf', sep= "") )

densityplot(Imputation, fullData ~ age_estimateyears + day_of_death)

dev.off()

freqTab(Imputation)


# Set fullData equal to the complete imputation

fullData1 <- complete(Imputation)

fullData1[  fullData1['death']==FALSE,  ]['day_of_death'] <- NA


# Add rows for day 5 and day 8

addRows <- function(fullData){
  
  for(i in (nrow(fullData) - n +1):nrow(fullData)    ){
    
    row <-  fullData[i,   ] 
    
    if( row['death'] == FALSE |  ( row['death'] == TRUE &   row['day_of_death'] > 5)  ){
      
      newrow <- row
      
      newrow['days_since_admission'] <- 5
      
      fullData <- rbind(fullData, newrow)
    }
    
    
    if( row['death'] == FALSE |  ( row['death'] == TRUE &   row['day_of_death'] > 0)   ){
      
      newrow <- row
      
      newrow['days_since_admission'] <- 0
      
      fullData <- rbind(fullData, newrow)
    }
    
  }
  
  rownames(fullData) <- 1:nrow(fullData)
  
  return(fullData)  
}


fullData2 <- addRows(fullData1)




impExclude2 <- setdiff( colnames(data), c('severity_scale_ordinal', 'sao2', 'sf94')    )

method <- createMethod(impExclude2)


Imputation2 <- mice(fullData2 ,m=1,maxit=100, predictorMatrix = quickpred(data), method = method)


pdf( paste( root, 'stevenkerr/Git/SF94/Code/dummy_data_generator/Imputation2.pdf', sep= "") )

densityplot(Imputation2, fullData2 ~ severity_scale_ordinal + sao2 + sf94)

dev.off()


fullData2 <- complete(Imputation2 )
