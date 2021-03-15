###################################################################### 

## Code author: Steven Kerr

## Description: 
# This code predicts fio2 as a function of sfr and other covariates
# The goal is to predict sfr for patients who initially have sao2 <= 0.94  | fio2 == 0.21,
# but then have fio2 reduced until sao2 = 0.94

###################################################################### 

library(dplyr)
library(ggplot2)
library(data.table)


df<- fread("/home/skerr/Data/ccp_subset_derived.csv", select = allVars, data.table = FALSE)

