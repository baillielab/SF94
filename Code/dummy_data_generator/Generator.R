library(dplyr)

library(ggplot2)

library(mice)

library(data.table)



data<- fread("Y:/mcswets/df_1_20210402.csv", data.table = FALSE)


keepCols <- c('sex', 'age_estimate_years', 'rr_vsorres', 'days_since_admission', 'severity_scale_ordinal', 'sao2', 'sf94',    )
