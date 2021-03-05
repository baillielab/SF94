library(dplyr)
library(ggplot2)
library(data.table)
library(tidyr)
library(rms)

df_1<-fread("/home/skerr/Data/ccp_subset_derived.csv", data.table = FALSE )

#df_1<-fread("/home/u034/mcswets/df_20211402.csv", data.table = FALSE)
#df_1<-df_1[,c(2:89)]

subjects_to_include <- filter(df_1, ( fio2 >=0.22 & days_since_start %in% c(0,1,2)  & age_estimateyears >19 & age_estimateyears <76 ) )['subjid']
subset1<-df_1[df_1$subjid %in% subjects_to_include$subjid,] 
subset1 <- as.data.frame(subset1)


#OUTPUT
#number of subjects before and after filters

numberSubs <- data.frame( before_filter =  length(unique(df_1$subjid)) ,
                          after_filter = length(unique(subset1$subjid))   )

write.csv(numberSubs,"/home/skerr/Git/SF94/Outputs/numberSubs.csv")

# variable should be either  'sf94' or 'severity_scale_ordinal'
# group should be 'base' if you want to include everyone except those who died or were discharged
# 'basedd' if you want to include those who died or were discharged
# 'day0' if you want to exclude people who died or were discharged on day_since_admission == 0
# Output is a table that you can query for summary statistics.

createDF <- function(df, group, variable, time){
  
  if( group == 'base' |  group == 'basedd' ) { 
    df2 <- filter( df[ c('subjid', 'days_since_start', variable ) ] , days_since_start <= time  )
  } else if (group == 'day0') {
    dropID <- df[   (df[ 'day_of_death' ] == 0 |  subset1['day_of_discharge'] == 0) %in% TRUE, 'subjid']
    
    df2 <- filter(df[ c('subjid', 'days_since_start', variable ) ], days_since_start <= time & !(subjid %in% dropID)  ) 
  } 
  
  derived <-  df2  %>%  mutate(rn= row_number()) %>% spread (days_since_start, variable) %>% select(-rn)
  
  newCols <- as.character( setdiff( 0:time,  setdiff( colnames(derived), 'subjid' ) ) )
  
  derived[newCols] <- NA
  
  derived <- derived[ c( 'subjid', as.character(0:time))  ]
  
  if(group == 'basedd' | group == 'day0' ){
    censorDeath <- filter( df,  death== "YES"  & day_of_death < time    )[c('subjid', 'day_of_death')]
    
    censorDischarge <- filter( df,  discharge== "YES"  & day_of_discharge < time    )[c('subjid', 'day_of_discharge')]
    
    if(variable == 'sf94'){
      deathValue <- 0.5
      dischargeValue <- 4.76
    } else if(variable == 'severity_scale_ordinal'){
      deathValue <- 10
      dischargeValue <- 4
    }
    
    for( i in as.numeric(rownames(censorDeath))   ){
      row <- which( derived['subjid']  ==  censorDeath[i, 'subjid']  )
      
      cols <- as.character( (censorDeath[i, 'day_of_death'] +1):time  )
      
      derived[ row ,  cols  ] <- deathValue
    }
    
    for( i in as.numeric(rownames(censorDischarge))    ){
      
      row <- which( derived['subjid']  ==  censorDischarge[i, 'subjid']  )
      
      cols <- as.character( (censorDischarge[i, 'day_of_discharge']+1):time  )
      
      derived[ row ,  cols  ] <- dischargeValue
    }
  }
  return(derived)
}

#if error message try detaching plyr:
#detach("package:plyr", unload=T) 

#summary 
basedd_sf94_10<-createDF(subset1, "basedd", "sf94", 10)

#OUTPUT:
#for day 5 and day 8: mean and SD
meanSD <- as.data.frame( rbind(sapply(basedd_sf94_10[c('5','8')], mean, na.rm=T),  
                               sapply(basedd_sf94_10[c('5','8')], sd, na.rm=T)) )

rownames(meanSD) <- c('mean', 'SD')

write.csv(meanSD,"/home/skerr/Git/SF94/Outputs/meanSD.csv")

#correlation
#Correlation subsets: same subjects for different days
#keep only 1 row for each subject
f <- function(x) {
  x <- na.omit(x)
  if (length(x) > 0) unique(x) else NA
}
correlation_subset<-basedd_sf94_10
correlation_subset<-correlation_subset %>% 
  group_by(subjid) %>% 
  summarise_all(funs(f))
#make subsets of data in which all subjects have SF94 available for those 2 days
correlation_subset_05<-subset(correlation_subset, ((!is.na(correlation_subset[,2])&(!is.na(correlation_subset[,7])))))
correlation_subset_08<-subset(correlation_subset, ((!is.na(correlation_subset[,2])&(!is.na(correlation_subset[,10])))))
length(correlation_subset_05$subjid)
length(correlation_subset_08$subjid)
# DAY 0/5
w <-  correlation_subset_05[,2]
x <-  correlation_subset_05[,7]
cor(w,x)
# DAY 0/8
y <-  correlation_subset_08[,2]
z <-  correlation_subset_08[,10]
cor(y,z)


#OUTPUT
# correlation value for day 0/5 and for day 0/8
corrs <- as.data.frame(rbind( cor(w,x), cor(y,z)  ))

rownames(corrs) <- c('day 0/5', 'day 0/8')

write.csv(corrs,"/home/skerr/Git/SF94/Outputs/corrs.csv")



#Summary of WHO values on day 5 and 8
#data
basedd_who_8<-createDF(subset1, "basedd", "severity_scale_ordinal",8)
#take only subject ID, day 5 and day 8
day8_who<-basedd_who_8[,c(1,7,10)]
#summarise rows
day8_who<-day8_who%>%
  group_by(subjid)%>%
  summarise_all(funs(f))
#change variable names 
day8_who<-day8_who%>%
  dplyr::rename(who_day8= "8",
                who_day5= "5")
summary(day8_who$who_day5)
summary(day8_who$who_day8)



#mortality at 28 days
mort<-subset1 %>%
  group_by(subjid)%>%
  count(mortality_28)

mort28 <- sum(mort$mortality_28 == 1, na.rm = T)/ sum(!is.na(mort$mortality_28))

#OUTPUT
#percentage of subjects who died in the first 28 days

write.csv(mort28,"/home/skerr/Git/SF94/Outputs/mort28.csv")


#OUTPUT
# Median + IQR for WHO for day 5 and day 8

#OUTPUT
#median + IQR for 1 level difference

#OUTPUT
#median + IQR for 2 levels difference

subset1Dist <- distinct( subset1[c('subjid', 'who_days_to_improve1', 'who_days_to_improve2')])

medianIQR <- as.data.frame(rbind(summary(day8_who$who_day5), summary(day8_who$who_day8)))

medianIQR <- rbind(medianIQR, summary(subset1Dist$who_days_to_improve1 , na.rm = T) )

medianIQR <- rbind(medianIQR, summary(subset1Dist$who_days_to_improve2 , na.rm = T) )

rownames( medianIQR) <- c('who_day5', 'who_day8', 'who 1 level difference', 'who 2 level difference')

write.csv(medianIQR,"/home/skerr/Git/SF94/Outputs/medianIQR.csv")



#difference in SF94 between different WHO scale steps
medians <- subset1 %>%
  group_by(severity_scale_ordinal)%>%
  summarise_at("sf94", median, na.rm=T)

#OUTPUT
# 7 median values
write.csv(medians,"/home/skerr/Git/SF94/Outputs/medians.csv")

#additional numbers:

notMissingV <- c(  sapply(basedd_sf94_10[c('5', '8')], function(x) sum(!is.na(x))),       
                   sapply(day8_who[c('who_day5', 'who_day8')], function(x) sum(!is.na(x))),
                   sum(!is.na( subset1Dist$who_days_to_improve1 )), 
                   sum(!is.na( subset1Dist$who_days_to_improve2 )),
                   sum(!is.na(mort$mortality_28))
)

notMissing <- data.frame(  variable =   c('sf94_day5', 'sf94_day8', 'who_day5', 'who_day8', 'severity_dif_1level',
                                          'severity_dif_2level', 'mortality_28'),
                           value = notMissingV)

write.csv(notMissing,"/home/skerr/Git/SF94/Outputs/notMissing.csv")


missingV <- c(  sapply(basedd_sf94_10[c('5', '8')], function(x) sum(is.na(x))),       
                sapply(day8_who[c('who_day5', 'who_day8')], function(x) sum(is.na(x))),
                sum(is.na( subset1Dist$who_days_to_improve1 )), 
                sum(is.na( subset1Dist$who_days_to_improve2 )),
                sum(is.na(mort$mortality_28))
)

missing <- data.frame(  variable =   c('sf94_day5', 'sf94_day8', 'who_day5', 'who_day8', 'who_days_to_improve1',
                                       'who_days_to_improve2', 'mortality_28'),
                        value = missingV)

write.csv(missing,"/home/skerr/Git/SF94/Outputs/missing.csv")


whoTable <- rbind( table(day8_who$who_day5), table(day8_who$who_day8)   )

write.csv(whoTable,"/home/skerr/Git/SF94/Outputs/whoTable.csv")

write.csv(table(mort$mortality_28),"/home/skerr/Git/SF94/Outputs/mort28Table.csv")


#make small dataframe for 28-day mortality and outcome
mortality<-df_1[,c("subjid","mortality_28")]
mortality<-mortality%>%
  group_by(subjid)%>%
  slice(which.min(mortality_28))


#----------------------------------- GRAPHS --------------------------------
#split violin plot
#data
base_sf94_12<-createDF(subset1, "base", "sf94", 12)
#transform to long format 
long_dfsf94_12<-gather(base_sf94_12, days_since_start, sf94, 2:14, factor_key=T)
#removing rows without SF94 value
long_dfsf94_12 <- subset(long_dfsf94_12, !is.na(sf94))
#add 30 day mortality data
long_dfsf94_12<-left_join(long_dfsf94_12, mortality, by="subjid")
long_dfsf94_12 <- subset(long_dfsf94_12, !is.na(mortality_28))
#change to character and set correct order
long_dfsf94_12$mortality_28<-as.character(long_dfsf94_12$mortality_28)
long_dfsf94_12$mortality_28<-factor(long_dfsf94_12$mortality_28,
                                    levels=c("0","1"))
long_dfsf94_12$days_since_start<-as.character(long_dfsf94_12$days_since_start)
long_dfsf94_12$days_since_start<-factor(long_dfsf94_12$days_since_start,
                                        levels=c("0","1","2","3","4","5",
                                                 "6","7","8","9","10", "11", "12"))
#function code for split violin plots
GeomSplitViolin <- ggproto("GeomSplitViolin", GeomViolin, 
                           draw_group = function(self, data, ..., draw_quantiles = NULL) {
                             data <- transform(data, xminv = x - violinwidth * (x - xmin), xmaxv = x + violinwidth * (xmax - x))
                             grp <- data[1, "group"]
                             newdata <- plyr::arrange(transform(data, x = if (grp %% 2 == 1) xminv else xmaxv), if (grp %% 2 == 1) y else -y)
                             newdata <- rbind(newdata[1, ], newdata, newdata[nrow(newdata), ], newdata[1, ])
                             newdata[c(1, nrow(newdata) - 1, nrow(newdata)), "x"] <- round(newdata[1, "x"])
                             
                             if (length(draw_quantiles) > 0 & !scales::zero_range(range(data$y))) {
                               stopifnot(all(draw_quantiles >= 0), all(draw_quantiles <=
                                                                         1))
                               quantiles <- ggplot2:::create_quantile_segment_frame(data, draw_quantiles)
                               aesthetics <- data[rep(1, nrow(quantiles)), setdiff(names(data), c("x", "y")), drop = FALSE]
                               aesthetics$alpha <- rep(1, nrow(quantiles))
                               both <- cbind(quantiles, aesthetics)
                               quantile_grob <- GeomPath$draw_panel(both, ...)
                               ggplot2:::ggname("geom_split_violin", grid::grobTree(GeomPolygon$draw_panel(newdata, ...), quantile_grob))
                             }
                             else {
                               ggplot2:::ggname("geom_split_violin", GeomPolygon$draw_panel(newdata, ...))
                             }
                           })

geom_split_violin <- function(mapping = NULL, data = NULL, stat = "ydensity", position = "identity", ..., 
                              draw_quantiles = NULL, trim = TRUE, scale = "area", na.rm = FALSE, 
                              show.legend = NA, inherit.aes = TRUE) {
  layer(data = data, mapping = mapping, stat = stat, geom = GeomSplitViolin, 
        position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
        params = list(trim = trim, scale = scale, draw_quantiles = draw_quantiles, na.rm = na.rm, ...))
}

#=============================================================================================#

#slightly confusing- part of the analysis is done on data without a filter (age + oxygen support)
#so to not have to change the createDF function- I change the input to the complete (cleaned) df
#If you need to re-run earlier analysis, please double check that you have the correct input file!!

#Data imputation based on total number of deaths
#ANALYSIS DONE WITH DF_1 DATA, SO NO AGE OR RESP SUPPORT FILTERS. START = ±79000 SUBJECTS
#using df_1 (without filters)
#subset1<-df_1
df_1_base_sf94<-createDF(df_1, "base", "sf94", 10)

#regression model D5 SF94 and mortality
#with sf94 values regression
day05<-df_1_base_sf94[,c(1,2,7,10)] #select subjid, day 0 and day 5 and day 8
day05<-day05%>%
  dplyr::rename(sf94_day5= "5", sf94_day0= "0", sf94_day8= "8")

#calculate what number of subjects needs to be added to get correct proportion
dead5<-length(unique(df_1$subjid[(df_1$day_of_death <5)])) #number of subjects that died before day 5
alive5<-length(unique(df_1$subjid[(df_1$day_of_discharge <5)])) #number of subjects that went home before day 5
available5<-sum((!is.na(df_1$sf94) & df_1$days_since_start == 5), na.rm=T) 
percdead5<-dead5/length(unique(df_1$subjid)) #% that died before day from total subjects
percalive5<-alive5/length(unique(df_1$subjid))# % that went home before day 5 from total subjects
percavailable5<- (1-percdead5- percalive5)# % of available values
dead_to_add5<-(percdead5*available5)/percavailable5 #number of 0.5 values to add to variable
alive_to_add5<-(percalive5*available5)/percavailable5 #number of 4.76 values to add to variable

day05_P<-day05 #for proportional deaths
day05_P<-setDT(day05_P)[, lapply(.SD, na.omit), by=subjid] #keep 1 entry/subject
day05_P<-data.frame(day05_P)
set.seed(1234)
sf94_day5_P<-day05_P$sf94_day5 #add day 5 to a separate df
rows_to_replace<-which(is.na(sf94_day5_P))
sf94_day5_P[sample(rows_to_replace, dead_to_add5)]<- 0.5
rows_to_replace<-which(is.na(sf94_day5_P))
sf94_day5_P[sample(rows_to_replace, alive_to_add5)]<- 4.76

sf94_day5_P<-data.frame(sf94_day5_P)
day05_P<-cbind(day05_P, sf94_day5_P)
day05_P<-data.frame(day05_P)

#same for day 8
dead8<-length(unique(df_1$subjid[(df_1$day_of_death <8)])) #number of subjects that died before day 8
alive8<-length(unique(df_1$subjid[(df_1$day_of_discharge <8)])) #number of subjects that went home before day 8
available8<-sum((!is.na(df_1$sf94) & df_1$days_since_start == 8), na.rm=T) 
percdead8<-dead8/length(unique(df_1$subjid)) #% that died before day from total subjects
percalive8<-alive8/length(unique(df_1$subjid))# % that went home before day 5 from total subjects
percavailable8<- (1-percdead8- percalive8)# % of available values
dead_to_add8<-(percdead8*available8)/percavailable8 #number of 0.5 values to add to variable
alive_to_add8<-(percalive8*available8)/percavailable8 #number of 4.76 values to add to variable

day08_P<-day05_P #for proportional deaths day 8 
set.seed(1234)
day08_P$sf94_day8_P<-day08_P$sf94_day8 #make new column with proportional deaths, copy measured values D8
rows_to_replace<-which(is.na(day08_P$sf94_day8) & is.na(day08_P$sf94_day5_P)) #replace if D5_P and D8 NA
day08_P$sf94_day8_P[sample(rows_to_replace, dead_to_add8)]<- 0.5 #add dead patients
rows_to_replace<-which(is.na(day08_P$sf94_day8) & is.na(day08_P$sf94_day5_P))
day08_P$sf94_day8_P[sample(rows_to_replace, alive_to_add8)]<- 4.76 #add discharged patients

day08_P<-data.frame(day08_P)
day08_P$delta_SF94_05<-day08_P$sf94_day5_P - day08_P$sf94_day0 #calculate difference from day 0 to day 5
day08_P$delta_SF94_08<-day08_P$sf94_day8_P - day08_P$sf94_day0 #same for D0-D8

#create small df with the independent predictor variables (except sf94) and outcome variable
predictor_variables<-df_1[,c("subjid", "sex", "age_estimateyears", "mortality_28")]
predictor_variables<-predictor_variables %>% group_by(subjid)%>%slice(which.min(mortality_28))

#join both together
regresson_df_P<-left_join(day08_P,predictor_variables, by="subjid")
rm(sf94_day5_P,sf94_day8_P) #remove from global environment

#use proportionally added outcome values, take subject ID and day 5 SF94_P values (from DF_1, so not using filters)
regresson_df_P <-data.frame(regresson_df_P)
regresson_df_P<-regresson_df_P %>% #change mortality to match proportionally added values
  mutate(
    mortality_28 = case_when(
      sf94_day5_P == 4.760 |sf94_day8_P == 4.760  ~ 0, sf94_day5_P == 0.5 |sf94_day8_P == 0.5 ~ 1,TRUE ~ as.numeric(mortality_28)))

#add proportional WHO D5 and D8 to regresson DF
df_1_base_who_8<-createDF(df_1, "base", "severity_scale_ordinal",8)
who_5_8<-df_1_base_who_8[,c(1,2,7,10)] #select subjid, day 0 and day 5 and day 8
who_5_8<-who_5_8%>%
  dplyr::rename(WHOD5= "5", WHOD0= "0", WHOD8= "8") #change to easier names
who_5_8<-setDT(who_5_8)[, lapply(.SD, na.omit), by=subjid] #keep 1 entry/subject
regresson_df_P<-left_join(regresson_df_P, who_5_8, by="subjid")
regresson_df_P$WHOD5_P<-regresson_df_P$WHOD5
regresson_df_P$WHOD8_P<-regresson_df_P$WHOD8
#match the proportionally added SF94 values to the WHO scores
#this is to avoid that someone without a known value gets randomly set to dead on day 5 for SF94 (0.5)
#but then randomly gets a value 4 (no oxygen sup) on day 5 or 8 for WHO
regresson_df_P<-regresson_df_P %>% #change mortality to match proportionally added values
  mutate(
    WHOD5_P = case_when(
      sf94_day5_P == 4.760  ~ 4, sf94_day5_P == 0.5 ~ 10,TRUE ~ as.numeric(WHOD5_P)))
regresson_df_P<-regresson_df_P %>% #change mortality to match proportionally added values
  mutate(
    WHOD8_P = case_when(
      sf94_day8_P == 4.760  ~ 4, sf94_day8_P == 0.5 ~ 10,TRUE ~ as.numeric(WHOD8_P)))

#add days to improvement variable
time_to_improvement<-df_1[,c("subjid", "who_days_to_improve2", "who_days_to_improve1")]
time_to_improvement<-time_to_improvement %>%
  group_by(subjid)%>%
  summarise_all(funs(f))
time_to_improvement<-time_to_improvement %>%
  mutate( #if there is a number, there is sustained improvement, so change to 1. If not, change to 0
    sustained_1L_improvement= case_when(
      !is.na(who_days_to_improve1) ~ 1, is.na(who_days_to_improve1) ~ 0 ))
time_to_improvement<-time_to_improvement %>%
  mutate( #if there is a number, there is sustained improvement, so change to 1. If not, change to 0
    sustained_2L_improvement= case_when(
      !is.na(who_days_to_improve2) ~ 1, is.na(who_days_to_improve2) ~ 0 ))
time_to_improvement<-time_to_improvement[,c("subjid", "sustained_1L_improvement", "sustained_2L_improvement")]
time_to_improvement<-data.frame(time_to_improvement)

#bind to rest of data
regresson_df_P<-left_join(regresson_df_P, time_to_improvement, by="subjid")

#apply age filter and supp oxygen filter
subjects_to_include <- filter(df_1, ( fio2 >=0.22 & days_since_start %in% c(0,1,2)  & age_estimateyears >19 & age_estimateyears <76 ) )['subjid']
regresson_df_P<-regresson_df_P[regresson_df_P$subjid %in% subjects_to_include$subjid,] 
regresson_df_P <- as.data.frame(regresson_df_P)

#First need to set data distribution for rms functions
attach(regresson_df_P)
ddist <- datadist(sf94_day0, delta_SF94_05,delta_SF94_08,sex, age_estimateyears, mortality_28,
                  sustained_1L_improvement, sustained_2L_improvement, WHOD5_P, WHOD8_P)
options(datadist='ddist')
detach(regresson_df_P)

#Effect size proportional odds logistic regression for WHO scale
# WHO D5/D8 as dependent variable
#SF94 at D5/D8 respectively, age and sex as independent variable
library(MASS)
WHOD5_model<-polr(as.factor(WHOD5_P) ~ delta_SF94_05+ age_estimateyears+ sex, data = regresson_df_P, Hess=T)
WHOD8_model<-polr(as.factor(WHOD8_P) ~ delta_SF94_08+ age_estimateyears+ sex, data = regresson_df_P, Hess=T)
sum_WHO_D5<-summary(WHOD5_model)
ci_5<-confint(WHOD5_model) #calculate confidence interval
OR_D5_WHO<-exp(cbind(OR=coef(WHOD5_model),ci_5)) #calculate OR
sum_WHO_D8<-summary(WHOD8_model)
ci_8<-confint(WHOD8_model)
OR_D8_WHO<-exp(cbind(OR=coef(WHOD8_model),ci_8))

write.rds(sum_WHO_D5,"/home/skerr/Git/SF94/Outputs/sum_WHO_D5.csv")
write.rds(sum_WHO_D8,"/home/skerr/Git/SF94/Outputs/sum_WHO_D8.csv")
write.rds(OR_D5_WHO,"/home/skerr/Git/SF94/Outputs/OR_D5_WHO.csv")
write.rds(OR_D8_WHO,"/home/skerr/Git/SF94/Outputs/OR_D8_WHO.csv")

#WHO time to improvement
sus_1L_D5<-lrm(sustained_1L_improvement ~ delta_SF94_05+ age_estimateyears+ sex, data = regresson_df_P)
sus_1L_D8<-lrm(sustained_1L_improvement ~ delta_SF94_08+ age_estimateyears+ sex, data = regresson_df_P)
sus_2L_D5<-lrm(sustained_2L_improvement ~ delta_SF94_05+ age_estimateyears+ sex, data = regresson_df_P)
sus_2L_D8<-lrm(sustained_2L_improvement ~ delta_SF94_08+ age_estimateyears+ sex, data = regresson_df_P)

write.rds(sus_1L_D5,"/home/skerr/Git/SF94/Outputs/sus_1L_D5.csv")
write.rds(sus_1L_D8,"/home/skerr/Git/SF94/Outputs/sus_1L_D8.csv")
write.rds(sus_2L_D5,"/home/skerr/Git/SF94/Outputs/sus_2L_D5.csv")
write.rds(sus_2L_D8,"/home/skerr/Git/SF94/Outputs/sus_2L_D8.csv")

#Then fit models (splines using 4 knots here)
linear_model_P <- lrm(mortality_28 ~ sf94_day0 + sf94_day5_P, regresson_df_P, x=TRUE, y=TRUE)
linear_uni_model<-lrm(mortality_28 ~ sf94_day5_P, regresson_df_P, x=TRUE, y=TRUE)
linear_uni_model_D8<-lrm(mortality_28 ~ sf94_day8_P, regresson_df_P, x=TRUE, y=TRUE)
univariate_model<-lrm(mortality_28 ~ sf94_day0, regresson_df_P, x=TRUE, y=TRUE)
#Visualise using exp scale
plot_associations_linear_exp_P <- ggplot(Predict(linear_model_P, fun=plogis), sepdiscrete='vertical',
                                         ylab= "Risk of 28-day mortality")

plot_linear_exp_p<-plot_associations_linear_exp_P + labs(title= "Relationship between S/F94 and mortality")
#change label names
plot_linear_exp_p$data$.predictor.<-factor(plot_linear_exp_p$data$.predictor., labels= c("Day 0 S/F94",
                                                                                         "Day 5 S/F94"))
plot_linear_exp_p + facet_grid(.~ .predictor., labeller = label_value)

#univariable model
plot_linear_uni<-ggplot(Predict(univariate_model))
plot_linear_exp_uni<-ggplot(Predict(univariate_model, fun=plogis), ylab= "Risk of 28-day mortality")
plot_linear_exp_uni

#OUTPUT 
#2 graphs


### input data: regression_df_P (cleaned, with proportional deaths on D5 + D8 and matching outcome)
#apply filter (age + resp support) on data for regression analysis, as this is for power calculation
subjects_to_include <- filter(df_1, ( fio2 >=0.22 & days_since_start %in% c(0,1,2)  & age_estimateyears >19 & age_estimateyears <76 ) )['subjid']
filter_regression_analysis<-regresson_df_P[regresson_df_P$subjid %in% subjects_to_include$subjid,] 
filter_regression_analysis <- as.data.frame(filter_regression_analysis)

#First need to set data distribution for rms functions
attach(filter_regression_analysis)
ddist <- datadist(sf94_day0, sf94_day5_P, sf94_day8_P, mortality_28)
options(datadist='ddist')
detach(filter_regression_analysis)
#Then fit models (splines using 4 knots here)
linear_uni_model<-lrm(mortality_28 ~ sf94_day5_P, filter_regression_analysis, x=TRUE, y=TRUE)
linear_uni_model_D8<-lrm(mortality_28 ~ sf94_day8_P, filter_regression_analysis, x=TRUE, y=TRUE)

# save intercept and coefficient for day 5
intercept5<-as.numeric(coef(linear_uni_model)[1]) 
coefday5<-as.numeric(coef(linear_uni_model)[2])

absolute_mortdif<-function(mort1) {
  logoddsmort1<-log(1/((1-mort1)/mort1))
  baselinesf94<-(logoddsmort1-intercept5)/coefday5
  sf94_2<-baselinesf94+0.5
  logoddsmort2<-intercept5+ (coefday5 * sf94_2)
  mort2OR<-exp(logoddsmort2) #to odds ratio
  mort2<-mort2OR/ (1 +mort2OR)
  mortdif<-mort1-mort2
  return(mortdif)
}
list_baseline_mort<-c(0.25,0.30,0.35)
D5_sf94_effectsize<-absolute_mortdif(list_baseline_mort)

write.csv(D5_sf94_effectsize,"/home/skerr/Git/SF94/Outputs/D5_sf94_effectsize.csv")

#day 8
interceptD8<-as.numeric(coef(linear_uni_model_D8)[1]) #uninvariate model D5 only
coefday8<-as.numeric(coef(linear_uni_model_D8)[2])

absolute_mortdifD8<-function(mort1) {
  logoddsmort1<-log(1/((1-mort1)/mort1))
  baselinesf94<-(logoddsmort1-interceptD8)/coefday8
  sf94_2<-baselinesf94+0.5
  logoddsmort2<-interceptD8+ (coefday8 * sf94_2)
  mort2OR<-exp(logoddsmort2) #to odds ratio
  mort2<-mort2OR/ (1 +mort2OR)
  mortdif<-mort1-mort2
  return(mortdif)
}
list_baseline_mort<-c(0.25,0.30,0.35)
D8_sf94_effectsize<-absolute_mortdifD8(list_baseline_mort)

write.csv(D8_sf94_effectsize,"/home/skerr/Git/SF94/Outputs/D8_sf94_effectsize.csv")


#effect size SF94- change in SF94 
linear_uni_model
intercept<-as.numeric(coef(linear_uni_model)[1]) #uninvariate model D5 only
coefday5<-as.numeric(coef(linear_uni_model)[2])

#calculate SF94 difference from baseline with various mortality reductions
abseffect_size<- function(mortdifference, mort1) {
  logoddsmort1<-log(1/((1-mort1)/mort1))
  baselinesf94<-(logoddsmort1-intercept)/coefday5
  mort2<-mort1-mortdifference
  logoddsmort2<-log(1/((1-mort2)/mort2)) #from probability to odds
  newsf94<-(logoddsmort2-intercept)/coefday5 #regression equation
  sf94_difference<-newsf94-baselinesf94
  return(sf94_difference)
}

absolute_mort_reduction<-c(0.05,0.06,0.07,0.08,0.09,0.1,0.11,0.12,0.13,0.14,0.15)
sf94_25<-abseffect_size(absolute_mort_reduction,0.25)
sf94_30<-abseffect_size(absolute_mort_reduction,0.30)
sf94_35<-abseffect_size(absolute_mort_reduction,0.35)

#make a graph
sf94_dif<-data.frame(sf94_25, sf94_30, sf94_35)
sf94_dif<- data.frame(sf94_dif= c(sf94_dif[,"sf94_25"], sf94_dif[,"sf94_30"],sf94_dif[,"sf94_35"]))
effectsize_graph<-data.frame(absolute_mort_dif=rep(absolute_mort_reduction,3),
                             baseline_mort=rep(c("0.25","0.30","0.35"), each = 11))
effectsize_graph<-cbind(effectsize_graph, sf94_dif)

graph_effectsize<-ggplot(effectsize_graph, aes(x=absolute_mort_dif, y=sf94_dif, group=baseline_mort,
                                               colour=baseline_mort))
graph_effectsize + geom_line()+
  xlab("Absolute mortality difference") +
  ylab("Difference in SF94")+
  ggtitle("Effectsize day 5")+
  scale_colour_discrete(name="Baseline mortality", labels=c("25%", "30%", "35%"))

#DAY 8
interceptD8<-as.numeric(coef(linear_uni_model_D8)[1]) #uninvariate model D5 only
coefday8<-as.numeric(coef(linear_uni_model_D8)[2])

#calculate SF94 difference from baseline with various mortality reductions

##input data: subset 1 + filters?? check before running on complete data on safehaven/argoshare

abseffect_size_D8<- function(mortdifference, mort1) {
  logoddsmort1<-log(1/((1-mort1)/mort1))
  baselinesf94<-(logoddsmort1-intercept)/coefday8
  mort2<-mort1-mortdifference
  logoddsmort2<-log(1/((1-mort2)/mort2)) #from probability to odds
  newsf94<-(logoddsmort2-intercept)/coefday8 #regression equation
  sf94_difference<-newsf94-baselinesf94
  return(sf94_difference)
}

absolute_mort_reduction<-c(0.05,0.06,0.07,0.08,0.09,0.1,0.11,0.12,0.13,0.14,0.15)
sf94_25_D8<-abseffect_size_D8(absolute_mort_reduction,0.25)
sf94_30_D8<-abseffect_size_D8(absolute_mort_reduction,0.30)
sf94_35_D8<-abseffect_size_D8(absolute_mort_reduction,0.35)

#make a graph
sf94_dif_D8<-data.frame(sf94_25_D8, sf94_30_D8, sf94_35_D8)
sf94_dif_D8<- data.frame(sf94_dif_D8= c(sf94_dif_D8[,"sf94_25_D8"], sf94_dif_D8[,"sf94_30_D8"],
                                     sf94_dif_D8[,"sf94_35_D8"]))
effectsize_graph_D8<-data.frame(absolute_mort_dif_D8=rep(absolute_mort_reduction,3),
                             baseline_mort_D8=rep(c("0.25","0.30","0.35"), each = 11))
effectsize_graph_D8<-cbind(effectsize_graph_D8, sf94_dif_D8)

graph_effectsize_D8<-ggplot(effectsize_graph_D8, aes(x=absolute_mort_dif_D8, 
                                                     y=sf94_dif_D8, group=baseline_mort_D8,
                                               colour=baseline_mort_D8))
graph_effectsize_D8 + geom_line()+
  xlab("Absolute mortality difference") +
  ylab("Difference in SF94")+
  ggtitle("Effectsize day 8")+
  scale_colour_discrete(name="Baseline mortality", labels=c("25%", "30%", "35%"))

#relative risk reductions
# (controlmort- experimentalgroupmort)/ controlmort = RRR
rrr_list<-c(0.12,0.13,0.14,0.15,0.16,0.17,0.18,0.19,0.20) #range of RRR of interest
#for 3 different baseline mortalities
mort2_25<- function(rrr) {
  expmort<-(rrr * 0.25) - 0.25
  expmort<--expmort
  return(expmort)
}
mort2_25_rrr<-mort2_25(rrr_list) #output is a list of mortalities in the experimental group

mort2_30<- function(rrr) {
  expmort<-(rrr * 0.30) - 0.30
  expmort<--expmort
  return(expmort)
}
mort2_30_rrr<-mort2_30(rrr_list)

mort2_35<- function(rrr) {
  expmort<-(rrr * 0.35) - 0.35
  expmort<--expmort
  return(expmort)
}
mort2_35_rrr<-mort2_35(rrr_list)

relativeeffect_size<- function(mort2, mort1) {
  logoddsmort1<-log(1/((1-mort1)/mort1))
  baselinesf94<-(logoddsmort1-intercept)/coefday5
  logoddsmort2<-log(1/((1-mort2)/mort2)) #from probability to odds
  newsf94<-(logoddsmort2-intercept)/coefday5 #regression equation
  sf94_difference<-newsf94-baselinesf94
  return(sf94_difference)
}
sf94_rel25<-relativeeffect_size(mort2_25_rrr,0.25)
sf94_rel30<-relativeeffect_size(mort2_30_rrr,0.30)
sf94_rel35<-relativeeffect_size(mort2_35_rrr,0.35)

#make a graph
library(data.table)
relsf94_dif<-data.frame(sf94_rel25, sf94_rel30, sf94_rel35)
relsf94_dif<- data.frame(sf94_dif= c(relsf94_dif[,"sf94_rel25"],
                                     relsf94_dif[,"sf94_rel30"],
                                     relsf94_dif[,"sf94_rel35"]))
releffectsize_graph<-data.frame(relative_mort_red=rep(rrr_list,3),
                             baseline_mort=rep(c("0.25","0.30","0.35"), each = 9))
releffectsize_graph<-cbind(releffectsize_graph, relsf94_dif)
#grahp
relgraph_effectsize<-ggplot(releffectsize_graph, 
                            aes(x=relative_mort_red, y=sf94_dif, group=baseline_mort, colour=baseline_mort))
relgraph_effectsize + geom_line()+
  xlab("Relative reduction in mortality") +
  ylab("Difference in SF94")


#OUTPUT
#3 


# Steven's suggestion
# elements of baseline are ( probability of death, SF94 day 0, SF94 day 5 )
# elements of coef are (intercept, coefficient of SF94 day 0, coefficient of SF94 day 5) from the logistic model.
# mortSF is a function that itself creates a function. Tell mortSF baseline and coef, and it outputs a function
# that gives the relationship between change in SF94 day5, and change in probability of death
linear_uni_model
intercept<-1.6342
coefday5<--0.8731
#calculate baseline SF94
mort1<-0.25 #baseline mortality
logoddsmort1<-log(1/((1-mort1)/mort1)) #to odds
 # y = intercept + x1b1 +x2b2 >> (y-intercept- x1b1)/b2 = x2 = SF94 'at baseline'D5
baselinesf94<-(logoddsmort1-intercept)/coefday5 #y = logoddsmort
baselinesf94
#mort 25% > baseline SF94 = 3.130011
#mort 30% > baseline SF94 = 2.842169
#mort 35% > baseline SF94 = 2.580734

baseline <- c(0.35, 0, 2.580734)
coef <- c(1.6342, 0, -0.8731) 
mortSF <- function(baseline, coef){
  f2 <- function(deltaSF){
    a <- log( (baseline[1] - deltaSF)/( 1-baseline[1]  + deltaSF)) - coef[1] - coef[2]*baseline[2] - coef[3]*baseline[3]
    return( a/coef[3]  )
  }
  return(f2) 
}

f2 <- mortSF(baseline, coef)


ticksx<-c(0.05,0.06,0.07,0.08,0.09,0.1,0.11,0.12,0.13,0.14,0.15)
ticksy<-c(0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0,1.1,1.2,1.3,1.4,1.5)
curve(f, from=0.05, to=0.15, xlab="Absolute mortality difference",
      ylab="Difference in SF94", ylim=c(0,1.5), col="Red", xaxt="n")
axis(1, at=ticksx)
axis(2, at=ticksy)
curve(f1, from=0.05, to=0.15, xlab="Absolute mortality difference", ylab="Difference in SF94", col="blue", add = T)
curve(f2, from=0.05, to=0.15, xlab="Absolute mortality difference", ylab="Difference in SF94",col="green", add = T)
legend("topleft", legend = c("25% mortality", "30% mortality", "35% mortality"),
       text.col = ("black"),
       fill= c("red", "blue", "green"),
       title="Baseline mortality")
rpng.off()

#numbers on the regression analysis
library(gtsummary)
linear_model_glm_P <- glm(mortality_28 ~ sf94_day0 + sf94_day5_P, family=binomial, data=regresson_df_P)
linear_model_glm_P  %>% tbl_regression(exponentiate=T, intercept= T)

#OUTPUT
#HTML code


