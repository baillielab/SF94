library(plyr)
library(dplyr)
library(ggplot2)
library(data.table)
library(tidyr)
library(rms)

df_1<-fread("/home/skerr/Data/ccp_subset_derived.csv", data.table = FALSE )

#df_1<-fread("/home/u034/mcswets/df_20211402.csv", data.table = FALSE)
#df_1<-df_1[,c(2:90)]

#OUTPUT
#number of subjects before and after filters

numberSubs <- data.frame( before_filter =  length(unique(df_1$subjid)) ,
                          after_filter = length(unique(subset1$subjid))   )

write.csv(numberSubs,"/home/skerr/Git/SF94/Outputs/numberSubs.csv")



########################## FUNCTIONS ############################

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
  
  derived <-  df2  %>%  mutate(rn= row_number()) %>% spread (days_since_start, variable) %>% dplyr::select(-rn)
  
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


#Correlation subsets: same subjects for different days
#keep only 1 row for each subject
f <- function(x) {
  x <- na.omit(x)
  if (length(x) > 0) unique(x) else NA
}

####################################################################################

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

#input data = df_1, filters are applied at a later point in time
df_1_base_sf94<-createDF(df_1, "base", "sf94", 16)

#regression model D5 SF94 and mortality
#with sf94 values regression
day05<-df_1_base_sf94[,c("subjid","0","5","8","10","11","12","13","14","15","16")] #select subjid, day 0 and day 5 and day 8
day05<-day05%>%
  dplyr::rename(sf94_day0= "0",sf94_day5= "5", sf94_day8= "8",
                sf94_day10= "10",sf94_day11= "11", sf94_day12= "12",
                  sf94_day13= "13",sf94_day14= "14", sf94_day15= "15",sf94_day16= "16" )
day05<-setDT(day05)[, lapply(.SD, na.omit), by=subjid] #keep 1 entry/subject

#calculate what number of subjects needs to be added to get correct proportion
dead_alive_to_add<-function(day_of_interest){
  dead<-(length(unique(df_1$subjid[(df_1$day_of_death < day_of_interest)])))/length(unique(df_1$subjid))
  alive<-(length(unique(df_1$subjid[(df_1$day_of_discharge < day_of_interest)])))/length(unique(df_1$subjid))
  in_study<-sum((!is.na(df_1$sf94) & df_1$days_since_start == day_of_interest), na.rm=T)
  percentage_in_study<- 1-dead-alive
  dead_to_add<- (dead*in_study)/percentage_in_study
  alive_to_add<-(alive*in_study)/percentage_in_study
  summary_dead_alive<-c("% dead" = dead,"% alive"= alive,"% in study" =percentage_in_study,
                        "dead to add" =dead_to_add,"alive to add" =alive_to_add)
  return(summary_dead_alive)
}
sum_d5<-dead_alive_to_add(5)
sum_d8<-dead_alive_to_add(8)
sum_d10<-dead_alive_to_add(10)
sum_d11<-dead_alive_to_add(11)
sum_d12<-dead_alive_to_add(12)
sum_d13<-dead_alive_to_add(13)
sum_d14<-dead_alive_to_add(14)
sum_d15<-dead_alive_to_add(15)
sum_d16<-dead_alive_to_add(16)

proportional_numbers<-cbind(sum_d5[c(1,2)], sum_d8[c(1,2)],sum_d10[c(1,2)],sum_d11[c(1,2)],sum_d12[c(1,2)],sum_d13[c(1,2)], 
                            sum_d14[c(1,2)],sum_d15[c(1,2)],sum_d16[c(1,2)])
write.csv(proportional_numbers,"/home/skerr/Git/SF94/Outputs/proportional_numbers.csv")

day05_P<-day05 #for proportional deaths
mortality<-df_1[,c("subjid","mortality_28", "day_of_death", "day_of_discharge")]
mortality<-mortality%>%
  group_by(subjid)%>%
  slice(which.min(mortality_28))
day05_P<-left_join(day05_P, mortality, by="subjid") #add mortality
head(day05_P)
prop_added<- function(sf_day_to_replace, dead_to_add_fun, alive_to_add_fun, day_of_interest){
  set.seed(1234)
  rows_to_replace<-which(is.na(day05_P[[sf_day_to_replace]]) &day05_P$day_of_death < day_of_interest ) #find rows with missing values and who died
  day05_P[[sf_day_to_replace]][sample(rows_to_replace, dead_to_add_fun)]<- 0.5 #and add correct number of dead patients
  rows_to_replace<-which(is.na(day05_P[[sf_day_to_replace]]) & day05_P$day_of_discharge < day_of_interest) #same for patients who lived
  day05_P[[sf_day_to_replace]][sample(rows_to_replace, alive_to_add_fun)]<- 4.76
  day05_P<-data.frame(day05_P)
  day05_P<-day05_P[c("subjid", sf_day_to_replace)]
  return(day05_P)
}

sf94_day5_P<-prop_added("sf94_day5", sum_d5[("dead to add")], sum_d5[("alive to add")], 5)
sf94_day8_P<-prop_added("sf94_day8", sum_d8[("dead to add")], sum_d8[("alive to add")], 8)

#merge dataframes together
#library(plyr)
sf94_D5_D8<-join_all(list(sf94_day5_P, sf94_day8_P), by="subjid", type="full") #if we want to add additional days to the analysis
sf94_D5_D8<-sf94_D5_D8%>%
  dplyr::rename(sf94_day5_P= sf94_day5, sf94_day8_P=sf94_day8) #change names to differentiate from un-edited values
#change this variable to include another day ^
#detach("package:plyr", unload=T)
#Missing data
library(naniar)
day5_P<-sf94_D5_D8[,c("subjid","sf94_day5_P", "sf94_day8_P")]
prop_original<-left_join(day5_P, day05, by="subjid") #merge with daily values
missing_add_prop<-miss_var_summary(prop_original)
write.csv(missing_add_prop,"/home/skerr/Git/SF94/Outputs/missing_add_prop.csv")
day_of_outcome<-df_1[,c("subjid", "day_of_death", "day_of_discharge")] #add these to calculate deaths/discharges in cohort D5+D8
day_of_outcome<-day_of_outcome%>% #1 value for each subject
group_by(subjid) %>% 
  summarise_all(funs(f))
day_of_outcome<-data.frame(day_of_outcome)
prop_original<-left_join(prop_original, day_of_outcome, by="subjid") #merge 2 dataframes
prop_original<-prop_original%>% #1 value for each subject
  group_by(subjid) %>% 
  summarise_all(funs(f))
prop_original<-data.frame(prop_original)


#missing values complete dataset
#df_1_basedd_who<-createDF(df_1, "basedd", "severity_scale_ordinal", 16)
#dd_dataset<-df_1_basedd_who
#dd_dataset<-dd_dataset%>%
#  group_by(subjid) %>% 
#  summarise_all(funs(f))
#dd_dataset<-data.frame(dd_dataset)
#miss_dd<-miss_var_summary(dd_dataset)
#write.csv(miss_dd,"/home/skerr/Git/SF94/Outputs/miss_dd.csv")
#cohort_size_total<-nrow(dd_dataset)
#cohort D5
day5_prop<-subset(prop_original, !is.na(sf94_day5_P)) #starting set: D5 is known
dead_alive_function_D5<-function(day_of_interest){
  dead<-(length(unique(day5_prop$subjid[(day5_prop$day_of_death < day_of_interest)])))/length(unique(day5_prop$subjid))
  alive<-(length(unique(day5_prop$subjid[(day5_prop$day_of_discharge < day_of_interest)])))/length(unique(day5_prop$subjid))
  summary_dead_alive_cohort<-c("% dead d5" = dead,"% alive d5"= alive)
  return(summary_dead_alive_cohort)
}
d5_cohort_sumD5<-dead_alive_function_D5(5)
d5_cohort_sumD8<-dead_alive_function_D5(8)
d5_cohort_sumD10<-dead_alive_function_D5(10)
d5_cohort_sumD11<-dead_alive_function_D5(11)
d5_cohort_sumD12<-dead_alive_function_D5(12)
d5_cohort_sumD13<-dead_alive_function_D5(13)
d5_cohort_sumD14<-dead_alive_function_D5(14)
d5_cohort_sumD15<-dead_alive_function_D5(15)
d5_cohort_sumD16<-dead_alive_function_D5(16)
cohort_size_d5<-nrow(day5_prop)

miss_day5<-miss_var_summary(day5_prop)
write.csv(miss_day5,"/home/skerr/Git/SF94/Outputs/miss_day5.csv")

dead_alive_d5<-cbind(d5_cohort_sumD5, d5_cohort_sumD8, d5_cohort_sumD10, d5_cohort_sumD11, d5_cohort_sumD12, 
                     d5_cohort_sumD13, d5_cohort_sumD14, d5_cohort_sumD15, d5_cohort_sumD16)
write.csv(dead_alive_d5,"/home/skerr/Git/SF94/Outputs/dead_alive_d5.csv")

#cohort D8
day8_prop<-subset(prop_original, !is.na(sf94_day8_P)) #starting set: D8 is known
dead_alive_function_D8<-function(day_of_interest){
  dead<-(length(unique(day8_prop$subjid[(day8_prop$day_of_death < day_of_interest)])))/length(unique(day8_prop$subjid))
  alive<-(length(unique(day8_prop$subjid[(day8_prop$day_of_discharge < day_of_interest)])))/length(unique(day8_prop$subjid))
  summary_dead_alive_cohort<-c("% dead d8" = dead,"% alive d8"= alive)
  return(summary_dead_alive_cohort)
}
d8_cohort_sumD5<-dead_alive_function_D8(5)
d8_cohort_sumD8<-dead_alive_function_D8(8)
d8_cohort_sumD10<-dead_alive_function_D8(10)
d8_cohort_sumD11<-dead_alive_function_D8(11)
d8_cohort_sumD12<-dead_alive_function_D8(12)
d8_cohort_sumD13<-dead_alive_function_D8(13)
d8_cohort_sumD14<-dead_alive_function_D8(14)
d8_cohort_sumD15<-dead_alive_function_D8(15)
d8_cohort_sumD16<-dead_alive_function_D8(16)
cohort_size_d8<-nrow(day8_prop)

miss_day8<-miss_var_summary(day8_prop)
write.csv(miss_day8,"/home/skerr/Git/SF94/Outputs/miss_day8.csv")

#cohort_sizes<-cbind(cohort_size_total, cohort_size_d5, cohort_size_d8)
#write.csv(cohort_sizes,"/home/skerr/Git/SF94/Outputs/cohort_sizes.csv")

dead_alive_d8<-cbind(d8_cohort_sumD5, d8_cohort_sumD8, d8_cohort_sumD10, d8_cohort_sumD11, d8_cohort_sumD12, 
                     d8_cohort_sumD13, d8_cohort_sumD14, d8_cohort_sumD15, d8_cohort_sumD16)
write.csv(dead_alive_d8,"/home/skerr/Git/SF94/Outputs/dead_alive_d8.csv")



#add proportional D5 and D8 to D0
sf94_D0<-day05[,c("subjid", "sf94_day0")] #only take necessary columns
#library(plyr)
sf94_D5_D8<-merge(sf94_D5_D8, sf94_D0, by="subjid") #combine 2 dataframes by subjid
#detach("package:plyr", unload=T)
#calculate delta variables
sf94_D5_D8$delta_SF94_05<-sf94_D5_D8$sf94_day5_P - sf94_D5_D8$sf94_day0 #calculate difference from day 0 to day 5
sf94_D5_D8$delta_SF94_08<-sf94_D5_D8$sf94_day8_P - sf94_D5_D8$sf94_day0 #same for D0-D8


#create small df with the independent predictor variables (except sf94) and outcome variable
predictor_variables<-df_1[,c("subjid", "sex", "age_estimateyears", "mortality_28")]
predictor_variables<-predictor_variables %>% group_by(subjid)%>%slice(which.max(mortality_28))

#join both together
regresson_df_P<-left_join(sf94_D5_D8,predictor_variables, by="subjid")
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
time_to_improvement<-df_1[,c("subjid", "who_days_to_improve2", "who_days_to_improve1", "severity_scale_ordinal")]
time_to_improvement<- time_to_improvement %>% group_by(subjid)%>%add_count(number=!is.na(severity_scale_ordinal))
time_to_improvement<-time_to_improvement %>% #keep 1 value per subject
  group_by(subjid)%>%
  slice(which.min(severity_scale_ordinal))
time_to_improvement<-time_to_improvement %>%
  mutate( #if zero or 1 observation, change to NA. if there is a number, there is sustained improvement, so change to 1. 
    sustained_1L_improvement= case_when( #if >1 who value but no improvement > 0
      n<2 ~ NA_real_, !is.na(who_days_to_improve1) ~ 1, is.na(who_days_to_improve1) ~ 0 ))
time_to_improvement<-time_to_improvement %>%
  mutate( #if there is a number, there is sustained improvement, so change to 1. If not, change to 0
    sustained_2L_improvement= case_when(n<2 ~ NA_real_,
      !is.na(who_days_to_improve2) ~ 1, is.na(who_days_to_improve2) ~ 0 ))
time_to_improvement<-time_to_improvement[,c("subjid", "sustained_1L_improvement", "sustained_2L_improvement")]
time_to_improvement<-data.frame(time_to_improvement)

#bind to rest of data
regresson_df_P<-left_join(regresson_df_P, time_to_improvement, by="subjid")
regresson_df_P<-regresson_df_P %>%
  group_by(subjid)%>%
  summarise_all(funs(f))
regresson_df_P<-regresson_df_P %>%
  group_by(subjid)%>%
  slice(which(WHOD5_P==min(WHOD5_P)|is.na(WHOD5_P)))
#write.csv(regresson_df_P,"regresson_df_P.csv")
#attach(regresson_df_P)
#ddist <- datadist(sf94_day5_P, sf94_day8_P, sf94_day0, sex, age_estimateyears, mortality_28, WHOD5_P, WHOD8_P,
#                  sustained_1L_improvement, sustained_2L_improvement)
#options(datadist='ddist')
#detach(regresson_df_P)

#apply age filter and supp oxygen filter
subjects_to_include <- filter(df_1, ( fio2 >=0.22 & days_since_start %in% c(0,1,2)  & age_estimateyears >19 & age_estimateyears <76 ) )['subjid']
subset1<-regresson_df_P[regresson_df_P$subjid %in% subjects_to_include$subjid,] 
subset1 <- as.data.frame(subset1)

#apply age filter and need for oxygen at day 0
subjects_to_include <- filter(df_1, ( fio2 >=0.22 & days_since_start %in% c(0)  & age_estimateyears >19 & age_estimateyears <76 ) )['subjid']
subset2<-regresson_df_P[regresson_df_P$subjid %in% subjects_to_include$subjid,] 
subset2<- as.data.frame(subset2)

#only age filter
subjects_to_include <- filter(df_1, (age_estimateyears >19 & age_estimateyears <76 ) )['subjid']
subset3<-regresson_df_P[regresson_df_P$subjid %in% subjects_to_include$subjid,] 
subset3 <- as.data.frame(subset3)

#######################################Mortality##################################################
## INPUTS REQUIRED ##
# alpha - significance level 
# power - specified power
# p1 - proportion experiencing event within specified time frame in control arm
# p2 - proportion experiencing event within specified time frame in active arm

riskratio_function<-function(subset_df,mort_dif){
  p1 <- sum(subset_df$mortality_28 == 1, na.rm = T)/ sum(!is.na(subset_df$mortality_28))
  p2=p1*mort_dif
  riskratio_mort<-p1/p2
  riskratio_output<-cbind(p1,p2,riskratio_mort)
  return(riskratio_output)
}
rr_subset1<-riskratio_function(subset1, 0.85)
rr_subset2<-riskratio_function(subset2, 0.85)
rr_subset3<-riskratio_function(subset3, 0.85)
rr_mort<-rbind(rr_subset1, rr_subset2, rr_subset3)
write.csv(rr_mort,"/home/skerr/Git/SF94/Outputs/rr_mort.csv")

sample_function<-function(subset_df, mort_dif){
  p1 <- sum(subset_df$mortality_28 == 1, na.rm = T)/ sum(!is.na(subset_df$mortality_28))
  p2=p1*mort_dif
  sample <- power.prop.test(n=NULL,p1=p1,p2=p2,power=0.8)
  return(sample)
}
sample_subset1<-sample_function(subset1,0.85)
sample_subset2<-sample_function(subset2,0.85)
sample_subset3<-sample_function(subset3,0.85)

cont_cor_function<-function(samplefunc){
  ncorrect <- function(n,p1,p2){
    round(2 * (n/4)*(1+sqrt(1+(4/(n*(p1-p2)))))^2)
  } 
  ncorrect(samplefunc$n, samplefunc$p1, samplefunc$p2)
}
sample_cont_correction_1<-cont_cor_function(sample_subset1)
sample_cont_correction_2<-cont_cor_function(sample_subset2)
sample_cont_correction_3<-cont_cor_function(sample_subset3)
samplesize_mortality<-cbind(sample_cont_correction_1,sample_cont_correction_2, sample_cont_correction_3)
write.csv(samplesize_mortality,"/home/skerr/Git/SF94/Outputs/samplesize_mortality.csv")

mort_table_function<-function(subset_df){
  mortTable <- table(subset_df[["mortality_28"]])
  return(mortTable)
}
mort_table_1 <- as.data.frame(mort_table_1)

#proportion who died/ who lived
mort_table_1<-mort_table_function(subset1)
mort_table_2<-mort_table_function(subset2)
mort_table_3<-mort_table_function(subset3)
mort_table_output<-rbind(mort_table_1, mort_table_2, mort_table_3)
write.csv(mort_table_output,"/home/skerr/Git/SF94/Outputs/mort_table_output.csv")


##############################################################
## Sample size formulae for analyses using log-rank methods ##
##############################################################

# taken from formulae described in Chapter 10 of Modelling Survival Data in Medical Research, 2nd Edition, Collett

## INPUTS REQUIRED ##
# alpha - significance level 
# power - specified power
# HR - assumed rate ratio for treatment effect
# hazc - baseline hazard in control arm (this is equal to the proportion that would be expected to experience the event during ONE unit of follow-up)
# a - length of recruitment period (defaults to 0 here as patients are to be followed for a set period of time, e.g. 28 days, which means the length of the recruitment period will not affect the median duration of follow-up
# f - length of follow-up (defaults to 28)

lrsamplesize <- function(subset_df,alpha,power,HR,a,f){
  # this function calculates the TOTAL sample size needed for a study that is to be analysed using log-rank methods
  p1 <- sum(subset_df$mortality_28 == 1, na.rm = T)/ sum(!is.na(subset_df$mortality_28))
  hazc = (1-exp(log(1-p1)/f)) 
  hazt <- hazc*HR
  d <- (4*((qnorm(1-alpha/2)+qnorm(power))^2))/((log(HR))^2)
  P_event <- 1-(1/6)*(avsurv(f,hazc,hazt)+4*avsurv(0.5*a+f,hazc,hazt)+avsurv(a+f,hazc,hazt))
  n <- d/P_event
  return(n)
}

avsurv <- function(time,haz1,haz2){
  (survfunc(time,haz1)+survfunc(time,haz2))/2
}

survfunc <- function(time,haz){
  (1-haz)^time
}
# calculate the rate ratio that is equivalent to the risk ratio assumed in the code above for sample size calculations based on difference in proportions
# p1 and p2 are proportions experiencing the event within specified time frame in control and active arms as defined above
HR_func<-function(subset_df,mort_dif, f=28){
  p1 <- sum(subset_df$mortality_28 == 1, na.rm = T)/ sum(!is.na(subset_df$mortality_28))
  p2=p1*mort_dif
  HR = (1-exp(log(1-p2)/f))/(1-exp(log(1-p1)/f))
  return(HR)
}

HR_ss_1<-HR_func(subset1, 0.85)
HR_ss_2<-HR_func(subset2, 0.85)
HR_ss_3<-HR_func(subset3, 0.85)
HR_mort<-cbind(HR_ss_1,HR_ss_2,HR_ss_3)
write.csv(HR_mort,"/home/skerr/Git/SF94/Outputs/HR_mort.csv")
ss_logrank_ss1<-round(lrsamplesize(subset1,0.05,0.8,HR_ss_1,0,28))
ss_logrank_ss2<-round(lrsamplesize(subset1,0.05,0.8,HR_ss_2,0,28))
ss_logrank_ss3<-round(lrsamplesize(subset1,0.05,0.8,HR_ss_3,0,28))
samplesize_logrank_mort<-cbind(ss_logrank_ss1,ss_logrank_ss2,ss_logrank_ss3)
write.csv(samplesize_logrank_mort,"/home/skerr/Git/SF94/Outputs/samplesize_logrank_mort.csv")

#############################################################################################################
#Summary stats
function_mean_sd<-function(subset_df){
  meanSD <- as.data.frame( rbind(sapply(subset_df[c('sf94_day5_P','sf94_day8_P')], mean, na.rm=T),  
                                 sapply(subset_df[c('sf94_day5_P','sf94_day8_P')], sd, na.rm=T)) )
  rownames(meanSD) <- c('mean', 'SD')
  return(meanSD)
}
meanSD_subset1<-function_mean_sd(subset1)
meanSD_subset2<-function_mean_sd(subset2)
meanSD_subset3<-function_mean_sd(subset3)
meanSD_output<-cbind(meanSD_subset1,meanSD_subset2, meanSD_subset3)


correlation_function<-function(subset_df) {
  correlation_subset_05<-subset(subset_df, ((!is.na(subset_df[,"sf94_day0"])&(!is.na(subset_df[,"sf94_day5_P"])))))
  correlation_subset_08<-subset(subset_df, ((!is.na(subset_df[,"sf94_day0"])&(!is.na(subset_df[,"sf94_day8_P"])))))
  # DAY 0/5
  w <-  correlation_subset_05[,"sf94_day0"]
  x <-  correlation_subset_05[,"sf94_day5_P"]
  day05_cor<-cor(w,x)
  # DAY 0/8
  y <-  correlation_subset_08[,"sf94_day0"]
  z <-  correlation_subset_08[,"sf94_day8_P"]
  day08_cor<-cor(y,z)
  cor_output<-list(day05_cor,day08_cor)
  return(cor_output)
}
cor_subset1<-correlation_function(subset1)
cor_subset2<-correlation_function(subset2)
cor_subset3<-correlation_function(subset3)
correlation_output<-cbind(cor_subset1, cor_subset2, cor_subset3)



sf94_regression<-function(subset_df, mort_difference){
  sf94_d5<-lrm(mortality_28 ~ sf94_day5_P+sf94_day0+ age_estimateyears+ sex, data = subset_df)
  sf94_d8<-lrm(mortality_28 ~ sf94_day8_P+sf94_day0+ age_estimateyears+ sex, data = subset_df)
  sf94_predictD5<-predict(sf94_d5, type = 'fitted'  )
  sf94_predictD8<-predict(sf94_d8, type = 'fitted'  )
  coef_d5<-sf94_d5$coef[2] #is sf94 day 5 coefficient
  coef_d8<-sf94_d8$coef[2] #is sf94 day 8 coefficient
  
  effect_size_calc <- function(prob_pred, treatment, coef){
    return( mean( log((treatment*(1-prob_pred)) / (1- treatment * prob_pred)) / coef , na.rm = TRUE) )
  }
  d5_sf94_effectsize<-effect_size_calc(sf94_predictD5, mort_difference, coef_d5)
  d8_sf94_effectsize<-effect_size_calc(sf94_predictD8, mort_difference, coef_d8)
  d5_d8_effectsize<-cbind(d5_sf94_effectsize, d8_sf94_effectsize)
  return(d5_d8_effectsize)
}

sf94_regression_subset1<-sf94_regression(subset1,0.85)
sf94_regression_subset2<-sf94_regression(subset2,0.85)
sf94_regression_subset3<-sf94_regression(subset3,0.85)
sf94_effectsize<-rbind(sf94_regression_subset1, sf94_regression_subset2,sf94_regression_subset3)

## INPUTS REQUIRED ##
# alpha - significance level =0.05
# power - specified power =0.8
# delta - difference between mean change scores
# sd - standard deviation of the outcome measure
# rho - correlation between the outcome measured at baseline and at follow-up

power_sf94<-function(alpha, power, delta, sd, rho){

power1 <- power.t.test(n=NULL, delta=delta, sd=sd, power=power, sig.level = alpha) # calculate sample size for a t test

ntotal <- 2*round(((1-(rho^2))*power1$n)) # apply ANCOVA correction
return(ntotal)
}

subset1_D5_SS<-power_sf94(0.05,0.8,sf94_regression_subset1[,1], meanSD_subset1[2,1], cor_subset1[[1]])
subset1_D8_SS<-power_sf94(0.05,0.8,sf94_regression_subset1[,2], meanSD_subset1[2,2], cor_subset1[[2]])
subset2_D5_SS<-power_sf94(0.05,0.8,sf94_regression_subset2[,1], meanSD_subset2[2,1], cor_subset2[[1]])
subset2_D8_SS<-power_sf94(0.05,0.8,sf94_regression_subset2[,2], meanSD_subset2[2,2], cor_subset2[[2]])
subset3_D5_SS<-power_sf94(0.05,0.8,sf94_regression_subset3[,1], meanSD_subset3[2,1], cor_subset3[[1]])
subset3_D8_SS<-power_sf94(0.05,0.8,sf94_regression_subset3[,2], meanSD_subset3[2,2], cor_subset3[[2]])

sf94_samplesize<-cbind(subset1_D5_SS,subset1_D8_SS,subset2_D5_SS,subset2_D8_SS,subset3_D5_SS,subset3_D8_SS)

#write to github
write.csv(meanSD_output,"/home/skerr/Git/SF94/Outputs/meanSD_output.csv")
write.csv(correlation_output,"/home/skerr/Git/SF94/Outputs/correlation_output.csv")
write.csv(sf94_effectsize,"/home/skerr/Git/SF94/Outputs/sf94_effectsize.csv")
write.csv(sf94_samplesize,"/home/skerr/Git/SF94/Outputs/sf94_samplesize.csv")


###########################################################################################################################
#WHO
library(MASS)
who_function<-function(subset_df, mortality_diff){
  WHOD5_model_S<-polr(as.factor(WHOD5_P) ~ age_estimateyears+ sex, data = subset_df, Hess=T)
  WHOD8_model_S<-polr(as.factor(WHOD8_P) ~ age_estimateyears+ sex, data = subset_df, Hess=T)
  
  pred_D5 <- predict(WHOD5_model_S, newdata = subset_df, type = 'probs')
  pred_D8 <- predict(WHOD8_model_S, newdata = subset_df, type = 'probs')
  
  effect_size_calc_OR <- function(prob_pred, treatment){
    mean_prob <- mean(prob_pred, na.rm = TRUE)
    return( treatment*( 1- mean_prob) / (1 - treatment * mean_prob)  )
  }
  d5_effectsize_who<-effect_size_calc_OR(pred_D5[,"10"], mortality_diff)
  d8_effectsize_who<-effect_size_calc_OR(pred_D8[,"10"], mortality_diff)
  who_effectsize<-cbind(d5_effectsize_who, d8_effectsize_who)
  return(who_effectsize)
}
who_subset1<-who_function(subset1, 0.85)
who_subset2<-who_function(subset2, 0.85)
who_subset3<-who_function(subset3, 0.85)
who_effectsize<-rbind(who_subset1, who_subset2, who_subset3)


who_table_function<-function(subset_df){
  whoTable <- rbind( table(subset_df[["WHOD5_P"]]), table(subset_df[["WHOD8_P"]])   )
  return(whoTable)
}

who_table_1<-who_table_function(subset1)
who_table_2<-who_table_function(subset2)
who_table_3<-who_table_function(subset3)
who_table_output<-rbind(who_table_1, who_table_2, who_table_3)
write.csv(who_table_output,"/home/skerr/Git/SF94/Outputs/who_table_output.csv")

# use function posamsize in package Hmisc
library(Hmisc)

## INPUTS REQUIRED ##
# alpha - significance level (=0.05)
# power - specified power (=0.8)
# OR - the odds ratio to be able to detect
# pavg  - a vector of probabilities of being in each category of the ordinal scale (averaged over the two treatment groups), needs to sum to one
# OR= this will need to be estimated from the models once finalised, but I have assumed a 15% reduction in odds for the moment 
# n= number of patients with non-missing values of WHO day 5 in the info I have
# We can only estimate the vector of probabilities for control group, denoted by p1 (the numbers in each WHO category at day 5 have been taken from the info I have)


who_prop<-function(subset_df){
  p1_D5 <-table(subset_df[,"WHOD5_P"])/sum(!is.na(subset_df[,"WHOD5_P"]))
  p1_D8 <-table(subset_df[,"WHOD8_P"])/sum(!is.na(subset_df[,"WHOD8_P"]))
  prop_who<-cbind(p1_D5,p1_D8)
  return(prop_who)
}
who_prop_ss1<-who_prop(subset1)
who_prop_ss2<-who_prop(subset2)
who_prop_ss3<-who_prop(subset3)
who_prop_output<-cbind(who_prop_ss1, who_prop_ss2, who_prop_ss3)
write.csv(who_prop_output,"/home/skerr/Git/SF94/Outputs/who_prop_output.csv")



who_effectsize_function_ss<-function(subset_df, who_subset_df){
  p1_D5 <-table(subset_df[,"WHOD5_P"])/sum(!is.na(subset_df[,"WHOD5_P"]))
  p1_D8 = table(subset_df[,"WHOD8_P"])/sum(!is.na(subset_df[,"WHOD8_P"]))
  
  who_effectsize_function<-function(alpha, power, OR, p1){ #here's how to compute the average over the two groups from p1 and OR
    p2   <- pomodm(p=p1, odds.ratio=OR)
    pavg <- (p1 + p2) / 2
    posamsize(p=pavg, odds.ratio=OR, alpha=alpha, power=power)
  }
  
  who_samplesize_d5<-who_effectsize_function( 0.05, 0.8, who_subset_df[[1]], p1_D5)
  who_samplesize_d8<-who_effectsize_function( 0.05, 0.8, who_subset_df[[2]], p1_D8)
  who_samplesize<-cbind(who_samplesize_d5, who_samplesize_d8)
  return(who_samplesize)
}
who_samplesize_SS1<-who_effectsize_function_ss(subset1, who_subset1)
who_samplesize_SS2<-who_effectsize_function_ss(subset2, who_subset2)
who_samplesize_SS3<-who_effectsize_function_ss(subset3, who_subset3)

who_samplesize<-cbind(who_samplesize_SS1,who_samplesize_SS2,who_samplesize_SS3)

write.csv(who_effectsize,"/home/skerr/Git/SF94/Outputs/who_effectsize.csv")
write.csv(who_samplesize,"/home/skerr/Git/SF94/Outputs/who_samplesize.csv")


####################################################################################################################################
## Sample size formulae for analyses using difference in proportions ##

sustained_improvement_power<-function(subset_df, mort_dif){
  p1_1L <- sum(subset_df$sustained_1L_improvement == 1, na.rm = T)/ sum(!is.na(subset_df$sustained_1L_improvement))
  p2_1L=p1_1L*mort_dif
  ss_sustained_improvement_1L<-power.prop.test(power=0.8, p1=p1_1L, p2=p2_1L)
  p1_2L <- sum(subset_df$sustained_2L_improvement == 1, na.rm = T)/ sum(!is.na(subset_df$sustained_2L_improvement))
  p2_2L=p1_2L*mort_dif
  ss_sustained_improvement_2L<-power.prop.test(power=0.8, p1=p1_2L, p2=p2_2L)
  ss_sustained_improvement<-cbind(ss_sustained_improvement_1L, ss_sustained_improvement_2L)
  return(ss_sustained_improvement)
}
sustained_improvement_subset1<-sustained_improvement_power(subset1, 0.85)
sustained_improvement_subset2<-sustained_improvement_power(subset2, 0.85)
sustained_improvement_subset3<-sustained_improvement_power(subset3, 0.85)

saveRDS(sustained_improvement_subset1,"/home/skerr/Git/SF94/Outputs/sustained_improvement_subset1.rds")
saveRDS(sustained_improvement_subset2,"/home/skerr/Git/SF94/Outputs/sustained_improvement_subset2.rds")
saveRDS(sustained_improvement_subset3,"/home/skerr/Git/SF94/Outputs/sustained_improvement_subset3.rds")
readRDS("/Users/Maaike/Downloads/sustained_improvement_subset3.rds")

table_sus_imp<-function(subset_df){
  susimp_1L<-table(subset_df$sustained_1L_improvement)
  susimp_2L<-table(subset_df$sustained_2L_improvement)
  sus_imp<-cbind(susimp_1L, susimp_2L)
  return(sus_imp)
}
sus_imp_ss1<-table_sus_imp(subset1)
sus_imp_ss2<-table_sus_imp(subset2)
sus_imp_ss3<-table_sus_imp(subset3)
sus_imp_output<-cbind(sus_imp_ss1, sus_imp_ss2, sus_imp_ss3)
write.csv(sus_imp_output,"/home/skerr/Git/SF94/Outputs/sus_imp_output.csv")