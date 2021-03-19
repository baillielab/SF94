library(dplyr)
library(ggplot2)
library(data.table)
library(tidyr)
library(rms)
library(dplyr)

df_1<-fread("/home/skerr/Data/ccp_subset_derived.csv", data.table = FALSE )

#df_1<-fread("/home/u034/mcswets/df_20211402.csv", data.table = FALSE)
#df_1<-df_1[,c(2:90)]

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
mortality<-df_1[,c("subjid","mortality_28")]
mortality<-mortality%>%
  group_by(subjid)%>%
  slice(which.min(mortality_28))
day05_P<-left_join(day05_P, mortality, by="subjid") #add mortality

prop_added<- function(sf_day_to_replace, dead_to_add_fun, alive_to_add_fun){
  set.seed(1234)
  rows_to_replace<-which(is.na(day05_P[[sf_day_to_replace]]) & day05_P$mortality_28 == 1) #find rows with missing values and who died
  day05_P[[sf_day_to_replace]][sample(rows_to_replace, dead_to_add_fun)]<- 0.5 #and add correct number of dead patients
  rows_to_replace<-which(is.na(day05_P[[sf_day_to_replace]]) & day05_P$mortality_28 == 0) #same for patients who lived
  day05_P[[sf_day_to_replace]][sample(rows_to_replace, alive_to_add_fun)]<- 4.76
  day05_P<-data.frame(day05_P)
  day05_P<-day05_P[c("subjid", sf_day_to_replace)]
  return(day05_P)
}

sf94_day5_P<-prop_added("sf94_day5", sum_d5[("dead to add")], sum_d5[("alive to add")])
sf94_day8_P<-prop_added("sf94_day8", sum_d8[("dead to add")], sum_d8[("alive to add")])
sf94_day10_P<-prop_added("sf94_day10", sum_d10[("dead to add")], sum_d10[("alive to add")])
sf94_day11_P<-prop_added("sf94_day11", sum_d11[("dead to add")], sum_d11[("alive to add")])
sf94_day12_P<-prop_added("sf94_day12", sum_d12[("dead to add")], sum_d12[("alive to add")])
sf94_day13_P<-prop_added("sf94_day13", sum_d13[("dead to add")], sum_d13[("alive to add")])
sf94_day14_P<-prop_added("sf94_day14", sum_d14[("dead to add")], sum_d14[("alive to add")])
sf94_day15_P<-prop_added("sf94_day15", sum_d15[("dead to add")], sum_d15[("alive to add")])
sf94_day16_P<-prop_added("sf94_day16", sum_d16[("dead to add")], sum_d16[("alive to add")])

#merge dataframes together
library(plyr)
sf94_d10_d16<-join_all(list(sf94_day5_P, sf94_day8_P, sf94_day10_P,sf94_day11_P,sf94_day12_P,sf94_day13_P,
                            sf94_day14_P,sf94_day15_P,sf94_day16_P), by="subjid", type="full")
sf94_D5_D8<-join_all(list(sf94_day5_P, sf94_day8_P), by="subjid", type="full") #if we want to add additional days to the analysis
sf94_D5_D8<-sf94_D5_D8%>%
  dplyr::rename(sf94_day5_P= sf94_day5, sf94_day8_P=sf94_day8) #change names to differentiate from un-edited values
#change this variable to include another day ^
detach("package:plyr", unload=T)
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

cohort_sizes<-cbind(cohort_size_total, cohort_size_d5, cohort_size_d8)
write.csv(cohort_sizes,"/home/skerr/Git/SF94/Outputs/cohort_sizes.csv")

dead_alive_d8<-cbind(d8_cohort_sumD5, d8_cohort_sumD8, d8_cohort_sumD10, d8_cohort_sumD11, d8_cohort_sumD12, 
                     d8_cohort_sumD13, d8_cohort_sumD14, d8_cohort_sumD15, d8_cohort_sumD16)
write.csv(dead_alive_d8,"/home/skerr/Git/SF94/Outputs/dead_alive_d8.csv")



#add proportional D5 and D8 to D0
sf94_D0<-day05[,c("subjid", "sf94_day0")] #only take necessary columns
library(plyr)
sf94_D5_D8<-merge(sf94_D5_D8, sf94_D0, by="subjid") #combine 2 dataframes by subjid
detach("package:plyr", unload=T)
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
#save numbers
table_1L_0<-sum(time_to_improvement$sustained_1L_improvement==0, na.rm=T)
table_1L_1<-sum(time_to_improvement$sustained_1L_improvement==1, na.rm=T)
table_2L_0<-sum(time_to_improvement$sustained_2L_improvement==0, na.rm=T)
table_2L_1<-sum(time_to_improvement$sustained_2L_improvement==1, na.rm=T)
insufdata_time_to_improvement<-sum(is.na(time_to_improvement$sustained_1L_improvement))
insufdata_time_to_improvement<-cbind(insufdata_time_to_improvement,table_1L_0,table_1L_1,table_2L_0,table_2L_1)
write.csv(insufdata_time_to_improvement,"/home/skerr/Git/SF94/Outputs/insufdata_time_to_improvement.csv")
#bind to rest of data
regresson_df_P<-left_join(regresson_df_P, time_to_improvement, by="subjid")
#to calculate mean for gender change to 1 and 0
regresson_df_P <- regresson_df_P %>%
  mutate(binairy_sex= case_when(
    regresson_df_P$sex == "Male" ~ "1",
    regresson_df_P$sex == "Female" ~ "0"
  ))

#apply age filter and supp oxygen filter
subjects_to_include <- filter(df_1, ( fio2 >=0.22 & days_since_start %in% c(0,1,2)  & age_estimateyears >19 & age_estimateyears <76 ) )['subjid']
regresson_df_P<-regresson_df_P[regresson_df_P$subjid %in% subjects_to_include$subjid,] 
regresson_df_P <- as.data.frame(regresson_df_P)

#First need to set data distribution for rms functions
attach(regresson_df_P)
ddist <- datadist(sf94_day0,sf94_day5_P,sf94_day8_P, delta_SF94_05,delta_SF94_08,sex, age_estimateyears, mortality_28,
                  sustained_1L_improvement, sustained_2L_improvement, WHOD5_P, WHOD8_P, binairy_sex)
options(datadist='ddist')
detach(regresson_df_P)

#WHO time to improvement
sus_1L_D5<-lrm(sustained_1L_improvement ~ sf94_day5_P + sf94_day0 + age_estimateyears+ sex, data = regresson_df_P)
sus_1L_D8<-lrm(sustained_1L_improvement ~ sf94_day8_P + sf94_day0 + age_estimateyears+ sex, data = regresson_df_P)
sus_2L_D5<-lrm(sustained_2L_improvement ~ sf94_day5_P + sf94_day0 + age_estimateyears+ sex, data = regresson_df_P)
sus_2L_D8<-lrm(sustained_2L_improvement ~ sf94_day8_P + sf94_day0 + age_estimateyears+ sex, data = regresson_df_P)

#######################################################################
## Sample size formulae for analyses using difference in proportions ##
#######################################################################

## these sample size calculations are done using the R function power.prop.test but with a continuity correction applied 
## see Sample Size Calculations for Randomized Controlled Trials, Wittes, Epidemiol Rev Vol. 24, No. 1, 2002 for details of formulae for continuity correction
head(regresson_df_P$sustained_1L_improvement)
table(regresson_df_P$sustained_1L_improvement)
prop_1L_sustained<-sum(regresson_df_P$sustained_1L_improvement == 1, na.rm=T)/ sum(!is.na(regresson_df_P$sustained_1L_improvement))
## INPUTS REQUIRED ##
# alpha - significance level 
# power - specified power
# p1 - proportion experiencing event within specified time frame in control arm
# p2 - proportion experiencing event within specified time frame in active arm

ncorrect <- function(n,p1,p2){
  round((n/4)*(1+sqrt(1+(4/(n*(p1-p2)))))^2)
}

alpha = 0.05
power = 0.8
p1 = prop_1L_sustained
p2 = 0.20
# check the risk ratio assumed from the proportions specified
p2/p1

sample <- power.prop.test(n=NULL,p1=p1,p2=p2,power=power,sig.level=alpha)
2*ncorrect(sample$n,sample$p1,sample$p2)

library(stringr)
#parser
get_model_stats = function(x) {
  cap = capture.output(print(x))
  #model stats
  stats = c()
  stats$R2.adj = str_match(cap, "R2 adj\\s+ (\\d\\.\\d+)") %>% na.omit() %>% .[, 2] %>% as.numeric()
  #coef stats lines
  coef_lines = cap[which(str_detect(cap, "Coef\\s+S\\.E\\.")):(length(cap) - 1)]
  #parse
  coef_lines_table = suppressWarnings(readr::read_table(coef_lines %>% stringr::str_c(collapse = "\n")))
  colnames(coef_lines_table)[1] = "Predictor"
  list(
    stats = stats,
    coefs = coef_lines_table
  )
}
#switch to a tibble as output format
sus_1L_D5<-get_model_stats(sus_1L_D5)
sus_1L_D8<-get_model_stats(sus_1L_D8)
sus_2L_D5<-get_model_stats(sus_2L_D5)
sus_2L_D8<-get_model_stats(sus_2L_D8)

write.csv(sus_1L_D5['coefs'],"/home/skerr/Git/SF94/Outputs/sus_1L_D5.csv")
write.csv(sus_1L_D8['coefs'],"/home/skerr/Git/SF94/Outputs/sus_1L_D8.csv")
write.csv(sus_2L_D5['coefs'],"/home/skerr/Git/SF94/Outputs/sus_2L_D5.csv")
write.csv(sus_2L_D8['coefs'],"/home/skerr/Git/SF94/Outputs/sus_2L_D8.csv")

#############################################################################################################
#Summary stats
meanSD <- as.data.frame( rbind(sapply(regresson_df_P[c('sf94_day5_P','sf94_day5_P')], mean, na.rm=T),  
                               sapply(regresson_df_P[c('sf94_day5_P','sf94_day5_P')], sd, na.rm=T)) )

rownames(meanSD) <- c('mean', 'SD')

#Correlation subsets: same subjects for different days
correlation_subset<-regresson_df_P
#make subsets of data in which all subjects have SF94 available for those 2 days
correlation_subset_05<-subset(correlation_subset, ((!is.na(correlation_subset[,"sf94_day0"])&(!is.na(correlation_subset[,"sf94_day5_P"])))))
correlation_subset_08<-subset(correlation_subset, ((!is.na(correlation_subset[,"sf94_day0"])&(!is.na(correlation_subset[,"sf94_day8_P"])))))
# DAY 0/5
w <-  correlation_subset_05[,"sf94_day0"]
x <-  correlation_subset_05[,"sf94_day5_P"]
day05_cor<-cor(w,x)
# DAY 0/8
y <-  correlation_subset_08[,"sf94_day0"]
z <-  correlation_subset_08[,"sf94_day8_P"]
day08_cor<-cor(y,z)

#Steven's code
#SF94
#day 0 as a separate predictor variable (instead of delta day5-day0)

sf94_d5_2<-lrm(mortality_28 ~ sf94_day5_P+sf94_day0+ age_estimateyears+ sex, data = regresson_df_P)
sf94_d8_2<-lrm(mortality_28 ~ sf94_day8_P+sf94_day0+ age_estimateyears+ sex, data = regresson_df_P)

sf94_predictD5<-predict(sf94_d5_2, type = 'fitted'  )
sf94_predictD8<-predict(sf94_d8_2, type = 'fitted'  )

coef_d5<-sf94_d5_2$coef[2] #is sf94 day 5 coefficient
coef_d8<-sf94_d8_2$coef[2] #is sf94 day 8 coefficient

effect_size_calc <- function(prob_pred, treatment, coef){
  return( mean( log((treatment*(1-prob_pred)) / (1- treatment * prob_pred)) / coef , na.rm = TRUE) )
}

d5_sf94_effectsize_15<-effect_size_calc(sf94_predictD5, 0.85, coef_d5) #15% relative mortality difference
d5_sf94_effectsize_10<-effect_size_calc(sf94_predictD5, 0.90, coef_d5) #10% relative mortality difference
d5_sf94_effectsize_05<-effect_size_calc(sf94_predictD5, 0.95, coef_d5) #05% relative mortality difference
d8_sf94_effectsize_15<-effect_size_calc(sf94_predictD8, 0.85, coef_d8) #15% relative mortality difference
d8_sf94_effectsize_10<-effect_size_calc(sf94_predictD8, 0.90, coef_d8) #10% relative mortality difference
d8_sf94_effectsize_05<-effect_size_calc(sf94_predictD8, 0.95, coef_d8) #05% relative mortality difference

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

sf94_05_samplesize<-power_sf94(0.05,0.8,d5_sf94_effectsize_05, meanSD[2,1], day05_cor)
sf94_10_samplesize<-power_sf94(0.05,0.8,d5_sf94_effectsize_10, meanSD[2,1], day05_cor)
sf94_15_samplesize<-power_sf94(0.05,0.8,d5_sf94_effectsize_15, meanSD[2,1], day05_cor)
sf94_05_samplesize_D8<-power_sf94(0.05,0.8,d8_sf94_effectsize_05, meanSD[2,2], day08_cor)
sf94_10_samplesize_D8<-power_sf94(0.05,0.8,d8_sf94_effectsize_10, meanSD[2,2], day08_cor)
sf94_15_samplesize_D8<-power_sf94(0.05,0.8,d8_sf94_effectsize_15, meanSD[2,2], day08_cor)

sf94_samplesize<-cbind(sf94_05_samplesize,sf94_10_samplesize,sf94_15_samplesize,sf94_05_samplesize_D8,
                       sf94_10_samplesize_D8,sf94_15_samplesize_D8, day05_cor, day08_cor)
sf94_effectsize<-cbind(d5_sf94_effectsize_15,d5_sf94_effectsize_10, d5_sf94_effectsize_05,
                       d8_sf94_effectsize_15, d8_sf94_effectsize_10, d8_sf94_effectsize_05)
write.csv(sf94_samplesize,"/home/skerr/Git/SF94/Outputs/sf94_samplesize.csv")
write.csv(sf94_effectsize,"/home/skerr/Git/SF94/Outputs/sf94_effectsize.csv")
write.csv(meanSD,"/home/skerr/Git/SF94/Outputs/meanSD.csv")
###########################################################################################################################
#WHO
WHOD5_model_S<-polr(as.factor(WHOD5_P) ~ age_estimateyears+ sex, data = regresson_df_P, Hess=T)
WHOD8_model_S<-polr(as.factor(WHOD8_P) ~ age_estimateyears+ sex, data = regresson_df_P, Hess=T)

pred_D5 <- predict(WHOD5_model_S, newdata = regresson_df_P, type = 'probs')
pred_D8 <- predict(WHOD8_model_S, newdata = regresson_df_P, type = 'probs')

effect_size_calc_OR <- function(prob_pred, treatment){
  mean_prob <- mean(prob_pred, na.rm = TRUE)
  return( treatment*( 1- mean_prob) / (1 - treatment * mean_prob)  )
}

d5_effectsize_who_15<-effect_size_calc_OR(pred_D5[,"10"], 0.85)
d5_effectsize_who_10<-effect_size_calc_OR(pred_D5[,"10"], 0.90)
d5_effectsize_who_05<-effect_size_calc_OR(pred_D5[,"10"], 0.95)
d8_effectsize_who_15<-effect_size_calc_OR(pred_D8[,"10"], 0.85)
d8_effectsize_who_10<-effect_size_calc_OR(pred_D8[,"10"], 0.90)
d8_effectsize_who_05<-effect_size_calc_OR(pred_D8[,"10"], 0.95)


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
p1_D5 <-table(regresson_df_P$WHOD5_P)/sum(!is.na(regresson_df_P$WHOD5_P))
p1_D8 = table(regresson_df_P$WHOD8_P)/sum(!is.na(regresson_df_P$WHOD8_P))

who_effectsize_function<-function(alpha, power, OR, p1){ #here's how to compute the average over the two groups from p1 and OR
p2   <- pomodm(p=p1, odds.ratio=OR)
pavg <- (p1 + p2) / 2
posamsize(p=pavg, odds.ratio=OR, alpha=alpha, power=power)
}

who_samplesize_d5_15<-who_effectsize_function( 0.05, 0.8, d5_effectsize_who_15, p1_D5)
who_samplesize_d5_10<-who_effectsize_function( 0.05, 0.8, d5_effectsize_who_10, p1_D5)
who_samplesize_d5_05<-who_effectsize_function( 0.05, 0.8, d5_effectsize_who_05, p1_D5)
who_samplesize_d8_15<-who_effectsize_function( 0.05, 0.8, d8_effectsize_who_15, p1_D8)
who_samplesize_d8_10<-who_effectsize_function( 0.05, 0.8, d8_effectsize_who_10, p1_D8)
who_samplesize_d8_05<-who_effectsize_function( 0.05, 0.8, d8_effectsize_who_05, p1_D8)

who_effectsize<-cbind(d5_effectsize_who_15, d5_effectsize_who_10, d5_effectsize_who_05,
              d8_effectsize_who_15, d8_effectsize_who_10, d8_effectsize_who_05)
write.csv(who_effectsize,"/home/skerr/Git/SF94/Outputs/who_effectsize.csv")
who_samplesize<-cbind(who_samplesize_d5_05,who_samplesize_d5_10,who_samplesize_d5_15,
                      who_samplesize_d8_05,who_samplesize_d8_10,who_samplesize_d8_15)
write.csv(who_samplesize,"/home/skerr/Git/SF94/Outputs/who_samplesize.csv")


#############################################################################################################

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

