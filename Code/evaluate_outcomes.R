library(dplyr)
library(ggplot2)
library(data.table)
library(tidyr)
library(rms)

df_1<-fread("/home/skerr/Data/ccp_subset_derived.csv", data.table = FALSE )

#df_1<-fread("/home/u034/mcswets/df_20211402.csv", data.table = FALSE)
#colnames(df_1)
#df_1<-df_1[,c(2:86)]

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

subset1Dist <- distinct( subset1[c('subjid', 'who_days_to_improve1')])

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
                                          'severity_dif_1level', 'mortality_28'),
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
  summarise_all(funs(f))


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
day05<-df_1_base_sf94[,c(1,2,7)] #select subjid, day 0 and day 5
day05<-day05%>%
  dplyr::rename(sf94_day5= "5", sf94_day0= "0")

#calculate what number of subjects needs to be added to get correct proportion
dead5<-length(unique(df_1$subjid[(df_1$day_of_death <5)])) #number of subjects that died before day 5
alive5<-length(unique(df_1$subjid[(df_1$day_of_discharge <5)])) #number of subjects that went home before day 5
available5<-sum((!is.na(df_1$sf94) & df_1$days_since_start == 5), na.rm=T) 
percdead5<-dead5/length(unique(df_1$subjid)) #% that died before day from total subjects
percalive5<-alive5/length(unique(df_1$subjid))# % that went home before day 5 from total subjects
percavailable5<- (1-percdead5- percalive5)# % of available values
dead_to_add<-(percdead5*available5)/percavailable5 #number of 0.5 values to add to variable
alive_to_add<-(percalive5*available5)/percavailable5 #number of 4.76 values to add to variable

day05_P<-day05 #for proportional deaths
day05_P<-day05_P%>% #keep 1 entry/subject
  group_by(subjid) %>% 
  summarise_all(funs(f))
set.seed(1234)
sf94_day5_P<-day05_P$sf94_day5 #add day 5 to a separate df
rows_to_replace<-which(is.na(sf94_day5_P))
sf94_day5_P[sample(rows_to_replace, dead_to_add)]<- 0.5
rows_to_replace<-which(is.na(sf94_day5_P))
sf94_day5_P[sample(rows_to_replace, alive_to_add)]<- 4.76

sf94_day5_P<-data.frame(sf94_day5_P)
day05_P<-cbind(day05_P, sf94_day5_P)
day05_P<-data.frame(day05_P)


#join both together
regresson_df_P<-left_join(day05_P, mortality, by="subjid")

rm(sf94_day5_P) #remove from global environment

#use proportionally added outcome values, take subject ID and day 5 SF94_P values (from DF_1, so not using filters)
regresson_df_P <-data.frame(regresson_df_P)
regresson_df_P<-regresson_df_P %>% #change mortality to match proportionally added values
  mutate(
    mortality_28 = case_when(
      sf94_day5_P == 4.760 & is.na(sf94_day5) ~ 0,sf94_day5_P == 0.5 & is.na(sf94_day5) ~ 1,TRUE ~ as.numeric(mortality_28)))
regresson_df_P<-subset(regresson_df_P, (!is.na(sf94_day5_P)&!is.na(sf94_day0) & !is.na(mortality_28))) #6248 unique subjects, 1 row/subject

#First need to set data distribution for rms functions
attach(regresson_df_P)
ddist <- datadist(sf94_day0, sf94_day5_P, mortality_28)
options(datadist='ddist')
detach(regresson_df_P)
#Then fit models (splines using 4 knots here)
linear_model_P <- lrm(mortality_28 ~ sf94_day0 + sf94_day5_P, regresson_df_P, x=TRUE, y=TRUE)
linear_uni_model<-lrm(mortality_28 ~ sf94_day5_P, regresson_df_P, x=TRUE, y=TRUE)
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

#effect size
#function to calculate SF94 at baseline for various baseline mortality proportions
intercept<-as.numeric(coef(linear_model_P)[1])
coefday0<-as.numeric(coef(linear_model_P)[2])
coefday5<-as.numeric(coef(linear_model_P)[3]) 

linear_uni_model
intercept<-as.numeric(coef(linear_uni_model)[1]) #uninvariate model D5 only
coefday5<-as.numeric(coef(linear_uni_model)[2])
mort1<-0.35 #baseline mortality
oddsmort1<-1/((1-mort1)/mort1) #to odds
logoddsmort1<-log(oddsmort1) # y = intercept + x1b1 +x2b2 >> (y-intercept- x1b1)/b2 = x2 = SF94 'at baseline'D5
baselinesf94<-(logoddsmort1-intercept)/coefday5 #y = logoddsmort
baselinesf94

#calculate SF94 difference from baseline with various mortality reductions
abseffect_size<- function(mortdifference) {
  mort2<-mort1-mortdifference
  oddsmort2<-1/((1-mort2)/mort2) #from probability to odds
  logoddsmort2<-log(oddsmort2) #y in regression equation (logodds)
  newsf94<-(logoddsmort2-intercept)/coefday5 #regression equation
  sf94_difference<-newsf94-baselinesf94
  return(sf94_difference)
}
absolute_mort_reduction<-c(0.05,0.06,0.07,0.08,0.09,0.1,0.11,0.12,0.13,0.14,0.15)
sf94_25<-abseffect_size(absolute_mort_reduction)
sf94_30<-abseffect_size(absolute_mort_reduction)
sf94_35<-abseffect_size(absolute_mort_reduction)


#make a graph
library(data.table)
sf94_dif<-data.frame(sf94_25, sf94_30, sf94_35)
sf94_dif<- data.frame(sf94_dif= c(sf94_dif[,"sf94_25"], sf94_dif[,"sf94_30"],sf94_dif[,"sf94_35"]))
sf94_dif
effectsize_graph<-data.frame(absolute_mort_dif=rep(absolute_mort_reduction,3),
                             baseline_sf94=rep(c(3.18,2.92,2.69), each = 11),
                             baseline_mort=rep(c("0.25","0.30","0.35"), each = 11))
effectsize_graph<-cbind(effectsize_graph, sf94_dif)
effectsize_graph

graph_effectsize<-ggplot(effectsize_graph, aes(x=absolute_mort_dif, y=sf94_dif, group=baseline_mort,
                                               colour=baseline_mort))
graph_effectsize + geom_line()+
  xlab("absolute mortality difference") +
  ylab("Difference in SF94")

#relative risk reductions
# (controlmort- experimentalgroupmort)/ controlmort = RRR
# RRR = 12.5-20%
# Controlmort= 25%, 30% or 35%
rrr_list<-c(0.12,0.13,0.14,0.15,0.16,0.17,0.18,0.19,0.20)
mort2_25<- function(rrr) {
  expmort<-(rrr * 0.25) - 0.25
  expmort<--expmort
  return(expmort)
}
mort2_25_rrr<-mort2_25(rrr_list)

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

relativeeffect_size<- function(mort2, baselinesf94) {
  oddsmort2<-1/((1-mort2)/mort2) #from probability to odds
  logoddsmort2<-log(oddsmort2) #y in regression equation
  newsf94<-(logoddsmort2-intercept- coefday0)/coefday5 #regression equation
  sf94_difference<-newsf94-baselinesf94
  return(sf94_difference)
}

sf94_rel25<-relativeeffect_size(mort2_25_rrr,2.59)
sf94_rel30<-relativeeffect_size(mort2_30_rrr,2.37)
sf94_rel35<-relativeeffect_size(mort2_35_rrr,2.16)

#make a graph
library(data.table)
relsf94_dif<-data.frame(sf94_rel25, sf94_rel30, sf94_rel35)
relsf94_dif<- data.frame(sf94_dif= c(relsf94_dif[,"sf94_rel25"],
                                     relsf94_dif[,"sf94_rel30"],
                                     relsf94_dif[,"sf94_rel35"]))
relsf94_dif
releffectsize_graph<-data.frame(relative_mort_red=rep(rrr_list,3),
                             baseline_mort=rep(c("0.25","0.30","0.35"), each = 9))
releffectsize_graph<-cbind(releffectsize_graph, relsf94_dif)
releffectsize_graph
#grahp
relgraph_effectsize<-ggplot(releffectsize_graph, 
                            aes(x=relative_mort_red, y=sf94_dif, group=baseline_mort,
                                               colour=baseline_mort))
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
#25: 3.18=baselineSF94
#30: 2.92
#35: 2.69
baseline <- c(0.30, 0, baselinesf94)
coef <- c(1.9782, 0, -0.9672) 
baseline
mortSF <- function(baseline, coef){
  f1 <- function(deltaSF){
    a <- log( (baseline[1] - deltaSF)/( 1-baseline[1]  + deltaSF)) - coef[1] - coef[2]*baseline[2] - coef[3]*baseline[3]
    return( a/coef[3]  )
  }
  return(f1) 
}

f1 <- mortSF(baseline, coef)
f(absolute_mort_reduction)
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
