library(dplyr)
library(ggplot2)
library(data.table)
library(tidyr)

df_1<-fread("/home/u034/mcswets/df_20211402.csv", data.table = FALSE)
df_1<-df_1[,c(3:87)]
length(df_1$subjid)

# start population (df_1) = 79843
# after applying age limits = 38919
# only subjects with supplemental oxygen on day 0,1 or 2 = 14049 unique subjects

subjects_to_include <- filter(df_1, ( fio2 >=0.22 & days_since_start %in% c(0,1,2)  & age_estimateyears >19 & age_estimateyears <76 ) )['subjid']
subset1<-df_1[df_1$subjid %in% subjects_to_include$subjid,] 
subset1 <- as.data.frame(subset1)

subset1<-df_1

head(subset1)
# variable should be either  'sf94' or 'severity_scale_ordinal'
# group should be 'base' if you want to include everyone except those who died or were discharged
# 'basedd' if you want to include those who died or were discharged
# 'day0' if you want to exclude people who died or were discharged on day_since_admission == 0
# Output is a table that you can query for summary statistics.

createDF <- function(group, variable, time){
  
  if( group == 'base' |  group == 'basedd' ) { 
    df <- filter( subset1[ c('subjid', 'days_since_start', variable ) ] , days_since_start <= time  )
  } else if (group == 'day0') {
    dropID <- subset1[   (subset1[ 'day_of_death' ] == 0 |  subset1['day_of_discharge'] == 0) %in% TRUE, 'subjid']
    
    df <- filter(subset1[ c('subjid', 'days_since_start', variable ) ], days_since_start <= time & !(subjid %in% dropID)  ) 
  } 
  
  
  derived <-  df  %>%  mutate(rn= row_number()) %>% spread (days_since_start, variable) %>% select(-rn)
  
  newCols <- as.character( setdiff( 0:time,  setdiff( colnames(derived), 'subjid' ) ) )
  
  derived[newCols] <- NA
  
  derived <- derived[ c( 'subjid', as.character(0:time))  ]
  
  
  if(group == 'basedd' | group == 'day0' ){
    
    censorDeath <- filter( subset1,  death== "YES"  & day_of_death < time    )[c('subjid', 'day_of_death')]
    
    censorDischarge <- filter( subset1,  discharge== "YES"  & day_of_discharge < time    )[c('subjid', 'day_of_discharge')]
    
    
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


sum((!is.na(subset1$day_of_death) | !is.na(subset1$day_of_discharge)) & subset1$days_since_start == 0, na.rm = T)
#(group,variable, time)
# variable = sf94, who
# group= base, basedd, day0
# time =days_since_start
detach("package:plyr", unload=T)
base_sf94_10<-createDF("base", "sf94", 10)
basedd_sf94_10<-createDF("basedd", "sf94", 10)
basedd_sf94day0_10<-createDF("day0", "sf94", 10)

base_sf94_12<-createDF("base", "sf94", 12)
basedd_sf94_12<-createDF("basedd", "sf94", 12)

base_who_8<-createDF("base", "severity_scale_ordinal", 8)
basedd_who_8<-createDF("basedd", "severity_scale_ordinal",8)
basedd0_who_8<-createDF("day0", "severity_scale_ordinal", 8)

base_who_5<-createDF("base", "severity_scale_ordinal", 5)

summary(base_sf94_10)
summary(basedd_sf94_10)
summary(basedd_sf94day0_10)
sapply(base_sf94_10, sd, na.rm=T)
sapply(basedd_sf94_10, sd, na.rm=T)
sapply(basedd_sf94day0_10, sd, na.rm=T)

head(basedd_sf94_10)
length(basedd_sf94_10[,7]

#correlation
library(data.table)
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
length(correlation_subset_08$subjid)
head(correlation_subset_05)
# DAY 0/5
x <-  correlation_subset_05[,2]
y <-  correlation_subset_05[,7]
cor(x,y)
# DAY 0/8
x <-  correlation_subset_08[,2]
y <-  correlation_subset_08[,10]
cor(x,y)

#take day 5 from who and sf data
day5who<-base_who_5[,c(1,7)]
day5who<-day5who%>%
  group_by(subjid)%>%
  summarise_all(funs(f))
day5who<-day5who%>%
  dplyr::rename(who_day5= "5")
#calculate mean level at day 5
summary(day5who$who_day5)
#add SF94 for other analysis
day5sf94<-base_sf94_10[,c(1,7)]
day5sf94<-day5sf94%>%
  group_by(subjid)%>%
  summarise_all(funs(f))
day5sf94<-day5sf94%>%
  dplyr::rename(sf94_day5= "5")
#join together
day5<-left_join(day5who, day5sf94, by="subjid")

#WHO summary stats for day 5 and day 8
day5_wide<-reshape(day5, idvar="subjid", timevar="who_day5", direction="wide")
#reorder columns
day5_wide<-day5_wide[,c(1,8,4,3,6,2,5,7)]
length(day5_wide$subjid)
summary(day5_wide)
sapply(day5_wide, sd, na.rm=T)

#day 8 WHO and SF dataframe
#WHO day 8
day8_who<-basedd_who_8[,c(1,7,10)]
day8_who<-day8_who%>%
  group_by(subjid)%>%
  summarise_all(funs(f))
day8_who<-day8_who%>%
  dplyr::rename(who_day8= "8",
                who_day5= "5")
head(day8_who)
hist(day8_who$who_day5, breaks=50)
rpng.off()
summary(day8_who$who_day5)
summary(day8_who$who_day8)
sd(day8_who$who_day5, na.rm = T)
#SF94 day 8
day8sf94<-base_sf94_10[,c(1,10)]
day8sf94<-day8sf94%>%
  group_by(subjid)%>%
  summarise_all(funs(f))
day8sf94<-day8sf94%>%
  dplyr::rename(sf94_day8= "8")
#join together
day8<-left_join(day8_who, day8sf94, by="subjid")
day8<-data.frame(day8)
day8<-subset(day8, (!is.na(who_day8)))
#make wide
day8_wide<-reshape(day8, idvar="subjid", timevar="who_day8", direction="wide")
#reorder columns
day8_wide<-day8_wide[,c(1,2,3,4,5,8,7,6)]
length(day8_wide$subjid)
summary(day8_wide)
sapply(day8_wide, sd, na.rm=T)

#mortality at 28 days
head(subset1)
length(unique(subset1$subjid))
mort<-subset1 %>%
  group_by(subjid)%>%
  count(mortality_28)
table(mort$mortality_28)
4420/(10331+4420)
#regression model D5 SF94 and mortality
library(rms)
#with sf94 values regression
day05<-base_sf94_10[,c(1,2,7)]
day05<-day05%>%
  dplyr::rename(sf94_day5= "5", sf94_day0= "0")

#Data imputation based on total number of deaths
dead5<-sum(subset1$day_of_death <5, na.rm = T) 
alive5<-sum(subset1$day_of_discharge <5, na.rm = T) 
available5<-sum((!is.na(subset1$sf94) & subset1$days_since_start == 5), na.rm=T) 
percdead5<-dead5/length(unique(subset1$subjid)) #% that died before day from total subjects
percalive5<-alive5/length(unique(subset1$subjid))# % that went home before day 5 from total subjects
percavailable5<- (1-percdead5- percalive5)# % of available values
dead_to_add<-(percdead5*available5)/percavailable5 #number of 0.5 values to add to variable
alive_to_add<-(percalive5*available5)/percavailable5 #number of 4.76 values to add to variable
head(day05)
day05_P<-day05 # base_s94_10, select subjid, day 0 and day 5
day05_P<-day05_P%>% #keep 1 entry/subject
  group_by(subjid) %>% 
  summarise_all(funs(f))
set.seed(1234)
sf94_D5_P<-day05_P$sf94_day5 #add day 5 to a separate df
rows_to_replace<-which(is.na(sf94_D5_P))
sf94_D5_P[sample(rows_to_replace, dead_to_add)]<- 0.5
rows_to_replace<-which(is.na(sf94_D5_P))
sf94_D5_P[sample(rows_to_replace, alive_to_add)]<- 4.76

sf94_D5_P<-data.frame(sf94_D5_P)
day05_P<-cbind(day05_P, sf94_D5_P)
day05_P<-data.frame(day05_P)

head(day05_P,55)
sum(!is.na(day05_P$sf94_day5)) #11135
sum(!is.na(day05_P$sf94_D5_P)) #15145, so all newly made values are added


#make small dataframe for 30-day mortality and outcome
mortality<-df_1[,c("subjid","mortality_28")]
mortality<-mortality%>%
  group_by(subjid)%>%
  summarise_all(funs(f))
#join both together
regresson_df<-left_join(day05_P, mortality, by="subjid")
regresson_df_P<-regresson_df # make separate df form the proportionally imputed values
#remove rows with missing values
regresson_df<-subset(regresson_df, (!is.na(sf94_day5)&!is.na(sf94_day0) & !is.na(mortality_28))) #5159 unique subjects, 1 row/subject
regresson_df <-data.frame(regresson_df)
head(regresson_df)
summary(regresson_df)

rm(sf94_D5_P) #remove from global environment
#First need to set data distribution for rms functions
attach(regresson_df)
ddist <- datadist(sf94_day0, sf94_day5, mortality_28)
options(datadist='ddist')
detach(regresson_df)
#Then fit models (splines using 4 knots here)
linear_model <- lrm(mortality_28 ~ sf94_day0 + sf94_day5, regresson_df, x=TRUE, y=TRUE)
splines_model <- lrm(mortality_28 ~ rcs(sf94_day0 , 4) + rcs(sf94_day5, 4), regresson_df, x=TRUE, y=TRUE)
#Visualise association between SF94 and mortality (note this will use log y axis scale)
plot_associations_linear <- ggplot(Predict(linear_model), ggtitle = "N=5159" )
plot_associations_linear
plot_associations_splines <- ggplot(Predict(splines_model), ggtitle = "N=5159")
plot_associations_splines 
plot_associations_linear_exp <- ggplot(Predict(linear_model, fun=plogis), ylab= "Risk of 28-day mortality")
plot_associations_linear_exp
#univariate model
univariate_model<-lrm(mortality_28 ~ sf94_day0, regresson_df, x=TRUE, y=TRUE)
plot_linear_uni<-ggplot(Predict(univariate_model))
plot_linear_exp_uni<-ggplot(Predict(univariate_model, fun=plogis), ylab= "Risk of 28-day mortality")
plot_linear_uni
plot_linear_exp_uni
rpng.off()


#with sf94_P values regression
#use proportionally added outcome values, take subject ID and day 5 SF94_P values (from DF_1, so not using filters)
regresson_df_P <-data.frame(regresson_df_P)
head(regresson_df_P,250)
regresson_df_P<-regresson_df_P %>% 
  mutate(
    mortality_28 = case_when(
      sf94_D5_P == 4.760 & is.na(sf94_day5) ~ 0,sf94_D5_P == 0.5 & is.na(sf94_day5) ~ 1,TRUE ~ as.numeric(mortality_28)))
regresson_df_P<-subset(regresson_df_P, (!is.na(sf94_D5_P)&!is.na(sf94_day0) & !is.na(mortality_28))) #6248 unique subjects, 1 row/subject

length(regresson_df_P$subjid)

#First need to set data distribution for rms functions
attach(regresson_df_P)
ddist <- datadist(sf94_day0, sf94_D5_P, mortality_28)
options(datadist='ddist')
detach(regresson_df_P)
#Then fit models (splines using 4 knots here)
linear_model_P <- lrm(mortality_28 ~ sf94_day0 + sf94_D5_P, regresson_df_P, x=TRUE, y=TRUE)
splines_model_P <- lrm(mortality_28 ~ rcs(sf94_day0 , 4) + rcs(sf94_D5_P, 4), regresson_df_P, x=TRUE, y=TRUE)
#Visualise association between SF94 and mortality (note this will use log y axis scale)
plot_associations_linear_P <- ggplot(Predict(linear_model_P), ggtitle = "N=6248" )
plot_associations_linear_P
plot_associations_splines_P <- ggplot(Predict(splines_model_P), ggtitle = "N=6248")
plot_associations_splines_P 
#Visualise using exp scale
plot_associations_linear_exp_P <- ggplot(Predict(linear_model_P, fun=plogis), ylab= "Risk of 28-day mortality")
plot_associations_linear_exp_P

rpng.off()

#stats

summary(linear_model)
linear_model
colnames(linear_model_P)
plot(linear_model_P$residuals)
linear_model_P
splines_model
splines_model_P
lrtest(linear_model, splines_model)
lrtest(linear_model_dd, splines_model_dd)
anova(linear_model_dd)
BIC(linear_model)
BIC(linear_model_P)
BIC(splines_model)
BIC(splines_model_P)
x <-  regresson_df_P$sf94_day0
y <-  regresson_df_P$sf94_D5_P
cor(x,y)

library(gtsummary)
linear_model_glm_P <- glm(mortality_28 ~ sf94_day0 + sf94_D5_P, family=binomial, data=regresson_df_P)
linear_model_glm_P  %>% tbl_regression(exponentiate=T, intercept= T)
linear_model_glm <- glm(mortality_28 ~ sf94_day0 + sf94_day5, family=binomial, data=regresson_df)
linear_model_glm  %>% tbl_regression(exponentiate=T, intercept= T)
#scatterplot
library(ggplot2)
ggplot(regresson_df, aes(x=sf94_day0, y=sf94_day5)) +
  geom_point()+
  xlab("S/F94 D0")+
  ylab("S/F94 D5")


#split violin plot
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
head(long_dfsf94_12)

p <- ggplot(long_dfsf94_12, aes(x=days_since_start, y=sf94, fill=mortality_28)) +
  geom_split_violin()+
  coord_flip()+
  xlab("Day")+
  ylab("S/F94")+
  ggtitle("")+
  scale_fill_discrete(name="28-day outcome", labels= c("Discharged alive", "Death"))
p <- ggplot(long_dfsf94_12, aes(x=days_since_start, y=sf94, fill=mortality_28)) +
  geom_violin()+
  xlab("Day")+
  ylab("S/F94")+
  ggtitle("")+
  scale_fill_discrete(name="28-day outcome", labels= c("Discharged alive", "Death"))
p


#find time it takes for a certain change in WHO level (2 or 1)
#subset dataframe
severity_difference<-subset1 #start with 17180 unique subjects
#>10.000 subjects with no WHO value on day of discharge, set those to level 4 (day of death all have a value)
severity_difference$severity_scale_ordinal<- ifelse(is.na(severity_difference$severity_scale_ordinal) &
                                                      !is.na(severity_difference$day_of_discharge), 4, 
                             severity_difference$severity_scale_ordinal)
#make new variable for last who level available
last_sev<-severity_difference %>%
  group_by(subjid)%>%
  filter(!is.na(severity_scale_ordinal))%>%
  dplyr::mutate(final_who_score= dplyr::last((severity_scale_ordinal)))%>%
  slice(which.min(final_who_score))
last_sev<-last_sev[,c("subjid","final_who_score")]
#make subset of data
severity_difference<-severity_difference[,c("subjid","days_since_start","severity_scale_ordinal")]
#only select subjects with >1 measurement, remove rows with missing days and severity score
severity_difference<-severity_difference %>%
  group_by(subjid)%>%
  filter(n()>=2) #17180 subjects
severity_difference<-subset(severity_difference, !is.na(days_since_start)) #17180 subjects
severity_difference<-subset(severity_difference, !is.na(severity_scale_ordinal)) #17180 subjects
#calculate time it takes for a change of 2
severity_dif_1level<-severity_difference %>%
  left_join(severity_difference, ("subjid")) %>%
  filter(severity_scale_ordinal.x != 10)%>% #some wrong entries where after death there is still a non-dead value
  mutate(Days = (days_since_start.y - days_since_start.x)) %>% # creates all possible combinations (day 3- day 1 and day 1- day 3)
  filter(Days>0)%>% #only keep if day y > day x (as this means 'forward' change)
  mutate(score_difference= severity_scale_ordinal.y- severity_scale_ordinal.x) %>% #calculate the change in severity levels 
  filter(score_difference <= -1) %>% # improvement >> score gets lower
  group_by(subjid) %>%
  slice(which.min(Days)) %>% #if multiple combination for a 1/2 level difference, take the smallest no of days
  ungroup %>%
  right_join(distinct(severity_difference["subjid"]), "subjid") # 17180 subjects

#add both together
severity_dif_1level<-left_join(severity_dif_1level, last_sev, by="subjid") #17180 subjects
#only keep if value is the same as last value
#if subject improves further- we don't want to lose them, so sev_scale_ord.y needs to be smaller than final score
#sev_scale.y can also be the same as final score
# if final score is higher than sev_score.y >> remove days value
severity_dif_1level$severity_scale_ordinal.y<-as.numeric(severity_dif_1level$severity_scale_ordinal.y)
severity_dif_1level$final_who_score<-as.numeric(severity_dif_1level$final_who_score)
severity_dif_1level<-severity_dif_1level %>% 
  mutate(
    Days = case_when(
      score_difference == -1 & 
        (severity_scale_ordinal.y >= final_who_score) ~ Days,
      score_difference == -2 & #if difference was 2 points and someone still has 1 point improvement at end, don't exclude
        ((severity_scale_ordinal.y +1) >= final_who_score) ~ Days,
      score_difference == -3 & 
        ((severity_scale_ordinal.y +2) >= final_who_score) ~ Days,
      score_difference == -4 & 
        ((severity_scale_ordinal.y +3) >= final_who_score) ~ Days
    ))
severity_dif_1level<-data.frame(severity_dif_1level)
head(severity_dif_1level)

summary(severity_dif_1level$Days, na.rm = T)
summary(severity_dif_2level$Days, na.rm = T)
#for 2 levels difference
severity_dif_2level<-severity_dif_2level %>% 
  mutate(
    Days = case_when(
      score_difference == -2 & 
        (severity_scale_ordinal.y >= final_who_score) ~ Days,
      score_difference == -3 & 
        ((severity_scale_ordinal.y +1) >= final_who_score) ~ Days,
      score_difference == -4 & 
        ((severity_scale_ordinal.y +2) >= final_who_score) ~ Days
      ))



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



# code not in use currently
#with sf94_dd values regression
#use extreme values, take subject ID and day 5 SF94_dd values (from DF_1, so not using filters)
length(unique(basedd_sf94_10$subjid))
day5_dd<-basedd_sf94_10[,c(1,2,7)]
day5_dd<-day5_dd%>%
  dplyr::rename(sf94_day5_dd= "5",sf94_day0_dd= "0")
sum(day5_dd$sf94_day5_dd== 4.76, na.rm = T)
sum(day5_dd$sf94_day5_dd== 0.5, na.rm = T)
sum(!is.na(day5_dd$sf94_day5_dd), na.rm = T)
sum(is.na(day5_dd$sf94_day5_dd), na.rm = T)
#join to mortality dataframe
dd_regression<-left_join(day5_dd, mortality, by="subjid")
#keep 1 value/ subject
dd_regression <- dd_regression %>% 
  group_by(subjid) %>% 
  summarise_all(funs(f))
dd_regression <- dd_regression %>% 
  group_by(subjid) %>% 
  slice(which.min(sf94_day5_dd))
dd_regression <-data.frame(dd_regression)
dd_regression<-subset(dd_regression, (!is.na(sf94_day5_dd)&!is.na(sf94_day0_dd) & !is.na(mortality_28))) #11532 unique subjects, 1 row/subject
head(dd_regression)
length((dd_regression$subjid))
table(dd_regression$mortality_28)
table(regresson_df$mortality_28)
table(df_1$mortality_28)

#First need to set data distribution for rms functions
attach(dd_regression)
ddist <- datadist(sf94_day0_dd, sf94_day5_dd, mortality_28)
options(datadist='ddist')
detach(dd_regression)
#Then fit models (splines using 4 knots here)
linear_model_dd <- lrm(mortality_28 ~ sf94_day0_dd + sf94_day5_dd, dd_regression, x=TRUE, y=TRUE)
splines_model_dd <- lrm(mortality_28 ~ rcs(sf94_day0_dd , 4) + rcs(sf94_day5_dd, 4), dd_regression, x=TRUE, y=TRUE)
#Visualise association between SF94 and mortality (note this will use log y axis scale)
plot_associations_linear <- ggplot(Predict(linear_model_dd), ggtitle = "N=11534" )
plot_associations_linear
plot_associations_splines <- ggplot(Predict(splines_model_dd), ggtitle = "N=11534")
plot_associations_splines 
rpng.off()