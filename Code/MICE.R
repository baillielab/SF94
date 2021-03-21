# 4 day mortality variable
df_1<-df_1 %>% 
  mutate(
    mortality_5 = case_when(
      day_of_death <6 ~ 1,
      day_of_discharge<6 ~ 0))
mort5<-df_1 %>%
  group_by(subjid)%>%
  count(mortality_5)
table(mort5$mortality_5)
sum(mort5$mortality_5 == 1, na.rm = T)/ sum(!is.na(mort5$mortality_5))
mort28 <- sum(mort$mortality_28 == 1, na.rm = T)/ sum(!is.na(mort$mortality_28))

#MICE multiple imputation
#https://www.analyticsvidhya.com/blog/2016/03/tutorial-powerful-packages-imputing-missing-values/
library(mice)
#try with days 0-10, after that mainly missing data
# this is cleaned data pre-filters
mice_df<-df_1_base_sf94
mice_df<-mice_df %>%
  rename(
    day_0 = X0,
    day_1 = X1,
    day_2 = X2,
    day_3 = X3,
    day_4 = X4,
    day_5 = X5,
    day_6 = X6,
    day_7 = X7,
    day_8 = X8,
    day_9 = X9,
    day_10 = X10
  )
mice_df<-data.frame(mice_df)
#1 row for each subject
f <- function(x) {
  x <- na.omit(x)
  if (length(x) > 0) unique(x) else NA
}
mice_df<-mice_df %>% 
  group_by(subjid) %>% 
  summarise_all(funs(f))
#remove subjid column
mice_df<-mice_df[,c(2:12)] 
#multilevel imputation, as observations are clustered within subjects
set.seed(1234)
#look at missing values patern
md.pattern(mice_df)
#PMM (Predictive mean matching) for numerical variables
imputed_df<-mice(mice_df, m=5, method= "pmm", maxit=10)
summary(imputed_df)
completeData<-complete(imputed_df)
summary(imputed_df$imp$day_0) # check summary stats for the 5 imputed datasets on day 0

# 2 level imputation
mice_2_df<-df_1_base_sf94
head(mice_2_df)
mice_2_df<-mice_2_df %>%
  rename(
    day_0 = "0",
    day_1 = "1",
    day_2 = "2",
    day_3 = '3',
    day_4 = "4",
    day_5 = "5",
    day_6 = "6",
    day_7 = "7",
    day_8 = "8",
    day_9 = "9",
    day_10 = "10"
  )
mice_2_df<-mice_2_df %>% 
  group_by(subjid) %>% 
  summarise_all(funs(f))
mice_2_df$numeric_id<-seq.int(nrow(mice_2_df))
mice_2_df<-mice_2_df[,c(2:13)]
mice_2_df<-data.frame(mice_2_df)

ini<-mice(mice_2_df, maxit= 0)
pred<-ini$pred
pred[,"numeric_id"]<- -2 #subject ID = to identify class variable
pred
multilevel_mice<-mice(mice_2_df, method = c("2l.norm","2l.norm", "2l.norm", "2l.norm", "2l.norm"
                                            , "2l.norm", "2l.norm" ,"2l.norm", "2l.norm", "2l.norm"
                                            ,"2l.norm", ""), pred= pred, maxit = 1, seed= 1234)


#old code not currently in use
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

#in case we need base_dd variable for sf94 and who
df_1_basedd_sf94_10<-createDF(df_1, "basedd", "sf94", 10)
df_1_basedd_who_10<-createDF(df_1, "basedd", "severity_scale_ordinal", 10)
dd_sf94<-df_1_basedd_sf94_10[,c(1,2,7,10)]
dd_who<-df_1_basedd_who_10[,c(1,2,7,10)]

dd_sf94<-dd_sf94 %>% 
  group_by(subjid) %>% 
  summarise_all(funs(f))
dd_who<-dd_who %>% 
  group_by(subjid) %>% 
  summarise_all(funs(f))

dd_sf94<-dd_sf94%>%
  dplyr::rename(sf94_dd_day5= "5", sf94_dd_day0= "0", sf94_dd_day8= "8")
dd_who<-dd_who%>%
  dplyr::rename(who_dd_day5= "5", who_dd_day0= "0", who_dd_day8= "8")

dd_data<-left_join(dd_sf94, dd_who, by="subjid")
dd_data<-data.frame(dd_data)

regresson_df_P<-left_join(regresson_df_P, dd_data, by="subjid")

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

saveRDS(sum_WHO_D5,"/home/skerr/Git/SF94/Outputs/sum_WHO_D5.rds")
saveRDS(sum_WHO_D8,"/home/skerr/Git/SF94/Outputs/sum_WHO_D8.rds")
saveRDS(OR_D5_WHO,"/home/skerr/Git/SF94/Outputs/OR_D5_WHO.rds")
saveRDS(OR_D8_WHO,"/home/skerr/Git/SF94/Outputs/OR_D8_WHO.rds")
readRDS("/Users/Maaike/Downloads/OR_D8_WHO (1).rds")

#Proportional odds model with D5/D8 and D0 as separate predictors
WHOD5_model_2<-polr(as.factor(WHOD5_P) ~ sf94_day5_P+sf94_day0+ age_estimateyears+ sex, data = regresson_df_P, Hess=T)
WHOD8_model_2<-polr(as.factor(WHOD8_P) ~ sf94_day8_P+sf94_day0+ age_estimateyears+ sex, data = regresson_df_P, Hess=T)
sum_WHO_D5_2<-summary(WHOD5_model_2)
ci_5<-confint(WHOD5_model_2) #calculate confidence interval
OR_D5_WHO_2<-exp(cbind(OR=coef(WHOD5_model_2),ci_5)) #calculate OR
ci_8<-confint(WHOD8_model_2)
OR_D8_WHO_2<-exp(cbind(OR=coef(WHOD8_model_2),ci_8))

saveRDS(OR_D5_WHO_2,"/home/skerr/Git/SF94/Outputs/OR_D5_WHO_2.rds")
saveRDS(OR_D8_WHO_2,"/home/skerr/Git/SF94/Outputs/OR_D8_WHO_2.rds")
readRDS("/Users/Maaike/Downloads/OR_D5_WHO_2 (1).rds")
readRDS("/Users/Maaike/Downloads/OR_D8_WHO_2 (1).rds")

#alternative WHO improvement
regresson_df_P$WHOD5_P<-as.factor(regresson_df_P$WHOD5_P)
WHO_D5_mort<-lrm(mortality_28 ~ WHOD5_P+ age_estimateyears+ sex, data = regresson_df_P)
who_intecept<-as.numeric(coef(WHO_D5_mort)[1])
who_model_4<-0
who_model_5<-as.numeric(coef(WHO_D5_mort)[2])
who_model_6<-as.numeric(coef(WHO_D5_mort)[3])
who_model_7<-as.numeric(coef(WHO_D5_mort)[4])
who_model_8<-as.numeric(coef(WHO_D5_mort)[5])
who_model_9<-as.numeric(coef(WHO_D5_mort)[6])
who_model_10<-as.numeric(coef(WHO_D5_mort)[7])
coefage_who5<-as.numeric(coef(WHO_D5_mort)[8])
coefsex_who5<-as.numeric(coef(WHO_D5_mort)[9])
age_value_5<-mean(regresson_df_P$age, na.rm=T)
sex_value_5<-mean(as.numeric(regresson_df_P$binairy_sex), na.rm=T)

who_mortality= function (who_level_coef){
  logoddmortwho = who_intecept + who_level_coef+ (age_value_5 * coefage_who5) + (sex_value_5 * coefsex_who5)
  mort_who_OR<-exp(logoddmortwho) #to odds ratio
  mort_who_prop<-mort_who_OR/ (1 +mort_who_OR)
  output<-list(mort_who_OR, mort_who_prop)
  return(output)
}
who_levels<-c(who_model_4, who_model_5, who_model_6, who_model_7, who_model_8, who_model_9, who_model_10)
who_mortality_output_D5<-who_mortality(who_levels)

write.csv(who_mortality_output_D5,"/home/skerr/Git/SF94/Outputs/who_mortality_output_D5.csv")

#same for D8
regresson_df_P$WHOD8_P<-as.factor(regresson_df_P$WHOD8_P)
WHO_D8_mort<-lrm(mortality_28 ~ WHOD8_P+ age_estimateyears+ sex, data = regresson_df_P)
whoD8_intecept<-as.numeric(coef(WHO_D8_mort)[1])
whoD8_model_4<-0
whoD8_model_5<-as.numeric(coef(WHO_D8_mort)[2])
whoD8_model_6<-as.numeric(coef(WHO_D8_mort)[3])
whoD8_model_7<-as.numeric(coef(WHO_D8_mort)[4])
whoD8_model_8<-as.numeric(coef(WHO_D8_mort)[5])
whoD8_model_9<-as.numeric(coef(WHO_D8_mort)[6])
whoD8_model_10<-as.numeric(coef(WHO_D8_mort)[7])
coefage_who8<-as.numeric(coef(WHO_D8_mort)[8])
coefsex_who8<-as.numeric(coef(WHO_D8_mort)[9])
age_value_5<-mean(regresson_df_P$age, na.rm=T) #mean age is the same
sex_value_5<-mean(as.numeric(regresson_df_P$binairy_sex), na.rm=T)

whoD8_mortality= function (who_level_coef){
  logoddmortwho = whoD8_intecept + who_level_coef+ (age_value_5 * coefage_who8) + (sex_value_5 * coefsex_who8)
  mort_who_OR<-exp(logoddmortwho) #to odds ratio
  mort_who_prop<-mort_who_OR/ (1 +mort_who_OR)
  output<-list(mort_who_OR, mort_who_prop)
  return(output)
}
whoD8_levels<-c(whoD8_model_4, whoD8_model_5, whoD8_model_6,
                whoD8_model_7, whoD8_model_8, whoD8_model_9, whoD8_model_10)
whoD8_mortality_output<-whoD8_mortality(whoD8_levels)


write.csv(whoD8_mortality_output,"/home/skerr/Git/SF94/Outputs/whoD8_mortality_output.csv")

#WHO improvement- as a numerical variable instead of as a factor
WHO_D5_mort_2<-lrm(mortality_28 ~ as.numeric(WHOD5_P)+ age_estimateyears+ sex, data = regresson_df_P)
WHO_D8_mort_2<-lrm(mortality_28 ~ as.numeric(WHOD8_P)+ age_estimateyears+ sex, data = regresson_df_P)
#function
WHO_abseffect_size<- function(mortdifference, mort1, model_who) {
  logoddsmort1<-log(1/((1-mort1)/mort1))
  baselinewho_male<-(logoddsmort1-as.numeric(coef(model_who)[1])-
                       (as.numeric(coef(model_who)[3]) * mean(regresson_df_P$age, na.rm=T))- 
                       (1* as.numeric(coef(model_who)[4])))/as.numeric(coef(model_who)[2])
  baselinewho_female<-(logoddsmort1-as.numeric(coef(model_who)[1])-
                         (as.numeric(coef(model_who)[3]) * mean(regresson_df_P$age, na.rm=T))- 
                         (0* as.numeric(coef(model_who)[4])))/as.numeric(coef(model_who)[2])
  baselinewho<-(baselinewho_male * male_perc) + (baselinewho_female * female_perc)
  mort2<-mort1-mortdifference
  logoddsmort2<-log(1/((1-mort2)/mort2)) #from probability to logodds
  newwho_male<-(logoddsmort2-as.numeric(coef(model_who)[1])-
                  (as.numeric(coef(model_who)[3]) * mean(regresson_df_P$age, na.rm=T))- 
                  (1* as.numeric(coef(model_who)[4])))/as.numeric(coef(model_who)[2])
  newwho_female<-(logoddsmort2-as.numeric(coef(model_who)[1])-
                    (as.numeric(coef(model_who)[3]) * mean(regresson_df_P$age, na.rm=T))- 
                    (0* as.numeric(coef(model_who)[4])))/as.numeric(coef(model_who)[2])
  newwho<-(newwho_male * male_perc) + (newwho_female * female_perc)
  who_difference<-newwho-baselinewho
  return(who_difference)
}
absolute_mort_reduction<-c(0.01,0.015,0.02,0.025,0.03,0.035,0.04,0.045,0.05)
WHO_25_D5<-WHO_abseffect_size(absolute_mort_reduction,0.25,WHO_D5_mort_2)
WHO_25_D8<-WHO_abseffect_size(absolute_mort_reduction,0.25,WHO_D8_mort_2)

write.csv(WHO_25_D5,"/home/skerr/Git/SF94/Outputs/WHO_25_D5.csv")
write.csv(WHO_25_D8,"/home/skerr/Git/SF94/Outputs/WHO_25_D8.csv")

# SF94 values
sf94_d5<-lrm(mortality_28 ~ delta_SF94_05+ age_estimateyears+ sex, data = regresson_df_P)
sf94_d8<-lrm(mortality_28 ~ delta_SF94_08+ age_estimateyears+ sex, data = regresson_df_P)
# save intercept and coefficient for day 5
intercept5<-as.numeric(coef(sf94_d5)[1]) 
coefday5<-as.numeric(coef(sf94_d5)[2])
coefage5<-as.numeric(coef(sf94_d5)[3])
coefsex5<-as.numeric(coef(sf94_d5)[4])
age_value_5<-mean(regresson_df_P$age, na.rm=T)
male_perc <- sum(regresson_df_P$binairy_sex == 1, na.rm = T)/ sum(!is.na(regresson_df_P$binairy_sex))
female_perc<-sum(regresson_df_P$binairy_sex == 0, na.rm = T)/ sum(!is.na(regresson_df_P$binairy_sex))

absolute_mortdif<-function(mort1) {
  logoddsmort1<-log(1/((1-mort1)/mort1))
  baselinesf94_male<-(logoddsmort1-intercept5- (coefage5 * age_value_5)- (1 * coefsex5))/coefday5
  baselinesf94_female<-(logoddsmort1-intercept5- (coefage5 * age_value_5)- (0 * coefsex5))/coefday5
  baselinesf94<-(baselinesf94_male * male_perc) + (baselinesf94_female * female_perc)
  sf94_2<-baselinesf94+0.5
  logoddsmort2_male<-intercept5+ (coefday5 * sf94_2) + (coefage5 * age_value_5) + (1 * coefsex5)
  logoddsmort2_female<-intercept5+ (coefday5 * sf94_2) + (coefage5 * age_value_5) + (0 * coefsex5)
  mort2OR_male<-exp(logoddsmort2_male) #to odds ratio
  mort2OR_female<-exp(logoddsmort2_female) #to odds ratio
  mort2_male<-mort2OR_male/ (1 +mort2OR_male)
  mort2_female<-mort2OR_female/ (1 +mort2OR_female)
  mort2<-(mort2_male* male_perc) + (mort2_female * female_perc)
  mortdif<-mort1-mort2
  return(mortdif)
}
list_baseline_mort<-c(0.25,0.30,0.35)
D5_sf94_effectsize<-absolute_mortdif(list_baseline_mort)

write.csv(D5_sf94_effectsize,"/home/skerr/Git/SF94/Outputs/D5_sf94_effectsize.csv")

#day 8
intercept8<-as.numeric(coef(sf94_d8)[1]) 
coefday8<-as.numeric(coef(sf94_d8)[2])
coefage8<-as.numeric(coef(sf94_d8)[3])
coefsex8<-as.numeric(coef(sf94_d8)[4])
age_value_8<-mean(regresson_df_P$age, na.rm=T)

absolute_mortdifD8<-function(mort1) {
  logoddsmort1<-log(1/((1-mort1)/mort1))
  baselinesf94_male<-(logoddsmort1-intercept8- (coefage8 * age_value_8)- (1 * coefsex8))/coefday8
  baselinesf94_female<-(logoddsmort1-intercept8- (coefage8 * age_value_8)- (0 * coefsex8))/coefday8
  baselinesf94<-(baselinesf94_male * male_perc) + (baselinesf94_female * female_perc)
  sf94_2<-baselinesf94+0.5
  logoddsmort2_male<-intercept8+ (coefday8 * sf94_2) + (coefage8 * age_value_8) + (1 * coefsex8)
  logoddsmort2_female<-intercept8+ (coefday8 * sf94_2) + (coefage8 * age_value_8) + (0 * coefsex8)
  mort2OR_male<-exp(logoddsmort2_male) #to odds ratio
  mort2OR_female<-exp(logoddsmort2_female) #to odds ratio
  mort2_male<-mort2OR_male/ (1 +mort2OR_male)
  mort2_female<-mort2OR_female/ (1 +mort2OR_female)
  mort2<-(mort2_male* male_perc) + (mort2_female * female_perc)
  mortdif<-mort1-mort2
  return(mortdif)
}

list_baseline_mort<-c(0.25,0.30,0.35)
D8_sf94_effectsize<-absolute_mortdifD8(list_baseline_mort)

write.csv(D8_sf94_effectsize,"/home/skerr/Git/SF94/Outputs/D8_sf94_effectsize.csv")

#--------------- calculate change in SF94 by mortality difference-------------
sf94_d5<-lrm(mortality_28 ~ delta_SF94_05+ age_estimateyears+ sex, data = regresson_df_P)

intercept5<-as.numeric(coef(sf94_d5)[1]) 
coefday5<-as.numeric(coef(sf94_d5)[2])
coefage5<-as.numeric(coef(sf94_d5)[3])
coefsex5<-as.numeric(coef(sf94_d5)[4])
age_value_5<-mean(regresson_df_P$age, na.rm=T)

absolute_mortdifD5<-function(mort1, mortdifference) {
  logoddsmort1<-log(1/((1-mort1)/mort1))
  baselinesf94_delta_male<-(logoddsmort1-intercept5- (coefage5 * age_value_5)- (1 * coefsex5))/coefday5
  baselinesf94_delta_female<-(logoddsmort1-intercept5- (coefage5 * age_value_5)- (0 * coefsex5))/coefday5
  baseline_delta_sf94<-(baselinesf94_delta_male * male_perc) + (baselinesf94_delta_female * female_perc)
  mort2<-mort1-mortdifference
  logoddsmort2<-log(1/((1-mort2)/mort2)) #from probability to logodds
  new_delta_sf94_male<-(logoddsmort2-intercept5- (coefage5 * age_value_5)- (1 * coefsex5))/coefday5
  new_delta_sf94_female<-(logoddsmort2-intercept5- (coefage5 * age_value_5)- (0 * coefsex5))/coefday5
  new_delta_sf94<-(new_delta_sf94_male * male_perc) + (new_delta_sf94_female * female_perc)
  sf94_delta_dif<-new_delta_sf94- baseline_delta_sf94
  return(sf94_delta_dif)
}

absolute_mort_reduction<-c(0.01,0.015,0.02,0.025,0.03,0.035,0.04,0.045,0.05)
D5_delta_sf94_effectsize<-absolute_mortdifD5(0.25, absolute_mort_reduction)
write.csv(D5_delta_sf94_effectsize,"/home/skerr/Git/SF94/Outputs/D5_delta_sf94_effectsize.csv")

#repeat for D8
sf94_d8<-lrm(mortality_28 ~ delta_SF94_08+ age_estimateyears+ sex, data = regresson_df_P)
intercept8<-as.numeric(coef(sf94_d8)[1]) 
coefday8<-as.numeric(coef(sf94_d8)[2])
coefage8<-as.numeric(coef(sf94_d8)[3])
coefsex8<-as.numeric(coef(sf94_d8)[4])
age_value_5<-mean(regresson_df_P$age, na.rm=T) #these don't change when between D5 and D8

absolute_mortdifD8<-function(mort1, mortdifference) {
  logoddsmort1<-log(1/((1-mort1)/mort1))
  baselinesf94_delta_male<-(logoddsmort1-intercept8- (coefage8 * age_value_5)- (1 * coefsex8))/coefday8
  baselinesf94_delta_female<-(logoddsmort1-intercept8- (coefage8 * age_value_5)- (0 * coefsex8))/coefday8
  baseline_delta_sf94<-(baselinesf94_delta_male * male_perc) + (baselinesf94_delta_female * female_perc)
  mort2<-mort1-mortdifference
  logoddsmort2<-log(1/((1-mort2)/mort2)) #from probability to logodds
  new_delta_sf94_male<-(logoddsmort2-intercept8- (coefage8 * age_value_5)- (1 * coefsex8))/coefday8
  new_delta_sf94_female<-(logoddsmort2-intercept8- (coefage8 * age_value_5)- (0 * coefsex8))/coefday8
  new_delta_sf94<-(new_delta_sf94_male * male_perc) + (new_delta_sf94_female * female_perc)
  sf94_delta_dif<-new_delta_sf94- baseline_delta_sf94
  return(sf94_delta_dif)
}

absolute_mort_reduction<-c(0.01,0.015,0.02,0.025,0.03,0.035,0.04,0.045,0.05)
D8_delta_sf94_effectsize<-absolute_mortdifD8(0.25, absolute_mort_reduction)
write.csv(D8_delta_sf94_effectsize,"/home/skerr/Git/SF94/Outputs/D8_delta_sf94_effectsize.csv")

### alternative SF94 model
sf94_d5_2<-lrm(mortality_28 ~ sf94_day5_P+sf94_day0+ age_estimateyears+ sex, data = regresson_df_P)
sf94_d8_2<-lrm(mortality_28 ~ sf94_day8_P+sf94_day0+ age_estimateyears+ sex, data = regresson_df_P)

abseffect_size<- function(mortdifference, mort1, model_sf94) {
  logoddsmort1<-log(1/((1-mort1)/mort1))
  baselinesf94_male<-(logoddsmort1-as.numeric(coef(model_sf94)[1])-
                        (as.numeric(coef(model_sf94)[4]) * mean(regresson_df_P$age, na.rm=T))- 
                        (1* as.numeric(coef(model_sf94)[5]))-
                        (mean(regresson_df_P$sf94_day0, na.rm=T) * as.numeric(coef(model_sf94)[3]) ))/as.numeric(coef(model_sf94)[2])
  baselinesf94_female<-(logoddsmort1-as.numeric(coef(model_sf94)[1])-
                          (as.numeric(coef(model_sf94)[4]) * mean(regresson_df_P$age, na.rm=T))- 
                          (0* as.numeric(coef(model_sf94)[5]))-
                          (mean(regresson_df_P$sf94_day0, na.rm=T) * as.numeric(coef(model_sf94)[3])) )/as.numeric(coef(model_sf94)[2])
  baselinesf94<-(baselinesf94_male * male_perc) + (baselinesf94_female * female_perc)
  mort2<-mort1-mortdifference
  logoddsmort2<-log(1/((1-mort2)/mort2)) #from probability to logodds
  newsf94_male<-(logoddsmort2-as.numeric(coef(model_sf94)[1])-
                   (as.numeric(coef(model_sf94)[4]) * mean(regresson_df_P$age, na.rm=T))- 
                   (1 * as.numeric(coef(model_sf94)[5]))-
                   (mean(regresson_df_P$sf94_day0, na.rm=T) * as.numeric(coef(model_sf94)[3])))/as.numeric(coef(model_sf94)[2])
  newsf94_female<-(logoddsmort2-as.numeric(coef(model_sf94)[1])-
                     (as.numeric(coef(model_sf94)[4]) * mean(regresson_df_P$age, na.rm=T))- 
                     (0 * as.numeric(coef(model_sf94)[5]))-
                     (mean(regresson_df_P$sf94_day0, na.rm=T) * as.numeric(coef(model_sf94)[3])))/as.numeric(coef(model_sf94)[2])
  newsf94<-(newsf94_male * male_perc) + (newsf94_female * female_perc)
  sf94_difference<-newsf94-baselinesf94
  return(sf94_difference)
}
absolute_mort_reduction<-c(0.01,0.015,0.02,0.025,0.03,0.035,0.04,0.045,0.05)
sf94_25_D5<-abseffect_size(absolute_mort_reduction,0.25,sf94_d5_2)
sf94_25_D8<-abseffect_size(absolute_mort_reduction,0.25,sf94_d8_2)

write.csv(sf94_25_D5,"/home/skerr/Git/SF94/Outputs/sf94_25_D5.csv")
write.csv(sf94_25_D8,"/home/skerr/Git/SF94/Outputs/sf94_25_D8.csv")

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