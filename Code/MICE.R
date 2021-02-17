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
