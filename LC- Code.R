library(mlr)
library(sqldf)
library(data.table)
library(dplyr)
library(mlr)
library(Amelia)
library(MASS)
library(sqldf)
library(RSQLite)
library(randomForest)
library(glmnet)
library(xgboost)
library(nnet)
library(caret)
library(FactoMineR)
library(caret)
library(lubridate)

##------------------------------Reading the data----------------------------------#
train1 <- read.csv("C:/Users/25001242.CILAPH12692/Documents/lending club/Data Shared/trainLC1.csv")
train2 <- read.csv("C:/Users/25001242.CILAPH12692/Documents/lending club/Data Shared/trainLC2.csv")
train3 <- read.csv("C:/Users/25001242.CILAPH12692/Documents/lending club/Data Shared/trainLC3.csv")
train4 <- read.csv("C:/Users/25001242.CILAPH12692/Documents/lending club/Data Shared/trainLC4.csv")


testLc <- read.csv("C:/Users/25001242.CILAPH12692/Documents/lending club/Data Shared/testLC.csv")
test = testLc

train = rbind(train1, train2, train3, train4)
 train =train_copy 
##-------------------------splitting test & train-----------------------------------##

train$ln_status <-
  ifelse(
    train$loan_status == 'Fully Paid' | train$loan_status == 'Current'
    | train$loan_status == 'Issued'  |
      train$loan_status == "Does not meet the credit policy. Status:Fully Paid" ,
    0,1)
##-------------------------clean the data-------------------------------------------##

#-----------remove duplicates--------
train = sqldf("select distinct *  from train")
train = sqldf("select distinct *  from train where id is not NULL")
sum(is.na(train$id))

train[train == ""] <- NA


#-------Check the target variable and dates-------
table(train$ln_status)

sum(is.na(train$issue_d))
class(train$issue_d)

sum(is.na(train$earliest_cr_line))
class(train$earliest_cr_line)

#--------Convert dates to date type to calc age of 1st credit
train$issue_d <- as.character(train$issue_d)
train$issue_d <- paste(train$issue_d, "-01", sep = "")
train$issue_d <- parse_date_time(train$issue_d, "myd")

train$earliest_cr_line <- as.character(train$earliest_cr_line)
train$earliest_cr_line <- paste(train$earliest_cr_line, "-01", sep = "")
train$earliest_cr_line <- parse_date_time(train$earliest_cr_line, "myd")

train$time_since_first_credit <- train$issue_d - train$earliest_cr_line
train$time_since_first_credit <- as.numeric(train$time_since_first_credit)
sum(is.na(train$time_since_first_credit))

train = sqldf("select distinct *  from train where time_since_first_credit is not NULL and time_since_first_credit >0")
sum(is.na(train$id))
head(train$time_since_first_credit)

table(train$ln_status, train$grade)
table(train$ln_status, train$term)


#--------find NA's in the data
sumc <- data.frame(summarizeColumns(train))
sumc$na_prcnt<- round((sumc$na/nrow(train))*100, 2)

#------drop the columns with more than 50 percent data missing
drop_Cols<-sqldf("select name from sumc where na_prcnt >50")
for (f in drop_Cols) {
  train[f] = NULL
  #print(train[f])
}
sumc <- data.frame(summarizeColumns(train))
sumc$na_prcnt<- round((sumc$na/nrow(train))*100, 2)


#-----impute all the numeric feinds which have missing values with 0
x<- 1:nrow(sumc)
for( f in x)
{
  if((sumc$type[f]== 'numeric'| sumc$type[f]== 'integer')&& sumc$na[f]>0)
  {
    y = sumc$name[f]
    print(y)
    #print(train$y[is.na(train$y)])
    #print(train[[y]])
    train[[y]][is.na(train[[y]])] = 0
    
  }
}

sumc <- data.frame(summarizeColumns(train))
sumc$na_prcnt<- round((sumc$na/nrow(train))*100, 2)

#--------decide which factor variables are useful based on intution
##--------Treatin the rows

train$emp_title = NULL
train$url = NULL
train$desc = NULL
train$title = NULL



#convert to factors all chars
features = names(train)

for (f in features) {
  if (class(train[[f]])=="character") {
    #cat("VARIABLE : ",f,"\n")
    levels <- unique(train[[f]])
    train[[f]] <- as.factor(factor(train[[f]], levels=levels))
  }
}

sumc <- data.frame(summarizeColumns(train))
sumc$na_prcnt<- round((sumc$na/nrow(train))*100, 2)

write.csv(train , "C:/Users/25001242.CILAPH12692/Documents/lending club/Data Shared/train_lending_club_cleaned_file.csv")

#----model

train$year =NULL
train$loan_status = NULL
train$X = NULL
train$time_since_first_credit = NULL
library(xgboost)

data <- train[,c(features)]
lab <- train$ln_status
train_T_N<-data.matrix(data)
table(train$ln_status)
?xgboost

model_xgb <- xgboost(data = train_T_N, label = lab,
                     nrounds = 10, seed =2 ,
                     eta = 0.07, nthread = 26, 
                     eval_metric = "auc",
                     objective = "binary:logistic")

imp<- xgb.importance(feature_names = colnames(data), model = model_xgb)
xgb.plot.importance(imp)
imp <- data.frame(imp)
imp$Feature

features = c("recoveries"   ,      "total_rec_prncp"   , "funded_amnt"   ,     "out_prncp",         
"last_pymnt_d"  ,     "last_pymnt_amnt"   , "total_rec_late_fee" , "next_pymnt_d" ,     
"issue_d"      ,                  "last_credit_pull_d" ,"int_rate" ,         
          "loan_amnt"       ,   "total_rec_int"  ,    "dti"  )









csqldf("select count (*) from (select distinct *  from train) as abc")
#count (*)
#1    709904
#removing the duplicates
train_main = train
train = sqldf("select distinct *  from train")

sumc <- data.frame(summarizeColumns(train))
sumc$na_prcnt<- round((sumc$na/nrow(train))*100, 2)

sqldf("delete from train where term = NULL")
train1 = train
train = train[!train$year == 0,]

sqldf("select count(*) , ln_status from train group by ln_status")

#(53952/655951)*100
#base_acc = 100 - 8.22
#91 percent

features =c( "total_rec_prncp" ,"funded_amnt" ,"out_prncp" ,
  "total_rec_late_fee" ,"last_pymnt_amnt" , "int_rate",  "tot_cur_bal" ,  
  "installment" , "out_prncp_inv" , "total_rev_hi_lim"  , "funded_amnt_inv"   ,
  "total_rec_int"  ,  "loan_amnt"  ,  "revol_bal" ,  "annual_inc" ,
  "annual_inc", "delinq_2yrs", "open_acc", "emp_length", "home_ownership")


features1 = c("loan_amnt", "int_rate" , "annual_inc", "delinq_2yrs", "open_acc", "dti", 
  "emp_length", "funded_amnt", "tot_cur_bal", "home_ownership",
  "recoveries", "total_rec_prncp" ,"funded_amnt" ,"out_prncp" ,
  "total_rec_late_fee" ,"last_pymnt_amnt" , "ln_status")

sqldf("select distinct home_ownership  from train")


train_subset =train[, c(features1)]
sumc_train <- data.frame(summarizeColumns(train_subset))
sumc_train$na_prcnt<- round((sumc_train$na/nrow(train_subset))*100, 2)

train_subset$home_ownership = as.numeric(as.factor(train_subset$home_ownership))
train_subset$emp_length = as.numeric(as.factor(train_subset$emp_length))
##---------------train the model------------------------------------------##
library(xgboost)

data <- train_subset[, -17]
lab <- train_subset$ln_status
train_T_N<-data.matrix(data)


model_xgb <- xgboost(data = train_T_N, label = lab,
                     nrounds = 10, seed =2 ,
                     eta = 0.07, nthread = 26, 
                     eval_metric = "auc",
                     objective = "binary:logistic")

imp<- xgb.importance(feature_names = colnames(data), model = model_xgb)
xgb.plot.importance(imp)
imp <- data.frame(imp)
imp$Feature


library(h2o)
h2o.shutdown()
h2o.init()
train.hex <- as.h2o(train_subset)
test.hex <- as.h2o(test)
features= colnames(data)
#colnames(train[,c(1,2)])
?h2o.deeplearning
dl_model_2 = h2o.deeplearning( x=features,
                               # x=features,
                               y = "ln_status",
                               training_frame =train.hex ,
                               #validation_frame =testHex ,
                               activation="Rectifier",
                               hidden=2,
                               epochs=4,
                               adaptive_rate =F)


#---------------------cleaning the test-----------------------------------##
test[test == ""] <- NA
#droppin the columns that are not part of train
drop_list <-  colnames(train)
drop_list = data.frame(drop_list)
features = drop_list$drop_list
test = test[ , c(features)]

c =  data.frame(colnames(train))
d =  data.frame(colnames(test))


test=
  test[, c("acc_now_delinq",
           "addr_state",
           "annual_inc",
           "application_type",
           "collection_recovery_fee",
           "collections_12_mths_ex_med",
           "delinq_2yrs",
           "dti",
           "emp_length",
           "funded_amnt",
           "funded_amnt_inv",
           "grade",
           "home_ownership",
           "initial_list_status",
           "inq_last_6mths",
           "installment",
           "int_rate",
           "last_pymnt_amnt",
           "loan_amnt",
           "open_acc",
           "out_prncp",
           "out_prncp_inv",
           "policy_code",
           "pub_rec",
           "purpose",
           "pymnt_plan",
           "recoveries",
           "revol_bal",
           "revol_util",
           "sub_grade",
           "term",
           "tot_coll_amt",
           "tot_cur_bal",
           "total_acc",
           "total_pymnt",
           "total_pymnt_inv",
           "total_rec_int",
           "total_rec_late_fee",
           "total_rec_prncp",
           "total_rev_hi_lim",
           "verification_status",
           "verification_status_joint")]
data_test <- test
lab_test <- test_lab
train_V_N <- data.matrix(data_test)