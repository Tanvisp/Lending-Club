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

##------------------------------Reading the data----------------------------------##
loan <- read.csv("C:/Users/25001242.CILAPH12692/Desktop/loan.csv")
summarizeColumns(loan)
loan_dump<- loan
#loan<- loan_dump
##-------------------------assigning the status-----------------------------------##
levels(loan$loan_status)
sqldf("select loan_status, count(*) cnt from loan group by loan_status order by cnt")
## "Charged Off"   = 1
## "Current" = 0
## "Default" = 1
## "Does not meet the credit policy. Status:Charged Off" =1
## "Does not meet the credit policy. Status:Fully Paid" = 0
## "Fully Paid"= 0
## "In Grace Period" =0
## "Issued" = 0
## "Late (16-30 days)" 1
## "Late (31-120 days)"1

loan$ln_status <-
  ifelse(
    loan$loan_status == 'Fully Paid' | loan$loan_status == 'Current'
    | loan$loan_status == 'Issued'  |
      loan$loan_status == "Does not meet the credit policy. Status:Fully Paid" ,
    0,1)

sqldf("select distinct loan_status, ln_status from loan order by ln_status")
#loan$loan_status <- NULL

##-------------------------splitting test & train-----------------------------------##

sqldf("select distinct issue_d  from loan ")
#gsub(c("Jan-", "Feb-","Mar-", "Apr-", "May-", "Jun-", "Jul-", "Aug-", "Sep-", "Oct-", "Nov-", "Dec-"),
##     '', loan$issue_d )
#gsub("Jan-", '', loan$issue_d)

loan$year <- as.numeric(substr(loan$issue_d , 5,8))
sqldf("select count(*) cnt, year from loan group by year order by cnt")


d = sort(sample(nrow(loan), nrow(loan)*0.8))
train =  loan[d,]
test = loan[-d,]
test$ln_status <- NULL

sqldf("select count(*), ln_status from train group by ln_status")
sqldf("select count(*), ln_status from test group by ln_status")
sqldf("select count(*), ln_status from loan group by ln_status")


x<- sort(sample(nrow(train), nrow(train)*0.25))
train1<- train[x, ]
#train2<- train[-x,]
train3<- train2[x,]
train4<- train2[-x,]
train11<- train1[x,]
train12<- train1[-x,]
y <- sort(sample(nrow(train), nrow(train)*0.5))

setwd("C:/Users/25001242.CILAPH12692/Documents/lending club")
write.csv(train11, file = "trainLC1.CSV", row.names = F )
write.csv(train12, file = "trainLC4.CSV", row.names = F )
write.csv(train3, file = "trainLC2.CSV", row.names = F )
write.csv(train4, file = "trainLC3.CSV", row.names = F )

write.csv(test, file = "testLC.csv", row.names = F )

##---------------clean the data------------------------------------------##
loan_1<- loan
loan$next_pymnt_d <- loan_1$next_pymnt_d
loan$last_pymnt_d <- loan_1$last_pymnt_d

#loan<-loan_1
loan[loan == ""] <- NA
summarizeColumns(loan)
sum(is.na(test))
sort(sapply(loan,function(x){sum(is.na(x))/length(x)}),decreasing =T) #percetage


##Treatin the rows
loan$id <- NULL
loan$member_id <- NULL
loan$desc <- NULL
loan$url <- NULL
loan$next_pymnt_d <- NULL
loan$last_pymnt_d <- NULL
loan$earliest_cr_line <- NULL
loan$last_credit_pull_d <- NULL
loan$year <- NULL
loan$title <- NULL
loan$loan_status <-  NULL

##dropping columsn with more than 50 pcnt blank/NA
sumc <- data.frame(summarizeColumns(loan))
sumc$na_prcnt<- round((sumc$na/887379)*100)
drop_Cols<-sqldf("select name from sumc where na_prcnt >50")

for (f in drop_Cols) {
  loan[f] = NULL
}

##check again
loan_red <- loan
summarizeColumns(loan)
sum(is.na(loan))
sumc_1 <- data.frame(summarizeColumns(loan))
sumc_1$na_prcnt<- round((sumc_1$na/887379)*100)

##thoughtfully impute the col
loan$emp_title<- as.character(loan$emp_title)
loan$emp_title[is.na(loan$emp_title)] = 'Others'
loan$emp_title<- as.factor(loan$emp_title)
loan$emp_title = NULL

#loan$title <- as.character(loan$title)
#loan$title[is.na(loan$title)] = 'Others'
#loan$title<- as.factor(loan$title)


#impute all the numeric feinds which have missing values with 0
x<- 1:nrow(sumc_1)
for( f in x)
{
  if(sumc_1$type[f]== 'numeric' && sumc_1$na[f]>0)
  {
    y = sumc_1$name[f]
    print(y)
    #print(loan$y[is.na(loan$y)])
    #print(loan[[y]])
    loan[[y]][is.na(loan[[y]])] = 0
    
  }
}

#check again for missing values.now only factors must remain.
sumc_1 <- data.frame(summarizeColumns(loan))
sumc_1$na_prcnt<- round((sumc_1$na/887379)*100)


x<- sqldf("update loan set last_pymnt_d= issue_d , next_pymnt_d = issue_d
          where (last_pymnt_d IS NULL AND next_pymnt_d  IS NULL)")

sqldf("select next_pymnt_d, last_pymnt_d, issue_d FROM loan where last_pymnt_d IS NULL ")

##update the next and last payment dates to issue date when both are null together
x= c(1:nrow(loan))
for (f in x)
{ 
  if(is.na(loan$last_pymnt_d[f]) && is.na(loan$next_pymnt_d[f]))
  {
    #print("true")
    #print(loan$last_pymnt_d[f])
    #print(loan$next_pymnt_d[f])
    #print(loan$issue_d[f])
    loan$last_pymnt_d[f] <- loan$issue_d[f]
    loan$next_pymnt_d[f] <- loan$issue_d[f]
    #print("true")
  }}

loan$last_pymnt_d[is.na(loan$last_pymnt_d)] = "Mar-2016"
loan$next_pymnt_d[is.na(loan$next_pymnt_d)] = "Mar-2016"
loan$last_credit_pull_d[is.na(loan$last_credit_pull_d)] = "Jan-2016"
loan$earliest_cr_line[is.na(loan$earliest_cr_line)] = "Jan-1999"
as.Date(loan$issue_d)
last_pymnt_d <- NULL

x<- 1:nrow(sumc_1)
for( f in x)
{
  if(sumc_1$type[f]== 'numeric' && sumc_1$na[f]>0)
  {
    y = sumc_1$name[f]
    print(y)
    #print(loan$y[is.na(loan$y)])
    #print(loan[[y]])
    loan[[y]][is.na(loan[[y]])] = 0
    
  }
}

#check again for missing values.now only factors must remain.
sumc_1 <- data.frame(summarizeColumns(loan))
sumc_1$na_prcnt<- round((sumc_1$na/887379)*100)

##check again
loan_red1 <- loan
summarizeColumns(loan)
sum(is.na(loan))
sumc_1 <- data.frame(summarizeColumns(loan))
sumc_1$na_prcnt<- round((sumc_1$na/887379)*100)

loan$title[is.na(loan$title)] = 'Others'
names(loan)

#fsa
fnl_dataset <- FSA(data = train, 
                   dependent.variable = "ln_status",  
                   depend.variable.type = "discrete") 
fsa_subset_data <- data.frame(fnl_dataset[[1]])
fsa_feature_list_summary <- data.frame(fnl_dataset[[2]])
fsa_feature_list_full <- data.frame(fnl_dataset[[3]])





##---------------train the model------------------------------------------##


##xgboost
memory.limit(4000000000)
gc()
library(data.table)
library(xgboost)


d = sort(sample(nrow(loan), nrow(loan)*0.8))
train =  loan[d,]
test = loan[-d,]
test$ln_status <- NULL

data <- train[,-45]
lab <- train[,45]
train_T_N<-data.matrix(data)


model_xgb <- xgboost(data = train_T_N, label = lab,
                     nrounds = 10, seed =1 ,
                     eta = 0.05, nthread = 2, 
                     eval_metric = "auc",
                     objective = "binary:logistic")


data_test <- test[,-45]
lab_test <- test[,45]
train_V_N <- data.matrix(data_test)


preds = NULL
preds = predict(model_xgb, train_V_N, outputmargin = 'F')
err <- mean(as.numeric(preds > 0.5) != lab_test)

predict <- ifelse(preds > 0.5,1,0)
test$predict <- predict
test$ln_status <- lab_test

misClasificError<- ifelse(test$predict != test$ln_status, 1, 0)
test$misClasificError <- misClasificError
miss_clas <- subset(test, test$misClasificError ==1 )
acc_test_metr <- (nrow(miss_clas)/nrow(test))*100
acc_test_metr_final<- 100- acc_test_metr
table(test$ln_status, test$predict)

#plot(varImp(model_xgb), top = 20)

imp<- xgb.importance(feature_names = colnames(data), model = model_xgb)
xgb.plot.importance(imp)

##-----------deep learning using h20-------------
install.packages("h2o", type="source",
                 repos=(c("http://h2o-release.s3.amazonaws.com/h2o/rel-wheeler/2/index.html")))
library(h2o)

h2o.shutdown()
h2o.server <- h2o.init( nthreads= -1)
train.hex <- as.h2o(train)
features= colnames(train[,-45])
?h2o.deeplearning
dl_model_2 = h2o.deeplearning( x=features,
                               # x=features,
                               y = "ln_status",
                               training_frame =train.hex ,
                               #validation_frame =testHex ,
                               activation="Rectifier",
                               hidden=1,
                               epochs=1,
                               adaptive_rate =F)
##---------------------------------------------------------------------
install.packages("neuralnet", dependencies = T)
library(neuralnet)
?neuralnet

install.packages("nnet")
library(nnet)
?multinom

memory.limit(800000000)
gc()
model_NN<- multinom(ln_status~., data=train, maxit=500, trace=T)

library(caret)
mostImportantVariables <- varImp(model_NN)
mostImportantVariables$Variables <- row.names(mostImportantVariables)
mostImportantVariables <- mostImportantVariables[order(-mostImportantVariables$Overall),]
print(head(mostImportantVariables))

preds = predict(model_NN, train_V)
train_V$predict <- preds
misClasificError<- ifelse(train_V$predict != train_V$Loan_Status, 1, 0)
train_V$misClasificError <- misClasificError
miss_clas <- subset(train_V, train_V$misClasificError ==1 )
acc_test_metr <- (nrow(miss_clas)/nrow(train_V))*100
acc_test_metr_final<- 100- acc_test_metr
table(train_V$Loan_Status, train_V$predict)
#predict_test<-predict(model ,test_F, type = 'response'



d = sort(sample(nrow(loan), nrow(loan)*0.8))
train =  loan[d,]
test = loan[-d,]
test$ln_status <- NULL
loan <- loan_fin
x<- 1:nrow(sumc_1)
for( f in x)
{
  if(sumc_1$type[f]== 'factor' )
  {
    y = sumc_1$name[f]
    print(y)
    #print(loan$y[is.na(loan$y)])
    loan[[y]] = (as.numeric(as.factor(loan[[y]])))
    
    
  }
}

as.numeric(loan[]

library(neuralnet)

f <- as.formula(paste("ln_status ~", paste(n[!n %in% "ln_status"], collapse = " + ")))
?neuralnet

nn <-neuralnet(f  , 
               data = train, hidden = c(4,2), err.fct = "ce", linear.output = F)

nn <- neuralnet(f,data=train_,hidden=c(5,3),linear.output=T)

sumc_1 <- data.frame(summarizeColumns(loan))
sumc_1$na_prcnt<- round((sumc_1$na/887379)*100)

#--------------------
d = sort(sample(nrow(loan), nrow(loan)*0.8))
train =  loan[d,]
test = loan[-d,]
test$ln_status <- NULL

data <- train[,-45]
lab <- train[,45]
train_T_N<-data.matrix(data)


data_test <- test[,-45]
lab_test <- test[,45]
train_V_N <- data.matrix(data_test)

train.hex <- as.h2o(train)
library(h2o)
h2o.init(nthreads = -1)


dl_fit1 <- h2o.deeplearning(x = data,
                            y = lab,
                            training_frame = train_V_N,
                            model_id = "dl_fit1",
                            hidden = c(20,20),
                            seed = 1)
