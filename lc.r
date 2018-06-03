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

##------------------------------Reading the data----------------------------------##
loan <- read.csv("C:/Users/25001242.CILAPH12692/Desktop/loan.csv")
summarizeColumns(loan)
loan_dump<- loan
loan<- loan_dump
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
loan<-loan_1
loan[loan == ""] <- NA
summarizeColumns(loan)
sum(is.na(test))
sort(sapply(loan,function(x){sum(is.na(x))/length(x)}),decreasing =T) #percetage


##Treatin the rows
loan$id <- NULL
loan$member_id <- NULL
loan$desc <- NULL

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
sumc_1 <- data.frame(summarizeColumns(loan))
sumc_1$na_prcnt<- round((sumc_1$na/887379)*100)

##thoughtfully impute the col

loan$emp_title[is.na(loan$emp_title)] = 'Others'

loan$tot_coll_amt[is.na(loan$tot_coll_amt)] = 0
loan$tot_cur_bal[is.na(loan$tot_cur_bal)] = 0
loan$total_rev_hi_lim[is.na(loan$total_rev_hi_lim)] = 0

sqldf("select next_pymnt_d, last_pymnt_d, issue_d FROM loan where last_pymnt_d IS NULL ")

x<- sqldf("update loan set last_pymnt_d= issue_d , next_pymnt_d = issue_d
       where (last_pymnt_d IS NULL AND next_pymnt_d  IS NULL)")



ifelse( is.na(loan$last_pymnt_d) & is.na(loan$next_pymnt_d) , issue_d,  )



ifelse(is.na(loan$last_pymnt_d) && is.na(loan$next_pymnt_d))
   {
   print("true")
   #loan$last_pymnt_d <- issue_d
   #loan$next_pymnt_d <- issue_d
}

as.Date(loan$issue_d)

#fsa
  fnl_dataset <- FSA(data = loan,
                     dependent.variable = "ln_status",
                     depend.variable.type = "discrete")


fsa_subset_data <- data.frame(fnl_dataset[[1]])
fsa_feature_list_summary <- data.frame(fnl_dataset[[2]])
fsa_feature_list_full <- data.frame(fnl_dataset[[3]])
