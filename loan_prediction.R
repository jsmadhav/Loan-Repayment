setwd("C:\\Users\\madha\\Downloads")
Dataset = read.csv("loans.csv")
View(Dataset)
str(Dataset)
summary(Dataset)
# to find what are the missing values
apply(Dataset,2, function(x) any(is.na(x)))

# loans that are not paid fully
table(Dataset$not.fully.paid)
# check for missing values
missing <- subset(Dataset, 
                  is.na(log.annual.inc) | is.na(days.with.cr.line) | is.na(revol.util) 
                  | is.na(inq.last.6mths) | is.na(delinq.2yrs) | is.na(pub.rec))
missing
str(missing)
nrow(missing)
table(missing$not.fully.paid)

summary(missing)

# filling the missing values
#install.packages("mice")
library(mice)
set.seed(1234)
# spliting the dataset exclude not.fully.paid column
vars.for.imputation <- setdiff(names(Dataset), "not.fully.paid")
imputed <- complete(mice(Dataset[vars.for.imputation]))
Dataset[vars.for.imputation] <- imputed
summary(imputed)
summary(Dataset)

## splitting the dataset
library(caTools)
set.seed(1234)
# splitting 70% data
spl <- sample.split(Dataset$not.fully.paid,0.7)
train <- subset(Dataset, spl==TRUE)
test <- subset(Dataset, spl==FALSE)

# create logistic regression model to predict not.fully.paid using rest of the variables
mod  <- glm(not.fully.paid ~ .,data = train,family = "binomial")
summary(mod)

mod1  <- glm(not.fully.paid ~ int.rate,data = train,family = "binomial")
summary(mod1)


# predicting the probablity of loans not being paid back in full
test$predicted.risk <- predict(mod, newdata = test, type = "response")

## create the confusion matrix with thresold of 0.5
table(test$not.fully.paid,test$predicted.risk > 0.5)

# use ROCR package to compute the test set AUC
library(ROCR)
pred <- prediction(test$predicted.risk, test$not.fully.paid) # prediction of risk
as.numeric(performance(pred, "auc")@y.values) #converting to AUC value



# create bivariate logistic regression model, on int.rate only
# int.rate is an independent variable
bivariate <- glm(not.fully.paid ~ int.rate, data = train, family = "binomial")
summary(bivariate)

# corelation of the bivariate
cor(train[c("int.rate", "installment" , "log.annual.inc", "dti" ,
            "fico" , "days.with.cr.line" , "revol.bal" , "revol.util" , 
            "inq.last.6mths" , "delinq.2yrs" , "pub.rec")])

# prediction of test set
# check for the highest predictedd value
pred.bivariate <- predict(bivariate,newdata = test,type = "response")
max(pred.bivariate)

install.packages("prediction")
library(ROCR)
# test set AUC for bivariate model
prediction.bivariate <- prediction(pred.bivariate, test$not.fully.paid)
print("AUC of bivariate model is ")
as.numeric(performance(prediction.bivariate, "auc")@y.values)



# Calculating profit on investment on loans for 3 years at CI , set C=1
test$profit = exp(test$int.rate*3) - 1   # profit on an investment of $1
test$profit[test$not.fully.paid == 1] = -1 # profit on an investment of $1, not fully paid
summary(test$profit)   # checking average profit


## build a test set model with intrest rate of atleast 15%
highInterest <- subset(test,int.rate>=0.15) # subset of high interset loans
summary(highInterest$profit) 
print("Average return on high interest investment is ")
mean(highInterest$profit)
table(highInterest$not.fully.paid)


# build a dataframe consisting of high intrest loans with predicted risk
cutoff <-  sort(highInterest$predicted.risk, decreasing=FALSE)[100]
print("Cut-off risk for our investment is")
cutoff



# use this cutoff value to make our investment strategy and 
# find out the toatl profit on an invsetment of $100 and percentage of loans not being paid fully.
selectedLoans=subset(highInterest,predicted.risk <= cutoff) # Making subset with predicted risk less than cutoff
print(" Total number of investemnt is")
nrow(selectedLoans)  # total numebr of investments
print("Total profit on these loans for $1 invested in each loan")
sum(selectedLoans$profit)  # total profit on these loans for $1 invested in each loan. 
print('Average profit on these loans is')
mean(selectedLoans$profit)   # average profit made on these loans
print("Breakdown of laons being paid or not")
table(selectedLoans$not.fully.paid) # breakdown of laons being paid or not
















setwd("C:\\Users\\madha\\Downloads")
loans = read.csv("loans.csv")

library(readr)
loans <- read.csv("loans.csv")
str(loans)
summary(loans)


# proportion of loans not paid fully
table(loans$not.fully.paid)

missing <- subset(loans, is.na(log.annual.inc) | is.na(days.with.cr.line) 
                  | is.na(revol.util) | is.na(inq.last.6mths) | is.na(delinq.2yrs) 
                  | is.na(pub.rec))
str(missing)
nrow(missing)
table(missing$not.fully.paid)



# to find what are the missing values
apply(loans,2, function(x) any(is.na(x)))


# filling the missing values
#install.packages("mice")
library(mice)
set.seed(1234)
# spliting the dataset exclude not.fully.paid column
vars.for.imputation <- setdiff(names(loans), "not.fully.paid")
imputed <- complete(mice(loans[vars.for.imputation]))
loans[vars.for.imputation] <- imputed
summary(imputed)
str(imputed)


## splitting the dataset
library(caTools)
set.seed(1234)
# splitting 70% data
spl <- sample.split(loans$not.fully.paid,0.7)
train <- subset(loans, spl==TRUE)
test <- subset(loans, spl==FALSE)

# create logistic regression model to predict not.fully.paid using rest of the variables
mod  <- glm(not.fully.paid ~ .,data = train,family = "binomial")
summary(mod)

mod1  <- glm(not.fully.paid ~ credit.policy+purpose_cr+int.rate+
               installment+log.annual.inc+dti+fico+days.with.cr.line+
               revol.bal+revol.util+inq.last.6mths+delinq.2yrs+
               pub.rec,data = train,family = "binomial")
summary(mod1)

test$predicted.risk1 <- predict(mod1, newdata = test, type = "response")
table(test$not.fully.paid,test$predicted.risk1 > 0.5)
library(ROCR)
pred <- prediction(test$predicted.risk1, test$not.fully.paid) # prediction of risk
as.numeric(performance(pred, "auc")@y.values) #converting to AUC value





loans$purpose_cr <- ifelse(loans$purpose == "credit_card",1,0)



install.packages("dummies")
library(dummies)

loans.new <- dummy.data.frame(loans,sep = "_")
names(loans.new)

model <- glm(not.fully.paid ~ credit.policy+purpose_cr+int.rate+installment+log.annual.inc+dti
             +fico+days.with.cr.line+revol.bal+revol.util+inq.last.6mths+delinq.2yrs
             +pub.rec, data=train, family = "binomial")
summary(model)
install.packages("leaps")
library(leaps)

subset_result <- regsubsets(not.fully.paid~.,data=loans, nbest=2, nvmax = 14)
subset_summary <-summary(subset_result)
plot(subset_result, scale="bic")

# 
#  length(levels(loans$purpose))
#  dummy_purpose_credit_card <- data.frame(model.matrix(~ purpose_credit_card, data=loans))
#  dummy_purpose <- dummy_purpose[, -1]
#  length(dummy_purpose)
#  loans_1 <- cbind(select(loans, -'purpose'),dummy_purpose)

 
# predicting the probablity of loans not being paid back in full
test$predicted.risk <- predict(mod, newdata = test, type = "response")

## create the confusion matrix with thresold of 0.5
table(test$not.fully.paid,test$predicted.risk > 0.5)

# use ROCR package to compute the test set AUC
library(ROCR)
pred <- prediction(test$predicted.risk, test$not.fully.paid) # prediction of risk
as.numeric(performance(pred, "auc")@y.values) #converting to AUC value


# create bivariate logistic regression model, on int.rate only
# int.rate is an independent variable
bivariate <- glm(not.fully.paid ~ int.rate, data = train, family = "binomial")
summary(bivariate)


# prediction of test set
# check for the highest predictedd value
pred.bivariate <- predict(bivariate,newdata = test,type = "response")
max(pred.bivariate)


install.packages("prediction")
library(ROCR)
# test set AUC for bivariate model
prediction.bivariate <- prediction(pred.bivariate, test$not.fully.paid)
print("AUC of bivariate model is ")
as.numeric(performance(prediction.bivariate, "auc")@y.values)


# Calculating profit on investment on loans for 3 years at CI , set C=1
test$profit = exp(test$int.rate*3) - 1   # profit on an investment of $1
test$profit[test$not.fully.paid == 1] = -1 # profit on an investment of $1, not fully paid
summary(test$profit)   # checking average profit


## build a test set model with intrest rate of atleast 15%
highInterest <- subset(test,int.rate>=0.15) # subset of high interset loans
summary(highInterest$profit) 
print("Average return on high interest investment is ")
mean(highInterest$profit)
table(highInterest$not.fully.paid)


plt.figure(figsize = (10, 6)) #Canvas
loans[loans["credit.policy"]==1]["fico"].hist(alpha = 0.5, color = "blue", bins = 30, label = "Credit Policy 1")
loans[loans["credit.policy"]==0]["fico"].hist(alpha = 0.5, color = "red", bins = 30, label = "Credit Policy 0")

