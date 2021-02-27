library(tidyverse) 
library(caret)
library(psych)
library(pROC)
library(ROCR)

# reading the file
Tdata <- read.csv('C:/Users/CSUFTitan/Desktop/574/Project/WA_Fn-UseC_-Telco-Customer-Churn.csv')
str(Tdata)

# remove null values
Tdata <- na.omit(Tdata)

# removing Customer id
Tdata <- Tdata[,-1]
#Convert SeniorCitizen as factor variable
Tdata$SeniorCitizen<- as.factor(Tdata$SeniorCitizen)

# changing values
levels(Tdata$StreamingMovies)[levels(Tdata$StreamingMovies)=="No internet service"] <- "No"
levels(Tdata$OnlineSecurity)[levels(Tdata$OnlineSecurity)=="No internet service"] <- "No"
levels(Tdata$OnlineBackup)[levels(Tdata$OnlineBackup)=="No internet service"] <- "No"
levels(Tdata$DeviceProtection)[levels(Tdata$DeviceProtection) =="No internet service"] <- "No"
levels(Tdata$TechSupport)[levels(Tdata$TechSupport)=="No internet service"] <- "No"
levels(Tdata$StreamingTV)[levels(Tdata$StreamingTV)=="No internet service"] <- "No"
levels(Tdata$MultipleLines)[levels(Tdata$MultipleLines)=="No phone service"] <- "No"

#check data
head(Tdata)
summary(Tdata)
str(Tdata)

chisq <- chisq.test(Tdata$gender,Tdata$Churn)
chisq
chisq <- chisq.test(Tdata$SeniorCitizen,Tdata$Churn)
chisq
chisq <- chisq.test(Tdata$Partner,Tdata$Churn)
chisq
chisq <- chisq.test(Tdata$Dependents,Tdata$Churn)
chisq
chisq <- chisq.test(Tdata$tenure,Tdata$Churn)
chisq
chisq <- chisq.test(Tdata$PhoneService,Tdata$Churn)
chisq
chisq <- chisq.test(Tdata$MultipleLines,Tdata$Churn)
chisq
chisq <- chisq.test(Tdata$OnlineSecurity,Tdata$Churn)
chisq
chisq <- chisq.test(Tdata$OnlineBackup,Tdata$Churn)
chisq
chisq <- chisq.test(Tdata$DeviceProtection,Tdata$Churn)
chisq
chisq <- chisq.test(Tdata$TechSupport,Tdata$Churn)
chisq
chisq <- chisq.test(Tdata$StreamingTV,Tdata$Churn)
chisq
chisq <- chisq.test(Tdata$StreamingMovies ,Tdata$Churn)
chisq
chisq <- chisq.test(Tdata$Contract,Tdata$Churn)
chisq
chisq <- chisq.test(Tdata$PaperlessBilling,Tdata$Churn)
chisq
chisq <- chisq.test(Tdata$PaymentMethod ,Tdata$Churn)
chisq
chisq <- chisq.test(Tdata$MonthlyCharges,Tdata$Churn)
chisq
chisq <- chisq.test(Tdata$TotalCharges,Tdata$Churn)
chisq

str(Tdata)
#remove gender 
Tdata <-Tdata[,-1]
str(Tdata)

#remove phone service

Tdata <-Tdata[,-5]
str(Tdata)

#remove total charges

Tdata <-Tdata[,-17]
str(Tdata)

#creating training and test data set - Cross validation
set.seed(123)
data<-sample(2,nrow(Tdata),replace=TRUE,prob=c(0.7,0.3))
dim(data)
train<-Tdata[data==1, ]
test<- Tdata[data==2, ]

dim(train)
dim(test)
head(train)

min.model1 = glm(Churn ~ 1, data = train, family = 'binomial')
max.model1 = glm(Churn ~ ., data = train, family = 'binomial')
max.formula.model1 = formula(max.model1)

backward = step(max.model1, direction='backward', scope=max.formula.model1)
summary(backward)

# predict Churn in Training dataset
mm<-predict.glm(backward, new=train,type="response",se.fit=TRUE)
train$pred <- mm$fit
table <- table(train$pred > 0.5, train$Churn) 
table

# check for model accuracy in training data set 
acc<-sum(diag(table))/sum(table)
acc

miss_Class_error<-1-acc
miss_Class_error

# predict Churn in Testing dataset
mtest<-predict.glm(backward, new=test,type="response",se.fit=TRUE)
test$pred <- mtest$fit
tabletest <- table(test$pred > 0.5, test$Churn) 
tabletest

# check for model accuracy in testing data set 
acc<-sum(diag(tabletest))/sum(tabletest)
acc

miss_Class_error<-1-acc
miss_Class_error

train_prob <- predict(backward, data = train, type = "response") 
test_prob <- predict(backward, newdata = test, type = "response")

#Checking ROC and AUC
library(pROC)
roc <- roc(train$Churn, train_prob, plot= TRUE, print.auc=TRUE)
roc <- roc(test$Churn, test_prob, plot= TRUE, print.auc=TRUE)


train_pred <- factor(ifelse(train_prob >= 0.5, "Yes", "No"))
train_actual <- factor(ifelse(train$Churn == "Yes", "Yes", "No"))

test_pred <- factor(ifelse(test_prob >= 0.5, "Yes", "No"))
test_actual <- factor(ifelse(test$Churn == "Yes", "Yes", "No"))

confusionMatrix(data = test_pred, reference = test_actual)

#####################0.3 cut off #############

tabletest <- table(test$pred > 0.3, test$Churn) 
tabletest

# check for model accuracy in testing data set 
acc<-sum(diag(tabletest))/sum(tabletest)
acc

miss_Class_error<-1-acc
miss_Class_error

test_pred <- factor(ifelse(test_prob >= 0.3, "Yes", "No"))
test_actual <- factor(ifelse(test$Churn == "Yes", "Yes", "No"))

# Test data
confusionMatrix(data = test_pred, reference = test_actual)

##############0.4 Cut off ############

tabletest <- table(test$pred > 0.4, test$Churn) 
tabletest

# check for model accuracy in testing data set 
acc<-sum(diag(tabletest))/sum(tabletest)
acc

miss_Class_error<-1-acc
miss_Class_error

test_pred <- factor(ifelse(test_prob >= 0.4, "Yes", "No"))
test_actual <- factor(ifelse(test$Churn == "Yes", "Yes", "No"))

# Test data
confusionMatrix(data = test_pred, reference = test_actual)

############0.6########

tabletest <- table(test$pred > 0.6, test$Churn) 
tabletest

# check for model accuracy in testing data set 
acc<-sum(diag(tabletest))/sum(tabletest)
acc

miss_Class_error<-1-acc
miss_Class_error

test_pred <- factor(ifelse(test_prob >= 0.6, "Yes", "No"))
test_actual <- factor(ifelse(test$Churn == "Yes", "Yes", "No"))

# Test data
confusionMatrix(data = test_pred, reference = test_actual)
