## Importing packages
library(tidyverse) 
library(MASS)
library(car)
library(e1071)
library(caret)
library(cowplot)
library(caTools)
library(pROC)
library(ggcorrplot)
library(plyr)
library(randomForest)
library(psych)
library(ROCR)
library(party)



telco <- read.csv('Telco-Customer-Churn.csv')
head(telco)
str(telco)

telco$SeniorCitizen<- as.factor(telco$SeniorCitizen)
str(telco)

telco<-telco[,-1]
str(telco)


nvar<-telco[,c(5,18,19)]
str(nvar)


dmy <- dummyVars(~gender+SeniorCitizen+Partner+Dependents+PhoneService+MultipleLines+InternetService+
                   OnlineSecurity+OnlineBackup+DeviceProtection+TechSupport+StreamingTV+StreamingMovies+Contract+
                   PaperlessBilling+PaymentMethod+Churn, data = telco, fullRank=T)

trsf <- data.frame(predict(dmy, newdata = telco))

head(trsf)

trsf<-lapply(trsf,factor)
str(trsf)

fdata<-cbind(nvar,trsf)
str(fdata)

fdata$Churn<-fdata$ChurnYes
fdata$Churn
fdata<-fdata[,-31]
dim(fdata)
head(fdata)

summary(fdata)

fdata<-na.omit(fdata)
dim(fdata)
head(fdata)

str(fdata)

set.seed(123)
data<-sample(2,nrow(fdata),replace=TRUE,prob=c(0.7,0.3))
train<-fdata[data==1, ]
test=fdata[data==2, ]

dim(train)
dim(test)
head(train)

test$Churn

rfm<-randomForest(Churn~SeniorCitizen.1+tenure+InternetServiceFiber.optic+
                    InternetServiceNo+StreamingTVYes+ContractOne.year+ContractTwo.year+
                    PaperlessBillingYes+PaymentMethodElectronic.check, data=train, type="classification" )
print(rfm)

attributes(rfm)

p<-predict(rfm,train)
confusionMatrix(p,train$Churn)

p<-predict(rfm,test)
confusionMatrix(p,test$Churn)

plot(rfm)

t<-tuneRF(train[,-31], train[,31], stepFactor=0.5,plot=TRUE,ntreeTry=100,trace=TRUE,mtry=4)

rf<-randomForest(Churn~SeniorCitizen.1+tenure+InternetServiceFiber.optic+
                   InternetServiceNo+StreamingTVYes+ContractOne.year+ContractTwo.year+
                   PaperlessBillingYes+PaymentMethodElectronic.check, data=train, type="classification",
                 ntreeTry=100,importance=TRUE,mtry=4,proximity=TRUE)
print(rf)

pt<-predict(rf,train)
confusionMatrix(pt,train$Churn)

pt<-predict(rf,test)
confusionMatrix(pt,test$Churn)

#cutoffs - sensitivity and specificity
prob1 = predict(rf, test , type = "prob")[,2]
prob1

actual <- as.numeric(ifelse(test$Churn== '1',1,0)) 
actual

pred.class1 = as.numeric(prob1 > .3)
pred.class1

tab1 = table(pred.class1, actual)
tab1

sen.spe = function(pred, obs) {
  tab1 = table(pred, obs)
  sensitivity = tab1[2,2]/sum(tab1[,2])
  specificity = tab1[1,1]/sum(tab1[,1])
  c(sensitivity,specificity)
}


sen.spe(pred.class1, actual)

# Calculating sensitivty and specificity at cutoff of 0.4
pred.class2 = as.numeric(prob1 > .4)
#pred.class2

tab2 = table(pred.class2, actual)
tab2

sen.spe(pred.class2, actual)

# Calculating sensitivty and specificity at cutoff of 0.5
pred.class3 = as.numeric(prob1 > .5)
#pred.class3

tab3 = table(pred.class3, actual )
tab3

sen.spe(pred.class3, actual)


# Calculating sensitivty and specificity at cutoff of 0.2
pred.class4 = as.numeric(prob1 > .2)
#pred.class4

tab4 = table(pred.class4, actual )
tab4

sen.spe(pred.class4, actual)

varImpPlot(rf)
