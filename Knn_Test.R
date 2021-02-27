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

Customer_Churn <- Tdata

Customer_Churn$gender = as.numeric(factor(Customer_Churn$gender,
                                          levels = c('Female', 'Male'),
                                          labels = c(1, 2)))
Customer_Churn$SeniorCitizen = as.numeric(factor(Customer_Churn$SeniorCitizen,
                                                 levels = c('0', '1'),
                                                 labels = c(1, 2)))
Customer_Churn$Partner = as.numeric(factor(Customer_Churn$Partner,
                                           levels = c('No', 'Yes'),
                                           labels = c(1, 2)))
Customer_Churn$Dependents = as.numeric(factor(Customer_Churn$Dependents,
                                              levels = c('No', 'Yes'),
                                              labels = c(1, 2)))
Customer_Churn$PhoneService = as.numeric(factor(Customer_Churn$PhoneService,
                                                levels = c('No', 'Yes'),
                                                labels = c(1, 2)))
Customer_Churn$MultipleLines = as.numeric(factor(Customer_Churn$MultipleLines,
                                                 levels = c('No','Yes'),
                                                 labels = c(1, 2)))
Customer_Churn$InternetService = as.numeric(factor(Customer_Churn$InternetService,
                                                   levels = c('DSL', 'Fiber optic','No'),
                                                   labels = c(1, 2,3)))
Customer_Churn$OnlineSecurity = as.numeric(factor(Customer_Churn$OnlineSecurity,
                                                  levels = c('No','Yes'),
                                                  labels = c(1, 2)))
Customer_Churn$OnlineBackup = as.numeric(factor(Customer_Churn$OnlineBackup,
                                                levels = c('No','Yes'),
                                                labels = c(1, 2)))
Customer_Churn$DeviceProtection = as.numeric(factor(Customer_Churn$DeviceProtection,
                                                    levels = c('No','Yes'),
                                                    labels = c(1, 2)))
Customer_Churn$TechSupport = as.numeric(factor(Customer_Churn$TechSupport,
                                               levels = c('No','Yes'),
                                               labels = c(1, 2)))
Customer_Churn$StreamingTV = as.numeric(factor(Customer_Churn$StreamingTV,
                                               levels = c('No','Yes'),
                                               labels = c(1, 2)))
Customer_Churn$StreamingMovies = as.numeric(factor(Customer_Churn$StreamingMovies,
                                                   levels = c('No','Yes'),
                                                   labels = c(1, 2)))
Customer_Churn$Contract = as.numeric(factor(Customer_Churn$Contract,
                                            levels = c('Month-to-month', 'One year','Two year'),
                                            labels = c(1, 2,3)))
Customer_Churn$PaperlessBilling = as.numeric(factor(Customer_Churn$PaperlessBilling,
                                                    levels = c('No', 'Yes'),
                                                    labels = c(1, 2)))
Customer_Churn$PaymentMethod = as.numeric(factor(Customer_Churn$PaymentMethod,
                                                 levels = c('Electronic check', 'Mailed check','Credit card (automatic)','Bank transfer (automatic)'),
                                                 labels = c(1, 2,3,4)))
str(Customer_Churn)

library(dplyr)
#scaling
Customer_Churn <- Customer_Churn %>%mutate_if(is.numeric, scale)
head(Customer_Churn)


set.seed(123)
split <- sample(2,nrow(Customer_Churn), replace = T,prob = c(0.70,0.30))
trainDF<- Customer_Churn[split ==1,]
testDF <- Customer_Churn[split ==2,]

dim(trainDF)
dim(testDF)


