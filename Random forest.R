##Random forest

setwd("D:/Research/HIV_BD Research")

# Load the packages
library(readxl)
library(caret)
library(randomForest)
library(MLmetrics)

#Upload data
data<-read_excel("data_HIV_BD.xlsx", sheet="Normalized data")


#Data split

data1<-data[c(2,4:9,12:15,17:23)]#cases

set.seed(123)  # For reproducibility
trainIndex <- createDataPartition(data1$y1, p = 0.8, 
                                  list = FALSE,
                                  times = 1)
training_data <-data1[trainIndex, ]
testing_data <-data1[-trainIndex, ]

ctrl <- trainControl(method = "cv", number = 10)
grid <- expand.grid(mtry = seq(1, ncol(training_data) - 1, by = 1))  # Hyperparameter grid

# Perform cross-validation and parameter tuning
rf_hiv <- train(y1 ~ ., data = training_data, method = "rf",
                   trControl = ctrl, tuneGrid = grid)

p1<-plot(rf_hiv,xlab="mtry",ylab="RMSE(Cross-Validation)");p1
rf_hiv$bestTune$mtry

fit.rf1 <- randomForest(y1 ~ ., data = training_data,
                        mtry = rf_hiv$bestTune$mtry)
summary(fit.rf1)
varImp(fit.rf1)

#prediction in training
pre<-predict(fit.rf1,training_data)
RMSE(training_data$y1,pre)
MAE(training_data$y1,pre)
MAPE(training_data$y1,pre)

#Model prediction
pred<-predict(fit.rf1,testing_data)
RMSE(testing_data$y1,pred)
MAE(testing_data$y1,pred)
MAPE(testing_data$y1,pred)

#HIV deaths
data2<-data[c(3,4:9,12:15,17:23)]#deaths

set.seed(123)  # For reproducibility
trainIndex <- createDataPartition(data2$y2, p = 0.8, 
                                  list = FALSE,
                                  times = 1)
training_data <-data2[trainIndex, ]
testing_data <-data2[-trainIndex, ]

ctrl <- trainControl(method = "cv", number = 10)
grid <- expand.grid(mtry = seq(1, ncol(training_data) - 1, by = 1))  # Hyperparameter grid

# Perform cross-validation and parameter tuning
rf_dth <- train(y2 ~ ., data = training_data, method = "rf",
                trControl = ctrl, tuneGrid = grid)

p2<-plot(rf_dth,xlab="mtry",ylab="RMSE(Cross-Validation)");p2
rf_dth$bestTune$mtry

fit.rf2 <- randomForest(y2 ~ ., data = training_data,
                        mtry = rf_dth$bestTune$mtry)
summary(fit.rf2)
varImp(fit.rf2)

#prediction in training
pre<-predict(fit.rf2,training_data)
RMSE(training_data$y2,pre)
MAE(training_data$y2,pre)
MAPE(training_data$y2,pre)

#Model prediction
pred<-predict(fit.rf2,testing_data)
RMSE(testing_data$y2,pred)
MAE(testing_data$y2,pred)
MAPE(testing_data$y2,pred)
