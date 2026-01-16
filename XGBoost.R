##XGBoost model
##Our working directory 
setwd("D:/Research/HIV_BD Research")

##Required libraries
library(readxl)
library(dplyr)
library(xgboost)
library(SHAPforxgboost)
library(caret)
library(ggplot2)
library(MLmetrics)

#Upload data
data<-read_excel("data_HIV_BD.xlsx", sheet="Normalized data")

#selected variables for model
data1<-data[c(2,4:9,12:15,17:23)]#for cases

#data partition
set.seed(123)  # For reproducibility
trainIndex <- createDataPartition(data1$y1, p = 0.8, 
                                  list = FALSE,
                                  times = 1)
training_data <-data1[trainIndex, ]
testing_data <-data1[-trainIndex, ]

##XGBoost model for waterborne
dtrain<-xgb.DMatrix(data=as.matrix(training_data),label=training_data$y1)
dtest<-xgb.DMatrix(data=as.matrix(testing_data),label=testing_data$y1)

# XGBoost parameters: This hyper-parameters set is tuned one
params.xgb <- list(booster = "gbtree", 
                   objective = "reg:squarederror", 
                   eta =0.01, #Reduced learning rate 
                   gamma=5, #Increased to prevent overfitting
                   min_child_weight = 5, # Increased to control complexity
                   lambda=1, #Increased for stronger L2 regularization
                   alpha=0.5 #Added L1 regularizatio
)
# xgb corss-validation train
xgbcv <- xgb.cv(params =  params.xgb, 
                data = dtrain, 
                nrounds = 1000, 
                nfold = 10, 
                showsd = F,
                prediction = T,
                stratified = T, 
                print_every_n = 1, 
                early_stopping_rounds = 30, 
                maximize = F
)
#' Final training and metrics evaluation on test sets
set.seed(1)
watchlist = list( test = dtest,
                  train = dtrain) 
model.xgb <- xgb.train(params = params.xgb, 
                       data = dtrain, 
                       nrounds =  1000,   
                       watchlist = watchlist, 
                       print_every_n = 1, 
                       early_stopping_rounds =30, 
                       maximize = F
)

#prediction in training
pre<-predict(model.xgb,dtrain)
RMSE(training_data$y1,pre)
MAE(training_data$y1,pre)
MAPE(training_data$y1,pre)

#Model prediction
pred<-predict(model.xgb,dtest)
RMSE(testing_data$y1,pred)
MAE(testing_data$y1,pred)
MAPE(testing_data$y1,pred)

#Feature importance using mean |SHAP| values for XGBoost model as best
shap_values <- shap.values(xgb_model = model.xgb, X_train = dtrain)
shap_values$mean_shap_score


# Learning curve
ggplot(data = xgbcv$evaluation_log, aes(x = iter, y = train_rmse_mean, color = "Train")) +
  geom_line() +
  geom_line(data = xgbcv$evaluation_log, aes(x = iter, y = test_rmse_mean, color = "Test")) +
  labs(title = "Learning Curve", x = "Boosting Rounds", y = "RMSE") +
  scale_color_manual(values = c("Train" = "blue", "Test" = "red"))+theme_bw()


#selected variables for model
data2<-data[c(3,4:9,12:15,17:23)]#for deaths

#data partition
set.seed(123)  # For reproducibility
trainIndex <- createDataPartition(data2$y2, p = 0.8, 
                                  list = FALSE,
                                  times = 1)
training_data <-data2[trainIndex, ]
testing_data <-data2[-trainIndex, ]

##XGBoost model for waterborne
dtrain<-xgb.DMatrix(data=as.matrix(training_data),label=training_data$y2)
dtest<-xgb.DMatrix(data=as.matrix(testing_data),label=testing_data$y2)

# XGBoost parameters: This hyper-parameters set is tuned one
params.xgb <- list(booster = "gbtree", 
                   objective = "reg:squarederror", 
                   eta =0.01, #Reduced learning rate 
                   gamma=5, #Increased to prevent overfitting
                   min_child_weight = 5, # Increased to control complexity
                   lambda=1, #Increased for stronger L2 regularization
                   alpha=0.5 #Added L1 regularizatio
)
# xgb corss-validation train
xgbcv <- xgb.cv(params =  params.xgb, 
                data = dtrain, 
                nrounds = 1000, 
                nfold = 10, 
                showsd = F,
                prediction = T,
                stratified = T, 
                print_every_n = 1, 
                early_stopping_rounds = 30, 
                maximize = F
)
#' Final training and metrics evaluation on test sets
set.seed(1)
watchlist = list( test = dtest,
                  train = dtrain) 
model.xgb <- xgb.train(params = params.xgb, 
                       data = dtrain, 
                       nrounds =  1000,   
                       watchlist = watchlist, 
                       print_every_n = 1, 
                       early_stopping_rounds =30, 
                       maximize = F
)

#prediction in training
pre<-predict(model.xgb,dtrain)
RMSE(training_data$y2,pre)
MAE(training_data$y2,pre)
MAPE(training_data$y2,pre)

#Model prediction
pred<-predict(model.xgb,dtest)
RMSE(testing_data$y2,pred)
MAE(testing_data$y2,pred)
MAPE(testing_data$y2,pred)

#Feature importance using mean |SHAP| values for XGBoost model as best
shap_values <- shap.values(xgb_model = model.xgb, X_train = dtrain)
shap_values$mean_shap_score


# Learning curve
ggplot(data = xgbcv$evaluation_log, aes(x = iter, y = train_rmse_mean, color = "Train")) +
  geom_line() +
  geom_line(data = xgbcv$evaluation_log, aes(x = iter, y = test_rmse_mean, color = "Test")) +
  labs(title = "Learning Curve", x = "Boosting Rounds", y = "RMSE") +
  scale_color_manual(values = c("Train" = "blue", "Test" = "red"))+theme_bw()
