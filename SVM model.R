##Support vector machine (SVM)

setwd("D:/Research/HIV_BD Research")

# Load the packages
library(readxl)
library(caret)
library(kernlab)
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

# Hyperparameter grid for polynomial kernel
svm_grid <- expand.grid(
  degree = c(2, 3, 4),     # Polynomial degree (d)
  scale  = c(0.01, 0.1, 1),# Kernel scale (gamma-related)
  C      = c(0.1, 1, 10)   # Cost parameter
)

# Train SVM with polynomial kernel
svm_hiv <- train(
  y1 ~ .,
  data = training_data,
  method = "svmPoly",
  trControl = ctrl,
  tuneGrid = svm_grid,
  metric = "RMSE"
)

# View model results
print(svm_hiv)

# Plot cross-validation performance
plot(svm_hiv)

# Best tuning parameters
svm_hiv$bestTune


#prediction in training
pre<-predict(svm_hiv,training_data)
RMSE(training_data$y1,pre)
MAE(training_data$y1,pre)
MAPE(training_data$y1,pre)

#Model prediction
pred<-predict(svm_hiv,testing_data)
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

# Train SVM with polynomial kernel
svm_dth <- train(
  y2 ~ .,
  data = training_data,
  method = "svmPoly",
  trControl = ctrl,
  tuneGrid = svm_grid,
  metric = "RMSE"
)

# View model results
print(svm_dth)

# Plot cross-validation performance
plot(svm_dth)

# Best tuning parameters
svm_dth$bestTune

#prediction in training
pre<-predict(svm_dth,training_data)
RMSE(training_data$y2,pre)
MAE(training_data$y2,pre)
MAPE(training_data$y2,pre)

#Model prediction
pred<-predict(svm_dth,testing_data)
RMSE(testing_data$y2,pred)
MAE(testing_data$y2,pred)
MAPE(testing_data$y2,pred)
