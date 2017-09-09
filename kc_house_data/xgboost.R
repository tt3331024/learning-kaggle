# kaggle House Sales in King County, USA的練習，xgboost

library(data.table)
library(corrplot)
library(dplyr)
library(ggplot2)
library(xgboost)

rm(list = ls())

# input data
setwd('D:/GitHub/learning-kaggle/kc_house_data//')
KC <- fread("kc_house_data.csv")

str(KC)
KC <- KC[,-c(1, 2, 17)]
KC$floors <- as.numeric(KC$floors)
colSums(sapply(KC, is.na))
summary(KC)

set.seed(770312)
sample_ID <- sample(nrow(KC), 0.5*nrow(KC))
training <- KC[sample_ID,]
testing <- KC[-sample_ID,]

## 資料清理
## 將price / sqft_living，算出每平方英尺的價格(PPF)
## 將PPF取log，讓分佈接近常態
cor(training) %>% corrplot(method="square")
ggplot(training) + geom_histogram(aes(price), binwidth = 100000)
training$PPF <- training$price / training$sqft_living
cor(training) %>% corrplot(method="square")
ggplot(training) + geom_histogram(aes(PPF), binwidth = 10)
ggplot(training) + geom_histogram(aes(log(PPF)), binwidth = 0.05)
training$log_PPF <- log(training$PPF)

## 創建xgboost可用的資料格式
label <- training$log_PPF
xgb_train <- xgb.DMatrix(as.matrix(training[,-c(1, 19, 20)]), label = label)
xgb_test <- xgb.DMatrix(as.matrix(testing[,-1]))

## 訓練xgb的樹模型
xtree <- xgboost(data = xgb_train, nrounds = 500, eta = 0.1, max_depth = 4)
pred_tree <- predict(xtree, xgb_test)
RMSE_tree <- sqrt(mean((testing$price - exp(pred_tree) * testing$sqft_living)^2))

## 訓練xgb的線性模型
xlinear <- xgboost(data = xgb_train, nrounds = 500, booster = "gblinear", lambda = 0.5, alpha = 0.5, lambda_bias = 0.5, print_every_n = 50)
pred_linear <- predict(xlinear, xgb_test)
RMSE_linear <- sqrt(mean((testing$price - exp(pred_linear) * testing$sqft_living)^2))

## 訓練xgb的dart模型
xdart <- xgboost(data = xgb_train, nrounds = 200, booster = "dart", eta = 0.2, max_depth = 4, print_every_n = 50)
pred_dart <- predict(xdart, xgb_test)
RMSE_dart <- sqrt(mean((testing$price - exp(pred_dart) * testing$sqft_living)^2))

# 比較三個模型對testing資料的預測效果
paste0('RMSE of tree model = ', RMSE_tree) # 112073.522626941
paste0('RMSE of linear model = ', RMSE_linear) # 227536.099261486
paste0('RMSE of dart model = ', RMSE_dart) # 111654.492557976
