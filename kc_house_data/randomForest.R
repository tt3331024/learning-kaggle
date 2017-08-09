# kaggle House Sales in King County, USA的練習，randomForest

library(data.table)
library(corrplot)
library(dplyr)
library(ggplot2)
library(randomForest)
library(useful)

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

X <- build.x(log_PPF ~ . - price - PPF, data = training)
Y <- build.y(log_PPF ~ . - price - PPF, data = training)
rf <- randomForest(x = X[,-1], y = Y, mtry = 6, ntree = 800, importance = T)
rf
pred_rf <- predict(rf, testing)
RMSE_rf <- sqrt(mean((testing$price - exp(pred_rf) * testing$sqft_living)^2))


paste0('RMSE of randomForest model = ', RMSE_rf) # 122361.556561902
