# kaggle House Price的練習，一般線性迴歸

library('lmtest') # bptest 檢驗殘差齊一性
library('car') # dubinWatsonTest 檢驗殘差獨立性; vif 檢驗共線性

rm(list = ls())
load('HP_data.RData')

my_full <- full[c('Id', 'Neighborhood', 'FullBath', 'YearBuilt', 'ExterQual', 'KitchenQual', 'OverallQual', 'TotalBsmtSF', 'GrLivArea', 'TotRmsAbvGrd', 'GarageArea', 'SalePrice')]
my_full$OverallQual <- factor(my_full$OverallQual)
OLS_full <- lm(SalePrice ~ ., data = my_full[, c(-1, -2, -3)])
OLS_null <- lm(SalePrice ~ 1, data = my_full[, c(-1, -2, -3)])
OLS <- step(OLS_null, scope = list(lower = OLS_null, upper = OLS_full), direction = 'forward')
# step(OLSfull, scope = list(lower = OLS_null, upper = OLS_full), direction = 'backward')
summary(OLS)
OLS_error = resid(OLS)
qqnorm(OLS_error)
qqline(OLS_error)
shapiro.test(OLS_error)
bptest(OLS)
durbinWatsonTest(OLS)
vif(OLS)


OLS_predict <- predict(OLS)
head(OLS_predict, 10)
test_OLS <- predict(OLS, my_full[1461:2919,])
ans_OLS <- data.frame(Id = test$Id, SalePrice = test_OLS)
write.csv(ans_OLS, file = '0529_OLS.csv', row.names = F) # RMSE: 0.16954

