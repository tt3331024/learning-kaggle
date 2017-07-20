# kaggle House Price的練習，廣義線性模型

library('AER') # dispersiontest
library('MASS') # glm.nb glm負二項分配的函式


my_full <- full[c('Id', 'Neighborhood', 'FullBath', 'YearBuilt', 'ExterQual', 'KitchenQual', 'OverallQual', 'TotalBsmtSF', 'GrLivArea', 'TotRmsAbvGrd', 'GarageArea', 'SalePrice')]
my_full$OverallQual <- factor(my_full$OverallQual)
poisson_full <- glm(SalePrice ~ ., data = my_full[, c(-1, -2, -3)], family = 'poisson')
poisson_null <- glm(SalePrice ~ 1, data = my_full[, c(-1, -2, -3)], family = 'poisson')
poisson <- step(poisson_null, scope = list(lower = poisson_null, upper = poisson_full), direction = 'forward')
summary(poisson)
dispersiontest(poisson, trafo = 1)

NB_full <- glm.nb(SalePrice ~ ., data = my_full[, c(-1, -2)])
NB_null <- glm.nb(SalePrice ~ 1, data = my_full[, c(-1, -2)])
NegativeBinomial <- step(NB_null, scope = list(lower = NB_null, upper = NB_full), direction = 'forward')
summary(NegativeBinomial)

vif(NegativeBinomial)

NB_predict <- predict(NegativeBinomial, type = 'response')
head(NB_predict, 10)
test_NB <- predict(NegativeBinomial, type = 'response', my_full[1461:2919,])
aws_NB <- data.frame(Id = test$Id, SalePrice = test_NB)
write.csv(aws_NB, file = '0528_NB.csv', row.names = F) # RMSE: 0.15562
