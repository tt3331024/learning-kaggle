# kaggle House Price的練習，廣義加法模型

library('gam')

my_full <- full[c('Id', 'Neighborhood', 'FullBath', 'YearBuilt', 'ExterQual', 'KitchenQual', 'OverallQual', 'TotalBsmtSF', 'GrLivArea', 'TotRmsAbvGrd', 'GarageArea', 'SalePrice')]
my_full$OverallQual <- factor(my_full$OverallQual)
GAM <- gam(SalePrice ~ s(YearBuilt) + ExterQual + KitchenQual + OverallQual + s(TotalBsmtSF) + s(GrLivArea) + s(GarageArea), data = my_full[, c(-1, -2, -3)])
summary(GAM)
test_GAM <- predict(GAM, type = 'response', test)
aws_GAM <- data.frame(Id = test$Id, SalePrice = test_GAM)
write.csv(aws_GAM, file = '0529_GAM.csv', row.names = F) # RMSE: 0.16850
