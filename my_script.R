# kaggle House Price的練習，資料處理檔

library('dplyr')
library('ggplot2')
library('ggthemes')

rm(list = ls())

# 資料讀取
train <- read.csv("/Users/80005099/Desktop/kaggle/House Prices/train.csv", stringsAsFactors = F)
test <- read.csv("/Users/80005099/Desktop/kaggle/House Prices/test.csv", stringsAsFactors = F)
test$SalePrice <- NA
full  <- rbind(train, test)
str(full)
# 檢查是否有重複資料
cat("The number of duplicated rows are", nrow(full) - nrow(unique(full)))

# 資料遺漏一覽(兩個方法都可以)
apply(full, 2, function(x) sum(is.na(x)))
colSums(sapply(my_full, is.na)) #另一筆資料

#遺漏值處理
# Utilities 將遺漏值加入眾數 
table(full$Utilities) 
.miss <- full[is.na(full$Utilities),'Id']
full$Utilities[c(.miss)] <- 'AllPub'
# 廚房 將遺漏值加入眾數 
table(full$KitchenQual)
.miss <- full[is.na(full$KitchenQual),'Id']
full$KitchenQual[c(.miss)] <- 'Fa'
# 車庫大小
# 先瀏覽所有與車庫有關的因子
full[is.na(full$GarageArea),c('GarageType', 'GarageYrBlt','GarageFinish','GarageCars', 'GarageArea','GarageQual','GarageCond')]
# 遺漏值只有一筆，且該遺漏值只有GarageType有資料
ggplot(full) + geom_boxplot(aes(GarageType, GarageArea))
# 利用ggplost2劃出Detchd的車庫類型，其中位數為多少
.miss <- full[is.na(full$GarageArea),'Id']
# 使用Detchd的中位數去填補遺漏值
full$GarageArea[c(.miss)] <- median(full[full$GarageType=="Detchd", "GarageArea"], na.rm = T)
# 地下室大小
# 先瀏覽所有與地下室有關的因子
full[is.na(full$TotalBsmtSF),c('BsmtQual', 'BsmtCond','BsmtExposure','BsmtFinType1', 'BsmtFinSF1','BsmtFinType2','BsmtFinSF2','BsmtUnfSF')]
# 發現全部都是遺漏值，因此用中位數代替
.miss <- full[is.na(full$TotalBsmtSF),'Id']
full$TotalBsmtSF[c(.miss)] <- median(full[,"TotalBsmtSF"], na.rm = T)
# 
table(full$MSZoning)
.miss <- full[is.na(full$MSZoning),'Id']
.miss
full$MSZoning[c(.miss)] <- 'AllPub'
# 不在巷弄上
.miss <- full[is.na(full$Alley),'Id']
full$Alley[c(.miss)] <- 'NA'
# 沒有壁爐
.miss <- full[is.na(full$FireplaceQu),'Id']
full$FireplaceQu[c(.miss)] <- 'NA'
# 沒有游泳池
.miss <- full[is.na(full$PoolQC),'Id']
full$PoolQC[c(.miss)] <- 'NA'
# 沒有籬笆
.miss <- full[is.na(full$Fence),'Id']
full$Fence[c(.miss)] <- 'NA'
# 沒有其他設施(第二車庫、棚子、網球場、其他)
.miss <- full[is.na(full$MiscFeature),'Id']
full$MiscFeature[c(.miss)] <- 'NA'

# 設定資料型態
.aa <- data.frame(apply(full, 2, function(x) length(unique(x))) < 9 | (sapply(full, class)=='character' & apply(full, 2, function(x) length(unique(x))) <30))
names(.aa) <- 'a'
.aa$name <- row.names(.aa)
factor_vars <- .aa[.aa$a==TRUE, 'name']
full[factor_vars] <- lapply(full[factor_vars], function(x) as.factor(x))

# Pearson correlations檢定
corr <- data.frame()
numeric_var <- names(train)[which(sapply(train, is.numeric))]
for (i in numeric_var){
  .tmp <- cor.test(train[[i]], train[[dim(train)[2]]])
  .tmp_df <- data.frame(.tmp[[3]], .tmp[[4]])
  row.names(.tmp_df) <- i
  corr <- rbind(corr, .tmp_df)
}
names(corr) <- c("p-value", "cor")

# onewayANOVA檢定
ANOVA <- data.frame()
for (i in factor_vars) {
  .tmp <- anova(lm(full$SalePrice ~ full[[i]]))
  row.names(.tmp)[1] <- i
  ANOVA <- rbind(ANOVA, .tmp)
}
ANOVA <- ANOVA[-1:-3]
ANOVA <- na.omit(ANOVA)
write.csv(ANOVA, "anova.csv", na = "")

save(full, test, train, file = 'HP_data.RData')


test <- my_full[1461:2919, -13]
.miss <- test[test$FullBath == 4, 'Id']
test[test$Id == c(.miss), 'FullBath'] <- 3

solution <- data.frame(Id = test$Id, SalePrice = p_test)
write.csv(solution, file = 'rf_mod_Solution.csv', row.names = F)
