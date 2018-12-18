library(tidyverse)
library(DataExplorer)
HousingI <- read.csv("train.csv", header = TRUE, sep = ",")
plot_missing(Housing)
Housing_1 <- select(Housing, -PoolQC, -MiscFeature, -Fence, -Alley, -Fence, -FireplaceQu)
Housing_2 <- Housing_1 %>%
  mutate(LotFrontage = ifelse(is.na(LotFrontage), 0, LotFrontage),
         GarageType = as.factor(ifelse(is.na(GarageType), "No Garage", GarageType)),
         GarageFinish = as.factor(ifelse(is.na(GarageFinish), "None", GarageFinish)),
         GarageCars = ifelse(is.na(GarageCars), 0, GarageCars),
         GarageArea = ifelse(is.na(GarageArea), 0, GarageArea),
         GarageQual = as.factor(ifelse(is.na(GarageQual), "None", GarageQual)),
         GarageCond = as.factor(ifelse(is.na(GarageCond),"None", GarageCond)),
         GarageYrBlt = ifelse(is.na(GarageYrBlt), 0, GarageYrBlt),
         Electrical = as.factor(ifelse(is.na(Electrical), mode(Electrical), Electrical)),
         MasVnrArea = ifelse(is.na(MasVnrArea), 0, 1),
         MasVnrType = as.factor(ifelse(is.na(MasVnrType), "No Masonry", MasVnrType)),
         BsmtFinType1 = as.factor(ifelse(is.na(BsmtFinType1), "None", BsmtFinType1)),
         BsmtFinType2 = as.factor(ifelse(is.na(BsmtFinType2), "None", BsmtFinType2)),
         BsmtCond = as.factor(ifelse(is.na(BsmtCond), "None", BsmtCond)),
         BsmtQual = as.factor(ifelse(is.na(BsmtQual), "None", BsmtQual)),
         BsmtExposure = as.factor(ifelse(is.na(BsmtExposure), "None", BsmtExposure)))
plot_missing(Housing_2)

Housing_Final <- Housing_2 %>%
  mutate(Housesqft = X1stFlrSF + X2ndFlrSF + TotalBsmtSF,
         Avg_Score = (OverallCond + OverallQual)/2,
         House_Size = as.factor(ifelse(Housesqft >= 1500, "Large", "Average")),
         NoBsmt = as.factor(ifelse(TotalBsmtSF == 0, 1, 0)),
         NoGarage = as.factor(ifelse(GarageType == "No Garage", 1, 0)),
         NoFrontLot = as.factor(ifelse(LotFrontage == 0, 1, 0)),
         YrSold = as.factor(YrSold),
         HouseAge = 2010 - YearBuilt,
         AdjustedRemodel = YearRemodAdd - YearBuilt,
        Not_Remodeled = as.factor(ifelse(AdjustedRemodel == 0, 1, 0)))
Housing_Final <- Housing_Final %>%
  group_by(LotConfig) %>%
  mutate(AvgLotPrice = mean(SalePrice)) %>%
  ungroup()
Housing_Final <- Housing_Final %>%
  group_by(Neighborhood, YrSold, MoSold) %>%
  mutate(Avg_Location_Price = mean(SalePrice)) %>%
  ungroup()

##Linear Models##

Linear_HandPicked <- lm(formula = SalePrice ~ Housesqft + Avg_Score + KitchenQual + CentralAir + GarageCars + LandContour + Fireplaces, Housing_Final)
summary(Linear_HandPicked)
par(mfrow = c(2,2))
plot(Linear_HandPicked)

BC <- MASS::boxcox(Linear_HandPicked)
best.lam = BC$x[which(BC$y == max(BC$y))]

Linear_HandPicked_Log <- update(Linear_HandPicked, log(SalePrice) ~ .)
summary(Linear_HandPicked_Log)
plot(Linear_HandPicked_Log)
car::mmps(Linear_HandPicked_Log)

##Boruta##

Boruta_output <- Boruta::Boruta(formula = log(SalePrice) ~ ., data = Housing_Final)
print(boruta_signif)
plot(Boruta_output, cex.axis=.7, las=2)

LinearBoruta <- lm(formula = log(SalePrice) ~ Housesqft + Avg_Score + HouseAge + LotArea + BsmtUnfSF + 
                     Fireplaces + AdjustedRemodel + TotRmsAbvGrd + LandContour + GarageCars + KitchenQual, Housing_Final)
summary(LinearBoruta)
par(mfrow = c(2,2))
plot(LinearBoruta)



##Poisson Model## 



