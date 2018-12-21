library(tidyverse)
library(DataExplorer)
HousingI <- read.csv("train.csv", header = TRUE, sep = ",") #Import Data Set

##Visualize Missing Data##
plot_missing(Housing)
#Drop unusable Variables##
Housing_1 <- select(Housing, -PoolQC, -MiscFeature, -Fence, -Alley, -Fence)
##Fix remaining missing values
Housing_2 <- Housing_1 %>%
  mutate(LotFrontage = ifelse(is.na(LotFrontage), 0, LotFrontage),
         FireplaceQu = as.factor(ifelse(is.na(FireplaceQu), "None", FireplaceQu)),
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
plot_missing(Housing_2) #Check that data set is complete#

###Variable Creation###
Housing_Final <- Housing_2 %>%
  mutate(Housesqft = X1stFlrSF + X2ndFlrSF + TotalBsmtSF,
         Avg_Score = (OverallCond + OverallQual)/2,
         House_Size = as.factor(ifelse(Housesqft >= 1500, "Large", "Average")),
         NoBsmt = as.factor(ifelse(TotalBsmtSF == 0, 1, 0)),
         NoGarage = as.factor(ifelse(GarageType == "No Garage", 1, 0)),
         NoFrontLot = as.factor(ifelse(LotFrontage == 0, 1, 0)),
         HouseAge = YrSold - YearBuilt,
         YrSoldF = as.factor(YrSold),
         MoSoldF = as.factor(MoSold),
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


#####BUILD INITIAL MODEL######

##Determine Importance of Variables through Boruta##
Boruta_output <- Boruta::Boruta(formula = SalePrice ~ ., data = Housing_Final)
plot(Boruta_output, cex.axis=.7, las=2)

###Initial Model###
Linear_HandPicked <- lm(formula = SalePrice ~ Housesqft + Avg_Score + KitchenQual + CentralAir + GarageCars + LandContour + Fireplaces, Housing_Final)
summary(Linear_HandPicked)
par(mfrow = c(2,2))
plot(Linear_HandPicked)

#Marginal Model Plot#
car::mmp(Linear_HandPicked, main = "Marginal Model Plot - Initial Model")

#Box-Cox#
BC <- MASS::boxcox(Linear_HandPicked)
best.lam = BC$x[which(BC$y == max(BC$y))]

##Apply Log Transformation##
Linear_HandPicked_Log <- update(Linear_HandPicked, log(SalePrice) ~ .)
summary(Linear_HandPicked_Log)
plot(Linear_HandPicked_Log, main = "Diagnostics After Transformation")


######BUILDING FINAL MODEL######

####Boruta Re-run on Log version of Variable####
Boruta_output <- Boruta::Boruta(formula = log(SalePrice) ~ ., data = Housing_Final)
print(boruta_signif)
plot(Boruta_output, cex.axis=.7, las=2)

####Add Variables To original model####
Linear_Final1 <- lm(formula = log(SalePrice) ~ Housesqft + Avg_Score + HouseAge + LotArea + BsmtUnfSF + 
                     Fireplaces + AdjustedRemodel + TotRmsAbvGrd + LandContour + GarageCars + KitchenQual, Housing_Final)
summary(Linear_Final1)
plot(Linear_Final1)

##Creating Variabales that may better control outliers##
Housing_Linear1 <- Housing_Final %>%
  mutate(Supply = ifelse(MoSold == 1 | MoSold == 2 | MoSold == 12, 1, ifelse(MoSold == 3 | MoSold == 4 | MoSold == 11, 2, 
                                                                             ifelse(MoSold == 5 | MoSold == 9 | MoSold == 10, 3, 4))),
         Demand = ifelse(MoSold == 10 | MoSold == 11 | MoSold == 12, 1, ifelse(MoSold == 7 | MoSold == 8 | MoSold == 9, 2, 
                                                                               ifelse(MoSold == 1 | MoSold == 2 | MoSold == 6, 3, 4))),
         MrktBal = Demand / Supply,
         SupplyF = as.factor(Supply),
         DemandF = as.factor(Demand),
         MoSoldF = as.factor(MoSold),
         AddonsSize = ScreenPorch + X3SsnPorch + WoodDeckSF + OpenPorchSF + EnclosedPorch,
         Bath = FullBath + (.5 * HalfBath) + BsmtFullBath + (.5 * BsmtHalfBath))

##Final Model Second Attempt##
Linear_Final2 <- lm(formula = log(SalePrice) ~ Housesqft + Avg_Score + HouseAge + LotArea + BsmtUnfSF + 
                      Fireplaces + TotRmsAbvGrd + LandContour + GarageCars + KitchenQual + CentralAir + AddonsSize + HouseAge + MrktBal + MoSoldF, Housing_Linear1)
summary(Linear_Final2)
plot(Linear_Final2)

###Create variables for third attempt##
Housing_Linear2 <- Housing_Linear1 %>%
  mutate(Ideal = ifelse(Neighborhood == "Edwards" | Neighborhood == "IDOTRR", 1, 0),
         Special_Sale = as.factor(ifelse(SaleCondition == "Normal",0,1)),
         Deed = as.factor(ifelse(SaleType == "WD", 0, 1)))

##Final Model Third Attempt##
Linear_Final3 <- lm(formula = log(SalePrice) ~ Housesqft + Avg_Score + HouseAge + log(LotArea + 1) + BsmtUnfSF + 
                      Fireplaces + TotRmsAbvGrd + GarageCars + KitchenQual + CentralAir + Bath + AddonsSize + HouseAge + MSZoning + Special_Sale + Deed + Ideal, Housing_Linear2)

summary(Linear_Final3)
plot(Linear_Final3)

###Remove Outliers###
No_outlier <- Housing_Linear2[-524,]
No_outlier2 <- No_outlier[-1298,]
####COMPLETED MODEL####
Linear_Final4 <- lm(formula = log(SalePrice) ~ Housesqft + Avg_Score + HouseAge + log(LotArea + 1) + BsmtUnfSF + 
                      Fireplaces + TotRmsAbvGrd + GarageCars + KitchenQual + CentralAir + Bath + AddonsSize + HouseAge + MSZoning + Special_Sale + Deed, No_outlier2)
plot(Linear_Final4)
summary(Linear_Final4)
##Distribution of Logged Dependent##
x <- density(log(No_outlier2$SalePrice))
plot(x, main = "Distribution of Log(SalePrice)")

##Marginal Model Plots##
car::mmp(Linear_Combination2, main = "Marginal Model Plot - Final Model")
Diag <- gvlma::gvlma(Linear_Combination2)
gvlma::display.gvlmatests(Diag)
summary(Diag)
#######LOGIT MODEL######
library(InformationValue)
##Create Locational Variables##
Housing_Logit <- Housing_Linear2 %>%
  group_by(Neighborhood, BldgType) %>%
  mutate(Median_Price = median(SalePrice)) %>%
  ungroup() %>%
  mutate(Sale = as.factor(ifelse(SalePrice <= Median_Price, 0, 1)),
         Perk = as.factor(ifelse(Condition1 == "PosN"|Condition1 == "PosN", 1,0)),
         Community = as.factor(ifelse(LotConfig == "CulDSac" | LotConfig == "Corner" | LotConfig == "Inside", 1, 0)))

##Test original Model first##
Logit_1 <- glm(Sale ~ Housesqft + Avg_Score + HouseAge + log(LotArea + 1) + BsmtUnfSF + 
                 Fireplaces + TotRmsAbvGrd + GarageCars + KitchenQual + CentralAir + Bath + AddonsSize + HouseAge + MSZoning + Special_Sale + Deed, family = binomial(link = "logit"), Housing_Logit)
summary(Logit_1)
LogitPred1 <- predict(Logit_1, type = "response")
plotROC(Housing_Logit$Sale, LogitPred)
Logit_1_Thresh <- optimalCutoff(Housing_Logit$Sale, LogitPred)
misClassError(Housing_Logit$Sale, LogitPred, threshold = Logit_1_Thresh)


##Drop poor predictors/add Locational Variables##
Logit_3 <- glm(Sale ~ Housesqft + Avg_Score + HouseAge + 
                 Fireplaces + CentralAir + Bath + AddonsSize + HouseAge + Ideal + Special_Sale + LandSlope + Perk + Community, family = binomial(link = "logit"), Housing_Logit)
options(scipen = 999)
summary(Logit_3)

LogitPred3 <- predict(Logit_3, type = "response")
plotROC(Housing_Logit$Sale, LogitPred3)
Logit_3_Thresh <- optimalCutoff(Housing_Logit$Sale,LogitPred3)
confusionMatrix(Housing_Logit$Sale,LogitPred3, threshold = Logit_3_Thresh)
misClassError(Housing_Logit$Sale,LogitPred3, threshold = Logit_3_Thresh)
