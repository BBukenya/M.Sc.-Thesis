# Sanitary risk inspection
# working directory
setwd("D:/UoM (MPEC)/EART60372 Research Project/2_Data/GW_San_risk&Quality2014")
getwd()

# read in the data
install.packages("readr")
library(readr)
Sanrisk <- read_csv("SanRisk_WQty_Data.csv")

getwd()

# categorical measurement scales
install.packages("dplyr")
library(dplyr)

count(Sanrisk, Area)


# table function: two-way table
table(Sanrisk$SanRiskScore, Sanrisk$Area)
table_1 <- table(Sanrisk$SanRiskScore, Sanrisk$Area)

# proportions
prop.table(table_1)
round(prop.table(table_1), 2)

# row proportions
round(prop.table(table_1, 1), 2)
100 * round(prop.table(table_1, 1), 2)

# table function: three + way table
table(Sanrisk$SanRiskScore, Sanrisk$Area, Sanrisk$Log10_TTC)
table_2 <- table(Sanrisk$SanRiskScore, Sanrisk$Area, Sanrisk$Log10_TTC)
ftable(table_2)

# xtabs function: two-way
xtabs(~ Area + SanRiskScore, data=Sanrisk)
table_3 <- xtabs(~ Area + SanRiskScore, data=Sanrisk)
prop.table(table_3)
round(prop.table(table_3), 2)
100 * round(prop.table(table_3, 1), 2)

# xtabs function: two-way
xtabs(~ Area + SanRiskScore + Log10_TTC, data=Sanrisk)
table_4 <- xtabs(~ Area + SanRiskScore + Log10_TTC, data=Sanrisk)
ftable(table_4)

# data visualization
install.packages("lessR")
library(lessR)

BarChart(SanRiskScore, data = Sanrisk)
?BarChart

BarChart(RiskFactorScore, data = Sanrisk)

BarChart(SanRiskScore, data = Sanrisk, by1 = Area)

BarChart(SanRiskScore, data = Sanrisk, by = Area)

PieChart(Area, data = Sanrisk)
?PieChart


# continuous variables: interval or ratio measurement scales 
# Histograms
hist(Sanrisk$SanRiskScore)
hist(Sanrisk$Log_SanRiskScore)
hist(Sanrisk$Log10_TTC)
hist(Sanrisk$Log_Log10_TTC)
hist(Sanrisk$Nitrate)
hist(Sanrisk$Log_Nitrate)
hist(Sanrisk$Log_Log_Nitrate)
hist(Sanrisk$Chloride)
hist(Sanrisk$Log_Chloride)
hist(Sanrisk$Log_Log_Chloride)
hist(Sanrisk$Sulphate)
hist(Sanrisk$Log_Sulphate)
hist(Sanrisk$Log_Log_Sulphate)
hist(Sanrisk$Flouride)
hist(Sanrisk$Log_Flouride)
hist(Sanrisk$Log_Log_Flouride)
hist(Sanrisk$Phosphate)
hist(Sanrisk$Log_Phosphate)
hist(Sanrisk$Log_Log_Phosphate)

library(lessR)

Plot(SanRiskScore, data = Sanrisk)

Plot(SanRiskScore, data = Sanrisk, by1 = Area)

Plot(Log10_TTC, data = Sanrisk)

Plot(Log10_TTC, data = Sanrisk, by1 = Area)

install.packages("psych")
library(psych)

describe(Sanrisk)
var(Sanrisk$Chloride)

# Box plots
install.packages("tidyverse")
library(tidyverse)
qplot(data = Sanrisk, x = Area, y = SanRiskScore, geom = "boxplot")
qplot(data = Sanrisk, x = SanRiskScore, geom = "boxplot")
qplot(data = Sanrisk, x = Area, y = Nitrate, geom = "boxplot")
qplot(data = Sanrisk, x = Area, y = SanRiskScore, geom = "boxplot", col = I("darkgreen"), fill = I("lightgreen"))
qplot(data = Sanrisk, x = Area, y = Nitrate, geom = "boxplot", col = I("darkblue"), fill = I("lightblue"))
qplot(data = Sanrisk, x = Area, y = Chloride, geom = "boxplot", col = I("darkblue"), fill = I("lightblue"))
qplot(data = Sanrisk, x = Area, y = Sulphate, geom = "boxplot", col = I("darkblue"), fill = I("lightblue"))
qplot(data = Sanrisk, x = Area, y = Flouride, geom = "boxplot", col = I("darkblue"), fill = I("lightblue"))
qplot(data = Sanrisk, x = Area, y = Phosphate, geom = "boxplot", col = I("darkblue"), fill = I("lightblue"))
qplot(data = Sanrisk, x = Area, y = SanRiskScore, fill = Area, geom = "boxplot")

# with numerical values only
boxplot(Sanrisk)

# Correlation in R
 # base R
cor(Sanrisk$SanRiskScore, Sanrisk$Log10_TTC, method = "pearson")
cor.test(Sanrisk$SanRiskScore, Sanrisk$Log10_TTC, method = "pearson")

 # lessR
library(lessR)
Correlation(SanRiskScore, Log10_TTC, data = Sanrisk)

# Simple linear regression (SLR)
## concurrent validation design (criterion-related validity)
nrow(Sanrisk)
names(Sanrisk)

## SLR statistical assumptions
#### Bivariate normal distribution, linear association, no bivariate outliers
library(lessR)

ScatterPlot(SanRiskScore, Log10_TTC, data=Sanrisk, ellipse=TRUE)

ScatterPlot(SanRiskScore, Area, data=Sanrisk)

ScatterPlot(RiskFactorScore, RiskFactor, data=Sanrisk)

ScatterPlot(SanRiskScore, Nitrate, data=Sanrisk)

# Regression from lessR
Regression(SanRiskScore ~ Log10_TTC, data = Sanrisk)
Regression(SanRiskScore ~ Log_Log10_TTC, data = Sanrisk)
Regression(Log10_TTC ~ SanRiskScore, data = Sanrisk)

Regression(SanRiskScore ~ Nitrate, data = Sanrisk)
Regression(SanRiskScore ~ Log_Nitrate, data = Sanrisk)
Regression(SanRiskScore ~ Log_Log_Nitrate, data = Sanrisk)
Regression(Nitrate ~ SanRiskScore, data = Sanrisk)

Regression(SanRiskScore ~ Chloride, data = Sanrisk)
Regression(SanRiskScore ~ Log_Chloride, data = Sanrisk)
Regression(SanRiskScore ~ Log_Log_Chloride, data = Sanrisk)
Regression(Chloride ~ SanRiskScore, data = Sanrisk)

Regression(SanRiskScore ~ Sulphate, data = Sanrisk)
Regression(SanRiskScore ~ Log_Sulphate, data = Sanrisk)
Regression(SanRiskScore ~ Log_Log_Sulphate, data = Sanrisk)
Regression(Sulphate ~ SanRiskScore, data = Sanrisk)

Regression(SanRiskScore ~ Flouride, data = Sanrisk)
Regression(SanRiskScore ~ Log_Flouride, data = Sanrisk)
Regression(SanRiskScore ~ Log_Log_Flouride, data = Sanrisk)
Regression(Flouride ~ SanRiskScore, data = Sanrisk)

Regression(SanRiskScore ~ Phosphate, data = Sanrisk)
Regression(SanRiskScore ~ Log_Phosphate, data = Sanrisk)
Regression(SanRiskScore ~ Log_Log_Phosphate, data = Sanrisk)
Regression(Phosphate ~ SanRiskScore, data = Sanrisk)

# met assumption of normally distributed residuals (errors) based on the 
  # distribution of residual plot
# met assumption that residual (errors) is zero for each level of prediction based 
  # on the residuals vs fitted values plot
# met assumption of homoscedasticity of variances based on the 
  # residuals vs fitted values plot

# Correlation benchmarks (p values)
 # 0.1 small
 # 0.3 medium
 # 0.5 large

# R-squared benchmarks
 # 0.01 small
 # 0.09 medium
 # 0.25 large

# Prediction of sanitary risk scores from microbial contamination
SanRiskScore-predict = 76.29 + 0.950*Log10_TTC

  # standardized regression coefficients
reg_brief(SanRiskScore ~ Log10_TTC, data = Sanrisk, recode="z")

reg_brief(SanRiskScore ~ Nitrate, data = Sanrisk, recode="z")

reg_brief(SanRiskScore ~ Chloride, data = Sanrisk, recode="z")

reg_brief(SanRiskScore ~ Sulphate, data = Sanrisk, recode="z")

reg_brief(SanRiskScore ~ Flouride, data = Sanrisk, recode="z")

reg_brief(SanRiskScore ~ Phosphate, data = Sanrisk, recode="z")

library(lessR)


# Multiple linear regression

Regression(SanRiskScore ~ Nitrate + Chloride + Sulphate + Flouride + 
             Phosphate, data = Sanrisk)
  # 0.85+ (correlation; variables are indistinguishable statistically speaking)

  # No multi-collinearity assumption met based on tolerance statistics 
    # (values above 0.2)
  # met assumption of normally distributed residuals (errors) based on the 
    # distribution of residual plot
  # met the assumption of homoscedasticity of variances and the assumption of 
    # average residual variance being somewhat close to zero (residual plot)  

# Rule of thumb for R-squared
 # 0.01 small
 # 0.09 medium
 # 0.25 large

# Prediction of sanitary risk scores from chemical contamination
SanRiskScore-predict = 75.181 + 0.024*Nitrate + (-0.021)*Chloride + 0.231*Sulphate + (-4.867)*Flouride 
                       + 1.098*Phosphate

Regression(SanRiskScore ~ Log10_TTC + Nitrate + Chloride + Sulphate + Flouride + 
             Phosphate, data = Sanrisk)

# Prediction of sanitary risk scores from microbial and chemical contamination
SanRiskScore-predict = 72.790 + 0.586*Log10_TTC + (-0.079)*Nitrate + (-0.015)*Chloride + 0.246*Sulphate
                       + (-4.711)*Flouride + 1.001*Phosphate

 # standardized regression coefficients
reg_brief(SanRiskScore ~ Nitrate + Chloride + Sulphate + Flouride + 
             Phosphate, data = Sanrisk, recode="z")

# p values above 0.05
  # null hypothesis is false, probably due to a small sample size or there is too much variability for the
    # hypothesis test to detect it.

# Simple logistic regression
library(lessR)
Logit(NarrowCementFloor ~ TTC, data = Sanrisk)
Logit(NarrowCementFloor ~ Log10_TTC, data = Sanrisk)
Logit(NarrowCementFloor ~ Nitrate, data = Sanrisk)
Logit(NarrowCementFloor ~ Chloride, data = Sanrisk)
Logit(NarrowCementFloor ~ Sulphate, data = Sanrisk)
Logit(NarrowCementFloor ~ Phosphate, data = Sanrisk)
Logit(NarrowCementFloor ~ Flouride, data = Sanrisk)

Logit(NarrowCementFloor ~ TTC, data = Sanrisk)
Logit(NarrowCementFloor ~ Log10_TTC, data = Sanrisk)
Logit(WaterPondingFloor ~ TTC, data = Sanrisk)
Logit(WaterPondingFloor ~ Log10_TTC, data = Sanrisk)
Logit(CementCracks ~ Log10_TTC, data = Sanrisk)
Logit(CementCracks ~ TTC, data = Sanrisk)


Logit(NarrowCementFloor ~ TTC + Log10_TTC + Nitrate + Chloride + Sulphate + Flouride + Phosphate, data = Sanrisk)
Logit(NarrowCementFloor ~ TTC, data = Sanrisk)
Logit(NarrowCementFloor ~ Log10_TTC, data = Sanrisk)
Logit(NarrowCementFloor ~ Nitrate, data = Sanrisk)
Logit(NarrowCementFloor ~ Chloride, data = Sanrisk)
Logit(NarrowCementFloor ~ Sulphate, data = Sanrisk)
Logit(NarrowCementFloor ~ Flouride, data = Sanrisk)
Logit(NarrowCementFloor ~ Phosphate, data = Sanrisk)
Logit(WaterPondingFloor ~ TTC + Log10_TTC + Nitrate + Chloride + Sulphate + Flouride + Phosphate, data = Sanrisk)
Logit(WaterPondingFloor ~ TTC, data = Sanrisk)
Logit(WaterPondingFloor ~ Log10_TTC, data = Sanrisk)
Logit(WaterPondingFloor ~ Nitrate, data = Sanrisk)
Logit(WaterPondingFloor ~ Chloride, data = Sanrisk)
Logit(WaterPondingFloor ~ Sulphate, data = Sanrisk)
Logit(WaterPondingFloor ~ Flouride, data = Sanrisk)
Logit(WaterPondingFloor ~ Phosphate, data = Sanrisk)
Logit(CementCracks ~ TTC + Log10_TTC + Nitrate + Chloride + Sulphate + Flouride + Phosphate, data = Sanrisk)
Logit(CementCracks ~ TTC, data = Sanrisk)
Logit(CementCracks ~ Log10_TTC, data = Sanrisk)
Logit(CementCracks ~ Nitrate, data = Sanrisk)
Logit(CementCracks ~ Chloride, data = Sanrisk)
Logit(CementCracks ~ Sulphate, data = Sanrisk)
Logit(CementCracks ~ Flouride, data = Sanrisk)
Logit(CementCracks ~ Phosphate, data = Sanrisk)
Logit(WaterPondingOther ~ TTC + Log10_TTC + Nitrate + Chloride + Sulphate + Flouride + Phosphate, data = Sanrisk)
Logit(WaterPondingOther ~ TTC, data = Sanrisk)
Logit(WaterPondingOther ~ Log10_TTC, data = Sanrisk)
Logit(WaterPondingOther ~ Nitrate, data = Sanrisk)
Logit(WaterPondingOther ~ Chloride, data = Sanrisk)
Logit(WaterPondingOther ~ Sulphate, data = Sanrisk)
Logit(WaterPondingOther ~ Flouride, data = Sanrisk)
Logit(WaterPondingOther ~ Phosphate, data = Sanrisk)
Logit(DrainChannelProb ~ TTC + Log10_TTC + Nitrate + Chloride + Sulphate + Flouride + Phosphate, data = Sanrisk)
Logit(DrainChannelProb ~ TTC, data = Sanrisk)
Logit(DrainChannelProb ~ Log10_TTC, data = Sanrisk)
Logit(DrainChannelProb ~ Nitrate, data = Sanrisk)
Logit(DrainChannelProb ~ Chloride, data = Sanrisk)
Logit(DrainChannelProb ~ Sulphate, data = Sanrisk)
Logit(DrainChannelProb ~ Flouride, data = Sanrisk)
Logit(DrainChannelProb ~ Phosphate, data = Sanrisk)
Logit(AnimalAccess ~ TTC + Log10_TTC + Nitrate + Chloride + Sulphate + Flouride + Phosphate, data = Sanrisk)
Logit(AnimalAccess ~ TTC, data = Sanrisk)
Logit(AnimalAccess ~ Log10_TTC, data = Sanrisk)
Logit(AnimalAccess ~ Nitrate, data = Sanrisk)
Logit(AnimalAccess ~ Chloride, data = Sanrisk)
Logit(AnimalAccess ~ Sulphate, data = Sanrisk)
Logit(AnimalAccess ~ Flouride, data = Sanrisk)
Logit(AnimalAccess ~ Phosphate, data = Sanrisk)
Logit(Latrines10m ~ TTC + Log10_TTC + Nitrate + Chloride + Sulphate + Flouride + Phosphate, data = Sanrisk)
Logit(Latrines30m ~ TTC + Log10_TTC + Nitrate + Chloride + Sulphate + Flouride + Phosphate, data = Sanrisk)
Logit(WasteNearby ~ TTC + Log10_TTC + Nitrate + Chloride + Sulphate + Flouride + Phosphate, data = Sanrisk)
Logit(DumpsNearby ~ TTC + Log10_TTC + Nitrate + Chloride + Sulphate + Flouride + Phosphate, data = Sanrisk)
Logit(NarrowCementFloor ~ TTC + Log10_TTC + Nitrate + Chloride + Sulphate + Flouride + Phosphate, data = Sanrisk)
Logit(UnsanWellCover ~ TTC + Log10_TTC + Nitrate + Chloride + Sulphate + Flouride + Phosphate, data = Sanrisk)
Logit(WallsUnsealed ~ TTC + Log10_TTC + Nitrate + Chloride + Sulphate + Flouride + Phosphate, data = Sanrisk)




Logit(NarrowCementFloor ~ Log10_TTC + Nitrate + Chloride + Sulphate + Flouride + Phosphate, data = Sanrisk)
Logit(WaterPondingFloor ~ Log10_TTC + Nitrate + Chloride + Sulphate + Flouride + Phosphate, data = Sanrisk)
Logit(CementCracks ~ Log10_TTC + Nitrate + Chloride + Sulphate + Flouride + Phosphate, data = Sanrisk)
Logit(WaterPondingOther ~ Log10_TTC + Nitrate + Chloride + Sulphate + Flouride + Phosphate, data = Sanrisk)
Logit(DrainChannelProb ~ Log10_TTC + Nitrate + Chloride + Sulphate + Flouride + Phosphate, data = Sanrisk)
Logit(AnimalAccess ~ Log10_TTC + Nitrate + Chloride + Sulphate + Flouride + Phosphate, data = Sanrisk)
Logit(Latrines10m ~ Log10_TTC + Nitrate + Chloride + Sulphate + Flouride + Phosphate, data = Sanrisk)
Logit(Latrines30m ~ Log10_TTC + Nitrate + Chloride + Sulphate + Flouride + Phosphate, data = Sanrisk)
Logit(WasteNearby ~ Log10_TTC + Log10_TTC:log(Log10_TTC) + Nitrate + Chloride + Sulphate + Flouride + Phosphate, data = Sanrisk)
Logit(DumpsNearby ~ Log10_TTC + Nitrate + Chloride + Sulphate + Flouride + Phosphate, data = Sanrisk)
Logit(UnsanWellCover ~ Log10_TTC + Log10_TTC:log(Log10_TTC) + Nitrate + Nitrate:log(Nitrate) + Chloride + Chloride:log(Chloride) + Sulphate + Flouride + Phosphate, data = Sanrisk)
Logit(WallsUnsealed ~ Log10_TTC + Nitrate + Chloride + Sulphate + Flouride + Phosphate, data = Sanrisk)

Logit(NarrowCementFloor ~ TTC + Nitrate + Chloride + Sulphate + Flouride + Phosphate, data = Sanrisk)
Logit(WaterPondingFloor ~ TTC + Nitrate + Chloride + Sulphate + Flouride + Phosphate, data = Sanrisk)
Logit(CementCracks ~ TTC + Nitrate + Chloride + Sulphate + Flouride + Phosphate, data = Sanrisk)
Logit(WaterPondingOther ~ TTC + Nitrate + Chloride + Sulphate + Flouride + Phosphate, data = Sanrisk)
Logit(DrainChannelProb ~ TTC + Nitrate + Chloride + Sulphate + Flouride + Phosphate, data = Sanrisk)
Logit(AnimalAccess ~ TTC + Nitrate + Chloride + Sulphate + Flouride + Phosphate, data = Sanrisk)
Logit(Latrines10m ~ TTC + Nitrate + Chloride + Sulphate + Flouride + Phosphate, data = Sanrisk)
Logit(Latrines30m ~ TTC + Nitrate + Chloride + Sulphate + Flouride + Phosphate, data = Sanrisk)
Logit(WasteNearby ~ TTC + Nitrate + Chloride + Sulphate + Flouride + Phosphate, data = Sanrisk)
Logit(DumpsNearby ~ TTC + Nitrate + Chloride + Sulphate + Flouride + Phosphate, data = Sanrisk)
Logit(UnsanWellCover ~ TTC + Nitrate + Chloride + Sulphate + Flouride + Phosphate, data = Sanrisk)
Logit(WallsUnsealed ~ TTC + Nitrate + Chloride + Sulphate + Flouride + Phosphate, data = Sanrisk)



