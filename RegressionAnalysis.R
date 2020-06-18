library(openxlsx)
setwd("G:/University of Denver Life!/C U Denver sem 2/BANA 6610 R programming/HW5/Model analysis")
getwd()
ski = read.xlsx("ski.xlsx", sheet = "Average values of outliers")




install.packages("boot")
library(readxl)
ski= read_excel("C:/Users/vedan/OneDrive/Desktop/Statistics/Homework/Assignment 5/ski.xlsx")
library(tidyverse)
library(funModeling)
library(Hmisc)
glimpse(ski)
df_status(ski)
freq(ski)
profiling_num(ski)
plot_num(ski)
describe(ski)

#.............................................Boxplot.................................................................
par(mfrow=c(2,4))
boxplot(ski$`rty #`, main = "rty# Boxplot")
boxplot(ski$Bedrooms, main = "Bedrooms Boxplot")
boxplot(ski$Bathrooms, main = "Bathroom Boxplot")
boxplot(ski$Sq_Ft, main = "Sq_Ft Boxplot" )
boxplot(ski$Downtwon, main = "Downtown Boxplot")
boxplot(ski$Mountain, main = "Mountain Boxplot")
boxplot(ski$`Lot size`, main = "LotSize Boxplot" )
boxplot(ski$Garage, main = "Garage Boxplot")

par(mfrow=c(2,2))
boxplot(ski$Age, main = "Age Boxplot")
boxplot(ski$`On market`, main = "On Market Boxplot" )
boxplot(ski$`Selling price`, main = "Selling Price Boxplot")
boxplot(ski$`List price`, main = "List Price Boxplot" )

#.............................................Histogram.................................................................

par(mfrow=c(2,4))
hist(ski$`rty #`, main = "rty# Histogram")
hist(ski$Bedrooms, main = "Bedrooms Histogram")
hist(ski$Bathrooms, main = "Bathroom Histogram")
hist(ski$Sq_Ft, main = "Sq_Ft Histogram" )
hist(ski$Downtwon, main = "Downtown Histogram")
hist(ski$Mountain, main = "Mountain Histogram")
hist(ski$`Lot size`, main = "LotSize Histogram" )
hist(ski$Garage, main = "Garage Histogram")

par(mfrow=c(2,2))
hist(ski$Age, main = "Age Histogram")
hist(ski$`On market`, main = "On Market Histogram" )
hist(ski$`Selling price`, main = "Selling Price Histogram")
hist(ski$`List price`, main = "List Price Histogram" )

pairs(ski)

#.....................................Bivariant Graphs........................................................
par(mfrow=c(2,4))
plot(ski$Bedrooms, ski$`Selling price`, main = "Bedroom vs selling price")
plot(ski$Bathrooms, ski$`Selling price`, main = "Bathroom vs selling price")
plot(ski$Sq_Ft, ski$`Selling price`, main = "Sq_Ft vs selling price")
plot(ski$Downtwon, ski$`Selling price`, main = "Downtown vs selling price")
plot(ski$Mountain, ski$`Selling price`, main = "Mountain vs selling price")
plot(ski$`Lot size`, ski$`Selling price`, main = "Lot size vs selling price")
plot(ski$Garage, ski$`Selling price`, main = "Garage vs selling price")

par(mfrow=c(1,2))
plot(ski$Age, ski$`Selling price`, main = "Age vs selling price")
plot(ski$`On market`, ski$`Selling price`, main = "On market vs selling price")
#.............................................Multivariate................................................................


#Relationship between Selling Price and number of Bedrooms with Sq_Ft
ggplot(ski, aes(x=ski$Bedrooms, y=ski$`Selling price`)) + 
  geom_bar(aes(fill = ski$Sq_Ft), stat="identity",position=position_dodge()) +
  labs(title = "Relationship between Selling Price and number of Bedrooms with Sq_Ft", 
       x= "Number of Bedrooms",
       y = "Selling Price", 
       colour="Sq_Ft")+
  theme(axis.text.x = element_text(angle = 90))



#Relationship between Mountain and Downtown distance with Selling Price in Color
ggplot(ski, aes(x=ski$Downtwon, y=ski$Mountain)) + 
  geom_bar(aes(fill = ski$`Selling price`), stat="identity",position=position_dodge()) +
  labs(title = "Relationship between Mountain and Downtown distance with Selling Price in Color", 
       x= "Downtown",
       y = "Mountain", 
       colour="Selling price") +
  theme(axis.text.x = element_text(angle = 90))


qplot(ski$Sq_Ft,ski$`Lot size`, color = ski$`Selling price`,geom = c("point","smooth"))

qplot(ski$Bedrooms,ski$Bathrooms, color = ski$`Selling price`,geom = c("point","smooth")) 


#Relationship between Garage and Downtown distance with Selling Price in Color
qplot(ski$Garage,ski$Downtwon,color = ski$`Selling price`,geom = c("point","smooth")) 

ggplot(ski, aes(x=ski$Garage, y=ski$Downtwon)) + 
  geom_bar(aes(fill = ski$`Selling price`), stat="identity",position=position_dodge()) +
  labs(title = "Relationship between Garage and Downtown distance with Selling Price in Color", 
       x= "Garage",
       y = "Downtown", 
       colour="Selling price") +
  theme(axis.text.x = element_text(angle = 90))



#Relationship between Garage and Mountaun distance with Selling Price in Color
qplot(ski$Garage,ski$Mountain,color = ski$`Selling price`,geom = c("point","smooth"))

ggplot(ski, aes(x=ski$Garage, y=ski$Mountain)) + 
  geom_bar(aes(fill = ski$`Selling price`), stat="identity",position=position_dodge()) +
  labs(title = "Relationship between Garage and Mountaun distance with Selling Price in Color", 
       x= "Garage",
       y = "Mountain", 
       colour="Selling price") +
  theme(axis.text.x = element_text(angle = 90))



#................................................Model.finding.through.subset.method...................................................................


install.packages("caret")
install.packages("leaps")

library(tidyverse)
library(caret)
library(leaps)

modelSub = lm(ski$`Selling price` ~ ski$`List price` + ski$Bedrooms 
              + ski$Bathrooms + ski$Sq_Ft + ski$Downtwon 
              + ski$Mountain + ski$`Lot size` 
              + ski$Garage + ski$Age + ski$`On market`, data = ski)
summary(modelSub)
anova(modelSub)

#Using regsubsets to find out the top 10 best subset model to predice selling price - Considering List Price
Submodels = regsubsets(ski$`Selling price` ~ ski$`List price`
                       + ski$Bedrooms + ski$Bathrooms + ski$Sq_Ft
                       + ski$Downtwon + ski$Mountain + ski$`Lot size`
                       + ski$Garage + ski$Age + ski$`On market`, data = ski, nvmax = 10)
summary(Submodels)

modelSubA = lm(ski$`Selling price` ~ ski$`List price`, data = ski)
summary(modelSubA)
anova(modelSubA)


modelSubB = lm(ski$`Selling price` ~ ski$`List price` + ski$Downtwon, data = ski)
summary(modelSubB)
anova(modelSubB)


modelSubC = lm(ski$`Selling price` ~ ski$`List price` + ski$Downtwon + ski$`On market`, data = ski)
summary(modelSubC)
anova(modelSubC)

#considering parameters and finding best possible subset
Var_sum = summary(Submodels)
data.frame(
  Adj_R_Sq = which.max(Var_sum$adjr2),
  CP_Value = which.min(Var_sum$cp),
  BIC_Value = which.min(Var_sum$bic)
)

#no substantial solution to model found, each of these criteria will lead to slightly different models.
#adjusted R2 tells us that the best model has 5 predictor variable.But as per, BIC and Cp criteria, we should go for the model with 2 variables.

modelSub5Var = lm(ski$`Selling price` ~ ski$`List price` + ski$Sq_Ft 
                  + ski$Downtwon + ski$`Lot size`
                  + ski$`On market`, data = ski)
summary(modelSub5Var)
anova(modelSub5Var)
library(car)
vif(modelSub5Var)
qqnorm(modelSub5Var$residuals)
AIC(modelSub5Var)
BIC(modelSub5Var)



#Ignoring great r_sq  value as of now and considering 2 variables, we get less significant variable

modelSub2Var = lm(ski$`Selling price` ~ ski$`List price` + ski$Downtwon, data = ski)
summary(modelSub2Var)
anova(modelSub2Var)
vif(modelSub2Var)
qqnorm(modelSub2Var$residuals)
AIC(modelSub2Var)
BIC(modelSub2Var)

#Performing sqrt transformation to ski$Downtwon in order to increase significance

modelSub2Varsqrt = lm(ski$`Selling price` ~ ski$`List price` + sqrt(ski$Downtwon), data = ski)
summary(modelSub2Varsqrt)
anova(modelSub2Varsqrt)
vif(modelSub2Varsqrt)
qqnorm(modelSub2Varsqrt$residuals)
AIC(modelSub2Varsqrt)
BIC(modelSub2Varsqrt)
#dw test




#The list price all the time found to be most significant as it has comparably good resemblance with selling price. Need to build model depends on other factor


#Using regsubsets to find out the top best subset model with each number variable - Not Considering List Price
Submodels2 = regsubsets(ski$`Selling price` ~ ski$Bedrooms
                        + ski$Bathrooms + ski$Sq_Ft
                        + ski$Downtwon + ski$Mountain + ski$`Lot size`
                        + ski$Garage + ski$Age + ski$`On market`, data = ski, nvmax = 9)
summary(Submodels2)

modelSubA2 = lm(ski$`Selling price` ~ ski$Bathrooms, data = ski) #not a good r-square value
summary(modelSubA2)
anova(modelSubA2)

modelSubB2 = lm(ski$`Selling price` ~ ski$Bathrooms + ski$Mountain , data = ski) #not a good r-square value and mountain - insignificant
summary(modelSubB2)
anova(modelSubB2)

modelSubC2 = lm(ski$`Selling price` ~ ski$Sq_Ft + ski$Mountain + ski$`Lot size` , data = ski) # a moderate r-square value, good significance
summary(modelSubC2)
anova(modelSubBC)


#considering parameters and finding best possible subset
Var_sum = summary(Submodels2)
data.frame(
  Adj_R_Sq = which.max(Var_sum$adjr2),
  CP_Value = which.min(Var_sum$cp),
  BIC_Value = which.min(Var_sum$bic)
  
)

#interprets that the best r square, Cp and, BIC value found, is with 5 predictor variables,hence from summary(Submodels2)

ModelPerf = lm(ski$Selling.price ~ ski$Bedrooms + ski$Sq_Ft + ski$Mountain + ski$Lot.size + ski$Garage, data = ski )
summary(ModelPerf)
Anova(ModelPerf)
BIC(ModelPerf)
AIC(ModelPerf)
library(car)
vif(ModelPerf)
plot(ModelPerf$fitted.values,ModelPerf$residuals)
qqnorm(ModelPerf$residuals)
residualPlots(ModelPerf)



# Ski$garage has less significance hence, and bedroom has less significance after removing garage, so

ModelPerf2 = lm(ski$Selling.price ~  ski$Sq_Ft + ski$Mountain + ski$Lot.size , data = ski )
summary(ModelPerf2)
anova(ModelPerf2)
BIC(ModelPerf2)
AIC(ModelPerf2)
vif(ModelPerf2)
plot(ModelPerf2$fitted.values,ModelPerf2$residuals)
qqnorm(ModelPerf2$residuals)
library(car)
residualPlots(ModelPerf2)

#Cross validation on model without listing price.
# ModelPerf has more significane and r2 value

library(caret)
set.seed(123) 
train.control <- trainControl(method = "cv", number = 5)
# Train the model
modelcv <- train(Selling.price~ Bedrooms + Sq_Ft + Mountain + Lot.size + Garage, data = ski ,method = "lm",trControl = train.control)
# Summarize the results
print(modelcv)
#Rsquare value of every fold
modelcv$resample

set.seed(42)
partition <- createDataPartition(y = ski$Selling.price, p = 0.8, list = F)
trainingdata = ski[partition, ]
test <- ski[-partition, ]
pcv = predict(modelcv,test)
errorcv <- (pcv- test$Selling.price)
RMSE_NewDatacv <- sqrt(mean(errorcv^2))

#################################################################################################################3
##############################################################################################################3
#Model with listing price using stepwise regression algorithm

library(openxlsx)

setwd("G:/University of Denver Life!/C U Denver sem 2/BANA 6610 R programming/HW5/Model analysis")
getwd()
ski = read.xlsx("ski.xlsx", sheet = "Average values of outliers")

#Full model
model1 = lm(ski$Selling.price~ski$List.price + ski$Bathrooms + ski$Bedrooms + ski$Sq_Ft + ski$Downtwon + ski$Mountain + ski$Lot.size + ski$Garage + ski$Age + ski$On.market)
summary(model1)

#Performing stepwise AIC to find the best combination of independent variable
library(MASS)
model1_Step = stepAIC(model1)

model2 = lm(ski$Selling.price ~ ski$List.price + ski$Downtwon + 
              ski$On.market)
summary(model2)
library(car)
vif(model2)

#Transform data
model3 = lm(ski$Selling.price ~ ski$List.price + sqrt(ski$Downtwon) + 
              sqrt(ski$On.market))
summary(model3)
vif(model3)

#Eliminating On.Market 
model4 = lm(ski$Selling.price ~ ski$List.price + sqrt(ski$Downtwon))
summary(model4)
vif(model4)
anova(model4)

residualPlots(model4)
plot(model4)
durbinWatsonTest(model4)
################################################ CROSS VALIDATION ##############################################################

library(caret)
set.seed(123) 
train.control <- trainControl(method = "cv", number = 5)
# Train the model
model <- train(Selling.price~ List.price + sqrt(Downtwon), data = ski ,method = "lm",trControl = train.control)
# Summarize the results
print(model)
#Rsquare value of every fold
model$resample

set.seed(42)
partition <- createDataPartition(y = ski$Selling.price, p = 0.8, list = F)
trainingdata = ski[partition, ]
test <- ski[-partition, ]
pcv = predict(model,test)
errorcv <- (pcv- test$Selling.price)
RMSE_NewDatacv <- sqrt(mean(errorcv^2))