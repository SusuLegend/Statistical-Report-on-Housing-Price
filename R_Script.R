rm(list = ls())

library(boot)
library(tidyverse)
library(caret)
library(caTools)

#importing data
mydata = read.csv('Housing.csv',header = T,sep = ",")
head(mydata)

#checking null value
colSums(is.na(mydata))

summary(mydata)

#setting seed
set.seed(101)
#SETTING The ratio of train and test (0.7 / 0.3)
split = sample.split(mydata,SplitRatio = 0.7) 

# Split train and test data
train_data = subset(mydata, split == TRUE) #making train data
test_data = subset(mydata, split == FALSE) #making test_data

attach(train_data)

#fitting multiple regression
model <- lm(price ~ bedrooms + bathrooms +sqft_living + sqft_lot + floors + waterfront + view + condition + grade + sqft_above + yr_built + lat + long + sqft_living15 + sqft_lot15)
summary(model)

#first model ANOVA
anova(model)

#second model with significant variable
new_model <- lm(price ~ bedrooms + bathrooms +sqft_living + sqft_lot+ waterfront + view + condition + grade + sqft_above + yr_built + lat + long + sqft_living15 + sqft_lot15)
summary(new_model)
#second model ANOVA
anova(new_model)


#residual vs fitted 
plot(new_model)

#QQ plot
qnorm(new_model)


#prediction
predictions = predict(new_model,test_data)

#Data Frame for actual vs predicted value
actual_preds = data.frame(cbind(actuals=test_data$price,predicted=predictions))
head(actual_preds)

#correlation accuracy
correlation_acc = cor(actual_preds)
correlation_acc

#export data
write.csv(train_data, file = "train_data_house.csv")
write.csv(test_data, file = "test_data_house.csv")
