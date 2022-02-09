#Question 1
library(MASS)
library(caTools)
set.seed(18)
Boston_idx = sample(1:nrow(Boston),nrow(Boston)/2)
Boston_train = Boston[Boston_idx,]
Boston_test = Boston[-Boston_idx,]

#Question 2
library(rpart)
Boston_tree = rpart(formula = medv~., data = Boston_train)

#Question 3
plot(Boston_tree)
text(Boston_tree,pretty = 0)
title(main = "Regression Tree")

#Question 4
#install.packages('rpart.plot')
library(rpart.plot)
rpart.plot(Boston_tree)
prp(Boston_tree)

#Question 5
summary(Boston_tree)
printcp(Boston_tree)
plotcp(Boston_tree)

#Question 6
RMSE = function(vector1, vector2)
  {sqrt(mean((vector1 - vector2)^2))}

#Question 7
Boston_pred = predict(Boston_tree,newdata = Boston_test)
print(Boston_pred)
Boston_RMSE=RMSE(Boston_pred,Boston_test$medv)
print(Boston_RMSE)

#Question 8
Boston_lm = lm(formula = medv~., data = Boston_train)
print(Boston_lm)
Boston_pred2 = predict(Boston_lm,newdata = Boston_test)
print(Boston_pred2)
Boston_RMSE2=RMSE(Boston_pred2,Boston_test$medv)
print(Boston_RMSE2)

plot(Boston_pred,Boston_test$medv,main = 'Single Tree',col = 'Red')
abline(0,1,lwd=2)
grid()
plot(Boston_pred2,Boston_test$medv, main = 'Linear Regression Model', col = 'Blue')
abline(0,1,lwd=2)
grid()
#According to the plots the best model seems to be the linear regression

#Question 9
#install.packages('randomForest')
library(randomForest)
Boston_bagging = randomForest(formula = medv~.,data = Boston_train)
print(Boston_bagging)

#Question 10
Boston_pred3 = predict(Boston_bagging,newdata = Boston_test)
print(Boston_pred3)
Boston_RMSE3 = RMSE(Boston_pred3,Boston_test$medv)
print(Boston_RMSE3)
plot(Boston_pred3,Boston_test$medv, main = 'Bagging Trees', col = 'Green')
abline(0,1,lwd=2)
#The model seems to be very performing, more than the previous models

#Question 11
Boston_forest = randomForest(formula = medv~., data = Boston_train)
print(Boston_forest)
Boston_pred4 = predict(Boston_forest,newdata = Boston_test)
print(Boston_pred4)
Boston_RMSE4 = RMSE(Boston_pred4,Boston_test$medv)
print(Boston_RMSE4)
plot(Boston_pred4,Boston_test$medv, main = 'Random Forests', col = 'Brown')
abline(0,1,lwd=2)

#Question 12
importance(Boston_forest)
#The 3 most important predictors are lstat,rm and indus, we don't find the same as linear regression model

#Question 13
varImpPlot(Boston_forest)

#Question 14
#install.packages('gbm')
library(gbm)
Boston_boost = gbm(medv ~ ., data = Boston_train, distribution = "gaussian", 
                   n.trees = 5000, interaction.depth = 4, shrinkage = 0.01)
print(Boston_boost)
Boston_pred5 = predict(Boston_boost, newdata = Boston_test)
print(Boston_pred5)
Boston_RMSE5 = RMSE(Boston_pred5,Boston_test$medv)
print(Boston_RMSE5)
#RMSE = 3.67 which is lower than the other RMSE
plot(Boston_pred5,Boston_test$medv, main = 'Boosting', col = 'Purple')
abline(0,1,lwd=2)

#Question 15
summary(Boston_boost)

#Question 16
#library(knitr)
#kable(head([, 2:4]), "simple")
par(mfrow=c(2,2))
plot(Boston_pred,Boston_test$medv,main = 'Single Tree',col = 'Red')
abline(0,1,lwd=2)
grid()
plot(Boston_pred3,Boston_test$medv, main = 'Bagging Trees', col = 'Green')
abline(0,1,lwd=2)
grid()
plot(Boston_pred4,Boston_test$medv, main = 'Random Forests', col = 'Brown')
abline(0,1,lwd=2)
grid()
plot(Boston_pred5,Boston_test$medv, main = 'Boosting', col = 'Purple')
abline(0,1,lwd=2)
grid()

#CLASSIFICATION TREES
library(MASS)
library(caTools)
library(rpart)
library(rpart.plot)
library(randomForest)
library(gbm)

#Question 1
myData = read.csv("spam.csv")
str(myData)
summary(myData)

#Question 2
set.seed(30)
split = sample.split(myData$spam, SplitRatio = 0.75)
training_set = subset(myData, split == TRUE)
test_set = subset(myData, split == FALSE)

#Question 3
myModel_logreg = glm(formula = spam~., data = training_set, family = "binomial")
print(myModel_logreg)
summary(myModel_logreg)

myModel_tree = rpart(formula = spam~., data = training_set)
plot(myModel_tree)
text(myModel_tree,pretty = 0)
title(main = "Regression Tree")
rpart.plot(myModel_tree)

myModel_bagging = randomForest(formula = spam~.,data = training_set)
print(myModel_bagging)
summary(myModel_bagging)

myModel_forest = randomForest(formula = spam~., data = training_set)
print(myModel_forest)
summary(myModel_forest)

myModel_boost = gbm(spam ~ ., data = training_set, distribution = "bernoulli", 
                   n.trees = 5000, interaction.depth = 4, shrinkage = 0.01)
print(myModel_boost)
summary(myModel_boost)

#Question 4
myModel_logreg_pred = predict(myModel_logreg, newdata = test_set, type ="response")
myModel_logreg_pred = ifelse(myModel_logreg_pred>0.5,1,0)

#myModel_tree_pred = predict(myModel_tree, newdata = test_set, type="class")
#myModel_tree_pred = ifelse(myModel_tree_pred>0.5,1,0)
#doesn't work for no reason

myModel_bagging_pred = predict(myModel_bagging, newdata = test_set, type="response")
myModel_bagging_pred = ifelse(myModel_bagging_pred>0.5,1,0)

myModel_forest_pred = predict(myModel_forest, newdata = test_set, type="response")
myModel_forest_pred = ifelse(myModel_forest_pred>0.5,1,0)

myModel_boost_pred = predict(myModel_boost, newdata = test_set, type="response")
myModel_boost_pred = ifelse(myModel_boost_pred>0.5,1,0)

Acc = function(vector1, vector2)
{mean(vector1 == vector2)}

myModel_logreg_acc = Acc(myModel_logreg_pred,test_set$spam)
print(myModel_logreg_acc)
#myModel_tree_acc = Acc(myModel_tree_pred,test_set$spam)
#print(myModel_tree_acc)
myModel_bagging_acc = Acc(myModel_bagging_pred,test_set$spam)
print(myModel_bagging_acc)
myModel_forest_acc = Acc(myModel_forest_pred,test_set$spam)
print(myModel_forest_acc)
myModel_boost_acc = Acc(myModel_boost_pred,test_set$spam)
print(myModel_boost_acc)