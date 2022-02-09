#Question 1
myData <- read.csv("Social_Network_Ads.csv")
myData

#Question 2
str(myData)
summary(myData)
cor(myData$Age,myData$EstimatedSalary)
plot(myData$Age, myData$EstimatedSalary ,main="Titre principal", xlab="Age", ylab="Estimated Salary", type = "h")

#Question 3
library(caTools) # install it first in the console
set.seed(123)
# we use the function set.seed() with the same seed number
# to randomly generate the same values, you already know that right? 
#and you know why we want to generate the same values, am I wrong? 
split = sample.split(myData$Purchased, SplitRatio = 0.75)
# here we chose the SplitRatio to 75% of the dataset,
# and 25% for the test set.
training_set = subset(myData, split == TRUE)
# we use subset to split the dataset
test_set = subset(myData, split == FALSE)

#Question 4
training_set$Age = scale(training_set$Age)
training_set$EstimatedSalary = scale(training_set$EstimatedSalary)
test_set$Age = scale(test_set$Age)
test_set$EstimatedSalary = scale(test_set$EstimatedSalary)
#To scale is to put all variables on the same scale

#Question 5
myModel = glm(formula = Purchased~Age,data = training_set, family = "binomial")
myModel

#Question 6
#We chose family to be binomial because the variable can be only 0 or 1 (purchased or not)
        
#Question 7
#According to the question 5, we have ??1=1.9913 and ??0=-0.9299 (constant)
#So the model is p(X) = exp(-0.9299 + 1.9913*X)/(1 + exp(-0.9299 + 1.9913*X))
p = function(X) {exp(-0.9299 + 1.9913*X)/(1 + exp(-0.9299 + 1.9913*X))}

#Question 8
plot(training_set$Age,exp(-0.9299 + 1.9913*training_set$Age)/(1 + exp(-0.9299 + 1.9913*training_set$Age)), xlab="Age", ylab="Purchased")
summary(myModel)
confint(myModel, level = 0.95)
#At the sight of this curve and the summary of the model we can conclude that the Age was increasing the probability of purchase
#Indeed, the confidence intervals for the coefficients show a significative positive correlation at level  ?? = 95%

#Question 9
summary(myModel)
#According to the summary of the model, AIC = 256.11

#Question 10
plot(training_set$Age, training_set$Purchased, xlab="Age", ylab="Purchased")
curve(p,add=TRUE)
library(ggplot2)
ggplot(training_set, aes(x=Age, y=Purchased)) + geom_point() + stat_smooth(method="glm", method.args=list(family="binomial"), se=FALSE)

#Question 11
myModel2 = glm(formula = Purchased~Age+EstimatedSalary,data = training_set, family = "binomial")
myModel2

#Question 12
summary(myModel2)
confint(myModel2, level = 0.95)
#At the sight of the summary of the model (Estimate Std. Age = 2.6324 and Estimate Std. EstimatedSalary = 1.3947, we can conclude that the Age and Salary was increasing the probability of purchase
#Indeed, the confidence intervals for the coefficients show a significative positive correlation at level  ?? = 95%
#So the predictors are significant to the new model

#Question 13
#We see in the second model that AIC = 205.78 < 256.11 = AIC of the first model so the model is better with the estimated salary

#Question 14
yhat = predict(myModel2, test_set, type = "response")

#Question 15
yhat = ifelse(yhat>0.5,1,0)
yhat

#Question 16
MC = table(test_set$Purchased,yhat)
MC
#We obtain a matrix with 57 correct values for Purchased = 0 (7 wrong values) and 26 correct values for Purchased = 1 (10 wrong values)

#Question 17
accuracy = (MC['0','0']+MC['1','1'])/(MC['0','0']+MC['1','1']+MC['0','1']+MC['1','0'])
print('accuracy = ')
print(accuracy)

sensitivity = MC['0','0']/(MC['0','0']+MC['0','1'])
print('sensitivity = ')
print(sensitivity)

specificity = MC['1','1']/(MC['1','1']+MC['1','0'])
print('specificity = ')
print(specificity)

precision = MC['0','0']/(MC['0','0']+MC['1','0'])
print('precision = ')
print(precision)

#Question 18
library(ROCR)
yhat = predict(myModel2, test_set, type = "response")
pred = prediction(yhat,test_set$Purchased)
perf = performance(pred, 'tpr','fpr')
plot(perf, col = 'Red')
abline(a=0,b=1)
auc.perf = performance(pred,measure="auc")
print("AUC Value = ")
auc.perf@y.values

#Question 19
yhat1 = predict(myModel, test_set, type = "response")
pred1 = prediction(yhat1,test_set$Purchased)
perf1 = performance(pred1, 'tpr','fpr')
plot(perf1,add = TRUE, col = 'Blue')
auc1.perf = performance(pred1,measure="auc")
print("AUC Value = ")
auc1.perf@y.values
#We can see the second model (in red) have an AUC = 0.91 and a true positive rate better than the first model (in blue) with an AUC = 0.89

