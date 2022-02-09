#Question 1
library(MASS)
dim(Boston)

#Question 2
train = 1:400
test = -train
variables = which(names(Boston) ==c("age", "medv"))
training_data = Boston[train, variables]
testing_data = Boston[test, variables]
dim(training_data)

#Question 3
cor.test(training_data$age,training_data$medv,method="pearson")

#Question 4
model = lm(medv ~ age, data = training_data)
model
plot(training_data$age, training_data$medv,main="Housing prices in fontcion of age", xlab='age', ylab='medv')
abline(model)

#Question 5
variables = which(names(Boston) ==c("age", "medv"))
training_data = Boston[train, variables]
testing_data = Boston[test, variables]
dim(training_data)
model = lm(medv ~ age + log(lstat), data = Boston)
model
#It' a model with 2 variables, better fitted.


#Question 6
summary(model)

#Question 7
#The adjusted R2 equals 0.6857 so the model is 68% significant.

#Question 8
#p-value is 2.2e-16 so the predictors are significant.

#Question 9
model = lm(medv ~., data = Boston)
model
summary(model)

#Question 10
model = lm(medv ~. -lstat + log(lstat), data = Boston)
model
summary(model)

#Question 11
#The adjusted R2 changed from 0.6857 to 0.7871 so it has improved.

#Question 12
cor(Boston,Boston,method = "pearson")

#Question 13
library(corrplot)
corrplot.mixed(cor(Boston,Boston,method = "pearson"))

#Question 14
#Using the preceding question, correlation between tax and rad is 0.91 (91%).

#Question 15
model = lm(medv ~. -lstat + log(lstat) - tax, data = Boston)
model
summary(model)
#The adjusted R2 changed to 0.7825 so it is almost the same than before. The F-statistic equals 152.4, which is a little higher than before (144.6).

#Question 16
MSE = mean(model$residuals^2)
MSE

#Question 17
str(which(Boston$chas==1))
#There are 35 suburbs in this data set bound the Charles River.

#Question 18
boxplot(Boston$medv[Boston$chas==1], Boston$medv)
#The median value for the neighborhood of Charles River is a little higher than the global median value.

#Question 19
aggregate(Boston$medv,by=Boston, FUN=mean)

#Question 20
a = aov(medv ~ chas, data = Boston)
a
summary(a)
#The sum of squares for the Residuals are very high so we need more variables to have a model better fitted.

#Question 21
model = lm(medv ~ chas + crim, data = Boston)
model
#Coefficients are 5.578 for chas (the presence of the Charles River increases significatively the price) and -0.406 for crim (the presence of criminality decreases the price) so the presence of a river adds a valuable information for explaining the house price.

#Question 22
#When there are more predictors, chas is less significant.

#Question 23
model = lm(medv ~ age + lstat, data = Boston)
model
summary(model)

#Question 24
model = lm(medv ~., data = Boston)
model
summary(model)

