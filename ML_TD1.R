#Exercice 1
x = c(1,7,3,4)
y=c(100:1)
x[3]+y[4]
cos(x[3])+sin(y[4])*exp(-y[2])
x[3]=0
y[2]=-1
x[3]+y[4]
cos(x[3])+sin(y[4])*exp(-y[2])
z=y[x+1]
z

#Exercice 2
df1=1
df2=5
qf(0.90,df1,df2)
qf(0.95,df1,df2)
qf(0.99,df1,df2)
rpois(100,5)
seq=seq(-4,4,l=100)
plot(seq,dt(seq,1),type='l')
lines(seq,dt(seq,5),type='o',col='red')
lines(seq,dt(seq,10),type='h',col='blue')
lines(seq,dt(seq,50),type='b',col='yellow')
lines(seq,dt(seq,100),type='s',col='green')

#Exercice 3
getwd()
load("EU.RData")
myModel <- lm(formula = CamCom2011 ~ Population2010, data = EU)
myModel$residuals
myModel$coefficients
summaryMyModel <- summary(myModel)
summaryMyModel$sigma

#Exercice 4
library(MASS)
dim(Boston)
train = 1:400
test = -train
variables = which(names(Boston) ==c("lstat", "medv"))
training_data = Boston[train, variables]
testing_data = Boston[test, variables]
dim(training_data)
plot(training_data$lstat, training_data$medv,main="Titre principal", xlab='lstat', ylab='medv',type='p',col='blue',pch=2,cex=3)
plot(log(training_data$lstat), training_data$medv)
model = lm(medv ~ log(lstat), data = training_data)
model
summary(model)
names(model)
model$coefficients
confint(model, level = 0.95)
plot(log(training_data$lstat), training_data$medv)
abline(model)
plot(log(training_data$lstat), training_data$medv,
     xlab = "Log Transform of % of Houshold with Low Socioeconomic Income",
     ylab = "Median House Value",
     col = "red",
     pch = 20)
abline(model, col = "blue", lwd =3)
predict(model, data.frame(lstat = c(5)))
predict(model, data.frame(lstat = c(5,10,15)), interval = "prediction")
y = testing_data$medv
y_hat = predict(model, data.frame(lstat = testing_data$lstat))
error = y-y_hat
error_squared = error^2
MSE = mean(error_squared)
MSE