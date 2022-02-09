#Question 1
dataset <- read.csv("Social_Network_Ads.csv")
str(dataset) # to show the structure of the dataset. 
summary(dataset) # will show some statistics of every column. 
boxplot(Age ~ Purchased, data=dataset, col = "blue", main="Boxplot Age ~ Purchased")
boxplot(EstimatedSalary ~ Purchased, data=dataset,col = "red", main="Boxplot EstimatedSalary ~ Purchased")
aov(EstimatedSalary ~Purchased, data=dataset)
summary(aov(EstimatedSalary ~Purchased, data=dataset))
summary(aov(Age ~Purchased, data=dataset))
table(dataset$Gender,dataset$Purchased)
mosaicplot(~ Purchased + Gender, data=dataset,
           main = "MosaicPlot of two categorical variables: Puchased & Gender",
           color = 2:3, las = 1)
chisq.test(dataset$Purchased, dataset$Gender)
dataset = dataset[3:5]
str(dataset)
library(caTools)
set.seed(706550)
split = sample.split(dataset$Purchased, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)
training_set[-3] <- scale(training_set[-3]) #only first two columns
test_set[-3] <- scale(test_set[-3])
classifier.logreg <- glm(Purchased ~ Age + EstimatedSalary , family = binomial, data=training_set)
classifier.logreg
summary(classifier.logreg)
pred.glm = predict(classifier.logreg, newdata = test_set[,-3], type="response")
pred.glm_0_1 = ifelse(pred.glm >= 0.5, 1,0)
head(pred.glm)
head(pred.glm_0_1)
cm = table(test_set[,3], pred.glm_0_1)
cm
cm = table(pred.glm_0_1, test_set[,3])
cm
mosaicplot(cm,col=sample(1:8,2)) # colors are random between 8 colors.
require(ROCR)
score <- prediction(pred.glm,test_set[,3]) # we use the predicted probabilities not the 0 or 1
performance(score,"auc") # y.values
plot(performance(score,"tpr","fpr"),col="green")
abline(0,1,lty=8)

#Question 2
plot(training_set$Age,-(2.578/1.263)*training_set$Age-(-1.112/1.263),main = "Predicted Age~EstimatedSalary", xlab="Age", ylab="EstimatedSalary")
plot(EstimatedSalary~Age, data= test_set, main="EstimatedSalary~Age")
abline(1.112/1.263,-(2.578/1.263),lty=8)

#Question 3
bg = ifelse(pred.glm_0_1 == 1, 'Blue', 'Red')
plot(EstimatedSalary~Age, data= test_set,main="EstimatedSalary~Age", pch = 21, col = bg)
abline(1.112/1.263,-(2.578/1.263),lty=8, lwd = 2)

#Question 4
bg2 = ifelse(test_set$Purchased == 1, 'Blue', 'Red')
plot(EstimatedSalary~Age, data= test_set,main="EstimatedSalary~Age", pch = 21, col = bg2)
abline(1.112/1.263,-(2.578/1.263),lty=8, lwd = 2)

differ = sum(bg>bg2)
differ
cm = table(pred.glm_0_1, test_set[,3])
cm
#We find the same amount of differences in the confusion matrix and in the prediction

#Question 5
library(MASS)
classifier.lda <- lda(Purchased~Age+EstimatedSalary, data=training_set)

#Question 6
classifier.lda
classifier.lda$prior
classifier.lda$means

#Question 7
pred.lda = predict(classifier.lda, newdata = test_set[,-3], type="response")
str(pred.lda)

#Question 8
cm2 = table(pred.lda$class, test_set[,3])
cm2
cm
#With LDA, we find 14 wrong predictions, the same than Logistic Regression
accuracy = (cm['0','0']+cm['1','1'])/(cm['0','0']+cm['1','1']+cm['0','1']+cm['1','0'])
cat('accuracy logistic regression = ', accuracy)
accuracy2 = (cm2['0','0']+cm2['1','1'])/(cm2['0','0']+cm2['1','1']+cm2['0','1']+cm2['1','0'])
cat('accuracy LDA = ', accuracy2)
#The accuracy is the same for both techniques

#Question 9
X1 = seq(min(training_set[, 1]) - 1, max(training_set[, 1]) + 1, by = 0.01)
X2 = seq(min(training_set[, 2]) - 1, max(training_set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
plot(test_set[, 1:2],
     main = 'Decision Boundary LDA',
     xlab = 'Age', ylab = 'Estimated Salary',
     xlim = range(X1), ylim = range(X2))
points(test_set[1:2], pch = 21, bg = ifelse(test_set[, 3] == 1, 'green4', 'red3'))
pred_grid = predict(classifier.lda, newdata = grid_set)$class
contour(X1, X2, matrix(as.numeric(pred_grid), length(X1), length(X2)), add = TRUE)

#Question 10.1
class0=subset(training_set, training_set$Purchased == 0)
class1=subset(training_set, training_set$Purchased == 1)

#Question 10.2
pi0 = length(class0$Purchased)/length(training_set$Purchased)
pi0
pi1 = length(class1$Purchased)/length(training_set$Purchased)
pi1

#Question 10.3
mu0 = c(mean(class0$Age),mean(class1$Age))
mu0
mu1 = c(mean(class0$EstimatedSalary),mean(class1$EstimatedSalary))
mu1

#Question 10.4
sigma = cov(mu0,mu1)


#Question 11
classifier.qda <- qda(Purchased~., data = training_set)
classifier.qda

#Question 12
pred.qda = predict(classifier.qda, newdata = test_set[,-3], type="response")
pred.qda
cm3 = table(pred.qda$class, test_set[,3])
cm3
cm2
#With QDA, we find 12 wrong predictions which is a little lower than LDA (14 wrong predictions)

#Question 13
plot(test_set$Age, pred.qda$class,main="EstimatedSalary~Age", pch = 21, col = bg2)

#Question 14
library(ROCR)
pred = prediction(pred.glm,test_set$Purchased)
perf = performance(pred, 'tpr','fpr')
plot(perf, col = 'Red')

pred1 = prediction(pred.lda$posterior[,2],test_set$Purchased)
perf1 = performance(pred1, 'tpr','fpr')
plot(perf1,add = TRUE, col = 'Blue')

pred2 = prediction(pred.qda$posterior[,2],test_set$Purchased)
perf2 = performance(pred2, 'tpr','fpr')
plot(perf2,add = TRUE, col = 'Green')

auc.perf = performance(pred,measure="auc")
print("Logistic Regression AUC Value = ")
auc.perf@y.values

auc.perf1 = performance(pred1,measure="auc")
print("LDA AUC Value = ")
auc.perf1@y.values

auc.perf2 = performance(pred2,measure="auc")
print("QDA AUC Value = ")
auc.perf2@y.values

#According to the AUC and the plots, the best model for this dataset is the QDA (AUC = 0.9416 against 0.9121 and 0.9112 for the other models)