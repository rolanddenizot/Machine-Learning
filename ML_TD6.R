#Question 1
dataset <- read.csv("iris.csv")
dataset

#Question 2
par(mfrow=c(2,2))
boxplot(dataset$sepal_length ~ dataset$class, data=dataset, col = "blue", main="Boxplot Sepal_length ~ Class", xlab = 'Class', ylab= 'Sepal Length')
boxplot(dataset$sepal_width ~ dataset$class, data=dataset, col = "red", main="Boxplot Sepal_width ~ Class", xlab = 'Class', ylab= 'Sepal Width')
boxplot(dataset$petal_length ~ dataset$class, data=dataset, col = "orange", main="Boxplot Petal_length ~ Class", xlab = 'Class', ylab= 'Petal Length')
boxplot(dataset$petal_width ~ dataset$class, data=dataset, col = "green", main="Boxplot Petal_width ~ Class", xlab = 'Class', ylab= 'Petal Width')

#Question 3
library(ggplot2)
# histogram of sepal_length
ggplot(dataset, aes(x=sepal_length, fill=class)) +
  geom_histogram(binwidth=.2, alpha=.5)
# histogram of sepal_width
ggplot(dataset, aes(x=sepal_width, fill=class)) +
  geom_histogram(binwidth=.2, alpha=.5)
# histogram of petal_length
ggplot(dataset, aes(x=petal_length, fill=class)) +
  geom_histogram(binwidth=.2, alpha=.5)
# histogram of petal_width
ggplot(dataset, aes(x=petal_width, fill=class)) +
  geom_histogram(binwidth=.2, alpha=.5)

#Question 4
par(mfrow=c(1,1))
pcairis=princomp(dataset[,-5], cor=T)
str(pcairis)
summary(pcairis) 
plot(pcairis) 
biplot(pcairis)

#Question 5
library(factoextra)
fviz_eig(pcairis, addlabels = TRUE) #Graph of individuals
fviz_pca_var(pcairis, repel = TRUE) #Graph of variables
fviz_pca_biplot(pcairis) #Biplot graph
fviz_contrib(pcairis, choice = 'var') #Contribution to the first component
fviz_contrib(pcairis, choice = 'var', axes = 2) #Contribution to the second component

#Question 6
X <- dataset[,-5]
y <- dataset[,5]
X
y

#Question 7
X_scaled = scale(X)
X_scaled

#Question 8
sigma = cov(X_scaled)
sigma

#Question 9
Eigen_decomposition = eigen(sigma)
Eigen_decomposition
#We obtain a matrix of eigenvalues

#Question 10
Eigen_decomposition_correlation = eigen(cor(X_scaled))
Eigen_decomposition_correlation

#Question 11
Eigen_decomposition_correlation_raw = eigen(cor(X))
Eigen_decomposition_correlation_raw
#We obtain the same results

#Question 12
Individual_explained_variation = Eigen_decomposition$values/sum(Eigen_decomposition$values)
Individual_explained_variation
Cumulative_explained_variation = cumsum(Individual_explained_variation)
Cumulative_explained_variation

#Question 13
plot(Individual_explained_variation, type='b', main = 'Individual Explained Variation', ylab = 'Individual Explained Variation')

#Question 14
Projection_matrix = Eigen_decomposition$vectors[,1:2]
Projection_matrix

#Question 15
scores_matrix = X_scaled%*%Projection_matrix
scores_matrix

#Question 16 and 17
plot(scores_matrix, main = 'Scores', xlab = 'PC1', ylab = 'PC2', col = 'blue')
