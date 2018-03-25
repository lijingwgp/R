#In this exercise, we will see how K-means clustering works with real data 
#Iris dataset contains data about sepal length and width, petal length and width, 
#and species of flowers.
rm(list=ls())
library(datasets)
head(iris)


#When you explore the data, you may notice that petal length and petal width are similar 
#among the same species but varied considerably between different species 
#x<-iris$Species
plot(iris$Petal.Length,iris$Petal.Width, col=iris$Species)
legend("topleft", legend=c("setosa","versicolor","virginica"), col=c(1,2,3),lty=1)


#This is a very simple example. We can clearly see that there are three clusters.
#Let's try to actually cluster it with K-means. 
set.seed(20)
irisCluster <- kmeans(iris[,3:4], 3, nstart =20)
irisCluster


#Compare the clusters with actual the species 
table(irisCluster$cluster, iris$Species)


#As we can see, setosa species data got grouped into cluster 1 perfectly
#k-means worngly classified 4 versicolor data points and 2 virginica data points 
irisCluster$cluster <- as.factor(irisCluster$cluster)
plot(iris$Petal.Length,iris$Petal.Width,col=irisCluster$cluster,main="K-means K=3")


#Compare two plots side by side
par(mfrow=c(1,2))
plot(iris$Petal.Length,iris$Petal.Width, col=iris$Species,main="Original Data")
plot(iris$Petal.Length,iris$Petal.Width,col=irisCluster$cluster,main="K-means K=3")
par(mfrow=c(1,1))
