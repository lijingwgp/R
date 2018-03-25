#In this exercise, we will go through a simple simulated example
#in which there truly are two clusters in the data 
#the first 25 observations have a mean shift relative to the next 25 observations
rm(list=ls())
set.seed(2)
x = matrix(rnorm(50*2),ncol=2) #100 random numbers: 50 in one column, 50 in the other 
x[1:25,1] = x[1:25,1]+3
x[1:25,2] = x[1:25,2]-4
plot(x) 
#You can see that there are two clusters 


#Now perform K-means clustering with K = 2 
#nstart =20 means R will try 20 different random starting assignments 
#and then select the one with the lowest within cluster variation 
km.out = kmeans(x,2,nstart=20)
km.out$cluster
plot(x, col =(km.out$cluster+1), main = "K-Means Clustering Results with K=2", pch=20, cex=2)
points(km.out$centers, col=2:3, pch=16, cex=3)
#Here the plotting is easy because they are two-dimensional.
#if there were more than two variables, we could perform PCA instead and plot the first two principal components score vectors 


#In general, we don't know the true number of clusters.
#Let's see what would happen if we apply K=3 to this example 
set.seed(4)
km.out = kmeans(x,3,nstart=20)
km.out
plot(x, col =(km.out$cluster+1), main = "K-Means Clustering Results with K=3", pch=20, cex=2)
points(km.out$centers, col=2:4, pch=16, cex=3)
#It is up to your analysis to decide what number of cluster is more appropriate for the data 


#Above, we mentioned that nstart determines the number of random starting assignments
#Let's compare using nstart=1 to nstart=20 by the total within-cluster sum of squares, 
#which we want to minimize. 
set.seed(3)
km.out = kmeans(x,3,nstart=1)
km.out$tot.withinss

km.out = kmeans(x,3,nstart=20)
km.out$tot.withinss

km.out = kmeans(x,3,nstart=50)
km.out$tot.withinss
#It is strongly recommended always running with a large nstart like 20 or 50 to avoid an undesirable local optimum 
