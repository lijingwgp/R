#Let's apply K-means clustering to Scotch data we have worked before
#
#Assume you run a small liquor shop in a well-to-do neighborhood, 
#and as part of the business strategy you want to be known as the place to go for 
#single-malt Scotch whiskeys. You may not be able to afford the largest selection, 
#given the limited space and ability to invest in inventory, but you might choose 
#a strategy of having a broad and eclectic collection. 
#
#If we understood how the single malts grouped by taste, we could choose from each 
#taste group a popular member and a lesser-known member. 
#
#setup 
rm(list=ls())
set.seed(527)
mydata<-read.table("scotch.csv",sep=",",header=TRUE)
mydata<-na.omit(mydata[,1:69])
x<-mydata[-1]
y<-mydata$NAME 


#pca 
pca <- prcomp(x, scale=TRUE)
pc.comp<-pca$x
pc.comp1<- 1*pc.comp[,1]
pc.comp2<- -1*pc.comp[,2]
plot(pc.comp1, pc.comp2)


#Now try elbow method to find the optimal k with kmax = 20 and plot it see where the "elbow" occurs 
set.seed(123)
kmax<-20
wss<-sapply(1:kmax,function(k){kmeans(x,k,nstart=20)$tot.withinss})
wss
plot(1:kmax,wss,type="b",pch=19, frame=FALSE, xlab="Number of Cluster K", ylab="Total within-clusters sum of squares")


#Did you see the "elbow" in the plot? Now try elbow method to find the optimal k with kmax = 30 and plot it.
set.seed(123)
kmax<-30
wss<-sapply(1:kmax,function(k){kmeans(x,k,nstart=30)$tot.withinss})
wss
plot(1:kmax,wss,type="b",pch=19, frame=FALSE, xlab="Number of Cluster K", ylab="Total within-clusters sum of squares")


#what did you observe? Do you think using the elbow method is appropriate for this example? If not, how would you choose your k? 
#Remember we are trying see how the single malts are grouped by taste and choose a few from each taste group. 
#So the optimal number of k depends on how many taste group you want to have. 
#
#For this example, let's choose k = 5 and run k-means clustering. 
#Then plot it to see where the clusters are. 
x<-cbind(pc.comp1, pc.comp2)
cl<-kmeans(x,5,nstart=20)
plot(pc.comp1,pc.comp2,col=cl$cluster)
points(cl$centers, pch=16)
