#In this example, we will see how PCA and elbow method are used with K-means clustering 
data<-read.table("diabetes.data.txt",sep=",",header=FALSE)
#768 samples in the dataset 
#8 quantitative variables 
#
#response variable is the last column and the remaining columns are the predictors 
y<-data[,dim(data)[2]] 
predictorx<-data[,1:(dim(data))[2]-1]


#For the convenience of visualization, take the first two principle components as the new feature variables
#and conduct k-means only on these two dimensional data 
pca <- prcomp(predictorx, scale=TRUE)
pc.comp<-pca$x
pc.comp1<- -1*pc.comp[,1]
pc.comp2<- 1*pc.comp[,2]
plot(pc.comp1, pc.comp2)


#Now we will use elbow method to help us decide the optimal number of clusters 
set.seed(123)
kmax<-15
wss<-sapply(1:kmax,function(k){kmeans(data,k,nstart=50)$tot.withinss})
wss


#we notices that k=5 is about right.. 
plot(1:kmax,wss,type="b",pch=19, frame=FALSE, xlab="Number of Cluster K", ylab="Total within-clusters sum of squares")
x<-cbind(pc.comp1, pc.comp2)
cl<-kmeans(x,5,nstart=20)
cl$cluster
plot(pc.comp1,pc.comp2,col=cl$cluster)
points(cl$centers, pch=16)
