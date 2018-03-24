#prepare random data for hierachical clustering analysis 
set.seed(2)
x=matrix(rnorm(50*2), ncol=2)
x[1:25,1]=x[1:25,1]+3
x[1:25,2]=x[1:25,2]-4


#create hierarchical clustering dendrogram using complete, single, and average
#linkage clustering, with Euclidean distance as the dissimilarity measure.
#
#the dist() function is used to compute the 50x50 inter-observation Euclidean distance matrix.
hc.complete=hclust(dist(x), method="complete")
hc.average=hclust(dist(x), method="average")
hc.single=hclust(dist(x), method="single")


#plot the dendrogram for three hierarchical clustering alove.
par(mfrow=c(1,3))
plot(hc.complete, hang = -1, main="Complete Linkage", xlab="", sub="", cex=.9)
rect.hclust(hc.complete, k=5, border="red")
plot(hc.average, hang = -1, main="Average Linkage", xlab="", sub="", cex=.9)
rect.hclust(hc.average, k=5, border="red")
plot(hc.single, hang = -1, main="Single Linkage", xlab="", sub="", cex=.9)
rect.hclust(hc.single, k=5, border="red")
par(mfrow=c(1,1))


#output the cluster label of each observation, k determines the number of cluster
#for this data, complete and average linkage generally separate the observations 
#into their correct groups. 
#however, single linkage identiﬁes one point as belonging to its own cluster.
cutree(hc.complete, k=2)
cutree(hc.average, k=2)
cutree(hc.single, k=2)
cutree(hc.single, k=4)


#To scale the variables before performing hierarchical clustering of the observations, 
#we use the scale() function
xsc=scale(x)
plot(hclust(dist(xsc), method="complete"), main="Hierarchical Clustering with Scaled Features")


#calculate correlated-based distance
#plot the dendrogram based on the cluster with correlated based distance 
x=matrix(rnorm(30*3), ncol=3)
dd=as.dist(1-cor(t(x)))
plot(hclust(dd, method="complete"), main="Complete Linkage with Correlation-Based Distance", xlab="", sub="")


#show USArrests data 
summary(USArrests)
data=scale(USArrests)
summary(data)
#draw hierarchical cluster 
hc=hclust(dist(USArrests))
plot(hc, hang=-1)
#change hclust object into dendrograms object
hcd=as.dendrogram(hc)
plot(hcd)
#zoom in the dendrogram plot 
plot(cut(hcd,h=200)$upper, main="Upper tree if cut at h=200")
plot(cut(hcd,h=200)$lower[[2]], main="Second branch of lower tree if cut at h=200")
