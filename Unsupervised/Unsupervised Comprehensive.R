rm(list=ls()) 
##################### 
#### Question 1
##################### 
## on the USArrest data, calculate PVE in two ways:
## a) using the sdev output of the prcomp() function
pr.out=prcomp(USArrests, scale=TRUE)
pr.out$sdev^2 / sum(pr.out$sdev^2)

## b) by applying equation 10.8 directly. that is, use the prcomp() function to 
## compute the component loadings. then, use these loadings in equation 10.8 to 
## obtain the pve
crime <- as.matrix(USArrests)
n <- nrow(crime)
i <- rep(1,n)
crime_centered <- crime - i%*%t(i)%*%crime*(1/n) 
apply(crime_centered,2,mean)
crime_scaled <- crime_centered %*% diag(1/(apply(crime_centered,2,sd)))  
apply(crime_scaled,2,sd)
sum((pr.out$x)[,1]**2)/sum(crime_scaled**2)
sum((pr.out$x)[,2]**2)/sum(crime_scaled**2)
sum((pr.out$x)[,3]**2)/sum(crime_scaled**2)
sum((pr.out$x)[,4]**2)/sum(crime_scaled**2)



rm(list=ls())
##################### 
#### Question 2
##################### 
## show that the correlation between i and j observations is propotional to 
## the squared Euclidean distance between i and j observations
data = scale(USArrests, center = T)
data = as.matrix(data)
euclidean = dist(data)^2
correlation = as.dist(1 - cor(t(data)))
euclidean.new = euclidean[lower.tri(euclidean, diag = F)][1:900]
correlation.new = correlation[lower.tri(correlation, diag = F)][1:900]
plot(euclidean.new/correlation.new,type = 'l')
sum(euclidean.new) / sum(correlation.new)
print("there are couple of ways to illustrate this property holds. First is to visually insepct the graph of ration between euclidean distance and correction. From the graph,
      we can tell that for the majority part, the ration between the two is constant. This shows that the ratio between euclidean distance and correlation is relatively stable.")
print("perhaps, we can also determine the sum of all the euclidean distances and the sum of all correations. Then determine the ratio between the two which is rougly 8.2. 
      This means that euclidean distances is almost 8 times proportional to the correlation")



rm(list=ls())
##################### 
#### Question 3
##################### 
## generate simulated data, then perform PCA and K-means on the data
## a) generate simulated data set called x.values
n <- 20
p <- 50
x1 <- matrix(rnorm(n*p), nrow=n, ncol=p) 
x2 <- matrix(rnorm(n*p), nrow=n, ncol=p) 
x3 <- matrix(rnorm(n*p), nrow=n, ncol=p) 
for(i in 1:n){   
  x1[i,] <- x1[i,] + rep(1, p)   
  x2[i,] <- x2[i,] + rep(-1, p)    
  x3[i,] <- x3[i,] + c(rep(+1, p/2), rep(-1, p/2))  
} 
x.values <- rbind(x1, x2, x3) 

## b) create an vector named y.values that represent class labels for the observations
y.values <- c(rep(1,n), rep(2,n), rep(3,n)) 

## c) perform PCA on the 60 observations and plot the first two principal component score vectors
pca <- prcomp(x.values, scale. = T, center = T)
plot(pca$x[,1:2], col = c("blue","green","black")[y.values])

## d) perform K-Means. how well do the clusters obtained from K-Means compare to the true class?
k.out <- kmeans(scale(x.values, center = T), 3, nstart = 50)
table(k.out$cluster, y.values)
print("perfect match")

## e) perform K-means clustering with K = 2. Describe your results.
k.out1 <- kmeans(scale(x.values, center = T), 2, nstart = 50)
table(k.out1$cluster, y.values)
print("all of the observations from the third class in y.values got absorbed into the second clustered group")

## f) perform K-means clustering with K = 4, and describe your results. 
k.out2 <- kmeans(scale(x.values, center = T), 4, nstart = 50)
table(k.out2$cluster, y.values)
print("observations from the second class in y.values got splited into the second and fourth clusters")

## g) perform K-means with K = 3 on the first two principal component score vectors,
## rather on the raw data. 
x <- cbind(pca$x[,1], pca$x[,2])
k.out3 <- kmeans(x, 3, nstart = 50)
plot(x, col = k.out3$cluster)
print("This result again, matches with the result when clustering on the raw data with 3 centers")

## h) using the scale() function, perform K-means clustering with K = 3 on the data after scaling
k.out4 <- kmeans(scale(x.values, center = T), 3, nstart = 50)
k.out4$cluster
print("We have 60 rows. They are divided into three groups by K-means clustering. The distance between each member from the same group is minimized. In addition, three groups are clearly sperated.")
print("This result agrees with the result c")
