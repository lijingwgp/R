## A 2-dimensionl Example of Principal Component Analysis ###
rm(list=ls())
myUSArrests <- as.matrix(USArrests)
n <- nrow(myUSArrests)
# Center the data
i <- rep(1,n)
# i is a vector multiply with it's transpose will yield a 50 x 50 matrix
# This 50 x 50 matrix times myUSArrest yields a 50 x 4 matrix
# Each entry of this 50 x 4 matrix will have a value that equal to the corresponding 
# column total of the original myUSArrest matrix  
myUSArrests <- myUSArrests - i%*%t(i)%*%myUSArrests*(1/n)  
# After scaling we see that each column has a mean of 0
apply(myUSArrests,2,mean)


# Calculate the covariance matrix of the centered data
sigma1 <- t(myUSArrests) %*% myUSArrests/(n-1)
# Extract eigenvectors from this covariance matrix - these
# are the principal components of the centered x
(u1 <- eigen(sigma1)$vectors)
# Check using the prcomp() function - note that since we have
# not scaled the data, we specify scale=F to obtain the same result
(myprs <- prcomp(myUSArrests,scale=F))
# Compute the norms of the principal components
apply(myprs$rotation, 2, function(x) {sqrt(t(x) %*% x)})
# Change basis with first eigenvector (i.e.: project the
# two-dimensional points onto a one-dimensional line).
# These are the "scores" of the first principal component
z1 <- myUSArrests %*% u1[,1]


# Scale the data
myUSArrestsScaled <- myUSArrests %*% diag(1/(apply(myUSArrests,2,sd)))  
apply(myUSArrestsScaled,2,sd)
# Calculate the covariance matrix of the centered and scaled data, as before
(sigma2 <- t(myUSArrestsScaled) %*% myUSArrestsScaled/(n-1)) 
# Extract eigenvetocrs from this covariance matric - these
# are the principal components of the centered and scaled x
(u2 <- eigen(sigma2)$vectors)
# Check using the prcomp() function - note the scale=T parameter this time
(myprs.scaled  <-  prcomp(myUSArrests,scale=T))
# Change basis with first eigenvector (i.e.: project the
# two-dimensional points onto a one-dimensional line)
z2 <- myUSArrestsScaled %*% u2[,1]


# Plot the points before and after scaling
par(mfrow=c(2,1))
plot(z1,rep(0,n),xlim=c(min(z1),max(z1)),ylim=c(-1,1),type='p',axes=F,col='blue')
axis(1, pos=0)
plot(z2,rep(0,n),xlim=c(min(z2),max(z2)),ylim=c(-1,1),type='p',axes=F,col='blue')
axis(1, pos=0)