rm(list=ls())
# If the response is a factor containing more than two levels
# then the svm() function will perform multi-class classiﬁcation 
# using the one-versus-one approach
require(e1071)
set.seed(1)
x = matrix(rnorm(200*2), ncol=2)
x = rbind(x, matrix(rnorm(50*2), ncol=2))
y = c(rep(1,150), rep(2,50))
y = c(y, rep(0,50))
x[y==0,2] = x[y==0,2]+2
dat = data.frame(x=x, y=as.factor(y)) 
par(mfrow=c(1,1)) 
plot(x,col=(y+1))
#
# we now fit an SVM to the data
svmfit = svm(y~., data = dat, kernel = 'radial', cost = 10, gamma = 1)
plot(svmfit, dat)
