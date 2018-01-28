rm(list=ls())
require(e1071)
# In order to ﬁt an SVM using a non-linear kernel, we once again use the svm() 
# function. However, now we use a diﬀerent value of the parameter kernel.
#
# To ﬁt an SVM with a polynomial kernel we use kernel="polynomial",
# and to ﬁt an SVM with a radial kernel we use kernel="radial".
# 
# In the polynomial case we also use the degree argument to specify a 
# degree for the polynomial kernel.
set.seed(1)
x = matrix(rnorm(200*2), ncol=2) 
x[1:100,] = x[1:100,]+2
x[101:150,] = x[101:150,]-2 
y = c(rep(1,150), rep(2,50)) 
dat = data.frame(x=x, y=as.factor(y))
# Plotting the data makes it clear that the class boundary is indeed nonlinear:
plot(x, col=y)
# The data is randomly split into training and testing groups. 
# We then ﬁt the training data using the svm() function with a radial kernel and γ = 1:
train = sample(200,100) 
svmfit = svm(y~., data=dat[train,], kernel="radial", gamma=1, cost=1) 
plot(svmfit, dat[train,])
summary(svmfit)
# We can see from the ﬁgure that there are a fair number of 
# training errors in this SVM ﬁt.
#
# If we increase the value of cost, we can reduce the number of training errors. 
#
# However, this comes at the price of a more irregular decision boundary that 
# seems to be at risk of overﬁtting the data. 
svmfit=svm(y~., data=dat[train,], kernel="radial", gamma=1, cost=1e5)
plot(svmfit, dat[train,])
# We can perform cross-validation using tune() to 
# select the best choice of γ and cost for an SVM with a radial kernel:
set.seed(1)
tune.out = tune(svm, y~., data=dat[train,], kernel="radial", ranges=list(cost=c(0.1,1,10,100,1000), gamma=c(0.5,1,2,3,4))) 
summary(tune.out)
# Therefore, the best choice of parameters involves cost=1 and gamma=2. 
# We can view the test set predictions for this model by applying the predict() 
table(true=dat[-train,"y"], pred=predict(tune.out$best.model, newx=dat[-train,]))
# 39% of test observations are misclassiﬁed by this SVM.


