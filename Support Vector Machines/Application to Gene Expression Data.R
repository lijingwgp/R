rm(list=ls()) 
require(ISLR)
require(e1071)
# we now examine the Khan data set. the data set consists of training data, 
# xtrain and ytrain, and testing data, xtest and ytest
#
# we examine the dimension of the data
names(Khan)
dim(Khan$xtrain)
dim(Khan$xtest)
# now examine the number of observations that belong to each class in the training
# and the test set
table(Khan$ytrain)
table(Khan$ytest)
# note that in this data set, there are very large number of features relative to 
# the number of observations. this suggests that we should use a linear kernel in
# order to avoid overfitting issue
dat = data.frame(x=Khan$xtrain, y=as.factor(Khan$ytrain))
svmlinear = svm(y~., data = dat, kernel = 'linear', cost = 10)
summary(svmlinear)
table(svmlinear$fitted, dat$y)
# we see that there is no training error. this is due to the large number of variables
# relative to the number of observations. this implies that it is easy to find 
# hyperplanes that fully seperates the classes
#
# we are most interested not in the support vector classidier's performance on the 
# training set, but rather its performance on the test observations
dat.test = data.frame(x=Khan$xtest, y=as.factor(Khan$ytest))
pred = predict(svmlinear, dat.test)
table(pred, dat.test$y)
