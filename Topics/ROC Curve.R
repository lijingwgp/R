rm(list=ls())
# We ???rst write a short function to plot an ROC curve given a vector 
# containing a numerical score for each observation, pred, 
# and a vector containing the class label for each observation, truth.
require(e1071)
require(ROCR)
rocplot = function(pred, truth, ...){
  predob = prediction(pred, truth)
  perf = performance(predob, 'tpr', 'fpr')
  plot(perf, ...)
}

svmfit.opt = svm(y~., data=dat[train,], kernel ="radial", gamma=2, cost=1, decision.values=T)
fitted = attributes(predict(svmfit.opt, dat[train,], decision.values=TRUE))$decision.values
par(mfrow=c(1,2)) 
rocplot(fitted, dat[train,"y"], main="Training Data")

svmfit.flex = svm(y~., data=dat[train,], kernel="radial", gamma=50, cost=1, decision.values=T) 
fitted = attributes(predict(svmfit.flex, dat[train,], decision.values=T))$decision.values 
rocplot(fitted, dat[train,"y"], add=T, col="red")

fitted = attributes(predict(svmfit.opt, dat[-train,], decision.values=T))$decision.values 
rocplot(fitted, dat[-train,"y"], main="Test Data") 

fitted = attributes(predict(svmfit.flex, dat[-train,], decision.values=T))$decision.values 
rocplot(fitted, dat[-train,"y"], add=T, col="red")

