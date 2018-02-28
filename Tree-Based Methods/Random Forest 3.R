rm(list=ls())
installIfAbsentAndLoad <- function(neededVector) {
  for(thispackage in neededVector) {
    if( ! require(thispackage, character.only = TRUE) )
    { install.packages(thispackage)}
    require(thispackage, character.only = TRUE)
  }
}
needed = c("randomForest", "pROC", "verification")
installIfAbsentAndLoad(needed)


###Import the csv
###We are trying to predict whether it will rain tomorrow based on the variable RainTomorrow.
weather<-read.table("myWeatherData.csv",sep=",",header=T)
dim(weather)


###Discard unwanted columns
names(weather)
discardcols <- names(weather) %in% c("Date", "Location","RISK_MM")
weather<- weather[!discardcols]
names(weather)
#Q1: Build 70% training vector of indices and 30% test data frame named weather.test
set.seed(42)
train<-sample(1:nrow(weather), 0.7*nrow(weather))
weather.test<-weather[-train,]


###Grow an initial random forest of 500 trees
###Observe performance of the forest
require(randomForest)
set.seed(42)
rf1 <- randomForest(RainTomorrow ~ .,data=weather[train,],ntree=500,mtry=4, 
                   importance=TRUE, na.action=na.roughfix,replace=FALSE) 
rf1


###New forest, with an additional argument: specify values for equal class representation in stratified random sample 
###of 70 observations - 35/35 split per sample
set.seed(42)
rf2 <- randomForest(RainTomorrow ~ .,data=weather[train,], ntree=500,mtry=4, 
                   importance=TRUE, na.action=na.roughfix, replace=FALSE, sampsize=c(35,35))
rf2
###sampsize = Size(s) of sample to draw. For classification, if sampsize is a vector of the length the number of strata, 
###then sampling is stratified by strata, and the elements of sampsize indicate the numbers to be drawn from the strata
#Q2:  Display Variable Importance of this forest: 
importance(rf2)[order(importance(rf2)[,"MeanDecreaseAccuracy"], decreasing=T),]


###Examine Error Rates for the number of trees
head(rf$err.rate)
plot(rf2, main="Error Rates for Random Forest")
legend("topright", c("OOB", "No", "Yes"), text.col=1:6, lty=1:3, col=1:3)
#Q3:  Find the minimum OOB (out-of-bag) error:
min.err.idx <- which.min(rf2$err.rate[,"OOB"])
min.err.idx
rf2$err.rate[min.err.idx,]
#Q4:  Rebuild the forest with the number of trees that minimizes the OOB error rate, and observe its performance.
set.seed(460)
rf3 <- randomForest(formula=RainTomorrow ~ .,data=weather[train,],ntree= min.err.idx, mtry=4,
                   importance=TRUE,localImp=TRUE,na.action=na.roughfix,replace=FALSE, sampsize = c(35,35))
rf3
#Q5:  Evaluate by scoring the training set
prtrain <- predict(rf3, newdata=weather[train,])
table(weather[train,"RainTomorrow"], prtrain,dnn=c("Actual", "Predicted"))
round(100* table(weather[train,"RainTomorrow"], prtrain,dnn=c("% Actual", "% Predicted"))/length(prtrain))
#Q6:  Evaluate by scoring the test set
prtest <- predict(rf3, newdata=weather.test)
table(weather.test[,"RainTomorrow"], prtest,dnn=c("Actual", "Predicted"))
round(100* table(weather.test[,"RainTomorrow"], prtest,dnn=c("% Actual", "% Predicted"))/length(prtest),1)
