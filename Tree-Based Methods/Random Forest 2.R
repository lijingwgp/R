### Load packages
rm(list=ls())
installIfAbsentAndLoad <- function(neededVector) {
  for(thispackage in neededVector) {
    if( ! require(thispackage, character.only = TRUE) )
    { install.packages(thispackage)}
    require(thispackage, character.only = TRUE)
  }
}
needed = c("randomForest", "pROC", "verification", "rpart")
installIfAbsentAndLoad(needed)
require(randomForest)


###Import the data
churndata<-read.table("churndata.csv",sep=",",header=T)


###Clean up, change area code to a factor
data <- na.omit(churndata)
str(data)
data$area<-factor(data$area)


###Create training and test sets
nobs <- nrow(data)
set.seed(527)
train <- sample(nobs, 0.7*nobs)


###Grow a 500-tree forest
rf <- randomForest(formula=churn ~ .,data=data[train,],ntree=500,mtry=4,
	importance=TRUE,localImp=TRUE,na.action=na.roughfix,replace=FALSE)
head(rf$predicted,25)
#mtry is the number of predictors randomly sampled as candidates at each split
#importance determines whether variable importance is assessed
#localimp: should casewise importance measure be computed? (Setting this to TRUE will override importance.)
#na.action is specified method for dealing with NA's in the data
#na.roughfix replaces NA's with column medians


###Display Variable Importance   
importance(rf)[order(importance(rf)[,"MeanDecreaseAccuracy"], decreasing=T),]
#MeanDecreaseAccuracy indicates the average number of observations that would be misclassified by removing the variable from the model.


###Examine Error Rates for the number of trees
#head(rf$err.rate)
plot(rf, main="Error Rates for Random Forest")
legend("topright", c("OOB", "No", "Yes"), text.col=1:6,lty=1:3,col=1:3)
min.err <- min(rf$err.rate[,"OOB"])
min.err.idx <- which(rf$err.rate[,"OOB"]==min.err)
min.err.idx
rf$err.rate[min.err.idx,]


###Rebuild the forest with the number of trees that minimizes the OOB error rate - use the first one if there is more than one minimum.
set.seed(460)
rf <- randomForest(formula=churn ~ .,data=data[train,],ntree= min.err.idx,mtry=4,
	importance=TRUE,localImp=TRUE,na.action=na.roughfix,replace=FALSE)


###Look at voting info for each observation
head(rf$votes)


###Plot the OOB ROC curve and calculate AUC
require(pROC)               #required for roc.plot
require(verification)       #required for roc.area
aucc <- roc.area(as.integer(as.factor(data[train, "churn"]))-1,rf$votes[,2])
aucc$A
aucc$p.value                #null hypothesis: aucc=0.5 
#windows()                   #open a new graphics window
roc.plot(as.integer(as.factor(data[train,"churn"]))-1,rf$votes[,2], main="", xlab = "False Positive Rate", ylab = "True Positive Rate")
legend("bottomright", bty="n",
       sprintf("Area Under the Curve (AUC) = %1.3f", aucc$A))
title(main="OOB ROC Curve Random Forest churndata.csv",
    sub=paste("David Murray", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))


###Evaluate by scoring the training set
prtrain <- predict(rf, newdata=data[train,])
table(data[train,"churn"], prtrain,dnn=c("Actual", "Predicted"))
round(100* table(data[train,"churn"], prtrain,dnn=c("% Actual", "% Predicted"))/length(prtrain))


###Evaluate by scoring the test set
test <- setdiff(1:nobs, train)
prtest <- predict(rf, newdata=na.omit(data[test,]))
table(data[test,"churn"], prtest,dnn=c("Actual", "Predicted"))
round(100* table(data[test,"churn"], prtest,dnn=c("% Actual", "% Predicted"))/length(prtest),1)


###Change vote cutoff to reduce Type II Error Rate (at the expense of the Type I Error Rate)
set.seed(527)
rfLowerT2Error <- randomForest(formula=churn ~ .,data=data[train,],ntree=500, mtry=4,
             importance=TRUE,localImp=TRUE,na.action=na.roughfix,replace=FALSE,cutoff=c(0.8,0.2))
rfLowerT2Error


###Evaluate by scoring the test set
prtestLowerT2Error <- predict(rfLowerT2Error, newdata=na.omit(data[test,]))
table(data[test,"churn"], prtestLowerT2Error ,dnn=c("Actual", "Predicted"))
round(100* table(data[test,"churn"], prtestLowerT2Error ,dnn=c("% Actual", "% Predicted"))/length(prtestLowerT2Error))
