rm(list=ls())
installIfAbsentAndLoad <- function(neededVector) {
  for(thispackage in neededVector) {
    if( ! require(thispackage, character.only = T) )
    { install.packages(thispackage)}
    require(thispackage, character.only = T)
  }
} 
needed <- c("ada","e1071","randomForest","gbm","xgboost","MASS",'tree')  
installIfAbsentAndLoad(needed)
library(ada) # ada() to do adaboost
library(e1071) # to use tune() method
library(randomForest)
library(gbm)



### read data and separate data into train and test set
churndata<-read.table("churndata.csv",sep=",",header=T)
data <- na.omit(churndata)
nobs <- nrow(data)
data<-data[-3]
set.seed(5082)
train <- sample(nobs, 0.7*nobs)
test <- setdiff(1:nobs, train) 



### adaboost (classification)
#
# in adaboost function, iter means how many trees to build, bag.frac means proportion of observation in bag, 
# maxdepth is the maximum depth of trees being built, cp is complexity parameter, minsplit is the minimum number of observations in a region
# xval is the number of cross-validations
set.seed(5082)
bm<- ada(formula=churn ~ .,data=data[train,],iter=50,bag.frac=0.5,control=rpart.control(maxdepth=30,
cp=0.01,minsplit=20,xval=10))
#
# evaluate by scoring the training set
prtrain <- predict(bm, newdata=data[train,])
table(data[train,"churn"], prtrain,dnn=c("Actual", "Predicted"))
round(100* table(data[train,"churn"], prtrain,dnn=c("% Actual", "% Predicted"))/length(prtrain),1)
#
# evaluate by scoring the test set
prtest <- predict(bm, newdata=data[test,])
table(data[test,"churn"], prtest,dnn=c("Actual", "Predicted"))
round(100* table(data[test,"churn"], prtest,dnn=c("% Actual", "% Predicted"))/length(prtest),1)
#
# train: 86.1+9.0=95.1
# test: 83.4+7.8=91.2
#
# tuning data, it will be very very slow, the results are shown below
set.seed(5082)
bm.tune<-tune(ada,churn~.,data=data[train,],ranges = list(iter=c(50,100,200,500),nu=c(0.01,0.1,1,2)),bag.frac=0.5,control=rpart.control(maxdepth=30,cp=0.01,minsplit=20,xval=10))
summary(bm.tune)
#
#    iter   nu      error dispersion
# 1    50 0.01 0.07418657 0.02332082
# 2   100 0.01 0.07461575 0.02053787
# 3   200 0.01 0.06989656 0.01884290
# 4   500 0.01 0.06946554 0.01802658
# 5    50 0.10 0.07203698 0.01625063
# 6   100 0.10 0.07418473 0.01938185
# 7   200 0.10 0.07674517 0.01500266
# 8   500 0.10 0.08017681 0.01782308
# 9    50 1.00 0.09818239 0.02057547
# 10  100 1.00 0.09518176 0.02018882
# 11  200 1.00 0.09603279 0.01715224
# 12  500 1.00 0.09131543 0.01757316
# 13   50 2.00 0.15905873 0.03525104
# 14  100 2.00 0.14276806 0.03337465
# 15  200 2.00 0.13846521 0.02637330
# 16  500 2.00 0.15515021 0.01755256
#
bestmodel=bm.tune$best.model
summary(bestmodel)
#
# evaluate by scoring the training set
prtrain <- predict(bestmodel, newdata=data[train,])
table(data[train,"churn"], prtrain,dnn=c("Actual", "Predicted"))
round(100* table(data[train,"churn"], prtrain,dnn=c("% Actual", "% Predicted"))/length(prtrain),1)
#
# evaluate by scoring the test set
prtest <- predict(bestmodel, newdata=data[test,])
table(data[test,"churn"], prtest,dnn=c("Actual", "Predicted"))
round(100* table(data[test,"churn"], prtest,dnn=c("% Actual", "% Predicted"))/length(prtest),1)
#
# train 86.1+9.1=95.2
# test 83.4+8.1=91.5



### gbm classification
#
# to use gbm to build boosting trees for classification, one needs to convert response value to 0 and 1
# and in gbm method, one needs to add parameter distribution='bernoulli'
set.seed(5082)
gbm.data<-data
gbm.data$churn=as.character(gbm.data$churn)
for (i in 1:nrow(gbm.data)){
  if (gbm.data[i,1]=="Yes"){
    gbm.data[i,1]=1
  } else {
    gbm.data[i,1]=0
  }
}
gbm.data$churn=as.numeric(gbm.data$churn)
#
# gbm bernoulli
gbmmodel<-gbm(churn~.,distribution = 'bernoulli',data=gbm.data[train,],n.trees=50,shrinkage=1,train.fraction=0.5)
summary(gbmmodel)
# relative influence of different predictors
#
# evaluate by scoring the training set
prtrain <- predict(gbmmodel, newdata=gbm.data[train,],n.trees=50,type='response')
for (i in 1:length(prtrain)){
  if (prtrain[i]<0.5){
    prtrain[i]=0
  } else {
    prtrain[i]=1
  }
}
table(gbm.data[train,"churn"], prtrain,dnn=c("Actual", "Predicted"))
round(100* table(gbm.data[train,"churn"], prtrain,dnn=c("% Actual", "% Predicted"))/length(prtrain),1)
#
# evaluate by scoring the test set
prtest <- predict(gbmmodel, newdata=gbm.data[test,],n.trees = 50,type = 'response')
for (i in 1:length(prtest)){
  if (prtest[i]<0.5){
    prtest[i]=0
  } else {
    prtest[i]=1
  }
}
table(gbm.data[test,"churn"], prtest,dnn=c("Actual", "Predicted"))
round(100* table(gbm.data[test,"churn"], prtest,dnn=c("% Actual", "% Predicted"))/length(prtest),1)
#
# train 83.0+5.2=88.2
# test 81.3+4.6=85.9
