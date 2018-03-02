###Set up###
rm(list=ls())

installIfAbsentAndLoad  <-  function(neededVector) {
  if(length(neededVector) > 0) {
    for(thispackage in neededVector) {
      if(! require(thispackage, character.only = T)) {
        install.packages(thispackage)}
      require(thispackage, character.only = T)
    }
  }
}
installIfAbsentAndLoad(c('rpart', 'rattle', 'pROC', 'MASS'))


###get data and explore###                    
churndata <- read.table("churndata.csv",header=T,sep=",")
names(churndata)
str(churndata) 
summary(churndata)


###transform variables and deal with missing values###
churndata$area <- factor(churndata$area)
churndata <- na.omit(churndata)


###partition data into train and test subsets (80/20)###
set.seed(5082)
nobs <- nrow(churndata) 
trainrows <- sample(nobs, 0.8* nobs) 
testrows <- setdiff(1:nobs, trainrows)
train <- churndata[trainrows,]
test <- churndata[testrows,]


###create and examine classification model###
rpart <- rpart(churn ~ .,data=train, method="class",parms=list(split="information"),
               control=rpart.control(usesurrogate=0, maxsurrogate=0,cp=0, minsplit=2,minbucket=1))
printcp(rpart)
plotcp(rpart)


###access training error rates###
preds <- predict(rpart, newdata = test, type="class")
table.unpruned <- table(test$churn, preds, dnn=c("Actual", "Predicted"))
table.unpruned
(table.unpruned[1,2] + table.unpruned[2,1])/sum(table.unpruned)


###prune classification tree###
rpart$cptable
min.xerror <- which.min(rpart$cptable[,'xerror'])
min.cp <- rpart$cptable[min.xerror,"CP"]
min.cp
rpart.prune <- prune(rpart,cp=min.cp)
rpart.prune$cptable
fancyRpartPlot(rpart.prune, main="Pruned Customer Churn Prediction Model")


###access test error rate###
predict <- predict(rpart.prune, newdata=test, type="class")
table1 <- table(test$churn, predict, dnn=c("Actual", "Predicted"))
table1
(table1[1,2] + table1[2,1])/sum(table1)


###let's try using Gini instead of entropy and compare###
rpart.gini <- rpart(churn ~ .,data=train, method="class",parms=list(split="gini"),control=rpart.control(
  usesurrogate=0, maxsurrogate=0,cp=0, minsplit=2,minbucket=1))
rpart.gini$cptable
min.xerror <- which.min(rpart.gini$cptable[,'xerror'])
min.cp <- rpart.gini$cptable[min.xerror,"CP"]
min.cp
rpart.gini.prune <- prune(rpart.gini,cp=min.cp)
predict <- predict(rpart.gini.prune, newdata=test, type="class")
table.gini <- table(test$churn, predict, dnn=c("Actual", "Predicted"))
table.gini
(table.gini[1,2] + table.gini[2,1])/sum(table.gini)


###add loss matrix###
###6 is added to the type II error spot which indicates we would like to gether more data###
###when we predict no but it is actually yes###
rpart2 <- rpart(churn ~ ., data=train, method="class", parms=list(split="information"), control=rpart.control(usesurrogate=0, 
          maxsurrogate=0, cp=0, minsplit=2, minbucket=1, loss = matrix(c(0,1,6,0), byrow = T, nrow = 2)))
rpart2$cptable
min.xerror <- which.min(rpart2$cptable[,'xerror'])
min.cp <- rpart2$cptable[min.xerror,"CP"]
min.cp
rpart2.prune <- prune(rpart2,cp=min.cp)
fancyRpartPlot(rpart2.prune)

predict2 <- predict(rpart2.prune, newdata=test, type="class")
table2 <- table(test$churn, predict2, dnn=c("Actual", "Predicted"))
table2

###type = prob will yield probabilities###
###now predict3 would become a matrix with two columns, no and yes, that each###
###is predicting the probabilities of a test observation is belong to no or yes###
###then we will compare all the probabilities of no's to the response column of the test data set###
###we use the no column primarily bacause we are interested in type II error###
###if we are interested in the type I error, then we would use the yes column###
predict3 <- predict(rpart.prune, newdata=test, type="prob")
roc1 <- roc(test$churn, predict3[,1])
plot(roc1, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2), 
     grid.col=c("green", "red"), max.auc.polygon=TRUE,
     print.thres = TRUE, xaxs = "i", yaxs = "i", main = 'Churn ROC Curve')
###we construct a yes column then change the corresponding element to no after we adjust the probability###
###this is because we want to reduce the type II error###
predict4 <- rep('Yes', nrow(predict3))
predict4[predict3[,1] >= .860] <- 'No'
table3 <- table(test$churn, predict4, dnn = c("Actual", "Predicted"))
(table3[1,2] + table3[2,1])/sum(table3)
