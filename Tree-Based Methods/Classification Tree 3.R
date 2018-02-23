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
installIfAbsentAndLoad(c('rpart', 'rattle'))
weather<-read.table("myweatherdata.csv",sep=",",header=T)


###Discard unwanted columns###
names(weather)
discardcols <- names(weather) %in% c("Date", "Location","RISK_MM")
weather<- weather[!discardcols]
names(weather)


###Build train with 80% and test set with 20% of indexes###
set.seed(5082)
train <- sample(1:nrow(weather), .8*nrow(weather))
weather.train <- weather[train,]
weather.test <- weather[-train,]


###Build a maximal model, print the cptable, and plot the cp as well###
mymodel <- rpart(RainTomorrow~.,data=weather[train,],method="class",
    parms=list(split="information"),control=rpart.control(usesurrogate=0,
    maxsurrogate=0,cp=0,minbucket=1,minsplit=2))
print(mymodel$cptable)
plotcp(mymodel)


###Prune the maximal model to minimize error, then plot the pruned model###
mymodel$cptable
xerr <- mymodel$cptable[,"xerror"]
minxerr <- which.min(xerr)
mincp <- mymodel$cptable[minxerr,"CP"]
mymodel.prune <- prune(mymodel,cp=mincp)
mymodel.prune$cptable
fancyRpartPlot(mymodel.prune, main="Decision Tree With Minimum C.V. Error")


###Use test set to predict, then make a confusion matrix to evaluate how it did###
mymodel.prune.predict <- predict(mymodel.prune, newdata=weather.test, type="class")
table(weather.test$RainTomorrow, mymodel.prune.predict,dnn=c("Actual", "Predicted"))


###Recreate decision tree with loss matrix###
mymodel.loss <- rpart(RainTomorrow~.,data=weather.train, method="class",
                 parms=list(split="information"),control=rpart.control(usesurrogate=0,
                 maxsurrogate=0,cp=0,minbucket=1,minsplit=2,loss=matrix(c(0,1,10,0), byrow=TRUE, nrow=2)))
mymodel.loss$cptable
xerr <- mymodel.loss$cptable[,"xerror"]
minxerr <- which.min(xerr)
mincp <- mymodel.loss$cptable[minxerr,"CP"]
mymodel.loss.prune <- prune(mymodel.loss, cp=mincp)
mymodel.loss.prune$cptable


###Predict on the test set
mymodel.loss.predict <- predict(mymodel.loss.prune, newdata=weather.test, type="class")
table(weather.test$RainTomorrow, mymodel.loss.predict,dnn=c("Actual", "Predicted"))
