rm(list = ls()) 
library(rpart)
library(rattle)
library(pROC)
library(randomForest)
library(ada)
library(e1071)
churn <- read.csv("Assignment2TrainingData.csv", header = T, sep = ",")
churn <- na.omit(churn)
churn <- churn[-1]
names(churn)
str(churn)
summary(churn)




########################## 
#### Classification Tree
########################## 
## Tune minsplit, and minbucket 
## Then determine the corresponding test error rate
err = matrix(nrow = 10,ncol = 10,byrow = TRUE)
folds <- 12
fold.indices <- cut(1:nrow(churn), breaks = folds, labels = FALSE)
for(i in 1:10){
  for(j in 1:10){
    cv.err = c()
    for(k in 1:10){
      fold.train <- sample(1:12,4)
      train.indices = c()
      for (z in 1:length(fold.train)) {
        temp <- which(fold.indices == fold.train[z])
        train.indices = append(train.indices, temp)
      }
      test.data <- churn[-train.indices,]
      train.data <- churn[train.indices,]
      original.tree <- rpart(Churn~., data = train.data, method = "class", parms = list(split="information"),
                             control = rpart.control(cp = 0, minsplit = i, minbucket = j))
      min.xerror <- which.min(original.tree$cptable[,"xerror"])
      min.cp <- original.tree$cptable[min.xerror,"CP"]
      best.tree <- prune(original.tree, cp = min.cp)
      preds <- predict(best.tree, newdata = test.data, type = "class")
      table.classfication <- table(test.data$Churn, preds)
      cv.err[k] <- (table.classfication[1,2] + table.classfication[2,1])/sum(table.classfication)
    }
    err[i,j] = mean(cv.err)
  }
}
bestsplit = which(err == min(err), arr.ind = TRUE)[1]
bestbucket = which(err == min(err), arr.ind = TRUE)[2]
print("Now we have only 1 combination of minsplit and minbucket that will yield the lowest test error rate")
print(paste("This combination appeared in row",which(err == min(err),arr.ind = TRUE)[1],"column",which(err == min(err),arr.ind = TRUE)[2]))
print(paste("It means that the lowest test error rate occured when minsplit=",which(err == min(err),arr.ind = TRUE)[1], "minbucket=",which(err == min(err),arr.ind = TRUE)[2]))
print(paste("The lowest test error rate appeared to be,",min(err)))


## Determine the best cutoff point
step.size <- seq(0,1,by=0.01)
folds <- 10
fold.indices <- cut(1:nrow(churn), breaks = folds, labels = FALSE)
results1 = c()
for(i in 1:length(step.size)){
  TP <- c()
  TN <- c()
  FP <- c()
  FN <- c()
  Overall <- c()
  for(j in 1:folds){
    test.indices <- which(fold.indices == j)
    test.data <- churn[test.indices,]
    train.data <- churn[-test.indices,]
    original.tree <- rpart(train.data$Churn~., data = train.data, method = "class", parms = list(split="information"),
                           control = rpart.control(cp = 0, minsplit = bestsplit, minbucket = bestbucket))
    min.xerror <- which.min(original.tree$cptable[,"xerror"])
    min.cp <- original.tree$cptable[min.xerror,"CP"]
    best.tree <- prune(original.tree, cp=min.cp)
    preds <- predict(best.tree, newdata = test.data, type = "prob")
    preds.vec <- rep("No", nrow(preds))
    preds.vec[preds[,2] >= step.size[i]] <- 'Yes'
    table.combination <- table(factor(test.data$Churn, levels=c("No", "Yes")), factor(preds.vec, levels=c("No", "Yes")))
    Overall[j] <- (table.combination[1,2] + table.combination[2,1])/sum(table.combination)
    TP[j] <- table.combination[2,2]/sum(table.combination[2,])
    TN[j] <- table.combination[1,1]/sum(table.combination[1,])
    FP[j] <- table.combination[1,2]/sum(table.combination[1,])
    FN[j] <- table.combination[2,1]/sum(table.combination[2,])
  }
  temp <- c(step.size[i],mean(TP),mean(TN),mean(FP),mean(FN),mean(Overall))
  results1 <- rbind(results1,temp)
}


## Determine expected cost for each customer
results1 <- cbind(results1, rep(0,nrow(results1)))
colnames(results1) <- c("Cutoff","TP","TN","FP","FN","Overall Error Rate","Expected Cost")
for (i in 1:nrow(results1)){
  results1[i,7] = (1-0.26448)*results1[i,4]*1600 + 
    0.26448*results1[i,5]*11500 + 
    0.26448*results1[i,2]*1600*0.45 + 
    (1600+11500)*0.55*0.26448*results1[i,2]
}
plot(1:nrow(results1), results1[,7], type = "l")
write.csv(results1,"classification tree cost.csv")




################################ 
#### Random Forest
################################
## Choose the optimum number of predictors
numPred = seq(1,7,1)
err.rf = matrix(nrow = 7,ncol = 1,byrow = TRUE)
folds <- 12
fold.indices <- cut(1:nrow(churn), breaks = folds, labels = FALSE)
for (i in 1:length(numPred)){
  cv.err = c()
  for(k in 1:10){
    fold.train <- sample(1:12,4)
    train.indices = c()
    for (z in 1:length(fold.train)) {
      temp <- which(fold.indices == fold.train[z])
      train.indices = append(train.indices, temp)
    }
    test.data <- churn[-train.indices,]
    train.data <- churn[train.indices,]
    churn.rf <- randomForest(train.data$Churn~., data = train.data, mtry = numPred[i], localImp = TRUE, 
                             na.action = na.roughfix, replace=TRUE, ntree = 1000)
    min.err.idx <- which.min(churn.rf$err.rate[,"OOB"])
    churn.rf <- randomForest(train.data$Churn~., data = train.data, mtry = numPred[i], localImp = TRUE, 
                             na.action = na.roughfix, replace=TRUE, ntree = min.err.idx)
    preds <- predict(churn.rf, newdata = test.data, type = "class")
    table.rf <- table(test.data$Churn, preds)
    cv.err[k] <- (table.rf[1,2] + table.rf[2,1])/sum(table.rf)
  }
  err.rf[i,1] = mean(cv.err)
}
mtry = which.min(err.rf)
print(paste("The lowest test error rate is",min(err.rf),"and it occured when mtry =",which.min(err.rf)))


## Determine the best cutoff point and expected cost per customer
step.size <- seq(0,1,by=0.01)
folds <- 5
fold.indices <- cut(1:nrow(churn), breaks = folds, labels = FALSE)
results2 = c()
for(i in 1:length(step.size)){
  TP <- c()
  TN <- c()
  FP <- c()
  FN <- c()
  Overall <- c()
  for(j in 1:folds){
    test.indices <- which(fold.indices == j)
    test.data <- churn[test.indices,]
    train.data <- churn[-test.indices,]
    best.rf <- randomForest(train.data$Churn~., data = train.data, mtry = mtry, localImp = TRUE, 
                            na.action = na.roughfix, replace=TRUE, ntree = 1000)
    min.err.idx <- which.min(best.rf$err.rate[,"OOB"])
    best.rf <- randomForest(train.data$Churn~., data = train.data, mtry = mtry, localImp = TRUE, 
                            na.action = na.roughfix, replace=TRUE, ntree = min.err.idx)
    preds <- predict(best.rf, newdata = test.data, type = "prob")
    preds.vec <- rep("No", nrow(preds))
    preds.vec[preds[,2] >= step.size[i]] <- 'Yes'
    table.combination <- table(factor(test.data$Churn, levels=c("No", "Yes")), factor(preds.vec, levels=c("No", "Yes")))
    Overall[j] <- (table.combination[1,2] + table.combination[2,1])/sum(table.combination)
    TP[j] <- table.combination[2,2]/sum(table.combination[2,])
    TN[j] <- table.combination[1,1]/sum(table.combination[1,])
    FP[j] <- table.combination[1,2]/sum(table.combination[1,])
    FN[j] <- table.combination[2,1]/sum(table.combination[2,])
  }
  temp <- c(step.size[i],mean(TP),mean(TN),mean(FP),mean(FN),mean(Overall))
  results2 <- rbind(results2,temp)
}
results2 <- cbind(results2, rep(0,nrow(results2)))
colnames(results2) <- c("Cutoff","TP","TN","FP","FN","Overall Error Rate","Expected Cost")
for (i in 1:nrow(results2)){
  results2[i,7] = (1-0.26448)*results2[i,4]*1600 + 
    0.26448*results2[i,5]*11500 + 
    0.26448*results2[i,2]*1600*0.45 + 
    (1600+11500)*0.55*0.26448*results2[i,2]
}
plot(1:nrow(results2), results2[,7], type = "l")
write.csv(results2,"random forest cost.csv")




################################ 
#### Boosting
################################
## determine the optimal depth for each tree
depth = seq(1,7,1)
err.depth = matrix(nrow = 7,ncol = 1,byrow = TRUE)
folds <- 12
fold.indices <- cut(1:nrow(churn), breaks = folds, labels = FALSE)
for (i in 1:length(depth)){
  cv.err = c()
  for(k in 1:5){
    fold.train <- sample(1:12,4)
    train.indices = c()
    for (z in 1:length(fold.train)) {
      temp <- which(fold.indices == fold.train[z])
      train.indices = append(train.indices, temp)
    }
    test.data <- churn[-train.indices,]
    train.data <- churn[train.indices,]
    churn.boost <- ada(train.data$Churn ~ .,data=train.data,iter=50,bag.frac=.5,
                                            control=rpart.control(maxdepth=depth[i],cp=min.cp,minsplit=bestsplit,minbucket=bestbucket,xval=10))
    preds <- predict(churn.boost, newdata = test.data, type = "class")
    table.boost <- table(test.data$Churn, preds)
    cv.err[k] <- (table.boost[1,2] + table.boost[2,1])/sum(table.boost)
  }
  err.depth[i,1] = mean(cv.err)
}
bestdepth = which.min(err.depth)
print(paste("The lowest test error rate is",min(err.depth),"and it occured when depth =",which.min(err.depth),"for each tree"))


## determine the optimal bag fraction
bag = seq(0,1,.1)
err.bag = matrix(nrow = 11,ncol = 1,byrow = TRUE)
folds <- 12
fold.indices <- cut(1:nrow(churn), breaks = folds, labels = FALSE)
for (i in 1:length(bag)){
  cv.err = c()
  for(k in 1:5){
    fold.train <- sample(1:12,4)
    train.indices = c()
    for (z in 1:length(fold.train)) {
      temp <- which(fold.indices == fold.train[z])
      train.indices = append(train.indices, temp)
    }
    test.data <- churn[-train.indices,]
    train.data <- churn[train.indices,]
    churn.boost <- ada(train.data$Churn ~ .,data=train.data,iter=50,bag.frac=bag[i],
                       control=rpart.control(maxdepth=bestdepth,cp=min.cp,minsplit=bestsplit,minbucket=bestbucket,xval=10))
    preds <- predict(churn.boost, newdata = test.data, type = "class")
    table.boost <- table(test.data$Churn, preds)
    cv.err[k] <- (table.boost[1,2] + table.boost[2,1])/sum(table.boost)
  }
  err.bag[i,1] = mean(cv.err)
}
bestbag = (which.min(err.bag)-1)/10
print(paste("The lowest test error rate is",min(err.bag),"and it occured when bag.frac =",bestbag))


## determine the optimal learning rate
learning = seq(1,0.01,length.out = 10)
err.learning = matrix(nrow = length(learning),ncol = 1,byrow = TRUE)
folds <- 12
fold.indices <- cut(1:nrow(churn), breaks = folds, labels = FALSE)
for(i in 1:length(learning)){
  cv.err = c()
  for(k in 1:5){
    fold.train <- sample(1:12,4)
    train.indices = c()
    for(z in 1:length(fold.train)) {
      temp <- which(fold.indices == fold.train[z])
      train.indices = append(train.indices, temp)
    }
    test.data <- churn[-train.indices,]
    train.data <- churn[train.indices,]
    churn.boost <- ada(train.data$Churn ~ .,data=train.data,iter=100,bag.frac=bestbag,nu=learning[i],
                       control=rpart.control(maxdepth=bestdepth,cp=min.cp,minsplit=bestsplit,minbucket=bestbucket,xval=10))
    preds <- predict(churn.boost, newdata = test.data, type = "class")
    table.boost <- table(test.data$Churn, preds)
    cv.err[k] <- (table.boost[1,2] + table.boost[2,1])/sum(table.boost)
  }
  err.learning[i,1] = mean(cv.err)
}
bestlearning = which.min(err.learning)
bestlearning = learning[bestlearning]
print(paste("The lowest test error rate is",min(err.learning),"and it occured when nu =",bestlearning))


## determine the optimal number of trees to grow
ntrees = seq(200,2000,length.out = 10)
err.trees = matrix(nrow = length(ntrees),ncol = 1,byrow = TRUE)
folds <- 12
fold.indices <- cut(1:nrow(churn), breaks = folds, labels = FALSE)
for(i in 1:length(ntrees)){
  cv.err = c()
  for(k in 1:5){
    fold.train <- sample(1:12,4)
    train.indices = c()
    for(z in 1:length(fold.train)) {
      temp <- which(fold.indices == fold.train[z])
      train.indices = append(train.indices, temp)
    }
    test.data <- churn[-train.indices,]
    train.data <- churn[train.indices,]
    churn.boost <- ada(train.data$Churn ~ .,data=train.data,iter=ntrees[i],bag.frac=bestbag,nu=bestlearning,
                       control=rpart.control(maxdepth=bestdepth,cp=min.cp,minsplit=bestsplit,minbucket=bestbucket,xval=10))
    preds <- predict(churn.boost, newdata = test.data, type = "class")
    table.boost <- table(test.data$Churn, preds)
    cv.err[k] <- (table.boost[1,2] + table.boost[2,1])/sum(table.boost)
  }
  err.trees[i,1] = mean(cv.err)
}
besttrees = which.min(err.trees)
besttrees = ntrees[besttrees]
print(paste("The lowest test error rate is",min(err.trees),"and it occured when iter =",besttrees))


## determine the best cutoff point and expected cost per customer
step.size <- seq(0,1,by=0.01)
folds <- 3
fold.indices <- cut(1:nrow(churn), breaks = folds, labels = FALSE)
results3 = c()
for(i in 1:length(step.size)){
  TP <- c()
  TN <- c()
  FP <- c()
  FN <- c()
  Overall <- c()
  for(j in 1:folds){
    test.indices <- which(fold.indices == j)
    test.data <- churn[test.indices,]
    train.data <- churn[-test.indices,]
    best.boost <- ada(train.data$Churn ~ .,data=train.data,iter=besttrees,bag.frac=bestbag,nu=bestlearning,
                      control=rpart.control(maxdepth=bestdepth,cp=min.cp,minsplit=bestsplit,minbucket=bestbucket,xval=3))
    preds <- predict(best.boost, newdata = test.data, type = "prob")
    preds.vec <- rep("No", nrow(preds))
    preds.vec[preds[,2] >= step.size[i]] <- 'Yes'
    table.combination <- table(factor(test.data$Churn, levels=c("No", "Yes")), factor(preds.vec, levels=c("No", "Yes")))
    Overall[j] <- (table.combination[1,2] + table.combination[2,1])/sum(table.combination)
    TP[j] <- table.combination[2,2]/sum(table.combination[2,])
    TN[j] <- table.combination[1,1]/sum(table.combination[1,])
    FP[j] <- table.combination[1,2]/sum(table.combination[1,])
    FN[j] <- table.combination[2,1]/sum(table.combination[2,])
  }
  temp <- c(step.size[i],mean(TP),mean(TN),mean(FP),mean(FN),mean(Overall))
  results3 <- rbind(results3,temp)
}
results3 <- cbind(results3, rep(0,nrow(results3)))
colnames(results3) <- c("Cutoff","TP","TN","FP","FN","Overall Error Rate","Expected Cost")
for (i in 1:nrow(results3)){
  results3[i,7] = (1-0.26448)*results3[i,4]*1600 + 
    0.26448*results3[i,5]*11500 + 
    0.26448*results3[i,2]*1600*0.45 + 
    (1600+11500)*0.55*0.26448*results3[i,2]
}
plot(1:nrow(results3), results3[,7], type = "l")
write.csv(results3,"boosting cost.csv")

