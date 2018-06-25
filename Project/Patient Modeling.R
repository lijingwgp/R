rm(list=ls())
require(dplyr)
require(glmnet)
require(boot)
require(randomForest)



########################################## 
#### Finalize Data Types and Problem set
##########################################
patient <- read.csv("patient profile with replaced distance.csv", sep = ',')
patient <- tbl_df(patient)
patient <- mutate(patient, totalEngage = OfficeVisit+Physical+AWV)
patient <- patient[,-c(1:5,9,17:26,28,43:45,48:54)]
glimpse(patient)
patient <- within(patient, {
  newPatient <- factor(newPatient, labels=c('0','1'), ordered = FALSE)
})
patient <- within(patient, {
  age1 <- factor(age1, labels=c('0','1','2','3','4','5','6','7','8'), ordered = FALSE)
})
patient <- within(patient, {
  Practice <- factor(Practice, labels=c('0','1','2','3','4','5','6','7'), ordered = FALSE)
})
glimpse(patient)
patient.risk <- patient[which(patient$HighRiskInd==1),-3]
patient.non <- patient[which(patient$HighRiskInd==0),-3]



################################## 
#### Lasso - Non At-Risk Patient
##################################
x1 = model.matrix(totalEngage~., patient.non)[,-1]
y1 = patient.non$totalEngage
cv.lasso.non = cv.glmnet(x1, y1, alpha=1)
plot(cv.lasso.non)
plot(cv.lasso.non$glmnet.fit, xvar="lambda", label=TRUE)
(bestlam.non = cv.lasso.non$lambda.min)
cv.lasso.non.result = c()
folds <- 12
fold.indices <- cut(1:nrow(x1), breaks = folds, labels = FALSE)
for(i in 1:10){
  fold.train <- sample(1:12,8)
  train.indices = c()
  for (z in 1:length(fold.train)) {
    temp <- which(fold.indices == fold.train[z])
    train.indices = append(train.indices, temp)
  }
  test.x <- x1[-train.indices,]
  train.x <- x1[train.indices,]
  test.y <- y1[-train.indices]
  train.y <- y1[train.indices]
  lasso.mod = glmnet(train.x, train.y, alpha=1, lambda=bestlam.non, family = "gaussian") 
  lasso.pred = predict(lasso.mod, s=bestlam.non, newx = test.x, type = "link")
  cv.lasso.non.result[i] = mean((lasso.pred-test.y)^2)
}
mean(cv.lasso.non.result)
mean(patient.non$totalEngage)
lasso.coef.non = predict(lasso.mod, type="coefficients", s=bestlam.non)
lasso.coef.non[lasso.coef.non != 0]



################################
#### Lasso - At-Risk Patient
################################
x2 = model.matrix(totalEngage~., patient.risk)[,-1]
y2 = patient.risk$totalEngage
cv.lasso.risk = cv.glmnet(x2, y2, alpha=1)
plot(cv.lasso.risk)
plot(cv.lasso.risk$glmnet.fit, xvar="lambda", label=TRUE)
(bestlam.risk = cv.lasso.risk$lambda.min)
cv.lasso.risk.result = c()
folds <- 12
fold.indices <- cut(1:nrow(x2), breaks = folds, labels = FALSE)
for(i in 1:10){
  fold.train <- sample(1:12,8)
  train.indices = c()
  for (z in 1:length(fold.train)) {
    temp <- which(fold.indices == fold.train[z])
    train.indices = append(train.indices, temp)
  }
  test.x <- x2[-train.indices,]
  train.x <- x2[train.indices,]
  test.y <- y2[-train.indices]
  train.y <- y2[train.indices]
  lasso.mod = glmnet(train.x, train.y, alpha=1, lambda=bestlam.risk, family = "gaussian") 
  lasso.pred = predict(lasso.mod, s=bestlam.risk, newx = test.x, type = "link")
  cv.lasso.risk.result[i] = mean((lasso.pred-test.y)^2)
}
mean(cv.lasso.risk.result)
mean(patient.risk$totalEngage)
lasso.coef.risk=predict(lasso.mod, type="coefficients", s=bestlam.risk)
lasso.coef.risk[lasso.coef.risk != 0]



########################################## 
#### Random Forest - Non At-Risk Patient
##########################################
## determine best number of tree to grow
try = round(length(patient.non)/3)+1
patient.non.rf = randomForest(totalEngage~., data = patient.non, mtry = round(length(patient.non)/3), importance = T)
plot(patient.non.rf)
## determine mtry and compare each mtry's OOB with test MSE
train = sample(nrow(patient.non),0.7*nrow(patient.non))
oob.err= c()
test.err= c()
for(i in 1:try){
  patient.non.rf = randomForest(totalEngage ~ . , data = patient.non, subset = train, mtry = i, ntree = 200) 
  oob.err[i] = patient.non.rf$mse[200] #Error of all Trees fitted
  
  pred<-predict(patient.non.rf, patient.non[-train,]) #Predictions on Test Set for each Tree
  test.err[i]= with(patient.non[-train,], mean((totalEngage - pred)^2))
}
matplot(1:10, cbind(oob.err,test.err), pch=19 , col=c("red","blue"),type="b",ylab="Mean Squared Error",xlab="Number of Predictors Considered at each Split")
mtry.non = which.min(oob.err)
## 
folds <- 12
fold.indices <- cut(1:nrow(patient.non), breaks = folds, labels = FALSE)
cv.rf.non = c()
for(k in 1:10){
  fold.train <- sample(1:12,8)
  train.indices = c()
  for (z in 1:length(fold.train)) {
    temp <- which(fold.indices == fold.train[z])
    train.indices = append(train.indices, temp)
  }
  test.data <- patient.non[-train.indices,]
  train.data <- patient.non[train.indices,]
  patient.non.rf.best <- randomForest(train.data$totalEngage~., data = train.data, mtry = mtry.non, importance = TRUE, ntree = 200)
  cv.rf.non[k] <- patient.non.rf.best$mse[which.min(patient.non.rf.best$mse)]
}
mean(cv.rf.non)
importance(patient.non.rf.best)
varImpPlot(patient.non.rf.best)



#######################################
#### Random Forest - At-Risk Patient
#######################################
## determine best number of tree to grow
try = round(length(patient.risk)/3)+1
patient.risk.rf = randomForest(totalEngage~., data = patient.risk, mtry = round(length(patient.risk)/3), importance = T)
plot(patient.risk.rf)
## determine mtry and compare each mtry's OOB with test MSE
train = sample(nrow(patient.risk),0.7*nrow(patient.risk))
oob.err= c()
test.err= c()
for(i in 1:try){
  patient.risk.rf = randomForest(totalEngage ~ . , data = patient.risk, subset = train, mtry = i, ntree = 200) 
  oob.err[i] = patient.risk.rf$mse[200] #Error of all Trees fitted
  
  pred<-predict(patient.risk.rf, patient.risk[-train,]) #Predictions on Test Set for each Tree
  test.err[i]= with(patient.risk[-train,], mean((totalEngage - pred)^2))
}
matplot(1:10, cbind(oob.err,test.err), pch=19 , col=c("red","blue"),type="b",ylab="Mean Squared Error",xlab="Number of Predictors Considered at each Split")
mtry.risk = which.min(oob.err)
## 
folds <- 12
fold.indices <- cut(1:nrow(patient.risk), breaks = folds, labels = FALSE)
cv.rf.risk = c()
for(k in 1:10){
  fold.train <- sample(1:12,8)
  train.indices = c()
  for (z in 1:length(fold.train)) {
    temp <- which(fold.indices == fold.train[z])
    train.indices = append(train.indices, temp)
  }
  test.data <- patient.risk[-train.indices,]
  train.data <- patient.risk[train.indices,]
  patient.risk.rf.best <- randomForest(train.data$totalEngage~., data = train.data, mtry = mtry.risk, importance = TRUE, ntree = 200)
  cv.rf.risk[k] <- patient.risk.rf.best$mse[which.min(patient.risk.rf.best$mse)]
}
mean(cv.rf.risk)
importance(patient.risk.rf.best)
varImpPlot(patient.risk.rf.best)

