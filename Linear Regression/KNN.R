rm(list=ls())
##################
### Question 1 ###
##################
## create a dataframe
homeprice <- read.table("HomePrices.txt", sep = "\t", header = T, stringsAsFactors = F)

## compute MSE, then print
numrows <- nrow(homeprice)
medv_mean <- mean(homeprice$medv)
se <- sum((homeprice$medv - medv_mean)^2)
print(mse <- se/numrows)

## print the variance of medv, assuming the data is the population
print(var(homeprice$medv)*((numrows-1)/numrows))

## scale and center the numeric and integer predictors
## display the first 6 rows
homeprice[, -c(13)] <- scale(homeprice[, -c(13)], center = TRUE, scale = TRUE)
head(homeprice)

## set the random seed to 5072
set.seed(5072)

## create training, validate, and test data frames
trainprop <- 0.75
validateprop <- 0.15
testprop <- 0.1

train <- sample(numrows, trainprop*numrows)
validate <- sample(setdiff(1:numrows,train), validateprop*numrows)
test <- setdiff(setdiff(1:numrows,train),validate)

trainset <- homeprice[train,]
validateset <- homeprice[validate,]
testset <- homeprice[test,]

## display the first row of each of my three data frames
head(trainset, 1)
head(validateset, 1)
head(testset, 1)

## create the following 6 data frames:
train.x <- trainset[-13]
validate.x <- validateset[-13]
test.x <- testset[-13]
train.y <- trainset$medv
validate.y <- validateset$medv
test.y <- testset$medv

## use the knn.reg() to fit a knn model with k = 1:20 (odd k's) and evaluating both
## the validate and training MSE's for each k
require("FNN")
numreps = 10
iterates = c(1,3,5,7,9,11,13,15,17,19)
validate.error <- rep(0,numreps)
train.error <- rep(0,numreps)

for(i in 1:length(iterates)){
  knn.pred <- knn.reg(train.x, validate.x, train.y, k=iterates[i])$pred
  validate.error[i] <- mean((knn.pred-validate.y)^2)
  
  knn.pred <- knn.reg(train.x, train.x, train.y, k=iterates[i])$pred
  train.error[i] <- mean((knn.pred-train.y)^2)
}

## plot the training and validate MSE's as a function of k's
## the x-axis should go from least flexible to most flexible
plot(NULL, NULL, type='n', xlim=c(19, 1), ylim=c(0,max(c(validate.error, train.error))), xlab='Increasing Flexibility (Decreasing k)', 
     ylab='Mean Square Errors', main='MSEs as a Function of \n Flexibility for KNN Regression')
lines(seq(19, 1, -2), validate.error[length(validate.error):1], type='b', col=2, pch=16)
lines(seq(19, 1, -2), train.error[length(train.error):1], type='b', col=1, pch=16)
legend("topright", legend = c("Validation MSE", "Training MSE"), col=c(2, 1), cex=.75, pch=16)

## print the k and associated MSE that produced the lowest training MSE
## do the same for validate MSE
print(paste("My best Validate MSE occured with k =", iterates[which.min(validate.error)], "and produced a validate MSE of", validate.error[which.min(validate.error)]))
print(paste("My best Training MSE occured with k =", iterates[which.min(train.error)], "and produced a training MSE of", train.error[which.min(train.error)]))

## predict medv for the test set using the optimal value of k 
## that you found for the validate set, and compute the associated MSE
knn.pred <- knn.reg(train.x, test.x, train.y, k=iterates[which.min(validate.error)])$pred
test.error <- mean((knn.pred - test.y)^2)
print(paste("My Test MSE is computed when k is found to have the optimal value of", iterates[which.min(validate.error)], "and thus, my Test MSE is", test.error[which.min(test.error)]))



rm(list=ls())
##################
### Question 2 ###
##################
## create a data frame from the file named LoanData.csv
loan <- read.table("LoanData.csv", sep = ",", header = T, stringsAsFactors = F)

## print the error rate that would result from always predicting Yes
no <- table(loan$loan.repaid)[1]
yes <- table(loan$loan.repaid)[2]
errRate <- no / sum(no + yes)
print(paste("The error rate that would result from always predicting Yes is", errRate))

## scale and center the numeric and integer predictors
## display the first 6 rows
loan[, -c(8)] <- scale(loan[, -c(8)], center = TRUE, scale = TRUE)
head(loan)

## set the random seed to 5072
set.seed(5072)

## create training, validate, and test data frames
numrows <- nrow(loan)
trainprop <- 0.75
validateprop <- 0.15
testprop <- 0.1

train <- sample(numrows, trainprop*numrows)
validate <- sample(setdiff(1:numrows,train), validateprop*numrows)
test <- setdiff(setdiff(1:numrows,train),validate)

trainset <- loan[train,]
validateset <- loan[validate,]
testset <- loan[test,]

## display the first row of each of my three data frames
head(trainset, 1)
head(validateset, 1)
head(testset, 1)

## creating the following 6 data frames
train.x <- trainset[-8]
validate.x <- validateset[-8]
test.x <- testset[-8]
train.y <- trainset$loan.repaid
validate.y <- validateset$loan.repaid
test.y <- testset$loan.repaid

## use knn() from the class package, predict the loan payment using all other variables
require(class)
iterates = c(1,3,5,7,9,11,13,15,17,19)
validate.error <- rep(0,length(iterates))
train.error <- rep(0,length(iterates))

for(i in 1:length(iterates)){
  knn.pred <- knn(train.x, validate.x, train.y, k=iterates[i])
  validate.error[i] <- mean(validate.y != knn.pred)
  
  knn.pred <- knn(train.x, train.x, train.y, k=iterates[i])
  train.error[i] <- mean(train.y != knn.pred)
}

## plot the training and validate error rates as a function of k's
## the x-axis should go from least flexible to most flexible
plot(NULL, NULL, type='n', xlim=c(19, 1), ylim=c(0,max(c(validate.error, train.error))), xlab='Increasing Flexibility (Decreasing k)', 
     ylab='Error Rates', main='Error Rates as a Function of \n Flexibility for KNN Classification')
lines(seq(19, 1, -2), validate.error[length(validate.error):1], type='b', col=2, pch=16)
lines(seq(19, 1, -2), train.error[length(train.error):1], type='b', col=1, pch=16)
legend("topleft", legend = c("Validation Error Rate", "Training Error Rate"), col=c(2, 1), cex=.55, pch=16)

## print the k and associated error rate that produced the lowest training error rate 
## do the same for the validate error rate
print(paste("My best Validate error rate occured with k =", iterates[which.min(validate.error)], "and produced a validate error rate of", validate.error[which.min(validate.error)]))
print(paste("My best Training error rate occured with k =", iterates[which.min(train.error)], "and produced a training error rate of", train.error[which.min(train.error)]))

## predict loan.repaid for the test set using the optimal value of k that you 
## found for the validate set
knn.pred <- knn(train.x, test.x, train.y, k=iterates[which.min(validate.error)])
test.error <- mean(knn.pred != test.y)
print(paste("My Test error rate is computed when k is found to have the optimal value of", iterates[which.min(validate.error)], "and thus, my Test error rate is", test.error[which.min(test.error)]))



rm(list=ls())
##################
### Question 3 ###
##################
## in this question, we want to investigate whether the difference between 
## validate MSE and test MSE is caused by random chance in a way that
## the test set was radically different from the evaluation set
## set random seed to 5072 and repeat part 1 with 50 different random 75/15/10
## partitions of the data into train, validate and test sets. each time using the validate
## set to choose the best k and then using that k to evaluate the test MSE
homeprice <- read.table("HomePrices.txt", sep = "\t", header = T, stringsAsFactors = F)
homeprice[, -c(13)] <- scale(homeprice[, -c(13)], center = TRUE, scale = TRUE)
numrows <- nrow(homeprice)

require("FNN")
set.seed(5072)

trainprop <- 0.75
validateprop <- 0.15
testprop <- 0.1

iterates = c(1,3,5,7,9,11,13,15,17,19)
validate.MSE <- rep(0,50)
test.MSE <- rep(0,50)

for(i in 1:50){
  train <- sample(numrows, trainprop*numrows)
  validate <- sample(setdiff(1:numrows,train), validateprop*numrows)
  test <- setdiff(setdiff(1:numrows,train),validate)
  
  trainset <- homeprice[train,]
  validateset <- homeprice[validate,]
  testset <- homeprice[test,]
  
  train.x <- trainset[-13]
  validate.x <- validateset[-13]
  test.x <- testset[-13]
  train.y <- trainset$medv
  validate.y <- validateset$medv
  test.y <- testset$medv
  
  validate.error <- rep(0,length(iterates))
  for(k in 1:length(iterates)){
    knn.pred <- knn.reg(train.x, validate.x, train.y, k=iterates[k])$pred
    validate.error[k] <- mean((knn.pred-validate.y)^2)
  }
  
  optimal_k <- iterates[which.min(validate.error)]
  validate.MSE[i] <- validate.error[which.min(validate.error)]
  knn.pred <- knn.reg(train.x, test.x, train.y, k=optimal_k)$pred
  test.MSE[i] <- mean((knn.pred - test.y)^2)
}


## print the means and sample standard deviation of both best validate and test MSE's
print(paste("mean of the best validate MSE is",mean(validate.MSE)))
print(paste("sample standard deviation of the best validate MSE is",sd(validate.MSE)))
print(paste("mean of the best test MSE is",mean(test.MSE)))
print(paste("standard deviation of the best test MSE is",sd(test.MSE)))


## plot the validate and test MSE's. add horizontal lines to the plot that show both
## the validate MSE and test MSE's mean
plot(NULL, NULL, type='n', xlim=c(0, 50), ylim=c(0,max(c(validate.MSE, test.MSE))), xlab='Replication', 
     ylab='MSEs', main='Test and Best Validation MSEs for Many Partitionings of the Data')
lines(seq(1,50), validate.MSE, type='b', col=2, pch=16)
lines(seq(1, 50), test.MSE, type='b', col=1, pch=16)
lines(seq(1,50), rep(mean(test.MSE),length(test.MSE)), type = 'l', col=1, lty=2, lwd=3)
lines(seq(1,50), rep(mean(validate.MSE),length(validate.MSE)), type = 'l',lty=2,lwd=3,col=2)
legend("topright", legend = c("Validation MSEs", "Validation MSE Mean","Test MSEs","Test MSE Mean"), col=c(2,1), cex=.55, pch=16)
