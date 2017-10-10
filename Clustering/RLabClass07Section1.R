rm(list=ls())
##########################################################################################
### Functions
##########################################################################################
installIfAbsentAndLoad <- function(neededVector) {
  for(thispackage in neededVector) {
    if( ! require(thispackage, character.only = T) )
    { install.packages(thispackage)}
    require(thispackage, character.only = T)
  }
}
##############################
### Load required packages ###
##############################
needed  <-  c("class")  #class contains the knn() function
installIfAbsentAndLoad(needed)
##############################
### Data Example           ###
##############################
set.seed(5072)
x <- matrix(rnorm(20 * 2, 100, 25), ncol = 2)
y <- c(rep("Down", 10), rep("Up", 10))
x[y == "Down",] <- x[y == "Down",] + 25
plot(x, col = ifelse(y == "Up", 4, 2), cex = 1.5, pch = ifelse(y == "Up", 15, 16))
mydata <- data.frame(x , y)
n <- nrow(mydata)
mydata[c(1:5, (n/2 + 1):(n/2 + 5)),]
##############################
### Now for real           ###
##############################
set.seed(5072)
##########################
### Draw a new sample   ##
##########################
x <- matrix(rnorm(2000 * 2, 100, 25), ncol = 2)
y <- c(rep("Down", 1000), rep("Up", 1000))
x[y == "Down",] <- x[y == "Down",] + 25
plot(x, col = ifelse(y == "Up", 4, 2), cex = .5, pch = ifelse(y == "Up", 15, 16))
mydata <- data.frame(x , y, stringsAsFactors = F)
n <- nrow(mydata)
trainprop <- 0.70  #say we want a 70/20/10 split of rows for training, validation and test respectively
validateprop <- 0.2
####################################
### Create three random subsets   ##
####################################
#create a vector of random integers of training size from the vector 1:n
train  <-  sample(n, trainprop * n)
#create a vector of the remaining  integers, then create a vector of random integers
#of validate size by sampling from these
validate  <-  sample(setdiff(1:n, train), validateprop * n) 
#create a vector of the integers not in either training or validate
test <- setdiff(setdiff(1:n, train), validate)
#Create the data frames using the indices created in the three vectors above
trainset <- mydata[train,]
validateset <- mydata[validate,]
testset <- mydata[test,]
# The knn() function requires 4 parameters: 
# 1. A matrix containing the predictors (the X's) associated with the training
#   data, here train.X 
# 2. A matrix containing the predictors (the X's) associated with the data for 
#   which we wish to make predictions, here validate.X for the validate set, 
#   train.X for the training set
# 3. A vector containing the class labels (the Y's) for the training observations, 
#   here train.Y
# 4. A value for K, the number of nearest neighbors to be # used by the 
#   classifier.
train.x <- trainset[-3]
train.y <- trainset$y
validate.x <- validateset[-3]
validate.y <- validateset$y
test.x <- testset[-3]
test.y <- testset$y
####################################
# Fit a knn model with k=1 and produce 
# predictions for the validate set
####################################
knn.pred <- knn(train.x, validate.x,  train.y, k=1)
# Produce a Confusion Matrix and compute the validate 
# error rate with k=1. In the table() function, always put Actuals
# first so they appear in the rows, predictions next so they
# appear in the columns
mytable <- table(validate.y, knn.pred) 
mytable
(mytable["Up", "Down"] + mytable["Down", "Up"]) / sum(mytable)
# A simpler way if all we want is the overall error rate
# without regard for the types of errors.
(mean(validate.y != knn.pred))
## Do the same with the training error rate
knn.pred <- knn(train.x ,train.x  ,train.y  , k=1)
mytable <- table(train.y, knn.pred)   
mytable
(mytable["Up", "Down"] + mytable["Down", "Up"]) / sum(mytable)
(mean(train.y != knn.pred))
##################################################
# Fit a knn model with increasing k and produce 
# corresponding predictions for the validate set
##################################################
numreps <- 100
validate.errors <- rep(0, numreps)
train.errors <- rep(0, numreps)
for(k in 1:numreps) {
  knn.pred <- knn(train.x, validate.x,  train.y, k = k)
  validate.errors[k] <- mean(validate.y != knn.pred)
  
  knn.pred <- knn(train.x, train.x,  train.y, k = k)
  train.errors[k] <- mean(train.y != knn.pred)    
}
print(paste("Minimum validate set error rate occurred at k =", which.min(validate.errors)))
print(paste("Minimum validate error rate was ", validate.errors[which.min(validate.errors)]))

plot(NULL, NULL, type='n', xlim=c(numreps, 1), ylim=c(0,max(c(validate.errors, train.errors))), xlab='Increasing Flexibility (Decreasing k)', ylab='Error Rates', main='Error Rates as a Function of \n Flexibility for KNN Classification')
lines(seq(numreps, 1), validate.errors[length(validate.errors):1], type='b', col=2, pch=16)
lines(seq(numreps, 1), train.errors[length(train.errors):1], type='b', col=1, pch=16)
legend("topleft", legend = c("Validation Error Rate", "Train Error Rate"), col=c(2, 1), cex=.75, pch=16)

#############################
# Compute test error rate 
#############################
knn.pred <- knn(train.x, test.x,  train.y, k = which.min(validate.errors))
mytable <- table(test.y, knn.pred)
print(mytable)
print(paste("Test set error rate was ",(mytable["Up", "Down"] + mytable["Down", "Up"]) / sum(mytable)))
############################################################
# Evaluate the model with the optimal k over many samples
############################################################
######################################
# Evaluate the model over many samples
######################################
test.error.rates <- rep(0,100)
for(i in 1:100) {
  xi <- matrix(rnorm(2000 * 2, 100, 25), ncol = 2)
  xi[y == "Down",] <- xi[y == "Down",] + 25
  knn.pred <- knn(train.x, xi, train.y, k = which.min(validate.errors))
  test.error.rates[i] <- mean(y != knn.pred)
}
print(paste("Expected test error rate: ", mean(test.error.rates), "Expected Standard Deviation of test error rate:", sd(test.error.rates)))



