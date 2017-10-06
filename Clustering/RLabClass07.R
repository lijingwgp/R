##################################################
###  Warning: There's a very subtle bug herein  ##
##################################################

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
plot(x)
y <- c(rep("Down", 10), rep("Up", 10))
x[y == "Down",] <- x[y == "Down",] + 25
plot(x, col = ifelse(y == "Up", 4, 2))
mydata <- data.frame(x , y)
n <- nrow(mydata)
mydata[c(1:5, (n/2 + 1):(n/2 + 6)),]
##############################
### Now for real           ###
##############################
set.seed(5072)
##########################
### Draw a new sample   ##
##########################
x <- matrix(rnorm(200 * 2, 100, 25), ncol = 2)
y <- c(rep("Down",100), rep("Up",100))
x[y == "Down",] <- x[y == "Down",] + 25
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
#   which we wish to make predictions, here test.X for the test set, 
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
knn.pred <- knn(train.x, validate.x,  as.vector(train.y), k=1)
#In the table() function, always put Actuals first so they
#appear in the rows, predictions next so they appear in the
#columns
mytable <- table(validate.y, knn.pred)   
mytable
(mytable["Up", "Down"] + mytable["Down", "Up"]) / sum(mytable)
##################################################
# Fit a knn model with increasing k and produce 
# predictions for the validate set
##################################################
numreps <- 50
validate.errors <- rep(0, numreps)
train.errors <- rep(0, numreps)
for(k in 1:numreps) {
  knn.pred <- knn(train.x, validate.x,  train.y, k = k)
  mytable <- table(validate.y, knn.pred)
  validate.errors[k] <- (mytable["Up", "Down"] + mytable["Down", "Up"]) / sum(mytable)
  
  knn.pred <- knn(train.x, train.x,  train.y, k = k)
  mytable <- table(train.y, knn.pred)
  train.errors[k] <- (mytable["Up", "Down"] + mytable["Down", "Up"]) / sum(mytable)
}
print(paste("Minimum validate set error rate occurred at k =", which.min(validate.errors)))
print(paste("Minimum validate error rate was ", validate.errors[which.min(validate.errors)]))
plot(1:numreps,validate.errors, main = "Validate Set error Rates", xlab = "k (Decreasing Flexibility)")
plot(1:numreps,train.errors, main = "Train Set error Rates", xlab = "k (Decreasing Flexibility)")
knn.pred <- knn(train.x, test.x,  train.y, k = which.min(validate.errors))
mytable <- table(test.y, knn.pred)
print(paste("Test error rate was ",(mytable["Up", "Down"] + mytable["Down", "Up"]) / sum(mytable)))






