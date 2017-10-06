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
needed  <-  c("ISLR", "class")  #class contains the knn() function
installIfAbsentAndLoad(needed)
###################################
### K-Nearest Neighbors (pp. 164)
###################################
#Create a training set of observations from 2001 to 2004 and a test set of observations from 2005 
str(Smarket)
train <- Smarket$Year < 2005
head(train)                                #train is s vector of T and F - the rows that are T will be training rows
tail(train)
test.Y <- Smarket$Direction[!train]        #test.Y is a vector of "Up" and "Down" in the test data frame (needed for the table() function)
length(test.Y)
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

train.X <- cbind(Smarket$Lag1, Smarket$Lag2)[train,]
head(train.X)
nrow(train.X)
test.X <- cbind(Smarket$Lag1, Smarket$Lag2)[!train,]
head(test.X)
nrow(test.X)
train.Y <- Smarket$Direction[train]
head(train.Y)
length(train.Y)
set.seed(123)           #Ties in nearest neighbors are broken randomly
#Fit a knn model with k=1 and produce predictions for the text X's
knn.pred <- knn(train.X, test.X, train.Y, k=1)
#In the table() function, always put Actuals first so they
#appear in the rows, predictions next so they appear in the
#columns
mytable <- table(test.Y, knn.pred)   
mytable
(mytable["Up", "Up"] + mytable["Down", "Down"]) / sum(mytable)
#Since the error rate with k=1 is so high, try with a less flexible model with k=3.
#Fit a knn model with k=3
knn.pred <- knn(train.X, test.X, train.Y, k=3)
mytable <- table(test.Y, knn.pred)
mytable
(mytable["Up", "Up"] + mytable["Down", "Down"]) / sum(mytable)
#Since the error rate with k=3 is still high, try with a still-less flexible model with k=10.
#Fit a knn model with k=10
knn.pred <- knn(train.X, test.X, train.Y, k=10)
mytable <- table(test.Y, knn.pred)
mytable
(mytable["Up", "Up"] + mytable["Down", "Down"]) / sum(mytable)
1-(mytable["Up", "Up"] + mytable["Down", "Down"]) / sum(mytable)

# Let's see how it would do on the training data...
#Fit a knn model with k=1 and produce predictions for the text X's
knn.pred <- knn(train.X, train.X, train.Y, k=1)
#In the table() function, always put Actuals first so they
#appear in the rows, predictions next so they appear in the
#columns
mytable <- table(train.Y, knn.pred)   
mytable
(mytable["Up", "Up"] + mytable["Down", "Down"]) / sum(mytable)
# detach(package:ISLR)
# detach(package:class)