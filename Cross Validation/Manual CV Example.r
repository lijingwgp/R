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
needed  <-  c("ISLR")  #contains Auto data
installIfAbsentAndLoad(needed)
# Get data into a data frame
mydf <- Auto
#Randomly shuffle the data frame
n <- nrow(mydf)
mydf <- mydf[sample(1:n, n),]
# Create 10 equally size folds
numfolds <- 10
fold.indices <- cut(1:n, breaks=numfolds, labels=FALSE)
#Perform 10 fold cross validation
mse <- rep(0, numfolds)
# Estimate the expected value of the true MSE
for(i in 1:numfolds){
  #Segement your data by fold using the which() function 
  test.indices <- which(fold.indices == i)
  test.data <- mydf[test.indices, ]
  train.data <- mydf[-test.indices, ]
  glm.fit=glm(mpg ~ poly(horsepower,2),data=train.data)
  mse[i] <- mean((predict.glm(glm.fit, test.data[-1], type = "response")-test.data$mpg) ^ 2)
}
mean(mse)  # This is the final estimate the expected value of the true MSE
sd(mse)    # This is a measure of its variability (but an imperfect measure)
