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
# MASS contains the Boston data
needed  <-  c("ISLR", "MASS", "corrplot")      
installIfAbsentAndLoad(needed)
##############################################
### Logistic Regression ######################
##############################################
# The Stock Market Data: Daily percentage returns for the
# S&P 500 stock index between 2001 and 2005. Each row also
# contains returns for a day and returns lagged from 1 to 5
# days, volume in billions, and a factor named Direction
# with levels Down and Up indicating whether the market had
# a positive or negative return that day.
names(Smarket)
dim(Smarket)
summary(Smarket)
pairs(Smarket)        #The pairs() function produces a matrix of scatter plots
str(Smarket)
# The cor() function produces a matrix that contains all of
# the pairwise correlations among the predictors in a data
# set.
cor(Smarket[, -9]) 
# Use the corrplot function to plot a Correlation Matrix
# (Need to install.packages("corrplot")
corrplot(cor(Smarket[, -9]), mar=c(0, 0, 1, 0))
plot(Smarket$Volume)
# Logistic Regression: fit a logistic regression model in
# order to predict Direction using Lag1 through Lag5 and
# Volume
glm.fit <- glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=Smarket, family=binomial)
# Alternatively, glm.fit1 <- glm(Direction~. -Today -Year,
# data=Smarket, family=binomial)

#Exploring the model...not surprisingly, none of the
#predictors are significant
summary(glm.fit)
coef(glm.fit)
summary(glm.fit)$coef[, 4]
# The predict() function can be used to predict the
# probability that the market will go up, given values of
# the predictors.
glm.probs <- predict(glm.fit, type="response")   #type="response" outputs probabilities P(Y = 1|X) instead of, for example, the logit.
glm.probs[1:10]
contrasts(Smarket$Direction)
# We know that these values correspond to the probability of
# the market going up, rather than down, because the
# contrasts() function indicates that R has created a dummy
# 0/1 variable - the probabilities always correspond to the
# category that has been assigned the value 1.

# Convert the predicted probabilities into class labels Up
# or Down
glm.pred <- rep("Down", nrow(Smarket))
glm.pred[glm.probs>.5] <- "Up"
# Compute the proportion of correct and incorrect
# predictions
mean(glm.pred==Smarket$Direction)
mean(glm.pred!=Smarket$Direction)
# Create a confusion matrix using the table() function to
# compute a more detailed summary of model performance
mytable <- table(Smarket$Direction, glm.pred)   #for consistency, we will always put the actuals on the left and the predictions on the top of the table
mytable
# Compute the relative frequency of correct predictions
str(mytable)
(mytable[1, 1]+mytable[2, 2])/sum(mytable)
# Compute the Training Error Rate  (using row and column names this time)
(mytable["Up", "Down"]+mytable["Down", "Up"])/sum(mytable)
# This is the training error rate. But we are most
# interested in the test error rate.

# Create a training set of observations from 2001 to 2004
# and a test set of observations from 2005
train <- Smarket$Year<2005
head(train)                                
tail(train)
Smarket.2005 <- Smarket[!train, ] #Smarket.2005 is a data frame of  test rows
dim(Smarket.2005)
Direction.2005 <- Smarket$Direction[!train]  #Direction.2005 is a vector of "Up" and "Down" in the test data frame
# Build a Classification model using the test set (note the
# addition of the last paramater "subset="
glm.fit <- glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=Smarket, family=binomial, subset=train)
# Use the model to create test set predictions
glm.probs <- predict(glm.fit, Smarket.2005, type="response")
# compute the test error rate
glm.pred <- rep("Down", nrow(Smarket.2005))   #Why not length(train)?
glm.pred[glm.probs>.5] <- "Up"
mean(glm.pred==Direction.2005)
mean(glm.pred!=Direction.2005)
# Create a confusion matrix for the test set and compute the
# test error rate
mytesttable <- table(Direction.2005, glm.pred)
(mytesttable["Up", "Down"]+mytesttable["Down", "Up"])/sum(mytesttable)
# Refit the logistic regression using just Lag1 and Lag2,
# which seemed to have the highest predictive power in the
# original logistic regression model
glm.fit <- glm(Direction~Lag1+Lag2, data=Smarket, family=binomial, subset=train)
glm.probs <- predict(glm.fit, Smarket.2005, type="response")
glm.pred <- rep("Down", nrow(Smarket.2005))
glm.pred[glm.probs>.5] <- "Up"
mytesttable <- table(Direction.2005, glm.pred)
mytesttable
(mytesttable["Up", "Down"]+mytesttable["Down", "Up"])/sum(mytesttable)
# Compute the test error rate...down from  0.4784 (full set
# as training) and 0.4802 (2005 withheld from training set)
1-mean(glm.pred==Direction.2005)
# What if we just used the naive approach (the majority in
# the training set)
numUp <- sum(Direction.2005=="Up")
numDown <- sum(Direction.2005=="Down")
numUp
numDown
numDown/(numUp+numDown)   #Same error rate as our model, so no advantage.
# Note that on days when the model predicted Up, we had a
# lower error rate (random chance?)
mytesttable["Down", "Up"]/sum(mytesttable[, "Up"])
# Finally, make a prediction for two new observations with
# known Lag1 and Lag2. Note the type = 'response' - otherwise we get the logit (log of the odds)
predict(glm.fit, newdata=data.frame(Lag1=c(1.2, 1.5), Lag2=c(1.1, -0.8)), type="response")
# Again, these are both probabilities of the market going Up