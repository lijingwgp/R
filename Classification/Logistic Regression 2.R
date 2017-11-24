rm(list=ls())
#############################
#### logistic regression ####
#############################
## prepare and inspect the data
require(ISLR)
names(Smarket)
dim(Smarket)
summary(Smarket)

## correlation matrix
cor(Smarket[,-9])       ## produces a matrix that contains all of the pairwise correlations
                        ## among the predictors in a data set
cor(Smarket[,-9])>0.5   ## volume and year has correlation

## fitting a logistic regression model
glm.fit <- glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data = Smarket, family = binomial)
summary(glm.fit)
coef(glm.fit)           ## retrieves coefficients of the model
summary(glm.fit)$coef
glm.probs <- predict(glm.fit, type = "response")      ## the predict() function can be used to predict the probability 
glm.probs                                             ## that the market will go up, given values of the predictors
contrasts(Smarket$Direction)                          ## constrasts() function indicates that R has created a dummy variable
                                                      ## with a 1 for up
glm.pred <- rep("Down", nrow(Smarket))                ## convert these predicted probabilities into class labels
glm.pred[glm.probs > 0.5] <- "Up"
table(glm.pred, Smarket$Direction)                    ## the table() function can be used to produce a confusion matrix
mean(glm.pred == Smarket$Direction)                   ## overall accuracy
mean(glm.pred != Smarket$Direction)                   ## training error rate

## improving model accuracy
train <- Smarket$Year < 2005                          ## creating training set index
Smarket.test <- Smarket[!train,]                      ## creating a test set
Direction.test <- Smarket$Direction[!train]           ## creating a test response vector 
length(Direction.test)                                ## the length of test response vector should match 
                                                      ## with the number of rows of the test set
glm.fit <- glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, 
               data = Smarket, subset = train, family = binomial)
glm.probs <- predict(glm.fit, Smarket.test, type = "response")
glm.pred <- rep("Down", 252)
glm.pred[glm.probs > 0.5] <- "Up"
table(glm.pred, Direction.test)
mean(glm.pred == Direction.test)
mean(glm.pred != Direction.test)
