rm(list=ls())
#########################################
#### Quadratic Discriminant Analysis ####
#########################################
## prepair the data
require(MASS)
require(ISLR)
train <- Smarket$Year < 2005 
Smarket.test <- Smarket[!train,]                      
Direction.test <- Smarket$Direction[!train]           
length(Direction.test) 

## fitting a QDA model
qda.fit <- qda(Direction~Lag1+Lag2, data = Smarket, family = binomial, subset = train)
qda.fit                       ## the output does not contain the coefficients of the linear discriminant
                              ## because the QDA classifier involves a quadratic, rather than linear, 
                              ## function of the predictors
qda.class <- predict(qda.fit, Smarket.test)$class
table(qda.class, Direction.test)
mean(qda.class == Direction.test)
