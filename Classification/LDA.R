rm(list=ls())
######################################
#### Linear Discriminant Analysis ####
######################################
## prepair the data
require(MASS)
require(ISLR)
train <- Smarket$Year < 2005 
Smarket.test <- Smarket[!train,]                      
Direction.test <- Smarket$Direction[!train]           
length(Direction.test)   

## fitting a LDA model
lda.fit <- lda(Direction~Lag1+Lag2, 
               data = Smarket, family = binomial, subset = train)
lda.fit$prior                ## 49.2% of the training observations correspond to days
                             ## during which the market went down
lda.fit$means                ## these are the average of each predictor within each class
                             ## these suggest that there is a tendency for the previous 2 days' 
                             ## returns to be negative on days when the market increases, and 
                             ## a tendency for the previous days' returns to be positive on days 
                             ## when the market declines
coef(lda.fit)                ## if ???0.642×Lag1???0.514×Lag2 is large, then the LDA classi???er will 
                             ## predict a market increase, and if it is small, then the LDA classi???er 
                             ## will predict a market decline
lda.pred <- predict(lda.fit, Smarket.test)
names(lda.pred)              ## the predict() function returns a list with three elements
                             ## class contains LDA's predictions
                             ## posterior is a matrix whose kth column contains the posterior probability
                             ## that the corresponding observation belongs to the kth class
                             ## x contains the linear discriminant
lda.class <- lda.pred$class
table(lda.class, Direction.test)
mean(lda.class == Direction.test)
