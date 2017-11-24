rm(list=ls())
#############
#### KNN ####
#############
## prepairing the data
require(ISLR)
require(class)

train <- Smarket$Year < 2005 
train.x <- cbind(Smarket$Lag1, Smarket$Lag2)[train,]
colnames(train.x) <- c("Lag1", "Lag2")
test.x <- cbind(Smarket$Lag1, Smarket$Lag2)[!train,]
colnames(test.x) <- c("Lag1", "Lag2")
train.y <- Smarket$Direction[train]

Smarket.test <- Smarket[!train,]                      
Direction.test <- Smarket$Direction[!train]  

## fitting a KNN model
set.seed(1)
knn.pred <- knn(train.x, test.x, train.y, k = 1)       ## knn() forms predictions using a single command
                                                       ## knn() can be used to make predictions
table(knn.pred, Direction.test)
