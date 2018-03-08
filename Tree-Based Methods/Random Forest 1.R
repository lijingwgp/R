## Random Forest 1
rm(list = ls()) 
library(MASS)
library(randomForest)
set.seed(1)
train = sample(1:nrow(Boston), nrow(Boston)/2) 


## Recall that bagging is simply a special case of a random forest with m = p.
## Therefore, the randomForest() function can be used to perform both 
## random forests and bagging. 
bag.boston = randomForest(Boston$medv~., data = Boston, subset = train, mtry = 13, importance = TRUE)
bag.boston
## mtry=13 indicates that all 13 predictors should be considered for each split of the tree.
## In other words, that bagging should be done. 
##
## How well does this bagged model perform on the test set?
preds.bag = predict(bag.boston,newdata = Boston[-train,])
plot(preds.bag, Boston[-train,14])
abline(0,1) 
mean((preds.bag-Boston[-train,14])^2)
## The test set MSE associated with the bagged regression tree is 12.9, 
## almost half that obtained using an optimally-pruned single tree.
##
## We could change the number of trees grown by randomForest() using the ntree argument.
bag.boston = randomForest(medv~., data=Boston, subset=train, mtry=13, ntree=25) 
preds.bag = predict(bag.boston, newdata=Boston[-train,]) 
mean((preds.bag - Boston[-train,14])^2)
##
## Growing a random forest proceeds in exactly the same way, except that we use a 
## smaller value of the mtry argument.
##
## By default, randomForest() uses p/3 variables when building a random forest of regression trees, 
## and √p variables when building a random forest of classiﬁcation trees. 
## Here we use mtry = 6.
rf.boston = randomForest(medv~., data=Boston, subset=train, mtry=6, importance=TRUE) 
preds.rf = predict(rf.boston, newdata = Boston[-train,])
mean((preds.rf-Boston[-train,14])^2)
## The test set MSE is 11.56; this indicates that random forests yielded 
## an improvement over bagging in this case.
##
## Using the importance() function, we can view the importance of each variable.
importance(rf.boston)
varImpPlot (rf.boston)