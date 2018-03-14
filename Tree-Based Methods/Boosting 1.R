## Boosting
## Here we use the gbm package, and within it the gbm() function, to ﬁt 
## boosted regression trees to the Boston data set. 
library(MASS)
library(gbm)
set.seed(1) 
train = sample(1:nrow(Boston), nrow(Boston)/2)
## We run gbm() with the option distribution="gaussian" since this is a regression problem; 
## if it were a binary classiﬁcation problem, we would use distribution="bernoulli". 
##
## The argument n.trees=5000 indicates that we want 5000 trees, 
## and the option interaction.depth=4 limits the depth of each tree.
boost.boston = gbm(medv~., data=Boston[train,], distribution= "gaussian", n.trees=5000, 
                   interaction.depth=4)
## The summary() function produces a relative inﬂuence plot and also outputs 
## the relative inﬂuence statistics.
## We see that lstat and rm are by far the most important variables. 
##
## We can also produce partial dependence plots for these two variables. 
## These plots illustrate the marginal eﬀect of the selected variables on the 
## response after integrating out the other variables. 
par(mfrow=c(1,2)) 
plot(boost.boston,i="rm") 
plot(boost.boston,i="lstat")
## We now use the boosted model to predict medv on the test set
yhat.boost = predict(boost.boston, newdata = Boston[-train,], n.trees=5000) 
mean((yhat.boost-Boston[-train,14])^2) 
## The test MSE obtained is 11.8; similar to the test MSE for random forests 
## and superior to that for bagging
##
## If we want to, we can perform boosting with a diﬀerent value of the shrinkage 
## parameter lambda in (8.10). The default value is 0.001, but this is easily modiﬁed
## Here we take lambda = 0.2
boost.boston = gbm(medv~., data=Boston[train,], distribution= "gaussian", n.trees=5000, 
                   interaction.depth=4, shrinkage=0.2, verbose=F) 
yhat.boost=predict (boost.boston, newdata =Boston[-train ,], n.trees=5000) 
mean((yhat.boost-Boston[-train,14])^2)
## In this case, using lambda = 0.2 leads to a slightly lower test MSE than lambda = 0.001
