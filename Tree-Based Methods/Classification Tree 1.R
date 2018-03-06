rm(list=ls())
## The tree libarary is used to construct classification and regression trees.
## We first use classification trees to analyze the Carseats data sat.
##
## We use ifelse() to create a variable "high" which takes the value of yes if 
## the sales variable exceeds 8, and takes on a value no otherwise.
library(ISLR)
library(tree)
attach(Carseats)
high = ifelse(Sales <= 8, "No", "Yes")
## Now we merge this variable with the rest of the data set.
Carseats = data.frame(Carseats, high)


## We now use tree() to fit a classification tree in order to predict high using 
## all variables but sales.
tree.carseats = tree(high~.-Sales, Carseats)
## The summary() function lists the variables that are used as interval nodes 
## in the tree, and the training error rate
summary(tree.carseats)
## A small deviance indicates a tree that provides a good ﬁt to the (training) 
## data.
plot(tree.carseats) 
text(tree.carseats, pretty =0)
## The most important indicator of Sales appears to be shelving location, 
## since the ﬁrst branch diﬀerentiates Good locations from Bad and Medium locations.
## Note that the left branch coming out of that node consists observations with 
## the bad and medium locations. 


## In order to properly evaluate the performance of a classification tree, 
## we must estimate the test error.
## So we split the observations into a training set and a test set. Building the 
## tree use the training set, and evaluate its performance on the test data.
##
## The predict() function can be used with type=class to return the actual class
## prediction.
set.seed(2) 
train=sample (1:nrow(Carseats), 200) 
Carseats.test = Carseats[-train,] 
high.test=high[-train]
tree.carseats = tree(high~.-Sales, Carseats, subset=train) 
tree.pred = predict(tree.carseats, Carseats.test, type="class") 
table(tree.pred, high.test)
(86+57) / 200


## Next, we consider whether pruning the tree might lead to improved results.
## cv.tree() performs cross-validation to determine the optimal level of complexity.
## Cost complexity is used in order to select a sequence of sub trees 
##
## We use the argument FUN=prune.misclass in order to indicate that we want the 
## classiﬁcation error rate to guide the cross-validation and pruning process, 
## rather than the default for the cv.tree() function, which is deviance.
##
## The cv.tree() function reports the size of each sequence of sub trees,
## as well as the corresponding error rate and the value of the 
## cost-complexity parameter used (k, which corresponds to alphas).
set.seed(3) 
cv.carseats = cv.tree(tree.carseats, FUN=prune.misclass) 
names(cv.carseats)
## Note that, despite the name, dev corresponds to the cross-validation 
## error rate in this instance.
##
## The tree with 9 terminal nodes results in the lowest cross-validation 
## error rate, with 50 cross-validation errors. 
par(mfrow=c(1,2)) 
plot(cv.carseats$size, cv.carseats$dev, type="b") 
plot(cv.carseats$k, cv.carseats$dev, type="b")


## We now apply the prune.misclass() function in order to prune the tree to 
## obtain the nine-node tree.
prune.carseats =prune.misclass (tree.carseats, best=9) 
par(mfrow=c(1,1))
plot(prune.carseats) 
text(prune.carseats, pretty = 0)
## Once again, we apply the predict() function to evaluate the performance.
tree.pred=predict(prune.carseats, Carseats.test, type="class") 
table(tree.pred, high.test) 
(94+60) / 200 
