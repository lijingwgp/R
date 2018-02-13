# Fitting Regression Trees
# Here we ﬁt a regression tree to the Boston data set. 
# First, we create a training set, and ﬁt the tree to the training data.
require(MASS)
require(tree)
set.seed(1)
train = sample(1:nrow(Boston), nrow(Boston)/2) 
tree.boston = tree(medv~., data=Boston, subset=train)
summary(tree.boston)

# summary() indicates that only three of the variables have been 
# used in constructing the tree. 
# the deviance is simply the sum of squared errors for the tree
# we now plot the tree
plot(tree.boston)
text(tree.boston, pretty = 0)

# The variable lstat measures the percentage of individuals with 
# lower socioeconomic status. The tree indicates that lower values 
# of lstat correspond to more expensive houses. 

# The tree predicts a median house price of $46,400 for larger homes 
# in suburbs in which residents have high socioeconomic status 
# (rm>=7.437 and lstat<9.715). 

# Now we use the cv.tree() function to see whether pruning the 
# tree will improve performance.
cv.boston = cv.tree(tree.boston)
plot(cv.boston$size, cv.boston$dev, type = 'b')

# In this case, the most complex tree is selected by cross-validation.
# However, if we wish to prune the tree, we could do so as follows, 
# using the prune.tree() function
prune.boston = prune.tree(tree.boston, best = 5)
plot(prune.boston)
text(prune.boston, pretty = 0)

# In keeping with the cross-validation results, we use the unpruned 
# tree to make predictions on the test set.
yhat = predict(tree.boston, newdata = Boston[-train,])
boston.test = Boston[-train, "medv"]
plot(yhat, boston.test)
abline(0,1)

# note that dots that are blow the line are under-predicted, versus the others that are
# above the line which are over-predicted.
# calculate MSE
mean((yhat-boston.test)^2) 
