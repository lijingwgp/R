########################
### Ridge Regression ###
########################
rm(list=ls())
# We will use the glmnet package in order to perform ridge regression and the lasso.
# The main function in this package is glmnet(), which can be used to fit 
# ridge regression models, lasso models and more.

# We must pass in an x matrix as well as a y vector, and we do not use the y~x syntax.
# Now we will perform ridge regression and the lasso in order to predict Salary on the 
# Hitters data.

require(ISLR)
Hitters <- na.omit(Hitters)
x = model.matrix(Salary~., Hitters)[,-1]
y = Hitters$Salary

# The model.matrix() function is particularly useful for creating x; not only does it 
# produce a matrix corresponding to the 19 predictors but it also automatically transforms 
# any qualitative variables into dummy variables. 

# The glmnet() function has an alpha argument that determines what type of model 
# is fit. If alpha=0 then a ridge regression model is fit, and if alpha=1 then 
# a lasso model is fit. We first fit a ridge regression model.

require(glmnet)
grid = 10^seq(10,-2,length=100)                 
ridge.mod = glmnet(x,y,alpha = 0,lambda = grid)

# By default the glmnet() function performs ridge regression for an automatically selected range 
# of lambda values. However, here we have chosen to implement the function over a grid of values.

# As we will see, we can also compute model fits for a particular value of lambda that is not one of 
# the original grid values.

# By default, the glmnet() function standardizes the variables so that they are on the same scale. 
# To turn off this default setting, use the argument standardize=FALSE.

# Associated with each value of lambda is a vector of ridge regression coefficients, 
# stored in a matrix that can be accessed by coef(). In this case, it is a 20×100
# matrix, with 20 rows (one for each predictor, plus an intercept) and 100 columns (one for each value of ??).
dim(coef(ridge.mod))

# When a large value of lambda is used, we expect the coefficient estimates to be much smaller
ridge.mod$lambda[50]       # lambda value
coef(ridge.mod)[,50]       # coefficients corrosponds to that lambda value
sqrt(sum(coef(ridge.mod)[-1,50]^2)) 

ridge.mod$lambda[60] 
coef(ridge.mod)[,60]
sqrt(sum(coef(ridge.mod)[-1,60]^2)) 

# We can use the predict() function for a number of purposes. For instance, we can obtain the ridge regression 
# coe???cients for a new value of lambda, say 50:
predict (ridge.mod,s=50,type="coefficients")[1:20,] 

# We now split the samples into a training set and a test set in order to estimate the 
# test error of ridge regression and the lasso. 

# There are two common ways to randomly split a data set. The first is to produce a random vector of TRUE, 
# FALSE elements and select the observations corresponding to TRUE for the training data. 
# The second is to randomly choose a subset of numbers between 1 and n.

set.seed(1)
train = sample(1:nrow(x),nrow(x)/2)
test = (-train)
y.test = y[test]

# We first set a random seed so that the results obtained will be reproducible.
# Next we fit a ridge regression model on the training set, and evaluate its MSE on the test set, using lambda = 4. 

ridge.mod=glmnet(x[train,],y[train],alpha=0,lambda=grid,thresh=1e-12) 
ridge.pred=predict(ridge.mod,s=4,newx=x[test,])
mean((ridge.pred-y.test)^2)         

# Note the use of the predict() function again. This time we get predictions for a test set, by replacing 
# type="coefficients" with the newx argument.

# Note that if we had instead simply fit a model with just an intercept, we would have predicted each test 
# observation using the mean of the training observations. 
mean((mean(y[train])-y.test)^2)

# We could also get the same result by fitting a ridge regression model with a very large value of lambda. 
ridge.pred=predict (ridge.mod ,s=1e10 ,newx=x[test,]) 
mean((ridge.pred -y.test)^2) 

# So fitting a ridge regression model with lambda = 4 leads to a much lower test MSE than fitting a model 
# with just an intercept. 

# We now check whether there is any benefit to performing ridge regression with lambda = 4 instead of 
# just performing least squares regression. Recall that least squares is simply ridge regression with lambda = 0.
ridge.pred=predict (x=x[train,], y=y[train], ridge.mod, s=0, newx=x[test, ], exact=T)
mean((ridge.pred-y.test)^2)
(lm.fit = lm(y~x, subset=train))
predict (x=x[train,], y=y[train], ridge.mod ,s=0,exact=T,type="coefficients")[1:20,]

# In general, instead of arbitrarily choosing lambda = 4, it would be better to use cross-validation to choose 
# the tuning parameter lambda. We can do this using the built-in cross-validation function, cv.glmnet().

# By default, the function performs ten-fold cross-validation, though this can be changed using the argument folds.
# Note that we set a random seed first so our results will be reproducible, since the choice of the cross-validation folds is random.
set.seed(1)
cv.out = cv.glmnet(x[train,],y[train],alpha=0) 
plot(cv.out)
(bestlam = cv.out$lambda.min)

# Therefore, we see that the value of lambda that results in the smallest crossvalidation error is 212.
# What is the test MSE associated with this value of lambda
ridge.pred=predict (ridge.mod,s=bestlam,newx=x[test,]) 
mean((ridge.pred-y.test)^2) 

# Finally, we re-fit our ridge regression model on the full data set, using the value of lambda chosen by cross-validation, 
# and examine the coefficient estimates.
ridge.mod=glmnet(x,y,alpha=0,lambda=bestlam,thresh=1e-12) 
ridge.coef=predict(ridge.mod,type="coefficients",s=bestlam)[1:20,] 
