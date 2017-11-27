#################
### The Lasso ###
#################
rm(list=ls())
# We saw that ridge regression with a wise choice of lambda can outperform 
# least squares.
# We now ask whether the lasso can yield either a more accurate or a more 
# interpretable model than ridge regression.

# In order to fit a lasso model, we once again use the glmnet() function; 
# however, this time we use the argument alpha=1. 
require(glmnet)
require(ISLR)
Hitters <- na.omit(Hitters)
x = model.matrix(Salary~., Hitters)[,-1]
y = Hitters$Salary

set.seed(1)
train = sample(1:nrow(x),nrow(x)/2)
test = (-train)
y.test = y[test]

grid = 10^seq(10,-2,length=100) 
lasso.mod=glmnet(x[train,],y[train],alpha=1,lambda=grid) 
plot(lasso.mod)

# We can see from the coeficient plot that depending on the choice of 
# tuning parameter, some of the coefficients will be exactly equal to zero. 
# We now perform cross-validation and compute the associated test error.
set.seed(1)
cv.out = cv.glmnet(x[train,],y[train],alpha=1)
plot(cv.out)
bestlam = cv.out$lambda.min
lasso.pred = predict(lasso.mod,s=bestlam,newx = x[test,])
mean((lasso.pred-y.test)^2)

# This is very similar to the test MSE of ridge regression with lambda chosen 
# by cross-validation. However, the lasso has a substantial advantage over ridge 
# regression in that the resulting coefficient estimates are sparse. 
out=glmnet(x,y,alpha=1,lambda=grid)
lasso.coef=predict (out ,type="coefficients",s=bestlam)[1:20,]
lasso.coef
lasso.coef[lasso.coef!=0]
