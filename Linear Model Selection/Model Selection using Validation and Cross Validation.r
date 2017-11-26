############################################################################################
####### Choosing Among Models Using the Validation Set Approach and Cross-Validation #######
############################################################################################
rm(list=ls())
# We just saw that it is possible to choose among a set of models of different
# sizes using Cp, BIC, and adjusted R2. We will now consider how to do this
# using the validation set and cross-validation approaches.
#
# In order for these approaches to yield accurate estimates of the test
# error, we must use only the training observations to perform all aspects of
# model-fitting-including variable selection. Therefore, the determination of
# which model of a given size is best must be made using only the training
# observations. This point is subtle but important. If the full data set is used
# to perform the best subset selection step, the validation set errors and
# cross-validation errors that we obtain will not be accurate estimates of the
# test error.
#
# In order to use the validation set approach, we begin by splitting the
# observations into a training set and a test set. Here's a way we haven't
# used before:

Hitters=na.omit(Hitters)
set.seed(1)
train=sample(c(TRUE,FALSE), nrow(Hitters),rep=TRUE)
test=(!train)

# Now, we apply regsubsets() to the training set in order to perform best
# subset selection.

regfit.best=regsubsets(Salary~.,data=Hitters[train,],nvmax=19)

# We now compute the validation set error for the best model
# of each model size. We first make a model matrix from the test data

test.mat=model.matrix(Salary~.,data=Hitters[test,])
head(test.mat)

# The model.matrix() function is used in many regression packages for building
# an "X" matrix from data. Now we run a loop, and for each size i, we extract
# the coefficients from regfit.best for the best model of that size,
# multiply them into the appropriate columns of the test model matrix to
# form the predictions, and compute the test MSE.

val.errors=rep(NA,19)
for(i in 1:19){                              
   coefi=coef(regfit.best,id=i)
   pred=test.mat[,names(coefi)]%*%coefi
   val.errors[i]=mean((Hitters$Salary[test]-pred)^2)
}
val.errors
(min<-which.min(val.errors))

# We find that the best model is the one that contains ten variables.
coef(regfit.best,min)

# This was a little tedious, partly because there is no predict() method
# for regsubsets(). Since we will be using this function again, we can capture
# our steps above and write our own predict method.

predict.regsubsets=function(object,newdata,id,...){
  form=as.formula(object$call[[2]])
  mat=model.matrix(form,newdata)
  coefi=coef(object,id=id)
  xvars=names(coefi)
  mat[,xvars]%*%coefi
}

# Our function pretty much mimics what we did above. The only complex
# part is how we extracted the formula used in the call to regsubsets(). We
# demonstrate how we use this function below, when we do cross-validation.
#
# Finally, we perform best subset selection on the full data set, and select
# the best ten-variable model. It is important that we make use of the full
# data set in order to obtain more accurate coefficient estimates. Note that
# we perform best subset selection on the full data set and select the best 
# ten-variable model, rather than simply using the variables that were obtained
# from the training set, because the best ten-variable model on the full data
# set may differ from the corresponding model on the training set.

regfit.best=regsubsets(Salary~.,data=Hitters,nvmax=19)
coef(regfit.best,10)

# In fact, we see that the best ten-variable model on the full data set has a
# different set of variables than the best ten-variable model on the training set
# 
# We now try to choose among the models of different sizes using cross validation.
# This approach is somewhat involved, as we must perform best
# subset selection within each of the k training sets. Despite this, we see that
# with its clever subsetting syntax, R makes this job quite easy. First, we
# create a vector that allocates each observation to one of k = 10 folds, and
# we create a matrix in which we will store the results.

k=10
set.seed(1)
folds=sample(1:k,nrow(Hitters),replace=TRUE)
cv.errors=matrix(NA,k,19, dimnames=list(NULL, paste(1:19)))

# Now we write a for loop that performs cross-validation. In the jth fold, the
# elements of folds that equal j are in the test set, and the remainder are in
# the training set. We make our predictions for each model size (using our
# new predict() method), compute the test errors on the appropriate subset,
# and store them in the appropriate slot in the matrix cv.errors.

for(j in 1:k){
  best.fit=regsubsets(Salary~.,data=Hitters[folds!=j,],nvmax=19)    # Find the best subset using the 9 folds not equal to j
  for(i in 1:19){                                                   # Loop over the number of predictors...
    pred=predict(best.fit,Hitters[folds==j,],id=i)                  # get prediction for the best model with this number of predictors 
    cv.errors[j,i]=mean( (Hitters$Salary[folds==j]-pred)^2)         # store in the jth row (the fold), ith column (the number of predictor)
    }
}
cv.errors

# This has given us a 10 by 19 matrix, of which the (i, j)th element corresponds
# to the test MSE for the ith cross-validation fold for the best j-variable
# model. We use the apply() function to average over the columns of this
# matrix in order to obtain a vector for which the jth element is the cross 
# validation error for the j-variable model.

mean.cv.errors=apply(cv.errors,2,mean)
mean.cv.errors
par(mfrow=c(1,1))
plot(mean.cv.errors,type='b')
(min=which.min(mean.cv.errors))

# We see that cross-validation selects an 11-variable model. We now perform
# best subset selection on the full data set in order to obtain the 11-variable
# model.

reg.best=regsubsets(Salary~.,data=Hitters, nvmax=19)

# Display the coefficients and cross validation error for the best subset model
coef(reg.best,min)
(cv.err<-mean.cv.errors[min])
