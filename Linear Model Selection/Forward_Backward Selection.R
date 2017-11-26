###########################################################
####### Forward and Backward Stepwise Selection ###########
###########################################################
## Here we apply the best subset selection approach to the Hitters data
## We found that there are 59 players' salaries are missing in the data using is.na()

require(ISLR)
names(Hitters)
dim(Hitters)
sum(is.na(Hitters$Salary))

## Therefore, we use na.omit() to remove all of the rows that have missing values in any variable
Hitters <- na.omit(Hitters)
dim(Hitters)
sum(is.na(Hitters))

## We can use the regsubsets() function to perform forward stepwise
## or backward stepwise selection, using the argument method="forward" or
## method="backward".

require(leaps)
regfit.fwd=regsubsets(Salary~.,data=Hitters,nvmax=19,method="forward")
summary(regfit.fwd)
regfit.bwd=regsubsets(Salary~.,data=Hitters,nvmax=19,method="backward")
summary(regfit.bwd)

## For instance, we see that using forward stepwise selection, the best 
## one-variable model contains only CRBI, and the best two-variable model 
## additionally includes Hits. 
##
## Let's examine the predictors that make up the best models of sizes 6 and 7:

coef(regfit.full,6)
coef(regfit.fwd,6)
coef(regfit.bwd,6)

coef(regfit.full,7)
coef(regfit.fwd,7)
coef(regfit.bwd,7)

## For this data, the best one through six-variable models are each identical for best subset,
## forward selection, and backward selection.
## However, the best seven-variable models identified by forward stepwise selection,
## backward stepwise selection, and best subset selection are all different.
