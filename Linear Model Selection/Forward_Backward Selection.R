###########################################################
####### Forward and Backward Stepwise Selection ###########
###########################################################
# We can also use the regsubsets() function to perform forward stepwise
# or backward stepwise selection, using the argument method="forward" or
# method="backward".

regfit.fwd=regsubsets(Salary~.,data=Hitters,nvmax=19,method="forward")
summary(regfit.fwd)
regfit.bwd=regsubsets(Salary~.,data=Hitters,nvmax=19,method="backward")
summary(regfit.bwd)
# For instance, we see that using forward stepwise selection, the best 
# one-variable model contains only CRBI, and the best two-variable model 
# additionally includes Hits. 
#
# Let's examine the predictors that make up the best models of sizes 6 and 7:
coef(regfit.full,6)
coef(regfit.fwd,6)
coef(regfit.bwd,6)

coef(regfit.full,7)
coef(regfit.fwd,7)
coef(regfit.bwd,7)
# For this data, the best six-variable models are each identical for best subset and
# forward selection.
# However, the best seven-variable models identified by forward stepwise selection,
# backward stepwise selection, and best subset selection are all different.
