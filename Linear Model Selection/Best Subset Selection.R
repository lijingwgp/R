rm(list=ls())
options(warn=-1)   # Supress warning messages
################################################################################
### Functions
#################################################################################
installIfAbsentAndLoad <- function(neededVector) {
  for(thispackage in neededVector) {
    if( ! require(thispackage, character.only = T) )
    { install.packages(thispackage)}
    require(thispackage, character.only = T)
  }
}
##############################
### Load required packages ###
##############################
# class contains the knn() function, KNN contains the
# knn.reg() function, combinat contains the combn() function
needed <- c("leaps", "ISLR", "glmnet")  
installIfAbsentAndLoad(needed)

##################################################
####### Best Subset Selection          ###########
##################################################
rm(list=ls())
# Here we apply the best subset selection approach to the Hitters data. We
# wish to predict a baseball player's Salary on the basis of various statistics
# associated with performance in the previous year.

head(Hitters)
names(Hitters)
dim(Hitters)
sum(is.na(Hitters$Salary))
Hitters=na.omit(Hitters)
dim(Hitters)
# The regsubsets() function (part of the leaps library) performs best 
# subset selection by identifying the best model that contains a given number
# of predictors, where best is quantified using RSS. The syntax is the same
# as for lm(). The summary() command outputs the best set of variables for
# each model size.
#install.packages("leaps")   #if necessary

regfit.full=regsubsets(Salary~.,Hitters)
summary(regfit.full)
# An asterisk indicates that a given variable is included in the corresponding
# model. For instance, this output indicates that the best two-variable model
# contains only Hits and CRBI. By default, regsubsets() only reports results
# up to the best eight-variable model. But the nvmax option can be used
# in order to return as many variables as are desired. Here we fit up to a
# 19-variable model.
regfit.full=regsubsets(Salary~.,data=Hitters,nvmax=19)
(reg.summary=summary(regfit.full))
# The summary() function also returns R2, RSS, adjusted R2, Cp, and BIC.
# We can examine these to try to select the best overall model.
names(reg.summary)
reg.summary$rsq
# For instance, we see that the R2 statistic increases from 32%, when only
# one variable is included in the model, to almost 55 %, when all variables
# are included. As expected, the R2 statistic increases monotonically as more
# variables are included.

# Plotting RSS, adjusted R2, Cp, and BIC for all of the models at once will
# help us decide which model to select. 
par(mfrow=c(2,2))
plot(reg.summary$rss,xlab="Number of Variables",ylab="RSS",type="l")
plot(reg.summary$adjr2,xlab="Number of Variables",ylab="Adjusted RSq",type="l")
(max<-which.max(reg.summary$adjr2))
points(max,reg.summary$adjr2[max], col="red",cex=2,pch=20)
# In a similar fashion we can plot the Cp and BIC statistics, and indicate the
# models with the smallest statistic using which.min().
plot(reg.summary$cp,xlab="Number of Variables",ylab="Cp",type='l')
(min<-which.min(reg.summary$cp))
points(min,reg.summary$cp[min],col="red",cex=2,pch=20)
(min<-which.min(reg.summary$bic))
plot(reg.summary$bic,xlab="Number of Variables",ylab="BIC",type='l')
points(min,reg.summary$bic[min],col="red",cex=2,pch=20)
# The regsubsets() function has a built-in plot() command which can
# be used to display the selected variables for the best model with a given
# number of predictors, ranked according to the BIC, Cp, adjusted R2, or
# AIC.
par(mfrow=c(2,2))
plot(regfit.full,scale="r2")
plot(regfit.full,scale="adjr2")
plot(regfit.full,scale="Cp")
plot(regfit.full,scale="bic")
# The top row of each plot contains a black square for each variable selected
# according to the optimal model associated with that statistic. For instance,
# we see that several models share a BIC close to -150. However, the model
# with the lowest BIC is the six-variable model (confirming we saw from our analysis  
# above)  that contains only AtBat, Hits, Walks, CRBI, DivisionW, and PutOuts. 

# We can use the coef() function to see the coefficient 
# estimates associated with a particular model - say 6.
coef(regfit.full,6)
par(mfrow=c(1,1))
