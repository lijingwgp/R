rm(list=ls())
require(boot)
require(ISLR)
###############################################################################
# The Bootstrap Exercise #1                                                   # 
# Repeat the example of two stocks from the text to estimate Var(aX +(1 -a)Y) #
###############################################################################

# Here we are using the Portfolio data frame (part of the ISLR package).
# Each of the 100 rows contains returns for the two investments of interest: X and Y 
str(Portfolio)
head(Portfolio)

# Begin by creating a function to compute the population
# statistic of interest - here alpha.
# This function is named alpha.fn (our choice) and takes 2
# paramaters: the first is a data frame containing the
# values X and Y as so-named columns, the second is a vector
# of indices which define the rows in the data frame to be
# used to estimate alpha (these two parameters are required
# by the boot() function used later).

alpha.fn<-function(data,index){
 X<-data$X[index]
 Y<-data$Y[index]
 #Compute alpha, the optimal amount to invest in X
 return((var(Y)-cov(X,Y))/(var(X)+var(Y)-2*cov(X,Y)))
 }

# As an example, call the alpha.fn() function using all the rows of
# the X and Y values in Portfolio to estimate alpha.
alpha.fn(Portfolio,1:nrow(Portfolio))
# As another example, we apply the alpha.fn() function to a random 
# sample (with replacement) of size 100 from the rows in the Portfolio data frame
alpha.fn(Portfolio,sample(100,100,replace=T))

# Finally, use the boot() function to generate 1,000 bootstrap replicates of alpha
# and report the standard error of those replicates
set.seed(1)
boot(Portfolio,alpha.fn,R=1000)

##############################################################################
# The Bootstrap Exercise #2                                                  #
# Estimating the Accuracy of the coefficients in a Linear Regression Model   #
##############################################################################
# Create a function to create linear regression coefficients for mpg~horsepower
# in the Auto data frame.
boot.fn<-function(data,index) return(coef(lm(mpg~horsepower,data=data,subset=index)))
# Use this function to estimate B0 and B1 using the full data frame as the training set
boot.fn(Auto,1:nrow(Auto))

# Example 2.1: use our boot.fn() function to produce two bootstrap estimates for 
# the intercept and slope 

boot.fn(Auto,sample(nrow(Auto),nrow(Auto),replace=T))

# Example 2.2: use the boot() function to compute the standard errors of 1,000 bootstrap
# estimates for the intercept and slope terms
set.seed(1)
boot(Auto,boot.fn,1000)

# Compare to results of formulae in Section 3.1.2 (pp. 66)
summary(lm(mpg~horsepower,data=Auto))$coef

# Which is better? The bootstrap is a more reliable estimate of the standard error since
# the values reported by the lm() function (and the formulae) rely on an estimate (RSE) of
# the true value of the population sigma (the population noise variance).

# Do the same for the quadratic model - discrepency is less since fit is closer
boot.fn<-function(data,index) coefficients(lm(mpg~horsepower+I(horsepower^2),data=data,subset=index))
set.seed(1)
boot(Auto,boot.fn,1000)
summary(lm(mpg~horsepower+I(horsepower^2),data=Auto))$coef
