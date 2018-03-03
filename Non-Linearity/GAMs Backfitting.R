## Chapter 7 Exercise 11 
## It was mentioned that GAMs are generally fit using a backfitting approach.
## The idea behind backfitting is actually quite simple.
## We will now explore backﬁtting in the context of multiple linear regression.
##
## Suppose that we would like to perform multiple linear regression, 
## but we do not have software to do so. Instead, we only have software 
## to perform simple linear regression.
##
## Therefore, we take the following iterative approach: 
## we repeatedly hold all but one coeﬃcient estimate ﬁxed at its current value, 
## and update only that coeﬃcient estimate using a simple linear regression.
##
## The process is continued until convergence—that is, 
## until the coeﬃcient estimates stop changing. 


## a) Generate a response Y and two predictors X1 and X2, with n = 100.
## Create some true values of betas.
rm(list=ls())
set.seed(5082)
beta.0 <- 2.0
beta.1 <- 7.0
beta.2 <- -0.5
n <- 100
x1 <- rnorm(n)
x2 <- rnorm(n)
y <- beta.0 + beta.1 * x1 + beta.2 * x2 + 0.1 * rnorm(n)


## b) Initialize beta.1.hat
beta.1.hat = -1.0


## c) Keeping beta.1.hat fixed, then fit the model.
## In other words, set a starting point for the partial.residual for f(2).
partial.residual <- y - beta.1.hat * x1
beta.2.hat <- lm(partial.residual ~ x2)$coef[2]


## d) Keeping beta.2.hat fixed, then fit the model.
## In other words, set a starting point for the partial.residual for f(1).
partial.residual <- y - beta.2.hat * x2
beta.1.hat <- lm(partial.residual ~ x1)$coef[2]


## e) Write a for loop to repeat c and d 1000 times. Report the estimates of 
## beta.1.hat, beta.2.hat, and beta.0.hat at each iteration of the for loop.
## Create a plot in which each of these values is displayed
numreps <- 1000
beta.0.reps <- rep(0,numreps)
beta.1.reps <- rep(0,numreps)
beta.2.reps <- rep(0,numreps)
for(rep in 1:numreps){
  ## Update beta.2.hat  
  partial.residual <- y - beta.1.hat * x1
  beta.2.hat <- lm(partial.residual ~ x2)$coef[2] 
  ## Update beta.1.hat and beta.0.hat 
  partial.residual <- y - beta.2.hat * x2
  temp.model <- lm(partial.residual ~ x1)
  beta.1.hat <- temp.model$coef[2]
  beta.0.hat <- temp.model$coef[1] 
  ## Record progress
  beta.0.reps[rep] = beta.0.hat
  beta.1.reps[rep] = beta.1.hat
  beta.2.reps[rep] = beta.2.hat
}
##
## For comparison, get the coefficient estimates produced by lm:
lm.model <- lm(y ~ x1 + x2)
##
## Backfitting iterations are red dots
## True value of the parameters are the horizontal green lines
## lm()'s estimated coefficients are the horizontal blue lines
##
## Plot beta.0 progress versus lm'r result
plot(1:numreps, beta.0.reps, main='beta.0', col='red', pch=19, ylim=c(beta.0*0.99,max(beta.0.reps)))
abline(h=beta.0,col='green',lwd=4)
abline(h=lm.model$coefficients[1], col='blue', lwd=4)
## Plot beta.1 progress versus lm'r result
plot(1:numreps, beta.1.reps, main='beta.1', col='red', pch=19)
abline(h=beta.1,col='green',lwd=4)
abline(h=lm.model$coefficients[2], col='blue', lwd=4)
## Plot beta.2 progress versus lm'r result
plot(1:numreps, beta.2.reps, main='beta.2', col='red', pch=19)
abline(h=beta.2,col='green',lwd=4)
abline(h=lm.model$coefficients[3], col='blue', lwd=4)