rm(list=ls())
options(warn=-1)   # Supress warning messages
###########################################################
### Functions
###########################################################
installIfAbsentAndLoad <- function(neededVector) {
  if(neededVector != "") {
    for(thispackage in neededVector) {
      if( ! require(thispackage, character.only = T) )
      { install.packages(thispackage)}
      require(thispackage, character.only = T)
    }    
  }
}
##############################
### Load required packages ###
##############################
installIfAbsentAndLoad(needed)
###########################################################
####### The learning Curve   ##############################
###########################################################
set.seed(5072)
###########################################################
####### Parameters           ##############################
###########################################################
max.sample.size <- 1000          # sample size
smoothing.reps <- 20            # number of iterations
polynomial.degree <- 15
drop.first <- 100
############################################################# 
# Create some synthetic sample data - fourth-degree
# polynomial in 2 variables with moderate irreducible error
#############################################################
x1  <-  runif(max.sample.size)
x2  <-  runif(max.sample.size)
y  <- -10 + 12 * x1 - 3 * x1 ^ 4 + 35 * x2 - 14 * x2 ^ 3 + 25 * x1 * x2 + rnorm(max.sample.size, mean = 0, sd = 2)  
############################################################## 
# Case 1: A high-bias, low variance situation... Fit a
# linear model and plot the learning curve for this model
############################################################## 
train <- sample(1:max.sample.size, max.sample.size * .5)
test <- setdiff(1:max.sample.size, train)
# Need to avoid rank deficiencies that arise when there are
# fewer unique points than the combined order of the
# polynomial terms. We do it here so that this plot agrees with the next.
start.loop.at <- 2 * polynomial.degree + 1
# Set up some results vectors
train.errors.calc <- rep(0,smoothing.reps)
test.errors.calc <- rep(0,smoothing.reps)
train.errors.final.case1 <- rep(0,length(train) - start.loop.at)
test.errors.final.case1 <- rep(0,length(test) - start.loop.at)
for(i in (start.loop.at+1):length(train)) {
  for(j in 1:smoothing.reps) {
    coeffs.train <- sample(train,i)
    coeffs.test <- test
    mydata.train <- data.frame("x1" = x1[coeffs.train], "x2" = x2[coeffs.train], "y" = y[coeffs.train])
    mydata.test <- data.frame("x1" = x1[coeffs.test], "x2" = x2[coeffs.test], "y" = y[coeffs.test])
    mymodel <- lm(y ~ x1 + x2, data = mydata.train)
    train.errors.calc[j] <- mean((predict(mymodel, newdata=mydata.train[setdiff(names(mydata.train), "y")]) - mydata.train["y"])^2)
    test.errors.calc[j] <- mean((predict(mymodel, newdata=mydata.test[setdiff(names(mydata.test), "y")]) - mydata.test["y"])^2)
  }
  train.errors.final.case1[i - start.loop.at] <- mean(train.errors.calc)
  test.errors.final.case1[i - start.loop.at] <- mean(test.errors.calc)
}
mean.train.case1 <- mean(train.errors.final.case1[(length(train)-start.loop.at-10):(length(train)-start.loop.at)])
mean.test.case1 <- mean(test.errors.final.case1[(length(train)-start.loop.at-10):(length(train)-start.loop.at)])

############################################################## 
# Case 2: A low-bias, high variance situation... Fit a
# highly non-linear model and plot the learning curve for
# this model
############################################################## 

train.errors.calc <- rep(0,smoothing.reps)
test.errors.calc <- rep(0,smoothing.reps)
train.errors.final.case2 <- rep(0,length(train) - start.loop.at)
test.errors.final.case2 <- rep(0,length(train) - start.loop.at)
for(i in (start.loop.at+1):length(train)) {
  for(j in 1:smoothing.reps) {
    coeffs.train <- sample(train,i)
    coeffs.test <- test
    x.train <- model.matrix(y[coeffs.train] ~ poly(x1[coeffs.train],polynomial.degree) + poly(x2[coeffs.train],polynomial.degree))[, -1]
    mydata.train <- data.frame(x.train, "y" = y[coeffs.train])
    x.test <- model.matrix(y[coeffs.test] ~ poly(x1[coeffs.test],polynomial.degree) + poly(x2[coeffs.test],polynomial.degree))[, -1]
    mydata.test <- data.frame(x.test, "y" = y[coeffs.test])
    names(mydata.test) <- names(mydata.train)
    mymodel <- lm(y ~ ., data = mydata.train)
    train.errors.calc[j] <- mean((predict(mymodel, newdata=mydata.train[setdiff(names(mydata.train), "y")]) - mydata.train["y"])^2)
    test.errors.calc[j] <- mean((predict(mymodel, newdata=mydata.test[setdiff(names(mydata.test), "y")]) - mydata.test["y"])^2)
  }
  train.errors.final.case2[i - start.loop.at] <- mean(train.errors.calc)
  test.errors.final.case2[i - start.loop.at] <- mean(test.errors.calc)
}
mean.train.case2 <- mean(train.errors.final.case2[(length(train)-start.loop.at-10):(length(train)-start.loop.at)])
mean.test.case2 <- mean(test.errors.final.case2[(length(train)-start.loop.at-10):(length(train)-start.loop.at)])

##########################################
# Plot the results...
##########################################
# pdf("LearningCurvePlots.pdf")
drop.first <- 150
plot.indices <- (drop.first + 1):length(train.errors.final.case1)
max.y <- max(c(train.errors.final.case1[plot.indices], test.errors.final.case1[plot.indices], train.errors.final.case2[plot.indices], test.errors.final.case2[plot.indices]))

par(mfrow=c(2,1))

plot(NULL, NULL, type='n', xlim=c(drop.first+1, length(train.errors.final.case1)), ylim=c(0,max.y), xlab='Increasing Training Set Size',ylab='Mean CV MSE', main='MSEs as a Function of Sample Size \n High Bias and Low Variance')
lines(seq(drop.first+1,length(train.errors.final.case1)), train.errors.final.case1[plot.indices], type='b', col=2, pch=16)
lines(seq(drop.first+1,length(train.errors.final.case1)), test.errors.final.case1[plot.indices], type='b', col=1, pch=16)
legend("topleft", legend = c(paste("Train Errors - Final Mean: ", round(mean.train.case1, 2), sep=""), paste("Test Errors - Final Mean MSE: ", round(mean.test.case1, 2), sep="")), col=c(2, 1), cex=.75, pch=16, lty=1)

plot(NULL, NULL, type='n', xlim=c(drop.first+1, length(train.errors.final.case1)), ylim=c(0,max.y), xlab='Increasing Training Set Size',ylab='Mean CV MSE', main='MSEs as a Function of Sample Size \n Low Bias and High Variance')
lines(seq(drop.first+1,length(train.errors.final.case1)), train.errors.final.case2[plot.indices], type='b', col=2, pch=16)
lines(seq(drop.first+1,length(train.errors.final.case1)), test.errors.final.case2[plot.indices], type='b', col=1, pch=16)
legend("topleft", legend = c(paste("Train Errors - Final Mean: ", round(mean.train.case2, 2), sep=""), paste("Test Errors - Final Mean MSE: ", round(mean.test.case2, 2), sep="")), col=c(2, 1), cex=.75, pch=16, lty=1)
options(warn=0)   # Reinstate warning messages
par(mfrow=c(1,1))
# dev.off()
