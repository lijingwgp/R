rm(list=ls())
##########################################################################################
### Functions
##########################################################################################
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
## MASS contains the Boston data, car contains the vif() function
needed  <-  c("ISLR", "MASS", "car")     
installIfAbsentAndLoad(needed)
#######################################
#### Linear Regression ################
#######################################

####################################################
### ISLR Lab Part 1: Simple Linear Regression ######
####################################################
## Investigate the Boston data frame
?Boston
str(Boston)
## Use the lm() function to produce a linear model called lm.fit that regresses median
## house value (medv) on percentage of households with low socioeconomic status (lstat)
lm.fit <- lm(medv ~ lstat, data = Boston)
## Investigate the structure of lm.fit
str(lm.fit)
## Display a summary of the regression model
summary(lm.fit)
names(lm.fit)
head(lm.fit$model )         ### Retrieve via col names
lm.fit$coefficients
head(lm.fit$residuals)
lm.fit$residuals
## Extractor functions
coef(lm.fit)
confint(lm.fit)             ### For CIs for parameter estimates
confint(lm.fit, level=.99)
predict(lm.fit, data.frame(lstat=(c(5,10,15))), interval="confidence")    ### For CIs of points
predict(lm.fit, data.frame(lstat=(c(5,10,15))), interval="prediction")    ### For PIs of points
plot(Boston$lstat, Boston$medv)
abline(lm.fit, lwd=3, col="red")
plot(Boston$lstat, Boston$medv, col="red", pch=20)
plot(Boston$lstat, Boston$medv, col="red", pch="+")
plot(1:20, 1:20, pch=1:20)
par(mfrow=c(2, 2))                       ### Divide plot window into 2x2
plot(lm.fit)                             ### Plot 4 standard diagnostic plots ? see Section 3.3.3
plot(predict(lm.fit), residuals(lm.fit)) ### Suggests non-linearity
plot(predict(lm.fit), rstudent(lm.fit))  ### Ditto ? scale is in num t-StdDevs
plot(hatvalues(lm.fit))                  ### Plots leverage of points
which.max(hatvalues(lm.fit))             ### ID highest-leverage point
par(mfrow=c(1,1))

## Assessing the accuracy of the coefficient estiimates 

## compute the least squares estimators for a linear
## regression model that regresses median house value (medv)
## on percentage of households with low socioeconomic status
## (lstat)
lm.fit <- lm(medv ~ lstat, data=Boston)    #For comparison
summary(lm.fit)
## Using the expressions in equations on Slide 7...
beta1hat <- sum((Boston$lstat - mean(Boston$lstat)) * (Boston$medv - mean(Boston$medv))) / sum((Boston$lstat - mean(Boston$lstat)) ^ 2)  
beta1hat
beta0hat <- mean(Boston$medv) - beta1hat * mean(Boston$lstat)
beta0hat
## Alternative calculation of beta1hat
cov(Boston$lstat, Boston$medv)/var(Boston$lstat)
## Calculate the RSS - Slide 6
n <- nrow(Boston)
RSS <- sum((lm.fit$residuals) ^ 2)
RSSalt <- sum((Boston$medv - (beta0hat + beta1hat * Boston$lstat)) ^ 2)
RSS
## Calculate the RSE (Residual Standard Error)  Slide 21
RSE <- sqrt(RSS / (n - 2))
RSE
## Calculating the standard error of the intercept using RSE as an estimate for sigma-squared - Slide 11
se0 <- sqrt(RSE ^ 2 * (1 / n + mean(Boston$lstat) ^ 2 / sum((Boston$lstat - mean(Boston$lstat)) ^ 2)))
se0
## Calculating the standard error of the slope - Slide 11
se1 <- sqrt(RSE ^ 2 / sum((Boston$lstat - mean(Boston$lstat)) ^ 2))
se1
## Construct a 95% confidence interval for the slope parameter Slide 15
confint(lm.fit, level=.95)
myconfint <- c(beta1hat - qt(.975, n - 2) * se1, beta1hat + qt(.975, n - 2) * se1)
myconfint
## Conduct a hypothesis of H0:beta1=0 H1:beta1<>0 - Slide 19
tstat <- beta1hat / se1
tstat
pvalue <- 2 * pt(tstat, n - 2)          ### p-value for 2-tailed test
pvalue
## Reject H0 since pvalue is VERY small
## Calculate TSS (Slide 24) and r-squared Slide 27
TSS <- sum((Boston$medv - mean(Boston$medv)) ^ 2)
TSS
rsquared <- 1 - RSS / TSS
rsquared
## For simple regression, R-squared is the square of the
## correlation coefficient between x and y
cor(Boston$lstat, Boston$medv) ^ 2   
######################################################
### ISLR Lab: Multiple Linear Regression ######
######################################################
lm.fit <- lm(medv ~ lstat + age, data = Boston)  ### Basic syntax is lm(y ~ a + b + c)
summary(lm.fit)
lm.fit <- lm(medv ~ ., data = Boston)            ### the "." is shorthand for "everything"
summary(lm.fit)
vif(lm.fit)                                      ### displays variance inflation factors. VIF > 5 to 10 implies multicollinearity
lm.fit1 <- lm(medv ~ . -age, data=Boston)        ### Excludes age
summary(lm.fit1)                                             
lm.fit1 <- update(lm.fit, ~ . -age)              ### achieves same result
#######################################
### ISLR Lab: Regression Extensions  ##
#######################################
## Interaction Terms
summary(lm(medv ~ lstat * age, data=Boston))
## Non-linear Transformations of the Predictors
lm.fit2 <- lm(medv ~ lstat + I(lstat ^ 2), data=Boston)
summary(lm.fit2)
lm.fit <- lm(medv ~ lstat, data=Boston)
anova(lm.fit, lm.fit2)
par(mfrow=c(2, 2))
plot(lm.fit2)
lm.fit5 <- lm(medv ~ poly(lstat, 5), data=Boston)
summary(lm.fit5)
summary(lm(medv ~ log(rm), data=Boston))
## Qualitative Predictors
names(Carseats)
head(Carseats)
lm.fit <- lm(Sales ~ . + Income : Advertising + Price : Age, data=Carseats)
summary(lm.fit)
contrasts(Carseats$ShelveLoc)
