# 8 - Polynomial, Step Functions, Splines, GAMs in brief
# Fit some of the non-linear models investigated in this chapter to the Auto data set
# Is there evidence for non-linear relationships in this data set? 
require(ISLR)
require(boot)
set.seed(1)
pairs(Auto)
print('mpg appears to inversely related to cylinders, horsepower, displacement, and weight')
# Polynomial
err = rep(NA, 15)
for(i in 1:15){
  glm.fit.poly = glm(mpg~poly(displacement, i), data = Auto)
  err[i] = cv.glm(Auto, glm.fit.poly, K = 10)$delta[2]
}
print(paste("The smallest mse occured when k =", which.min(err)))
rss = rep(NA, 10)
fits = c()
for (d in 1:10) {
  fits[[d]] = lm(mpg ~ poly(displacement, d), data = Auto)
  rss[d] = deviance(fits[[d]])
}
rss
anova(fits[[1]], fits[[2]], fits[[3]], fits[[4]])
print('The Anova result suggests that a quadratic polynomial model is sufficient')
# Step functions
err = rep(NA, 10)
for(i in 2:10){
  Auto$displacement.cut = cut(Auto$displacement, i)
  glm.fit.cut = glm(mpg~displacement.cut, data = Auto)
  err[i] = cv.glm(Auto, glm.fit.cut, K = 10)$delta[2]
}
print(paste("The CV shows that the lowest mse occured when there is",which.min(err),"cuts"))
# Splines
require(splines)
err = rep(NA, 10)
for(df in 3:10){
  glm.fit.spline = glm(mpg~ns(displacement, df = df), data = Auto)
  err[df] = cv.glm(Auto, glm.fit.spline, K = 10)$delta[2]
}
print(paste("The CV shows that the lowest mse occured when there is",which.min(err),"cuts"))
attr(ns(Auto$displacement, df=9), "knots") 
# GAMs
require(gam)
err = rep(NA, 10)
fits = list()
for(i in 2:10){
  fits[[i]] = gam(mpg~s(displacement, i)+s(horsepower, i), data = Auto)
  err[i] = fits[[i]]$deviance
}
anova(fits[[2]],fits[[3]],fits[[4]],fits[[5]],fits[[6]],fits[[7]])



# 10 - GAMs in depth
# This question relates to the College data set
# a)
# Split the data into a training set and a test set. Using out-of-state tuition 
# as the response and the other variables as the predictors
# perform forward stepwise selection on the training set in order to identify 
# a satisfactory model that uses just a subset of the predictors
rm(list=ls())
set.seed(1)
require(ISLR)
require(leaps)
attach(College)
#detach(College)
head(College)
dim(College)
train = sample(length(Outstate), length(Outstate)*0.8)
test = -train
College.train = College[train,]
College.test = College[test,]
reg.fit = regsubsets(Outstate~., data = College.train, nvmax = 17, method = 'forward')
reg.summary = summary(reg.fit)
par(mfrow = c(1,3))
plot(reg.summary$cp, xlab = "Number of Variables", ylab = "Cp", type = "l", 
     ylim = c(-10,820))
min.cp = min(reg.summary$cp)
std.cp = sd(reg.summary$cp)
abline(h = min.cp + 0.2 * std.cp, col = "red", lty = 2)
abline(h = min.cp - 0.2 * std.cp, col = "red", lty = 2)
plot(reg.summary$bic, xlab = "Number of Variables", ylab = "BIC", type = "l",
     ylim = c(-850,-350))
min.bic = min(reg.summary$bic)
std.bic = sd(reg.summary$bic)
abline(h = min.bic + 0.2 * std.bic, col = "red", lty = 2)
abline(h = min.bic - 0.2 * std.bic, col = "red", lty = 2)
plot(reg.summary$adjr2, xlab = "Number of Variables", ylab = "Adjusted R2", 
     type = "l", ylim = c(0.4, 0.84))
max.adjr2 = max(reg.summary$adjr2)
std.adjr2 = sd(reg.summary$adjr2)
abline(h = max.adjr2 + 0.2 * std.adjr2, col = "red", lty = 2)
abline(h = max.adjr2 - 0.2 * std.adjr2, col = "red", lty = 2)
print('All cp, BIC and adjr2 scores show that size 6 is the minimum size for the subset for which the scores are withing 0.2 standard deviations of optimum. We pick 6 as the best subset size')
reg.fit = regsubsets(Outstate ~ ., data = College, method = "forward")
coefi = coef(reg.fit, id = 6)
names(coefi)

# b)
# Fit a GAM on the training data, using out-of-state tuition as the response 
# and the features selected in the previous step as the predictors
# Plot the results, and explain your ﬁndings
require(gam)
pairs(~Outstate+Private+Room.Board+PhD+perc.alumni+Expend+Grad.Rate)
mydf <- College
n <- nrow(College)
mydf <- mydf[sample(1:n, n),]
numfolds <- 10
fold.indices <- cut(1:n, breaks=numfolds, labels=FALSE)
mse <- matrix(0,nrow=10,ncol= numfolds,byrow = TRUE)
for(i in 2:10){
  for(j in 1:numfolds){
    test.indices <- which(fold.indices == j)
    test.data <- mydf[test.indices, ]
    train.data <- mydf[-test.indices, ]
    gam.fit <- gam(Outstate~s(PhD,i))
    mse[i,j] <- mean((predict(gam.fit, newdata = test.data)-test.data$Outstate)^2)
  }
}
mse_mean = c()
for(i in 2:10){
  mse_mean[i] = mean(mse[i,1:10])
}

