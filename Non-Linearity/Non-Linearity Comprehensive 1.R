rm(list=ls()) 
require(ISLR)
require(e1071)
##################### 
#### Part 1
##################### 
# a)
# use the following code to create a set of indices containing a random sample
# of 800 integers representing the training subset of set OJ
# and a set of test indices representing the remaining observations
set.seed(5082) 
n = dim(OJ)[1] 
train_inds = sample(1:n,800) 
test_inds = (1:n)[-train_inds] 

# b)
# fit a support vector classifier to the training data using cost = 0.01
# with Purchase as the response and other variables as predictors
# use the summary() to produce summary statistics and describe the results obtained
svc.fit <- svm(Purchase~., data=OJ[train_inds,], kernel = 'linear', cost = 0.01, scale = T)
summary(svc.fit)
print('There are two classes being identified. They are CH and MM.')
print('There are total of 446 support vectors. 223 of them are in the CH class, and the remaining is in the MM class')

# c)
# compute and display the training and test error rate
pred.train <- predict(svc.fit, OJ[train_inds,])
table(truth=OJ[train_inds,1], predict=pred.train)
mean(pred.train != OJ[train_inds,1])
pred.test <- predict(svc.fit, OJ[test_inds,])
table(OJ[test_inds,1], pred.test)
mean(pred.test != OJ[test_inds,1])
print("The training error rate of SVC is 0.169, and the test error rate of SVC is 0.152")

# d)
# use the tune() to select an optimal cost
# consider values in the range 0.01 to 10
svc.cv <- tune(svm, Purchase~., data = OJ, kernel = 'linear', 
               ranges=list(cost=seq(0.01,10,by=0.1)), scale = T)
summary(svc.cv)
svc.best <- svc.cv$best.model

# e)
# compute and display the training and test error rates using this new value for cost
pred.train <- predict(svc.best, OJ[train_inds,])
table(OJ[train_inds,1], pred.train)
mean(pred.train != OJ[train_inds,1])
pred.test <- predict(svc.best, OJ[test_inds,])
table(OJ[test_inds,1], pred.test)
mean(pred.test != OJ[test_inds,1])
print('The CV training error rate of SVC is 0.165, and the CV test error rate of SVC is 0.148')

# f)
# repeat part b through part e using a svm with radial kernel
# use the default value for gamma
svm.radial <- svm(Purchase~., data=OJ[train_inds,], kernel = 'radial', cost = 0.01, scale = T)
summary(svm.radial)
print('There are two classes being identified. They are CH and MM.')
print('There are total of 613 support vectors. 308 of them are in the CH class, and the remaining is in the MM class')

pred.train <- predict(svm.radial, OJ[train_inds,])
table(truth=OJ[train_inds,1], predict=pred.train)
mean(pred.train != OJ[train_inds,1])
pred.test <- predict(svm.radial, OJ[test_inds,])
table(OJ[test_inds,1], pred.test)
mean(pred.test != OJ[test_inds,1])
print('The training error rate of SVM with radial method is 0.381, the test error rate of SVM with radial method is 0.415')

svm.cv1 <- tune(svm, Purchase~., data = OJ, kernel = 'radial', 
                ranges=list(cost=seq(0.01,10,by=0.1)), scale = T)
summary(svm.cv1)
svm.best1 <- svm.cv1$best.model

pred.train <- predict(svm.best1, OJ[train_inds,])
table(OJ[train_inds,1], pred.train)
mean(pred.train != OJ[train_inds,1])
pred.test <- predict(svm.best1, OJ[test_inds,])
table(OJ[test_inds,1], pred.test)
mean(pred.test != OJ[test_inds,1])
print('The CV training error rate of SVM with radial method is 0.155, the CV test error rate of SVM with radial method is 0.137')

# g)
# repeat part b through part e using a svm with polynomial kernel
# use the default value for gamma
# set degree =2
svm.poly <- svm(Purchase~., data=OJ[train_inds,], kernel = 'polynomial', cost = 0.01, 
                scale = T, degree = 2)
summary(svm.poly)
print('There are two classes being identified. They are CH and MM.')
print('There are total of 614 support vectors. 309 of them are in the CH class, and the remaining is in the MM class')

pred.train <- predict(svm.poly, OJ[train_inds,])
table(truth=OJ[train_inds,1], predict=pred.train)
mean(pred.train != OJ[train_inds,1])
pred.test <- predict(svm.poly, OJ[test_inds,])
table(OJ[test_inds,1], pred.test)
mean(pred.test != OJ[test_inds,1])
print('The training error rate of SVM with poly method is 0.381, the test error rate of SVM with poly method is 0.415')

svm.cv2 <- tune(svm, Purchase~., data = OJ, kernel = 'polynomial', 
                ranges=list(cost=seq(0.01,10,by=0.1)), scale = T, degree = 2)
summary(svm.cv2)
svm.best2 <- svm.cv2$best.model

pred.train <- predict(svm.best2, OJ[train_inds,])
table(OJ[train_inds,1], pred.train)
mean(pred.train != OJ[train_inds,1])
pred.test <- predict(svm.best2, OJ[test_inds,])
table(OJ[test_inds,1], pred.test)
mean(pred.test != OJ[test_inds,1])
print('The CV training error rate of SVM with poly method is 0.158, the CV test error rate of SVM with poly method is 0.144')

# h)
# overall, which approach seems to give the best results on this data
print('Overall, SVM with radial based kernel relatively performs the best')



rm(list=ls()) 
require(ISLR)
require(boot)
##################### 
#### Part 2
##################### 
# b)
# set seed to 5082. using the wage data, performing polynomial regression to predict
# wage using polynomial degrees from 1 to 10, and use 10-fold cross validation
# to select the optimal degree d from 10 choices
set.seed(5082)
err = rep(NA, 10)
for(i in 1:10){
  glm.fit.poly = glm(wage~poly(age, i), data = Wage)
  err[i] = cv.glm(Wage, glm.fit.poly, K = 10)$delta[2]
}
which.min(err)

# c)
# plot the errors from the cross validation. what degree was chosen?
plot(1:10, err, xlab = 'Degree', ylab = 'CV Error', type = 'l', pch = 20, lwd = 2,
     ylim = c(1590, 1700))
err_min = min(err)
err_sd = sd(err)
abline(h=err_min + 0.1 * err_sd, col="red", lty="dashed")
abline(h=err_min - 0.1 * err_sd, col="red", lty="dashed")
legend("topright", "0.1-standard deviation lines", lty="dashed", col="red")
print(paste("The smallest mse occured when degree d =", which.min(err)))

# d)
# plot the original data and polynomial fit using the optimal degree d
# by breaking the range of age into 100 partitions, use the model to predeict
# these points and plot the points against the predictions
plot(wage~age, data=Wage, col="darkgrey")
age_limit = range(Wage$age)
age_grid = seq(age_limit[1], age_limit[2], length.out = 100)
lm.fit = lm(wage~poly(age, 9), data=Wage)
lm.pred = predict(lm.fit, newdata = list(age=age_grid))
lines(age_grid, lm.pred, col="blue", lwd=2)

# e)
# fit a step function to predict wage using age
# set seed to 5082, then investigate step functions using steps from 1 to 12
# use 10-fold cross validation to choose the optimal number of steps
set.seed(5082)
err = rep(NA, 13)
for(i in 2:13){
  Wage$age.cut = cut(Wage$age, i)
  glm.fit.cut = glm(wage~age.cut, data = Wage)
  err[i] = cv.glm(Wage, glm.fit.cut, K = 10)$delta[2]
}
which.min(err)

# f)
# plot the CV errors as a function of the number of cuts. what was the optimum number of 
# cut?
plot(2:13, err[-1], xlab = '# Cuts', ylab = 'CV Error', type = 'l', pch = 20, lwd = 2,
     ylim = c(1590, 1750))
err_min = min(err[-1])
err_sd = sd(err[-1])
abline(h=err_min + 0.1 * err_sd, col="red", lty="dashed")
abline(h=err_min - 0.1 * err_sd, col="red", lty="dashed")
legend("topright", "0.1-standard deviation lines", lty="dashed", col="red")
print(paste("The CV shows that the lowest mse occured when there is",which.min(err),"intervals"))

# g)
# create a model using this optimal number of cuts and plot this model's fitted values 
# as a function of the Wage$age data
glm.fit.cut = glm(wage~cut(age, 8), data = Wage)
lm.pred = predict(glm.fit.cut, newdata = list(age=age_grid))
plot(wage~age, data=Wage, col="darkgrey")
lines(age_grid, lm.pred, col="blue", lwd=2)



rm(list=ls()) 
require(MASS)
require(boot)
require(splines)
##################### 
#### Part 3
##################### 
# b) 
# plot the polynomial fits for polynomial degress from 1 to 10, and report the associated 
# RSS for the training error in a table
attach(Boston)
set.seed(5082)
disrange <- range(dis)
dissamples <- seq(from=disrange[1], to=disrange[2], length.out=100) 
rss = rep(NA, 10)
for(i in 1:10){
  glm.fit.poly = lm(nox~poly(dis, i), data = Boston)
  rss[i] = sum(glm.fit.poly$residuals^2)
}
rss
plot(nox~dis, data=Boston, col="darkgrey")
for (i in 1:10) {
  lm.fit = lm(nox~poly(dis, i), data = Boston)
  lm.pred = predict(lm.fit, newdata = list(dis = dissamples))
  lines(dissamples, lm.pred, col = i, lwd = 1)
}
legend("topright","Degree", col=c(1,2,3,4,5,6,7,8,9,10))

# b) 1)
# set the seed to 5082, then perform CV to select the optimal degree for the 
# polynomial, determine the degree with the minimum CV error and plot the 
# original data points and the fit resulting from the optimal degree
set.seed(5082)
err = rep(NA, 10)
for(i in 1:10){
  glm.fit.poly = glm(nox~poly(dis, i), data = Boston)
  err[i] = cv.glm(Boston, glm.fit.poly, K = 10)$delta[2]
}
print(paste("The CV shows that the lowest mse is",min(err)))
print(paste("it is occured when the degree is",which.min(err)))
plot(1:10, err, xlab = "Degree", ylab = "CV error", type = "l", pch = 20, 
     lwd = 2)
err_min = min(err)
err_sd = sd(err)
abline(h=err_min + 0.1 * err_sd, col="red", lty="dashed")
abline(h=err_min - 0.1 * err_sd, col="red", lty="dashed")
lm.fit = lm(nox ~ poly(dis, 3), data = Boston)
lm.pred = predict(lm.fit, list(dis = dissamples))
plot(nox ~ dis, data = Boston, col = "darkgrey")
lines(dissamples, lm.pred, col = "red", lwd = 2)

# c)
# use the bs() function to fit a regression spline to predict nox using dis
# with 4 degrees of freedom
# report the output for the fit using summary(), then answer the following questions:
# how were the knots chosen?
# where was the knots placed?
# plot the resulting fit
glm.fit.spline = lm(nox~bs(dis, df=4), data = Boston)
summary(glm.fit.spline)
attr(bs(Boston$dis, df=4), "knots") 
print("since we specify the df to be 4, the bs() function will produce 4 basis functions of the form X, X^2, X^3, and h(x,3.20745)")
print("this implies that there will be one knot chosen and the place where this knot is placed is at x=3.20745")
print('the reason why this knot is placed at x=3.20745 is because the bs() automatically place this knot at 50th percentile of the population in the X space')
spline.pred = predict(glm.fit.spline, newdata = list(dis = dissamples))
plot(nox ~ dis, data = Boston, col = "darkgrey")
lines(dissamples, spline.pred, col = "red", lwd = 2)

# d)
# now fit a regression spline for a range of degrees of freedom from 3 to 10, plot
# the resulting fits and report the associated RSS in a table
rss = rep(NA, 10)
for(i in 3:10){
  lm.fit.spline = lm(nox~bs(dis, df = i), data = Boston)
  rss[i] = sum(lm.fit.spline$residuals^2)
}
rss[-(1:2)]
plot(nox~dis, data=Boston, col="darkgrey")
for (i in 3:10) {
  lm.fit.spline = lm(nox~bs(dis, df = i), data = Boston)
  lm.pred = predict(lm.fit.spline, newdata = list(dis = dissamples))
  lines(dissamples, lm.pred, col = i, lwd = 1)
}
legend()

# e)
# set the seed to 5082, then perform 10-fold CV in order to select the best df (from 3 to 10)
# for a regression spline on this data. plot your results, including your best df in the 
# chart title
set.seed(5082)
err = rep(NA, 10)
for(i in 3:10){
  glm.fit.spline = glm(nox~bs(dis, df = i), data = Boston)
  err[i] = cv.glm(Boston, glm.fit.spline, K = 10)$delta[2]
}
print(paste('the minimum mse is', min(err[-(1:2)])))
print(paste("The CV shows that the lowest mse occured at",which.min(err),"degrees of freedom"))
plot(3:10, err[-c(1, 2)], lwd = 2, type = "l", xlab = "df", ylab = "CV error")
err_min = min(err[-(1:2)])
err_sd = sd(err[-(1:2)])
abline(h=err_min + 0.1 * err_sd, col="red", lty="dashed")
abline(h=err_min - 0.1 * err_sd, col="red", lty="dashed")
legend("topright", "0.1-standard deviation lines", lty="dashed", col="red", cex = .75)
lm.fit.spline = lm(nox~bs(dis, df = 10), data = Boston)
pred = predict(lm.fit.spline, newdata = list(dis = dissamples))
plot(nox ~ dis, data = Boston, col = "darkgrey", main = 'Regression Spine with best
     d.f.(10) chosen with c.v.')
lines(dissamples, pred, col = "red", lwd = 2)

# g)
# set the seed to 5082, then perform 10-fold CV in order to select the best lambda (from 3 to 10)
# for a smoothing spline on this data
# plot your results, including your best lambda in the chart title
set.seed(5082)
smooth.fit = smooth.spline(jitter(dis), nox, cv = T)
smooth.fit$lambda
smooth.fit$df
plot(nox ~ dis, data = Boston, col = "darkgrey", main = 'Smoothing Spine with best
     lambda(6.9e-05) chosen with c.v.')
lines(smooth.fit, col = 'red', lwd = 2)
