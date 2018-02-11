rm(list=ls())
######################### 
######## GLM  ########### 
######################### 
set.seed(5082)
mydata <- read.table('http://quantedu.com/wp-content/uploads/2014/05/job.txt', header=T)
n <- nrow(mydata)
names(mydata)
summary(mydata)
str(mydata)
mydata[,'Race'] <- factor(mydata[,'Race'])
train = sample(1:n, 0.75 * n)
fit.1 <- glm(Job ~ ., data=mydata, family = poisson, subset=train)
summary(fit.1)
# These are the coefficients of the log of the fit - we need to take the exp of these 
exp(cbind(coef(fit.1), confint(fit.1)))
# Normally if 0 is in the confint of the coefficient, then the variable is not significant.
# Since exp(0) = 1, if 1 is in the confint of the coefficient, then the variable is not significant.
# So GPA and Race3 are not significant - since Race2 is significant, we will leave Race in but exclude GPA.
# The interpretation of these values is Race2, for example, will bet 4.75 times as many job offers as Race1.
fit.2 <- glm(Job ~ Race + Income, data=mydata, family = poisson, subset=train)
summary(fit.2)
exp(cbind(coef(fit.2), confint(fit.2)))
# Predict on the test set
preds <- predict.glm(fit.2, newdata=mydata[-train, ], type='response')
table('actuals'=mydata[-train, 1], 'prediction'=round(preds, 0))
