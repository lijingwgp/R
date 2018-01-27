rm(list=ls())
require(ISLR)


# We first fit a fourth degree polynomial function 
fit <- lm(wage~poly(age, 4), data = Wage)
summary(fit)
coef(summary(fit))


# The poly() command allows us to avoid having to write out a long formula with powers of age
# There are several other equivalent ways of ﬁtting this model
# We can also create the polynomial basis functions or use cbind()
fit2a <- lm(wage~age+I(age^2)+I(age^3)+I(age^4),data = Wage)
summary((fit2a))
coef(summary((fit2a)))
fit2b <- lm(wage~cbind(age,age^2,age^3,age^4), data=Wage) 


# We now create a grid of values for age at which we want predictions, and then 
# call the generic predict() function, specifying that we want standard errors as well
age_limits <- range(Wage$age)
age_grid <- seq(age_limits[1], age_limits[2])
preds <- predict(fit, newdata = list(age = age_grid), se = TRUE)
se.bands <- cbind(preds$fit + 2*preds$se.fit, preds$fit - 2*preds$se.fit)


# Finally, we plot the data and add the ﬁt from the degree-4 polynomial
# Here the mar and oma arguments to par() allow us to control the margins of the plot
par(mfrow=c(1,1),mar=c(4.5,4.5,1,1) ,oma=c(0,0,4,0)) 
plot(Wage$age, Wage$wage, xlim=age_limits, cex=.5, col="darkgrey") 
title("Degree-4 Polynomial", outer=T) 
lines(age_grid, preds$fit, lwd=2, col="blue") 
matlines (age_grid, se.bands, lwd=1, col="blue", lty=3)


# In performing a polynomial regression we must decide on the degree of the 
# polynomial to use. One way to do this is by using hypothesis tests
# We now ﬁt models ranging from linear to a degree-5 polynomial and seek to determine 
# the simplest model which is suﬃcient to explain the relationship between wage and age
#
# We use the anova() function, in order to test the null
# hypothesis that a model M1 is suﬃcient to explain the data against the alternative 
# hypothesis that a more complex model M2 is required
fit.1 <- lm(wage~age, data = Wage)
fit.2 <- lm(wage~poly(age, 2), data = Wage)
fit.3 <- lm(wage~poly(age, 3), data = Wage)
fit.4 <- lm(wage~poly(age, 4), data = Wage)
fit.5 <- lm(wage~poly(age, 5), data = Wage)
anova(fit.1,fit.2,fit.3,fit.4,fit.5)


# The p-value comparing the linear Model 1 to the quadratic Model 2 is essentially zero
# Similarly the p-value comparing the quadratic Model 2 to the cubic Model 3 is very low 
# (0.0017), so the quadratic ﬁt is also insuﬃcient
# The p-value comparing the cubic and degree-4 polynomials, Model 3 and Model 4
# is approximately 5% while the degree-5 polynomial Model 5 seems unnecessary 
# because its p-value is 0.37. Hence, either a cubic or a quartic polynomial appear to 
# provide a reasonable ﬁt to the data, but lower- or higher-order models are not justiﬁed 
#
# As an alternative to using hypothesis tests and ANOVA, we could choose the polynomial 
# degree using cross-validation


# Next we consider the task of predicting whether an individual earns more than $250,000 per year
# Frst we create the appropriate response vector, and then apply the glm() function using family="binomial" 
# in order to ﬁt a polynomial logistic regression model
# Note that we again use the wrapper I() to create this binary response variable on the ﬂy
# Once again, we make predictions using the predict() function
fit=glm(I(wage >250)~poly(age,4), data=Wage, family='binomial') 
preds=predict(fit, newdata = list(age = age_grid), se=T)
pfit=exp(preds$fit) / (1+exp(preds$fit)) 


# The default prediction type for a glm() model is type="link", which is what we use here 
# This means we get predictions for the logit
# In order to obtain conﬁdence intervals for Pr(Y =1|X), we use the transformation
se.bands.logit = cbind(preds$fit + 2*preds$se.fit, preds$fit - 2*preds$se.fit) 
se.bands = exp(se.bands.logit)/(1+exp(se.bands.logit))


# Note that we could have directly computed the probabilities by selecting 
# the type="response" option in the predict() function
#preds=predict(fit, newdata =list(age=age_grid), type="response", se=T)
#se.bands = cbind(preds$fit+2*preds$se.fit, preds$fit-2*preds$se.fit)
plot(Wage$age, I(Wage$wage > 250), xlim=age_limits, type="n", ylim=c(0,.2)) 
points(jitter(Wage$age), I((Wage$wage > 250)/5), cex=.5, pch="|", col="darkgrey") 
lines(age_grid, pfit, lwd=2, col="blue")
#lines(age_grid, preds$fit, lwd=2, col="blue")
matlines(age_grid, se.bands, lwd = 1, col = 'blue', lty = 3)


# In order to ﬁt a step function, we use the cut() function
# Here cut() automatically picked the cutpoints at 33.5, 49, and 64.5 years of age
# We could also have speciﬁed our own cutpoints directly using the breaks option
# The function cut() returns an ordered categorical variable
# The lm() function then creates a set of dummy variables for use in the regression
#
# The age<33.5 category is left out, so the intercept coeﬃcient of $94,160 can be interpreted 
# as the average salary for those under 33.5 years of age
# and the other coeﬃcients can be interpreted as the average additional salary for those 
# in the other age groups
table(cut(Wage$age, 4))
fit = lm(wage~cut(age, 4), data = Wage)
coef(summary(fit))
