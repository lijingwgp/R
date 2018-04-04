# In order to ﬁt regression splines in R, we use the splines library
# Regression splines can be ﬁt by constructing an appropriate matrix of basis functions
# The bs() function generates the entire matrix of basis functions for splines with the 
# speciﬁed set of knots
# By default, cubic splines are produced
library(ISLR)
library(splines)
age_limits <- range(Wage$age)
age_grid <- seq(from=age_limits[1], to=age_limits[2]) 
fit = lm(wage~bs(age, knots = c(25,40,60)), data = Wage)
pred = predict(fit, newdata = list(age = age_grid), se = T)
plot(Wage$age, Wage$wage, col = 'gray')
lines(age_grid, pred$fit, lwd = 2)
lines(age_grid, pred$fit + 2*pred$se, lty="dashed") 
lines(age_grid, pred$fit - 2*pred$se, lty="dashed")


# Here we have prespeciﬁed knots at ages 25, 40, and 60
# This produces a spline with six basis functions, which means we have a 
# total of 6 degrees of freedom
# Out of these 6 degrees of freedom, three comes from the form X, X^2, X^3
# The other three comes from the form h(x,25), h(x,40), h(x,60)
#
# Recall that a cubic spline with three knots has seven degrees of freedom in total
# One intercept plus six basis functions
# An intercept essentially frees up one degrees of freedom
# Therefore, by excluding the intercept, we have a remaining of 6 degrees of freedom
dim(bs(Wage$age, knots = c(25,40,60)))
# We could also use the df option to produce a spline with knots at uniform quantiles of the data
attr(bs(Wage$age, df=6), "knots") 
# In this case R chooses knots at ages 33.8,42.0, and 51.0, which correspond to the 
# 25th, 50th, and 75th percentiles of age


# The function bs() also has a degree argument, so we can ﬁt splines of any degree, 
# rather than the default degree of 3 (which yields a cubic spline)
fit = lm(wage~bs(age, knots = c(25,40,60), degree = 4), data = Wage)
dim(bs(Wage$age, knots = c(25,40,60), degree = 4))
# Now we specified the total degree of freedom to be 8
# This means a quartic polynomial function would be produced with total of 
# 5 degrees of freedom, including the intercept
#
# There would be 4 knots automatically chosen since these 4 knots would have 4 predictor
# coefficients that need to be determined, and we have specified the total df to be 8
attr(bs(Wage$age, df=8, degree = 4), "knots") 


# In order to instead ﬁt a natural spline, we use the ns() function
# Here we ﬁt a natural spline with four degrees of freedom
fit2 = lm(wage~ns(age, df=4), data=Wage) 
pred2 = predict(fit2, newdata = list(age=age_grid), se = T)
lines(age_grid, pred2$fit, col = 'red', lwd = 2)
# As with the bs() function, we could instead specify the knots 
# directly using the knots option
fit2 = lm(wage~ns(age, knots = c(25,40,60)), data = Wage)


# We would have a total of 4 degrees of freedom
# each predictor space would need one regression coefficient
dim(ns(Wage$age, knots = c(25,40,60)))
# This would automatically produce 7 knots across the predictor space interval 
attr(ns(Wage$age, df=8), "knots") 


# In order to ﬁt a smoothing spline, we use the smooth.spline() function
plot(Wage$age, Wage$wage, xlim=age_limits, cex=.5, col="darkgrey") 
title("Smoothing Spline", outer = T) 
fit = smooth.spline(Wage$age, Wage$wage, df=16) 
fit2 = smooth.spline(Wage$age, Wage$wage, cv = TRUE)
fit2$df
lines(fit,col="red",lwd=2)
lines(fit2, col="blue", lwd=2)
legend("topright",legend=c("16 DF","6.8 DF"),col=c("red","blue"),lty=1,lwd=2,cex=.8)
# Notice that in the ﬁrst call to smooth.spline(), we speciﬁed df=16
# The function then determines which value of λ leads to 16 degrees of freedom
# In the second call to smooth.spline(), we select the smoothness level by 
# crossvalidation; this results in a value of λ that yields 6.8 degrees of freedom


# In order to perform local regression, we use the loess() function
plot(Wage$age, Wage$wage, xlim=age_limits, cex=.5, col="darkgrey") 
title("Local Regression", outer = T) 
fit=loess(wage~age, span=.2, data=Wage) 
fit2=loess(wage~age, span=.5, data=Wage) 
lines(age_grid, predict (fit, data.frame(age=age_grid)), col="red", lwd=2) 
lines(age_grid, predict (fit2, data.frame(age=age_grid)), col="blue", lwd=2)
legend("topright",legend=c("Span=0.2","Span=0.5"),col=c("red","blue"),lty=1,lwd=2,cex=.8)
