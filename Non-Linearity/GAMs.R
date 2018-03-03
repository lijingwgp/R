# We now ﬁt a GAM to predict wage using natural spline functions of year and age, 
# treating education as a qualitative predictor
# Since this is just a big linear regression model using an appropriate 
# choice of basis functions, we can simply do this using the lm() function
library(ISLR)
library(splines)
library(gam)
library(akima)
gam1 = lm(wage~ns(year,4) + ns(age,5) + education, data=Wage) 


# We now ﬁt the model (7.16) using smoothing splines rather than natural splines
# In order to ﬁt more general sorts of GAMs, using smoothing splines or other components 
# that cannot be expressed in terms of basis functions and then ﬁt using 
# least squares regression, we will need to use the gam library in R
install.packages("gam")
library(gam)
gam2 = gam(wage~s(year,4) + s(age,5) + education, data=Wage)
# The s() function, which is part of the gam library, is used to indicate that 
# we would like to use a smoothing spline
# We specify that the function of year should have 4 degrees of freedom, 
# and that the function of age will have 5 degrees of freedom
# Since education is qualitative, we leave it as is
par(mfrow=c(1,3))
plot(gam2, se=TRUE, col = 'blue')
par(mfrow=c(1,3))
plot.gam(gam2, se=TRUE, col = 'blue')
par(mfrow=c(1,3))
plot.gam(gam1, se=TRUE, col = 'blue')


# We observe that the function of year looks rather linear
# We can perform a series of ANOVA tests in order to determine which of these three models 
# is best: a GAM that excludes year, a GAM that uses a linear function of year, 
# or a GAM that uses a spline function of year
gam.m1=gam(wage~s(age,5) + education, data = Wage) 
gam.m2=gam(wage~year + s(age,5) + education, data = Wage)
gam.m3=gam(wage~s(year,4) + s(age,5) + education, data = Wage)
anova(gam.m1, gam.m2, gam.m3, test = 'F')
# We ﬁnd that there is compelling evidence that a GAM with a linear function of year 
# is better than a GAM that does not include year at all (p-value=0.00014)
# However, there is no evidence that a non-linear function of year is needed


# The summary() function produces a summary of the gam ﬁt
summary(gam.m3)
# The large p-value for year reinforces our conclusion from the ANOVA test 
# that a linear function is adequate for this term
# However, there is very clear evidence that a non-linear term is required for age


# We can make predictions from gam objects, just like from lm objects, 
# using the predict() method for the class gam
# Here we make predictions on the training set
pred = predict(gam.m2, newdata = Wage)


# We can also use local regression ﬁts as building blocks in a GAM, using the lo() function
gam.local = gam(wage~s(year, 4) + lo(age, span = 0.7) + education, data = Wage)
plot.gam(gam.local, se=TRUE, col = 'green')
# Here we have used local regression for the age term, with a span of 0.7
# We can also use the lo() function to create interactions 
# before calling the gam() function
gam.local.interaction = gam(wage~lo(year, age, span = 0.5) + education, data = Wage)
# This model fits a two-term model, in which the ﬁrst term is an interaction 
# between year and age, ﬁt by a local regression surface
install.packages("akima")
library(akima)
par(mfrow = c(1,2))
plot(gam.local.interaction)


# In order to ﬁt a logistic regression GAM, we once again use the I() function 
# in constructing the binary response variable, and set family=binomial
gam.log = gam(I(wage>250)~year + s(age,5) + education, family = binomial, data = Wage)
par(mfrow = c(1,3))
plot(gam.log, se=T, col = 'green')
# It is easy to see that there are no high earners in the <HS category
table(Wage$education, I(Wage$wage>250))
# Hence, we ﬁt a logistic regression GAM using all but this category
# This provides more sensible results
gam.log.subset=gam(I(wage >250)~year+s(age,5)+education,family= binomial,data=Wage,subset =(education !="1. < HS Grad")) 
plot(gam.log.subset, se=T, col="green")
