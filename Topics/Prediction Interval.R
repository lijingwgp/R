# This is an addendum to the LearningCurveExample.R Script 
predict(mymodel, newdata=data.frame("x1"=mean(x1), "x2"=mean(x2)), interval='prediction')
# So we can't obtain a prediction for a new point (x1, x2) 
# because these have been transformed by the poly() function
# into a series of orthogonal vectors, one for each degree 
# requested in the original call to lm. We need to subject 
# our new observation to the same process, but we need at
# least as many unique points as the sum of all the degrees
# of the polynomial terms. The easiest way to do this is to
# pre-pend the observation we want to evaluate to the test
# set and re-do the model matrix. Hence...hence...
newy <- c(1, y[coeffs.test])  # the 1 could be any value - model.matrix doesn't use this
newx1 <- c(mean(x1), x1[coeffs.test])
newx2 <- c(mean(x2), x2[coeffs.test])
# Repeat the model matrix that created the model with the
# new item as the first observation
x.test <- model.matrix(newy ~ poly(newx1,polynomial.degree) + poly(newx2,polynomial.degree))[, -1]
newdata <- data.frame(x.test, "y"=1)
# Change the names of the test set to the names of the
# training set so they match up with the model
names(newdata) <- names(mydata.train)
# Get the prediction interval for a "typical" new point
predict(mymodel, newdata=newdata[1,], interval='prediction')