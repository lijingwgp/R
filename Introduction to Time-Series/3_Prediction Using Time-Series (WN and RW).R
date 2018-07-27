####################################
### Prediction using time series ###
####################################
# Time series trend:
#   linear
#   rapid growth
#   periodic
#   increasing variance trends over time

# Sample transformations:
#   log() can linearize a rapid growth trend
#   diff() can remove linear trend
#   diff(..., lag=s) can remove periodic trends

linear_growth <- log(rapid_growth)
ts.plot(linear_growth)

# Differencing a time series can remove a time trend
# The function diff() will calculate the first difference or change series
# A difference series lets you examine the increments or changes in a given time series
# It always has one fewer observations than the original series

dz <- diff(z)
ts.plot(dz)
length(z)
length(dz)
dx <- diff(x, s=4)
ts.plot(dx)

# White Noise 
# The white noise (WN) model is a basic time series model
# WN is the simplest example of a stationary process
#   a fixed, constant mean
#   a fixed, constant variance
#   no correlation over time
#
# arima.sim() function can be used to simulate data from a variety of time series models
# arima(p,d,q) model has three parts, the autoregressive order p, the order of integration d, and the moving average order q
# arima(0,0,0) is simply the WN model

white_noise <- arima.sim(model = list(order= c(0,0,0)), n = 100)
ts.plot(white_noise)    
white_noise_2 <- arima.sim(model = list(c(0,0,0)), n = 100, mean = 100, sd = 10)
ts.plot(white_noise_2)

# Estimate the white noise model
# For a given time series y we can fit the white noise (WN) model using 
# the arima(..., order = c(0, 0, 0)) function
#
# Recall that the WN model is an ARIMA(0,0,0) model
# Applying the arima() function returns information or output about the estimated model
#
# For the WN model this includes the estimated mean (labeled intercept)
# and the estimated variace (labeled sigma^2)

arima(y, order = c(0,0,0))
mean(y)
var(y)

# Random Walk
# The random walk (RW) model is the cumulative sum of a mean zero white noise series
# RW is a simple example of a non-stationary process
#   no specified mean or variance
#   strong dependence over time
#   its changes or increments are white noise
#
# The random walk model:
# today = yesterday + noise
# the noise term represent white noise
# noise = today - yesterday
# 
# Apply diff() to RW, get WN
# a RW model is an arima(0,1,0) model

random_walk <- arima.sim(model = list(order = c(0,1,0)), n = 100)
ts.plot(random_walk)
random_walk_diff <- diff(random_walk)
ts.plot(random_walk_diff)

# RW model with a drift
# A RW need not wander about zero, it can have an upward or downward trajectory
# This is done by including an intercept in the RW model, which corresponds to the slope
# of the RW time trend

rw_drift <- arima.sim(model = list(order = c(0,1,0)), n = 100, mean = 1)
ts.plot(rw_drift)
rw_drift_diff <- diff(rw_drift)
ts.plot(rw_drift_diff)

# Estimate the random walk model
# For a given time series y we can fit the random walk model with a drift by first 
# differencing the data, then fitting the white noise model to the differenced data using
# the arima() function with the order = c(0,0,0) argument
#
# The arima() displays outputs about the fitted model
# Under the coefficients: heading is the estimated drift variable, named the intercept
# The variance of the WN part of the model is also estimated under the label sigma^2

# The time series random_walk has already been loaded
# Use diff() to generate the first difference of the data

rw_diff <- diff(random_walk)
model_wn <- arima(rw_diff, order= c(0,0,0))
int_wn <- model_wn$coef
ts.plot(random_walk)
abline(0, int_wn)

# Or, we could just directly estimate the RW model by using the arima() function 
# with order = c(0,1,0)

arima(random_walk, order = c(0,1,0))

# Stationary Processes
#   stationary models are parsimonious
#   stationary models have fewer parameters
#   stationary processes have distributional stability over time
#
# Observed time series fluctuate randomly but behave similarly from one time 
# period to the next, this is called mean reverting
#
# Weak Stationarity mean, variance, covariance constant over time

# Are the WN or the RW model stationary?
# The white noise (WN) and random walk (RW) models are very closely related
# However, only the RW is always non-stationary, both with and without a drift term
#
# Recall that if we start with a mean zero WN process and compute its running or cumulative
# sum, the result is a RW process. The cumsum() function will make this transformation for us
# 
# Similarly, if we create a WN process, but change its mean from zero, and then compute its
# cumulative sum, the result is a RW process with a drift

# Use arima.sim() to generate WN data
white_noise <- arima.sim(model = list(order = c(0,0,0)), n = 100)
# Use cumsum() to convert your WN data to RW
random_walk <- cumsum(white_noise)
# Use arima.sim() to generate WN drift data
wn_drift <- arima.sim(model = list(order = c(0,0,0)), n = 100, mean = 0.4)
# Use cumsum() to convert your WN drift data to RW
rw_drift <- cumsum(wn_drift)
# Plot all four data objects
plot.ts(cbind(white_noise, random_walk, wn_drift, rw_drift))
