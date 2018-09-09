######################
### Autoregression ###
######################
# The simplest AR model is the AR recursion model:
#   today = constant + slope * yesterday + noise
# 
# Mean centered version of the AR model:
#   (today - mean) = slope * (yesterday - mean) + noise
#
# Autocorrelation means correlation with the past
# The magnitude of autocorrelation is determined by the slope
#
# If slope = 0, then today = mean + noise
# If slope != 0, then today depends on both yesterday and noise
#
# large values of slope lead to greater autocorrelation
# Negative values of slope result in oscillatory time series
# 
# When mean = 0, and slope = 1 then the AR model become the RW model
# Thus the AR model includes both the WN and RW model as special cases

# The arima.sim() function can also be used to simulate data from an AR model by setting
# the model argument equal to list(ar = phi), in which phi is a slope parameter from the 
# interval (-1,1). We also need to specify a series lenght n

a <- arima.sim(model = list(ar = 0.1), n = 100)
x <- arima.sim(model = list(ar = 0.5), n = 100)
y <- arima.sim(model = list(ar = 0.9), n = 100)
z <- arima.sim(model = list(ar = -0.75), n = 100)
plot.ts(cbind(a, x, y, z))

# Estimate the autocorrelation function (ACF) for an autoregression
# acf() function allow us to estimate the autocorrelation function by exploring lags in the data
# By default, this function generates a plot of the relationship between the current observation
# and the lags extending backwards

acf(a)
acf(x)
acf(y)
acf(z)

# Compare the RW model and AR model
# The RW model is a special case of the AR model, in which the slope parameter equal to 1
# Recall from previous note that RW model is not stationary and exhibits very strong persistance
# This implies that when an AR model has slope = 1, it exhibits high persistance
# This further implies that an AR model achieves stationarity when its slope is between -1 and 1
#
# ACF graph shows decaying to zero slowly meaning that past values have a long lasting 
# impact on current values
# ACF graph shows decaying to zero quickly meaning that values far in the past have little impact on the
# current values

# Simulate and plot AR model with slope 0.9 
x <- arima.sim(model = list(ar = 0.9), n = 200)
ts.plot(x)
acf(x)
# Simulate and plot AR model with slope 0.95
y <- arima.sim(model = list(ar = 0.95), n = 200)
ts.plot(y)
acf(y)
# Simulate and plot RW model
z <- arima.sim(model = list(order = c(0,1,0)), n = 200)
ts.plot(z)
acf(z)

# the AR model represented by series y exhibits greater persistence than series x, 
# but the ACF continues to decay to 0. By contrast, the RW model represented 
# by series z shows considerable persistence and relatively little decay in the ACF

# AR Model Estimation and Forecasting
require(Ecdat)
data(Mishkin, package = "Ecdat")
# The first column is a one-month US inflation rate
inflation <- as.ts(Mishkin[,1])
# Mostly positive and moderately strong persistence
ts.plot(inflation)
# Strong positive but decaying autocorrelation
acf(inflation)
# Applying AR model 
inflation.arima <- arima(inflation, order = c(1,0,0))
inflation.arima

# From the output, we typically monitor three parameters
# They are mean (intercept), slope (ar1) and variance (sigma^2)

inflation.fitted <- inflation - residuals(inflation.arima)
ts.plot(inflation)
points(inflation.fitted, type = 'l',col = 'red', lty = 2)

# Forecasting
predict(inflation.arima)
predict(inflation.arima, n.ahead = 6)

