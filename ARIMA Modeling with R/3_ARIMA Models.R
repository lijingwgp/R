# the autocorrelation function measures how a series is correlated with itself at different lags
#
# the partial autocorrelation function can be interpreted as a regression of the series against 
# its past lags. it helps you come up with a possible order of the auto regressive term
#
# as a rule of thumb, you use the acf for confirming trend and inferring possible values of the
# moving average parameters, and the pacf for the auto regressive part

# another way to explain pacf
# suppose we have 3 points in a time series, x3 - x2 - x1
# using acf we would generally find the correlation between x1 and x2, but this correlation
# is likely to be inspired by the x3
# so pacf is that portion of the correlation between x1 and x2 which is not explained by the correlation
# between x3 and x2



###############################
### ARIMA - Integrated ARMA ###
###############################
# a time series exhibits ARIMA behavior if the differenced data has ARMA behavior
# d = 1 means if we difference once, we will get stationarity
#
# an ARIMA model has the form:
# delta_X_current = slope * delta_X_previous + white noise
# delta_X: today - yesterday

x <- arima.sim(list(order = c(1,1,0), ar=.9), n=200)
plot(x, main="ARIMA(1,1,0)")
plot(diff(x), main="ARMA(1,0,0)")
acf2(x)
acf2(diff(x))

# as we observe, differencing the data in ARIMA(1,1,0) model makes it stationary and allows
# for further analysis

# globtemp data
# A plot of the data shows random walk behavior, which suggests you should work 
# with the differenced data

plot(globtemp)
plot(diff(globtemp))
acf2(diff(globtemp))

# After plotting the sample ACF and PACF of the differenced data diff(globtemp), 
# you can say that either:
#   The ACF and the PACF are both tailing off, implying an ARIMA(1,1,1) model
#   The ACF cuts off at lag 2, and the PACF is tailing off, implying an ARIMA(0,1,2) model
#   The ACF is tailing off and the PACF cuts off at lag 3, implying an ARIMA(3,1,0)
#   model. Although this model fits reasonably well, it is the worst of the three (you can check it) because it uses too many parameters for such small autocorrelations

# Fit an ARIMA(1,1,1) model to globtemp
sarima(globtemp, 1,1,1)
# Fit an ARIMA(0,1,2) model to globtemp. Which model is better?
sarima(globtemp, 0,1,2)
# Judging by the AIC and BIC, the ARIMA(0,1,2) model performs better than the ARIMA(1,1,1) model



#########################
### ARIMA Diagnostics ###
#########################
# We will consider overfitting, that is, once the model seems reasonable we can try
# add in a parameter to see if doing so makes a difference.
# If there is a difference, then we change our model. Otherwise, we are done.

# We generated 250 observations from an ARIMA(0,1,1) model with MA parameter .9.
#
# First, you will fit the model to the data using established techniques.
# Then, you can check a model by overfitting (adding a parameter) to see if it makes a difference.

x <- arima.sim(model = list(order = c(0,1,1), ma = .9), n = 250)
plot(x)
plot(diff(x))
# Plot sample P/ACF pair of the differenced data
# Note that the model is easily identified
acf2(x)
# Fit an ARIMA(0,1,1) model to the simulated data using sarima(). 
# Compare the MA parameter estimate to the actual value of .9, and examine 
# the residual plots.
sarima(x, 0,1,1)
# Overfit the model by adding an additional MA parameter. That is, 
# fit an ARIMA(0,1,2) to the data and compare it to the ARIMA(0,1,1) run.
sarima(x, 0,1,2)
# We can see that from the t-table, the second MA parameter is not significantly
# different from zero and the first MA parameter is approximately the same in each run.
# Also, the AIC and BIC both increase when the parameter is added.

# Fit an ARIMA(0,1,2) model to globtemp and check the diagnositcs.
sarima(globtemp, 0,1,2)
# Fit an ARIMA(1,1,1) model to globtemp and check the diagnostics.
sarima(globtemp, 1,1,1)
# Your model diagnostics suggest that both the ARIMA(0,1,2) and the ARIMA(1,1,1) 
# are reasonable models. However, the AIC and BIC suggest that the ARIMA(0,1,2) 
# performs slightly better, so this should be your preferred model. 



#########################
### Forecasting ARIMA ###
#########################
# Forecasting ARIMA Processes
#   the model describes how the dynamics of the time series behave over time
#   forecasting simply continues the model dynamics into the future
#   use sarima.for() to forecast

frequency(astsa::oil)
oil <- window(astsa::oil, end = 2006)
oil.real <- window(astsa::oil, end = 2007)
sarima.for(oil, n.ahead = 52, 1,1,1)
lines(oil.real)

# We generated 120 observations from an ARIMA(1,1,0) model with AR parameter .9. 
# The data are in y and the first 100 observations are in x.
full <- arima.sim(model = list(order = c(1,1,0), ar = .9), n = 120)
plot(full)
plot(diff(full))
x <- window(full, end = 100)
plot(x)
# Plot P/ACF pair of differenced data 
acf2(diff(x))
# Fit model - check t-table and diagnostics
# Use sarima() to fit an ARIMA(1,1,0) to the data
sarima(x, 1,1,0)
# Forecast the data 20 time periods ahead
sarima.for(x, n.ahead = 20, p = 1, d = 1, q = 0) 
lines(full)

# Fit an ARIMA(0,1,2) to globtemp and check the fit
sarima(globtemp, 0,1,2)
# Forecast data 35 years into the future
sarima.for(globtemp, n.ahead = 35, 0,1,2)

