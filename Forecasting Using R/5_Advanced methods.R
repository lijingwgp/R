##########################
### Dynamic regression ###
##########################
# so far we have used forecasting methods only based on historical data, but do not use 
# other information. but often, addtional information will help us make better decision.
#
# forcasting montly sales, we could use advertisement expenditure for the month to 
# improve forecast. or perhaps information based on comparable competitor activities.

# dynamic regression is one way to combine external information with history of the time series
# in a single model.
#   y = constant + slope1*x1 + ... + slop100*x100 + error
#   in dynamic regression, we allow the error term to ban an ARIMA process
#   in ordinary regression, we assume that error term is white noise

# US personal consumption and income
# we could forecast consumption and use income as a predictor variable

fit <- auto.arima(uschange[,"Consumption"], xreg = uschange[,"Income"])

# to forecast a dynamic regression model, we need to provide future values of the 
# predictors. either we could forecast the predictor values in a seperate model or
# the future values of the predictor needs to be pass to the xreg argument in the 
# forecast() function

fcast <- forecast(fit, xreg = rep(.8, 8))
autoplot(fcast) + 
  xlab("Year") + ylab("Percentage change")

# we can set up matrix of regressor to include more than 1 variables
xreg <- cbind(MaxTemp = elec[, "Temperature"], 
              MaxTempSq = elec[,'Temperature']^2, 
              Workday = elec[,'Workday'])
fit <- auto.arima(elec[,'Demand'], xreg = xreg)
# Forecast fit one day ahead
forecast(fit, xreg = cbind(20, 20^2, 1))



###################################
### Dynamic harmonic regression ###
###################################
# This type of regression use Fourier terms to handle seasonality
# Fourier was a French mathematician who showed that a series of sine and cosine 
# terms of the right frequencies can approximate any periodic function.
# We can use this for seasonal patterns when forecasting.

# Fourier terms come in pairs consisting of sine and cosine
# the frequency of these terms are called harmonic frequencies and they increase with K
# These Fourier terms are our predictors in the dynamic harmonic regression model.
# The more terms included the more complicated that seasonality will be.

# we choose k to represent how many terms are included
# alpha and gamma are coefficients 
# m is seasonal period
# because the seasonality is being modeled by the Fourier terms, we normally use a 
# non-seasonal ARIMA model for the errors

# sine term = sin((2*pi*k*t)/m)
# cosine term = cos((2*pi*k*t)/m)
# y_t = Beta_0 + summation of 1~K (alpha * sine term(t) + gamma * cosine term(t)) + error

# the main difference between a seasonal ARIMA and a dynamic harmonic regression is that 
# seasonal ARIMA allows the seasonal pattern to change over time
# whereas dynamic harmonic regression assumes the seasonal pattern stays the same

# With weekly data, it is difficult to handle seasonality using ETS or ARIMA models
# as the seasonal length is too large (approximately 52). Instead, you can use 
# harmonic regression which uses sines and cosines to model the seasonality.
#
# The fourier() function makes it easy to generate the required harmonics. 
# The higher the order (K), the more "wiggly" the seasonal pattern is allowed to be.
# With K=1, it is a simple sine curve. You can select the value of K by minimizing 
# the AICc value.

# Set up harmonic regressors of order 13
harmonics <- fourier(gasoline, K = 13)
# Fit regression model with ARIMA errors
fit <- auto.arima(gasoline, xreg = harmonics, seasonal = FALSE)
# Forecasts next 3 years
newharmonics <- fourier(gasoline, K = 13, h = 156)
fc <- forecast(fit, xreg = newharmonics)
# Plot forecasts fc
autoplot(fc)



####################################################
### Harmonic regression for multiple seasonality ###
####################################################
# Harmonic regressions are also useful when time series have multiple seasonal patterns. 
# For example, "taylor" dataset contains half-hourly electricity demand in England 
# and Wales over a few months in the year 2000. The seasonal periods are 48 
# (daily seasonality) and 7 x 48 = 336 (weekly seasonality). 
# There is not enough data to consider annual seasonality.
#
# auto.arima() would take a long time to fit a long time series such as this one, 
# so instead you will fit a standard regression model with Fourier terms using the 
# tslm() function. This is very similar to lm() but is designed to handle time series.

# Fit a harmonic regression using order 10 for each type of seasonality
fit <- tslm(taylor ~ fourier(taylor, K = c(10, 10)))
# Forecast 20 working days ahead
fc <- forecast(fit, newdata = data.frame(fourier(taylor, K = c(10,10), h = 20*48)))
# Plot the forecasts
autoplot(fc)
# Check the residuals of fit
checkresiduals(fit)

# Another time series with multiple seasonal periods is calls, which contains 20 
# consecutive days of 5-minute call volume data for a large North American bank. 
# There are 169 5-minute periods in a working day, and so the weekly seasonal 
# frequency is 5 x 169 = 845. The weekly seasonality is relatively weak, so here 
# you will just model daily seasonality.

# Plot the calls data
autoplot(calls)
# Set up the xreg matrix
xreg <- fourier(calls, K = c(10,0))
# Fit a dynamic regression model
fit <- auto.arima(calls, xreg = xreg, seasonal = FALSE, stationary = TRUE)
# Check the residuals
checkresiduals(fit)
# Plot forecasts for 10 working days ahead
fc <- forecast(fit, xreg =  fourier(calls, c(10, 0), h = 10*169))
autoplot(fc)
