#################################################
### Transformation for variance stabilization ###
#################################################
# with ETS models, we used multiplicative errors and multiplicative seasonality 
# to handel time series that have variance which increases with the level of the series
#
# an alternative approach is to transform the time series
# some common transformation for stabilizing variation:
#   square root
#   cube root
#   log
#   inverse

autoplot(AirPassengers)
autoplot(AirPassengers^.5)
autoplot(AirPassengers^.33333)
autoplot(log(AirPassengers))
autoplot(-1/AirPassengers)

# these four transformations are closely related to the Box-Cox transformations
# it has single parameter lambda which controls how strong the transformation is
#   lambda = 1 means no substantive transformation
#   lambda = 0 means log transformation
#   lambda = -1 means inverse transformation

BoxCox.lambda(AirPassengers)
AirPassengers %>%
  ets(lambda = -.2947156) %>%
  forecast(h = 60) %>%
  autoplot()



###############################################################
### Non-seasonal and seasonal differencing for stationarity ###
###############################################################
# differencing is a way of making a time series stationary; this means that you 
# remove any systematic patterns such as trend and seasonality from the data. a 
# white noise series is considered a special case of a staitonary time series.

# with non-seasonal data, you use lag-1 differences to model changes between observations
# rather than the observations directly. 

autoplot(Nile)
autoplot(diff(Nile))
ggAcf(diff(Nile))

# seasonal differencing for stationarity
# with seaonal data, differences are often taken between observations in the same season 
# of consecutive years, tather than in consecutive periods. 
#
# for example, with quarterly data, one would take the difference between Q1 in one year 
# and Q1 in the previous year. this is called seasonal differencing.
#
# sometimes we need to apply both seasonal differences and lag-1 difference to the 
# same series, thus, calculating the differeces in the differences. 

autoplot(AirPassengers)
autoplot(diff(log(AirPassengers), lag = 12))
autoplot(diff(diff(log(AirPassengers), lag = 12)))
ggAcf(diff(diff(log(AirPassengers), lag = 12)))



####################
### ARIMA models ###
####################
# ARIMA stands for auto-regressive integrated moving average
#   auto-regressive is simply a regression of a time series with lagged observations as predictors
#   moving average is simply a regression of a time series with lagged errors as predictors
#   ARMA is the regression with lagged observations and lagged errors as predictors
#
# ARMA can only work with stationary data
# that brings us ARIMA model with I indicates the number of times 
# we need to difference the data to make it stationary

# ARIMA(p,d,q)
# we need to decide the value for p, d, and q

fit <- auto.arima(AirPassengers)
summary(fit)

# in this case, a model with 2,1,1 and 0,1,0, lag=12 is chosen
# so the data has been differenced once, and two past observations and two past errors
# have been used in the equation.
#
# note that we can not compare a ARIMA AICc value to a ETS AICc value
# we can only compare model performance metrics between models that are in the same class

fit %>% forecast() %>% autoplot()

# auto ARIMA models for non-seasonal time series
# Fit an automatic ARIMA model to the austa series
fit <- auto.arima(austres)
# Check that the residuals look like white noise
checkresiduals(fit)
# Summarize the model
summary(fit)
# Plot forecasts of fit
fit %>% forecast(h = 40) %>% autoplot()

# forecasting with ARIMA models
# The automatic method in the previous exercise chose an ARIMA(0,1,1) 
# with drift model for the data
#
# You will now experiment with various other ARIMA models for the data to see 
# what difference it makes to the forecasts.

# The Arima() function can be used to select a specific ARIMA model. 
# Its first argument, order, is set to a vector that specifies the values of p, d and q.
# The second argument, include.constant, is a booolean that determines if the 
# constant c, or drift, should be included. 

# Plot forecasts from an ARIMA(0,1,1) model with no drift.
austres %>% Arima(order = c(0, 1, 1), include.constant = F) %>% forecast() %>% autoplot()
# Plot forecasts from an ARIMA(2,1,3) model with drift.
austres %>% Arima(order = c(2, 1, 3), include.drift = T) %>% forecast() %>% autoplot()
# Plot forecasts from an ARIMA(0,0,1) model with a constant
austres %>% Arima(order = c(0, 0, 1), include.constant = T) %>% forecast() %>% autoplot()
# Plot forecasts from an ARIMA(0,2,1) model with no constant
austres %>% Arima(order = c(0, 2, 1), include.constant = F) %>% forecast() %>% autoplot()

# comparing auto.arima() and ets() on non-seasonal data
# The AICc statistic is useful for selecting between models in the same class.
# However, you cannot use it to compare ETS and ARIMA models because they are in 
# different model classes.
#
# Instead, you can use time series cross-validation to compare an ARIMA model 
# and an ETS model on the austa data. 
#
# Because tsCV() requires functions that return forecast objects, you will set 
# up some simple functions that fit the models and return the forecasts. 
#
# The arguments of tsCV() are a time series, forecast function, and forecast horizon h.

# Set up forecast functions for ETS and ARIMA models
fets <- function(x, h) {
  forecast(ets(x), h = h)
}
farima <- function(x, h) {
  forecast(auto.arima(x), h = h)
}
# Compute CV errors for ETS as e1
e1 <- tsCV(austa, fets, h = 1)
# Compute CV errors for ARIMA as e2
e2 <- tsCV(austa, farima, h = 1)
# Find MSE of each model class
mean(e1^2, na.rm = TRUE)
mean(e2^2, na.rm = TRUE)
# Plot 10-year forecasts using the best model class
austa %>% farima(h = 10) %>% autoplot()



#############################
### Seasonal ARIMA models ###
#############################
# arima models can also handle seasonlities, we just need to add seasonal differencing
# and seasonal lag terms into the model. these parameters are represented as
#   non-seasonal part: (p, d, q)
#     p: number of ordinary AR lags
#     q: number of ordinary MA lags
#     d: number of lag-1 differences
#   seasonal part: (P, D, Q)_m
#     P: number of seasonal AR lags
#     Q: number of seasonal MA lags
#     m: number of observations per year
#
# for even lower AICc when using auto.arima(), set stepweise = FALSE

# comparing auto.arima() and ets() on seasonal data
#
# What happens when you want to create training and test sets for data that 
# is more frequent than yearly? If needed, you can use a vector in form 
# c(year, period) for the start and/or end keywords in the window() function. 
# You must also ensure that you're using the appropriate values of h in 
# forecasting functions. Recall that h should be equal to the length of the 
# data that makes up your test set.
#
# For example, if your data spans 15 years, your training set consists of the 
# first 10 years, and you intend to forecast the last 5 years of data, 
# you would use h = 12 * 5 not h = 5 because your test set would include 60 
# monthly observations. If instead your training set consists of the first 9.5 
# years and you want forecast the last 5.5 years, you would use h = 66 to account 
# for the extra 6 months.

