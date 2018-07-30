############################
### Pure Seasonal Models ###
############################
# We often collect data with a known seasonal component
#   Air Passengers (1 cycle every season = 12 month)
#   Johnson & Johnson Earnings (1 cycle every season = 4 quarters)

# A pure seasonal ARMA time series is correlated at the seasonal lags only. 
# Consequently, the ACF and PACF behave as the nonseasonal counterparts, but at the 
# seasonal lags, 1S, 2S, ..., where S is the seasonal period (S = 12 for monthly data).

# Fit a Pure Seasonal Model
# To get a feeling of how pure seasonal models work, we generated 250 observations from
# sarma(P=1, D=0, Q=1)_S=12 

# Plot sample P/ACF to lag 60 and compare to the true values
acf2(x, max.lag = 60)
# Fit the seasonal model to x
sarima(x, p = 0, d = 0, q = 0, P = 1, D = 0, Q = 1, S = 12)



#############################
### Mixed Seasonal Models ###
#############################
# Mixed model: SARIMA(p,d,q)x(P,D,Q)_S
# where capital letters denote the seasonal orders
#
# Consider SARIMA(0,0,1)x(1,0,0)
#   this month's value related to last year's value
#   this month's value related to last month's shock

# Air Passengers
plot(AirPassengers)
# Note there is non-constant variance
x <- log(AirPassengers)
plot(x)
# Note there is linear trend
x.1 <- diff(x)
plot(x.1)
# Note there is seasonality
x.2 <- diff(x.1, 12)
plot(x.2)

acf2(x.2, max.lag = 60)
sarima(x.2, 1,1,1,1,1,0,12)

# Now, fit a seasonal ARIMA model to the monthly unemployment data
# First, plot the data and notice the trend and the seasonal persistence
plot(unemp)
unemp.d <- diff(unemp)
plot(unemp.d)
# Then look at the detrended data and remove the seasonal persistence. 
unemp.d.d <- diff(unemp.d, lag = 12)
# After that, the fully differenced data should look stationary.
plot(unemp.d.d)
# Now, we are ready to fit an SARIMA model to the monthly unemployment time series data
acf2(unemp.d.d, max.lag = 60)
sarima(unemp, 2,1,0,0,1,1,12)

# Commodity prices data
# Monthly whole bird spot price
plot(chicken)
# Plot differenced chicken
plot(diff(chicken))
# Note that the trend is removed and note the seasonal behavior.
#
# Plot the sample ACF and PACF of the differenced data to lag 60 (5 years).
# After removing the trend, the sample ACF and PACF suggest an AR(2) model because 
# the PACF cuts off after lag 2 and the ACF tails off. However, the ACF has a small 
# seasonal component remaining. This can be taken care of by fitting an addition 
# SAR(1) component.
acf2(diff(chicken), max.lag = 60)
# Fit an ARIMA(2,1,0) to the chicken data to see that there is correlation remaining 
# in the residuals.
sarima(chicken,2,1,0)
# Fit SARIMA(2,1,0,1,0,0,12) to chicken - that works
sarima(chicken,2,1,0,1,0,0,12)

# Birth Rate data
# The data are monthly live births (adjusted) in thousands for the United States, 
# 1948-1979, and includes the baby boom after WWII.
# Plot P/ACF to lag 60 of differenced data
d_birth <- diff(birth)
plot(birth)
plot(d_birth)
acf2(d_birth, max.lag = 60)
# Use another call to diff() to take the seasonal difference of the data. 
# Save this to dd_birth. Use another call to acf2() to view the ACF and PACF 
# of this data, again to lag 60. 
dd_birth <- diff(d_birth, lag = 12)
plot(dd_birth)
acf2(dd_birth, max.lag = 60)
# Fit SARIMA(0,1,1)x(0,1,1)_12
sarima(birth,0,1,1,0,1,1,12)
# Add an additional AR (nonseasonal, p = 1) parameter to account for additional correlation.
sarima(birth,1,1,1,0,1,1,12)



##################################
### Forecasting Seasonal ARIMA ###
##################################
# Once model is chosen, forecasting is easy because the model describes how the dynamics
# of the time series behave over time
#
# Simply continue the model dynamics into the future
# Fit your previous model to unemp and check the diagnostics
sarima(unemp, 2,1,0,0,1,1,12)
# Forecast the data 3 years into the future
sarima.for(unemp,2,1,0,0,1,1,12,n.ahead = 36)

