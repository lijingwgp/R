#######################################
### The Simple Moving Average Model ###
#######################################
# The weighted sum of current and previous noise is called a simple moving average process
#   today = mean + today's noise + slope(yesterday's noise)
# 
# If slope is 0, then the MA is simply a WN process
# If slope is not 0, then the MA model depends on both current and previous noise
# A larger slope leads to greater autocorrelation
# Negative slope result in oscillartory time series
#
# The MA process has autocorrelation determined by the magnitude and sign of the slope parameter
# However, it only has dependency over 1 period. The autocorrelation is 0 at lag 2 and higher
# 
# If the slope is positive, the lag 1 autocorrelation will be positive
# If the slope is negative, the lag 1 autocorrelation will be negative

# The simple moving average (MA) model is a parsimonious time series model used to 
# account for very short-run autocorrelation. It does have a regression like form, 
# but here each observation is regressed on the previous innovation, which is not 
# actually observed. Like the autoregressive (AR) model, the MA model includes the 
# white noise (WN) model as special case

# Generate MA model with slope 0.1
x <- arima.sim(model = list(ma = 0.1), n = 100)
# Generate MA model with slope 0.5
y <- arima.sim(model = list(ma = 0.5), n = 100)
# Generate MA model with slope 0.9
z <- arima.sim(model = list(ma = 0.9), n = 100)
# Generate MA model with slope -0.5
k <- arima.sim(model = list(ma = -.5), n = 100)
# Plot all three models together
plot.ts(cbind(x, y, z, k))

# Note that there is some very short-run persistence for the positive slope values 
# (x and y), and the series has a tendency to alternate when the slope value 
# is negative (z)

# Estimate the autocorrelation function (ACF) for a MA model
acf(a)
acf(x)
acf(y)
acf(z)

# MA Model Estimation and Forecasting
require(Ecdat)
data("Mishkin", package = "Ecdat")
inflation <- as.ts(Mishkin[,1])
inflation_changes <- diff(inflation)
ts.plot(inflation)
ts.plot(inflation_changes)
acf(inflation_changes, lag.max = 24)

MA_inflation_changes <- arima(inflation_changes, order = c(0,0,1))
print(MA_inflation_changes)

ts.plot(inflation_changes)
MA_inflation_changes_fitted <- inflation_changes - residuals(MA_inflation_changes)
points(MA_inflation_changes_fitted, type = 'l', col = 'red', lty = 2)

predict(MA_inflation_changes)
predict(MA_inflation_changes, n.ahead = 6)



####################################
### Compare the AR and MA Models ###
####################################
# AR model
# today - mean = slope * (yesterday - mean) + noise
#
# MA model
# today = mean + today's noise + slope * yesterday's noise
#
# Where noise are centered around 0 and has variance of sigma^2
# AR has autocorrelation at many lags, MA has autocorrelation at one lag
#
# AIC and/or BIC is used for selecting an optimal model
# Smaller values are preferred in both cases

# Find correlation between AR_fit and MA_fit
cor(AR_fit, MA_fit)
# Find AIC of AR
AIC(AR)

