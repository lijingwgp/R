########################
### Time Series Data ###
########################
require(astsa)
require(xts)

plot(jj, main = "Johnson & Johnson Quarterly Earnings per Share", type = "c")
text(jj, labels = 1:4, col = 1:4)

# upward trend
# seasonality
# 2nd and 3rd quarters are usually up, and the 4th quarters are usually down

plot(globtemp, main = "Global Temperature Deviations", type = "o")

# noise
plot(sp500w, main = "S&P 500 Weekly Returns")


#####################################
### Time Series Regression Models ###
#####################################
# autogression: today = slope * yesterday + noise
#
# typically time series data are correlated, failed to meet this criterion may lead to bad forecast
# one way to overcome this issue is to use moving average
#
# moving average: error = today's noise + slope * yesterday's noise
#
# ARMA: today = slope1 * yesterday + today's noise + slope2 * yesterday's noise

plot(AirPassengers)
plot(djia$Close)
plot(soi)


########################################
### Stationarity and Nonstationarity ###
########################################
# a time series is stationary when it is "stable", meaning:
#   the mean is constant (no trend)
#   the correlation structure remains constant over time

# stationarity is important because given data, we can estimate by averaging
# for example, if the mean is constant, we can estimate it by the sample average x bar
#
# if the correlation structure is constant, then we can use all pairs of data that are 1 time unit apart
# (x1,x2), (x2,x3), (x3,x4), ... 
# to estimate the lag 1 correlation

# it is resonable to assume the SOI data stationary, but perhaps some slight trend
#
# the scatterplot shows correlation in terms of lag
# to estimate autocorrelation, compute the correlation coefficient between the time series
# and itself at various lags

# when a time series is trend stationary, it will have stationary behavior around a trend

# random walk trend
# not stationary, but differenced data are stationary
# log() and diff() will help remove nonstationarity
# 
# random walk has the form:
# today = yesterday + white noise
# it is called a random walk because at time t the process is where it was at t-1 plus a 
# completely random movement
#
# for a random walk with drift, a constant is added to the model and will cause the 
# random walk to drift in the direction

par(mfrow = c(2,1))
plot(globtemp) 
plot(diff(globtemp))
par(mfrow = c(2,1))
plot(gnp)
plot(diff(log(gnp)))


####################################
### Stationary Time Series: ARMA ###
####################################
# it's been proved that any stationary time series may be represented as a linear combination
# of white noise
#
# it's also observed that any ARMA model has this form
# recall that ARMA model has the form:
#   today = slope1 * yesterday + today's noise + slope2 * yesterday's noise
# thus ARMA is suited to modeling these stationary time series

# generating ARMA using arima.sim()
# arima.sim(model, n)
#   model is a list with ordr of the model as c(p,d,q)
#   p is the order of AR
#   q is the order of MA
#   n is the lenght of the series

par(mfrow = c(1,1))
WN <- arima.sim(model = list(order = c(0,0,0)), n = 100)
plot(WN)
MA <- arima.sim(model = list(order = c(0,0,1), ma = 0.9), n = 100)
plot(MA)
AR <- arima.sim(model = list(order = c(2,0,0), ar = c(1.5,-.75)), n = 100)
plot(AR)

