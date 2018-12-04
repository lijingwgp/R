########################################
### Exponentially weighted forecasts ###
########################################
# two very simple forecasting methods, the naive method and the mean method
# the naive method uses only the most recent observation as the forecast for all future periods
# the mean method use the average of all observations as forecast for all future periods
#
# something between these two extremes would be useful
# forecast based on all observations but with the most recent observations more heavily weighted
# this is the idea of exponentially weighted forecasts which is commonly known as 
# simple exponential smoothing
#
# simple exponential smoothing equation
# tomorrow's price given today's price
#     = smoothing factor*today's price
#     + smoothing factor*(1-smoothing factor)*yesterday's price
#     + smoothing factor*(1-smoothing factor)^2 * two days before price

# ses() function is designed to handle simple exponential smoothing
autoplot(oil)
oildata <- window(oil, start = 1996)
end(oildata)
fc <- ses(oildata, h=5)
summary(fc)
# in summary we observe that the alpha value is 0.8339 which suggest that 83% of 
# the forecast is based on the most recent observation
autoplot(fc) + ylab("Oil (millions of tonnes)") + xlab("Year")
# we can also see the fitted values and add that to the plot
autoplot(fc) + autolayer(fitted(fc))

# SES vs naive
#   1. import and load the data, determine how much of data we want to allocate to training
#   2. subset the data to create a trianing set
#   3. compute forecast of the training set using whichever forecasting functions we choose, and set h
#   4. to view the result, use accuracy() function
#   5. pick a measure in the output, such as RMSE or MAE to evaluate the forecast

autoplot(marathon)
# Using subset(), create a training set for marathon comprising all but the last 20 years of 
# the data which you will reserve for testing.
train <- subset(marathon, end = length(marathon) - 20)
# Compute the SES and naive forecasts of this training set and save them to fcses and fcnaive, respectively.
fcses <- ses(train, h = 20)
fcnaive <- naive(train, h = 20)
# Calculate forecast accuracy measures of the two sets of forecasts using the accuracy() function.
accuracy(fcses, marathon)
accuracy(fcnaive, marathon)

autoplot(fcses) + autolayer(fitted(fcses))
autoplot(fcnaive) + autolayer(fitted(fcnaive))



################################################
### Exponential smoothing methods with trend ###
################################################
# Holt's method is SES with a trend
AirPassengers %>% holt(h = 5) %>% autoplot
# Holt's method produces forecasts with a trend continues as the same slope idefinitely 
# into the future.
# 
# a variation on the Holt's method is to allow the trend to damp over time so that 
# it levels off to its constant value
fc1 <- holt(oil, h = 15, PI = FALSE)
fc2 <- holt(oil, damped = T, h = 15, PI = FALSE)
autoplot(oil) + autolayer(fc1, series = "Linear trend") + autolayer(fc2, series = "Damped trend")

summary(fc1)
summary(fc2)
checkresiduals(fc1)
checkresiduals(fc2)



################################################################
### Exponential smoothing methods with trend and seasonality ###
################################################################
# Holt-Winters' method is SES with trend and seasonality
aust <- window(austourists, start = 2005)
fc1 <- hw(aust, seasonal = "additive")
fc2 <- hw(aust, seasonal = "multiplicative")
autoplot(fc1)
autoplot(fc2)

# Holt-Winters method on daily data
# Using subset(), set up a training set where the last 4 weeks of the available 
# data in hyndsight have been omitted.
train <- subset.ts(hyndsight, end = length(hyndsight)-28)
# Produce forecasts for these last 4 weeks using hw() and additive seasonality 
# applied to the training data. Assign this to fchw.
fchw <- hw(train, seasonal = "additive", h = 28)
# Produce seasonal naive forecasts for the same period. Use the appropriate 
# function, introduced in a previous chapter, and assign this to fcsn.
fcsn <- snaive(train, h = 28)
# Which is the better of the two forecasts based on RMSE? Use the accuracy() to determine.
accuracy(fchw, hyndsight)
accuracy(fcsn, hyndsight)
autoplot(fchw)



####################################################
### State space models for exponential smoothing ###
####################################################
# all of the exponential smoothing methods can be written as an "innovations state space model"
#
# recall that there are there are:
#   3 possible trends: none, additive, damped
#   3 possible seasonality: none, additive, multiplicative
#   
# given 9 possible exponential smoothing methods, each of these can be written as
# state space modeling two different ways. one with additive errors and one with 
# multiplicative errors. so there are 18 possible staet space models
#
# multiplicative errors means the noise increases with the level of the series
# multiplicative seasonality means the seasonal flunctuation increase the the level of the series
#
# these are known as ETS models: error, trend, seasonal
# the advantage of the ETS models are following:
#   parameters: estimated using the "likelihood", the probability of the data arising from 
#               the specified model
#   for models with additive errors, this is equivalent to minimizing SSE

ets(austres)
# ets() function finds the best ETS model when a time series is passed in
# from this example, the best model is a ETS(M,Ad,A) model.
#   multiplicative errors
#   damped trend
#   additive seasonality
#
# ets() does not forecast for you, rather it produces a model
# we can pass the model to the forecast functions
austres %>% ets() %>% forecast() %>% autoplot()

# Fit ETS model to austa in fitaus
fitaus <- ets(austres)
# Check residuals
checkresiduals(fitaus)
# Plot forecasts
autoplot(forecast(fitaus))

# ETS vs seasonal naive
# Function to return ETS forecasts
fets <- function(y, h) {
  forecast(ets(y), h = h)
}
# Apply tsCV() for both methods
e1 <- tsCV(cement, fets, h = 4)
e2 <- tsCV(cement, snaive, h = 4)
# Compute MSE of resulting errors (watch out for missing values)
mean(e1^2, na.rm = TRUE)
mean(e2^2, na.rm = TRUE)
# Copy the best forecast MSE
bestmse <- 0.02921384

# When ETS fail
# Plot the lynx series
autoplot(lynx)
# Use ets() to model the lynx series
fit <- ets(lynx)
# Use summary() to look at model and parameters
summary(fit)
# Plot 20-year forecasts of the lynx series
fit %>% forecast(h=20) %>% autoplot() 
