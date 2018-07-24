#######################################
### Forecasts and potential futures ###
#######################################
# Each time we simulate from the data, we get a different future. As we keep sampling
# we get a distribution of possible future sample paths. This forms the distribution of
# potential future. This is because the future is full of many possibilities. 
#
# The lines from this distribution is made of simulated futures. A forecast is a mean of such  
# distribtion.

# The very simplest forecasting method is to use the most recent observation; 
# this is called a naive forecast and can be implemented in a namesake function.
# 
# This is the best that can be done for many time series including most stock 
# price data, and even if it is not a good forecasting method, it provides a 
# useful benchmark for other forecasting methods.

# For seasonal data, a related idea is to use the corresponding season from 
# the last year of data. For example, if you want to forecast the sales volume 
# for next March, you would use the sales volume from the previous March.
# This is implemented in the snaive() function, meaning, seasonal naive.

# naive(y, h=10)
# snaive(y, h=2*frequency(X))
# For both forecasting methods, we can set the second argument h, which specifies the 
# number of values you want to forecast.

# Use naive() to forecast the next 20 values of the goog series, and save this to fcgoog.
fcgoog <- naive(goog, h=20)
# Plot and summarize the forecasts
summary(fcgoog)
autoplot(fcgoog)
# Use snaive() to forecast the ausbeer series
fcbeer <- snaive(ausbeer, h=16)
summary(fcbeer)
autoplot(fcbeer)



###################################
### Fitted values and residuals ###
###################################
# one way to check if our forecasting method is any good is to try to forecast
# what you have already seen. 
# 
# the data that is already seen is called fitted values. 
# A fitted value is the forecast of an observation using all previous observations
#
# A redidual is the difference between an observation and its fitted value

fc <- naive(oil)
autoplot(oil, series = "Data") + 
  xlab("Year") +
  autolayer(fitted(fc), series = "Fitted") +
  ggtitle("Oil production in Saudi Arabia")

# we hope that the residuals would look like white noise because that implies the forecast
# method we use captured all the avaliable information in the data.

# there are 4 assumptions that we make about the residuals, 2 are essential and 2 are convenient
#   1. residuals should be uncorrelated
#   2. residuals should have 0 mean
#   3. residuals should have constant variance
#   4. residuals should be normally distributed
#
# when residuals looks like white noise, assumption 1,2,3 are satisfied.
# we can test these assumptions using the checkresiduals() function. 

checkresiduals(fc)

# Check the residuals from the naive forecasts applied to the goog series
goog %>% naive() %>% checkresiduals()
# Check the residuals from the seasonal naive forecasts applied to the ausbeer series
ausbeer %>% snaive() %>% checkresiduals()



##############################
### Training and test sets ###
##############################
# we can hide some amount of data near towards the end of the data and this will be the test set
# the test set must not be used for any aspect of calculating forecast
#
# build forecasts using training set
# a model which fits the training data well will not necessarily forecast well

training <- window(oil, end=2003)
test <- window(oil, start=2004)
fc <- naive(training, h=10)
autoplot(fc) +
  autolayer(test, series = "Test data")

# forecast "error" is the difference between observed value and its forecast in the test set
# forecast "error" is not residuals
#
# MAE: mean absolute error
# MSE: mean squared error
# MAPE: mean absolute percentage error
# MASE: mean absolute scaled error

# Create the training data as train
autoplot(gold)
train <- subset(gold, end = 1000)
autoplot(train)
test <- subset(gold, start = 1001, end = 1108)
# Compute naive forecasts for the remaining 108 days
naive_fc <- naive(train, h = 108)
# Compute mean forecasts and save to mean_fc
mean_fc <- meanf(train, h = 108)
# Use accuracy() to compute RMSE statistics
accuracy(mean_fc, gold)
accuracy(naive_fc, gold)
autoplot(naive_fc) +
  autolayer(test, series = "Test data")
autoplot(mean_fc) +
  autolayer(test, series = "Test data")



########################################################
### Evaluating forecast accuracy of seasonal methods ###
########################################################
# window() function specifies the start and end of a time series using the 
# relevant times rather than the index values. Either of those two arguments 
# can be formatted as a vector like c(year, period) which you have also previously 
# used as an argument for ts(). Again, period refers to quarter here.

# We will work with the Melbourne quarterly visitor numbers
# Create three training series omitting the last 1, 2, and 3 years
train1 <- window(vn[, "Melbourne"], end = c(2014, 4))
train2 <- window(vn[, "Melbourne"], end = c(2013, 4))
train3 <- window(vn[, "Melbourne"], end = c(2012, 4))
# Produce forecasts using snaive()
fc1 <- snaive(train1, h = 4)
fc2 <- snaive(train2, h = 4)
fc3 <- snaive(train3, h = 4)
# Use accuracy() to compare the MAPE of each series
accuracy(fc1, vn[, "Melbourne"])["Test set", "MAPE"]
accuracy(fc2, vn[, "Melbourne"])["Test set", "MAPE"]
accuracy(fc3, vn[, "Melbourne"])["Test set", "MAPE"]



####################################
### Time series cross-validation ###
####################################
# The tsCV() function computes time series cross-validation errors. 
# It requires you to specify the time series, the forecast method, 
# and the forecast horizon.

# Using the goog data and forecasting with the naive() function, compute the 
# cross-validated errors for up to 8 steps ahead. Assign this to e.
length(goog)
e <- matrix(NA, nrow = 1000, ncol = 8)
for (i in 1:8)
  e[,i] <- tsCV(goog, forecastfunction = naive, h = i)
# Compute the MSE values and remove missing values
mse <- colMeans(e^2, na.rm = TRUE)
# Plot the MSE values against the forecast horizon
data.frame(h = 1:8, MSE = mse) %>%
  ggplot(aes(x = h, y = MSE)) + geom_point()

