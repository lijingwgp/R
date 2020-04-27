############################
### General Introduction ###
############################

https://blogs.oracle.com/datascience/introduction-to-forecasting-with-arima-in-r

## Business Uses
##  explaining seasonal patterns
##  predicting the expected number
##  estimating the effect of a newly launched product
##  detecting unusual events and estimating the magnitude


## The following is a general guidance when fitting ARIMA models;
##
##  1. Examine your data
##        plot the data and examine its patterns and irregularities
##        clean up any outliers or missing values
##        tsclean() is a convenient method for outlier removal and inputing missing values
##        take a log() to help stabilize a strong growth trend
##
##  2. Decompose your data
##        does the series appear to have trends or seasonality?
##        decompose() or stl() can examine and possibly remove components of the series
##
##  3. Stationarity
##        is the series stationary?
##        adf.test(), acf, pacf plots can determine the order of differencing needed
##
##  4. Autocorrelations and choosing model order
##        choose order of the ARIMA by examining acf and pacf plots
##
##  5. Fit an ARIMA model
##
##  6. Evaluate and iterate
##        check residuals, which should have no patterns and be normally distributed
##        if there are visible patterns or bias, plot acf/pacf. are any additional order parameters needed?
##        re-fit model if needed. compare model errors and fit criteria such as AIC and BIC
##        calculate forecast using the chosen model




#################
### Procedure ###
#################

# Step 1: Load R Packages

setwd('C:/Users/jing.o.li/Desktop')
require(ggplot2)
require(forecast)
require(tseries)
daily <- read.csv('day.csv', header = TRUE, stringsAsFactors = FALSE)


# Step 2: Examine the data
# a good starting point is to plot the series and visually examine it for any outliers
# or irregularities.
#
# from the graph of bicycle checkouts, we observe that lower usage of bicycles occurs
# in the winter months and higher checkout numbers are observed in the summer months

daily$Date <- as.Date(daily$dteday)
ggplot(daily, aes(Date, cnt)) +
  geom_line() +
  scale_x_date('month') +
  ylab("Daily Bike Checkouts")

# in some cases, the number of checked out dropped below 100 on one day and rose
# to over 4000 the next day. it is suspected that there are outliers that could bias 
# the model by skewing statistical summaries.
#
# tsclean() is a convenient way for removing time series outliers
# tsclean() is also capable of inputing missing values in the series 
# note that we need to use ts() to create a time series object and pass to tsclean()

cnt <- ts(daily[,c('cnt')])
daily$cnt_clean = tsclean(cnt)
autoplot(cnt)
autoplot(daily$cnt_clean)

# even after remomving outliers, the daily data is still pretty volatile. 
# visually, we could draw a line thorugh the series tracing its bigger troughts and peaks
# while smoothing out noisey fluctuations. 
#
# this line can be desbribed by one of the simplest - but also very useful - concepts in time series
# known as moving average. it's idea is intuitive that averages points across serveral 
# time periods, thereby smoothing the observed data into a more stable predictable series.

# note that the moving average in this context is distinct from the MA(q) component in the above
# ARIMA definition. MA(q) as part of the ARIMA framework refers to error lags and combinations, whereas the summary
# statistic of moving average refers to a data smoothing technique.
#
# the wider the window of the moving average, the smoother original series becomes.
# In our bicycle example, we can take weekly or monthly moving average, smoothing the series into 
# something more stable and therefore predictable:

daily$cnt_ma7 = ma(daily$cnt_clean, order=7) # using the clean count with no outliers
daily$cnt_ma30 = ma(daily$cnt_clean, order=30)
ggplot() +
  geom_line(data = daily, aes(x = Date, y = cnt_clean,   colour = "Counts"))  +
  geom_line(data = daily, aes(x = Date, y = cnt_ma7,   colour = "Weekly Moving Average"))  +
  geom_line(data = daily, aes(x = Date, y = cnt_ma30, colour = "Monthly Moving Average"))  +
  ylab('Bicycle Count')


# Step 3: Decompose your data
# the building block of a time series analysis are seasonality, trend, and cycle. 
# these intuitive components capture the historical patterns in the series. 
#
# not every series will have all three of these components, but if they are present,
# deconstructing the series can help you understand its behavior and prepare a foundation
# for building a forecasting model

# seasonal component
#   refers to fluctuations in the data related to calendar cycles.
#   usually, seasonality is fixed at some number.
#
# trend component
#   is the overall pattern of the series.
#
# cycle component
#   consists of decreasing or increasing patterns that are not seasonal.
#   usually, trend and cycle components are grouped together. trend-cycle component
#   is estimated using moving averages.

# finally, part of the series that can't be attributed to seasonal, cycle, or trend 
# components is referred to as residual or error. the process of extracting these
# components is referred to as decomposition.

# let's assume Y is the number of bikes rented, we can decompose the series in two ways:
# by using either additive or multiplicative model.
#
# multiplicative model
#   Y = season * trend and cycle * error
# additive model
#   Y = season + trend and cycle + error
#
# an additive model is better when the seasonal or trend component is not proportional
# to the level of the series, as we can just overlay components together to reconstruct
# the series. If the seasonality or trend changes with the level of series, a simply overlay
# won't be sufficient to reconstruct the series. in that case, a multiplicative model
# might be more appropriate.

# as mentioned, ARIMA models can be fitted to both seasonal and non-seasonal data. 
# Seasonal ARIMA requires a more complicated specification of the model structure,
# through the process of determining (P, D, Q)

# now, let's explore how to de-seasonalize the series and use a non-seasonal ARIMA model.
# first we need to calculate seasonal component of the data using stl().
# stl() is a flexible function for decomposing and forecasting the series. 
# it calculates the seasonal component of the series using smoothing, and adjusts the 
# original series by subtracting seasonality.

cnt_daily <- ts(na.omit(daily$cnt_ma7), frequency=30)
decomp <- stl(cnt_daily, s.window = "periodic")
plot(decomp)

# Note that stl() by default assumes additive model structure.
# Use allow.multiplicative.trend=TRUE to incorporate the multiplicative model.
#
# As for the frequency parameter in ts() object, we are specifying periodicity of 
# the data, i.e., number of observations per period. Since we are using smoothed 
# daily data, we have 30 observations per month.

# In the case of additive model structure, the same task of decomposing the series
# and removing the seasonality can be accomplished by simply subtracting the 
# seasonal component from the original series. seasadj() is a convenient method from
# the forecast package

deseasonal_cnt <- seasadj(decomp)
plot(deseasonal_cnt)


# Step 4: Stationarity
# fitting an ARIMA model requires the series to be stationary. a series is said to be
# stationary when its mean, variance, and autocovariance are time invariant.
#
# this assumption make sense since ARIMA uses previous lags of series to model its behavior,
# modeling stable series with consistent properties involves less uncertainty.

# the ADF test is a formal statistical test for stationarity. the null hypothesis 
# assumes that the series is non-stationary. ADF tests whether the change in Y can be
# explained by lagged value and a linear trend. 
#
# if contribution of the lagged value to the change in Y is non-significant and 
# there is a presence of a trend component, the series is non-stationary and null 
# hypothesis will not be rejected.

# Our bicycle data is non-stationary; the average number of bike checkouts changes 
# through time, levels change. A formal ADF test does not reject the null 
# hypothesis of non-stationarity, confirming our visual inspection: 

adf.test(cnt_daily, alternative = "stationary")

# Usually, non-stationary series can be corrected by a simple transformation such as 
# differencing. Differencing the series can help in removing its trend or cycles.
# 
# The idea behind differencing is that, if the original data series does not have 
# constant properties over time, then the change from one period to another might.

# The difference is calculated by subtracting one period's values from the previous 
# period's values
#
#   Y_diff1 = Y_today - Y_yesterday
#   Y_diff2 = (Y_today - Y_yesterday) - (Y_yesterday - Y_before_yesterday)
#
# Similarly, differencing can be used if there is a seasonal pattern at specific lags.
# in such case, subtracting a value for the "season" from a previous period represents
# the change from one period to another.
#
#   Y_diff1_season = (Y_today - Y_today_last season) - (Y_yesterday - Y_yesterday_last season)


# Step 5: Autocorrelations and Choosing Model Order
# autocorrelation plots, also known as ACF, are a useful visual tool in determinng whether a
# a series is stationary. these plots can also help to choose the order parameters
# for ARIMA model.
#
# if the series is correlated with its lags then, there are some trend or seasonal components
# and therefore its statistical properties are not constant over time.

# ACF plots isplay correlation between a series and its lags. In addition to 
# suggesting the order of differencing, ACF plots can help in determining the 
# order of the MA(q) model.
#
# Partial autocorrelation plots (PACF), as the name suggests, display correlation 
# between a variable and its lags that is not explained by previous lags. PACF 
# plots are useful when determining the order of the AR(p) model.

Acf(cnt_daily)
Pacf(cnt_daily)

# We can start with the order of d = 1 and re-evaluate whether further differencing is needed.
# 
# The augmented Dickey-Fuller test on differenced data rejects the null hypotheses 
# of non-stationarity. Plotting the differenced series, we see an oscillating pattern
# around 0 with no visible strong trend. This suggests that differencing of order 1 
# terms is sufficient and should be included in the model. 

deseasonal_cnt1 <- diff(deseasonal_cnt, differences = 1)
plot(deseasonal_cnt1)
adf.test(deseasonal_cnt1, alternative = "stationary")

# Next, spikes at particular lags of the differenced series can help inform the choice of 
# p or q for our model:

Acf(deseasonal_cnt1, main='ACF for Differenced Series')
Pacf(deseasonal_cnt1, main='PACF for Differenced Series')

# There are significant auto correlations at lag 1 and 2 and beyond. Partial correlation plots 
# show a significant spike at lag 1 and 7. This suggests that we might want to test 
# models with AR or MA components of order 1, 2, or 7. A spike at lag 7 might suggest
# that there is a seasonal pattern present, perhaps as day of the week.


# Step 6: Fitting an ARIMA model
# now let's fit a model. the forecast package allows the uers to explicitly specify
# the order of the model using the arima() function, or automatically generate a set
# of optimal (p,d,q) using auto.arima(). This function searches through combinations
# of order parameters and picks the set that optimizes model fit criteria.

# the two most widely used critieria for comparing the quality of fit across multiple
# models are AIC and BIC. These two metrics are closely related and We would 
# want to minimize AIC and BIC.

# while auto.arima() can be very useful, it is still important to complete steps 1-5 in
# order to understand the series and interpret model results. note that auto.arima()
# also allows the user to specify maximum order for (p,d,q) which is set to 5 by default.

auto.arima(deseasonal_cnt, seasonal = FALSE)

# we can specify non-seasonal ARIMA structure and fit the model to de-seasonalize
# data. Parameters (1,1,1) suggested by the automated procedure are in line with our 
# expectations based on the steps above.
#
# the model suggested incorporates differencing of degree 1, and uses an autoregressive
# term of first lag and a moving average model of order 1

# using the ARIMA notation introduced above, the fitted model can be written as
# 
#   y_predicted_delta = 0.551*y_yesterday - 0.2496*error_yesterday + E
#
# where E is some error and the origianl series is differenced with order 1.


# Step 7: Evaluate and Iterate
# so now we have a fitted model that can produce a forecast, but does it make sense?
# can we trust this model? We can start answering these questions by examinig ACF and 
# PACF plots for model residuals. if model order parameters and structure are correctly
# specified, we would expect no significant autocorrelations present.

fit <- auto.arima(deseasonal_cnt, seasonal = FALSE)
tsdisplay(residuals(fit), lag.max = 45, main = '(1,1,1) Model Residuals')
