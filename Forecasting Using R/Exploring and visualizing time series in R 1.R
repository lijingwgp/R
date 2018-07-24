#########################################
### Creating time series objects in R ###
#########################################
# A time series can be thought of as a vector or matrix of numbers along 
# with some information about what times those numbers were recorded. 
#
# This information is stored in a ts object in R.
# The function ts() takes in three arguments:
#   data
#   start is set to the form c(year, period). 
#     January corresponds with period 1; likewise, a start date in April would 
#     refer to 2, July to 3, and October to 4. Thus, period corresponds to the 
#     quarter of the year.
#   frequency is set to 4 because the data are quarterly.

# Read the data from Excel into R
mydata <- read_excel("exercise1.xlsx")
# Look at the first few lines of mydata
head(mydata)
# Create a ts object called myts
myts <- ts(mydata[2:4], start = c(1981, 1), frequency = 4)



#########################
### Time series plots ###
#########################
# You can use the autoplot() function to produce a time plot of the data 
# with or without facets, or panels that display different subsets of data
require(forecast)
autoplot(gold, facets = T)
# Find the outlier in the gold series
goldoutlier <- which.max(gold)
# Look at the seasonal frequencies of the gold series
frequency(gold)



######################
### Seasonal plots ###
######################
# Along with time plots, there are other useful ways of plotting data to 
# emphasize seasonal patterns and show changes in these patterns over time.
#
# A seasonal plot is similar to a time plot except that the data are plotted 
# against the individual "seasons" in which the data were observed.
# You can create one using the ggseasonplot() function the same way you do with autoplot().
#
# A subseries plot comprises mini time plots for each season. Here, the mean for each 
# season is shown as a blue horizontal line.

# One way of splitting a time series is by using the window() function, 
# which extracts a subset from the object x observed between the times start 
# and end.

# Load fpp2 package and use two of its datasets, a10 and ausbeer
require(fpp2)
# Use autoplot() and ggseasonplot() to produce plots of the a10 data
autoplot(a10)
ggseasonplot(a10)
# Produce a polar coordinate season plot for the a10 data
ggseasonplot(a10, polar = T)
# Restrict the ausbeer data to start in 1992
beer <- window(ausbeer, start=1992)
autoplot(beer)
ggseasonplot(beer)
ggsubseriesplot(beer)



##########################################
### Trends, seasonality, and cyclicity ###
##########################################
# Trend: a pattern exists involving a long-term increase OR decrease in the data
# Seasonal: a periodic pattern exists due to the calendar
# Cyclic: a pattern exists where the data exhibits rises and falls that are not of fixed period

# Difference between seasonal and cyclic patterns:
#   seasonal pattern constant vs. cyclic pattern variable length
#   if both exist together, then the average length of cycle longer than the average length of seasonal length
#   magnitude of cycle more variable than magnitude of seasonal pattern



####################################################
### Autocorrelatioin of non-seasonal time series ###
####################################################
# Another way to look at time series data is to plot each observation against 
# another observation that occurred some time previously by using gglagplot().
# This is called a lag plot because you are plotting the time series against lags of itself.

# The correlations associated with the lag plots form what is called the 
# autocorrelation function (ACF). The ggAcf() function produces ACF plots.

# Create an autoplot of the oil data
autoplot(oil)
# Create a lag plot of the oil data
gglagplot(oil)
# Create an ACF plot of the oil data
ggAcf(oil)



##########################################################
### Autocorrelation of seasonal and cyclic time series ###
##########################################################
# When data are either seasonal or cyclic, the ACF will peak around the seasonal 
# lags or at the average cycle length.

# Produce a time plot and ACF plot of sunspot.year
autoplot(sunspot.year)
ggAcf(sunspot.year)
# By observing the ACF plot, at which lag value (x) can you find the maximum 
# autocorrelation (y)? Set this equal to maxlag_sunspot.
maxlag_sunspot <- 1
# Plot the traffic on the Hyndsight blog
autoplot(hyndsight)
ggAcf(hyndsight)
# Save the lag corresponding to maximum autocorrelation
maxlag_hyndsight <- 7



###################
### White noise ###
###################
# iid data: no trend, no seasonality, no cyclicity, not even any correlations
set.seed(3)
wn <- ts(rnorm(36))
autoplot(wn)
# Because data are random, we expect each autocorrelations is close 0
# 95% of all autocorrelations for white noise should lie within the blue lines
ggAcf(wn)

# Sometimes it is useful to test all the autocorrelations together, rather than 
# Looking at each one separatly
#
# Ljung-Box test
# It considers the first h autocorrelation values together
# A significant test indicates the data are probably not white noise
autoplot(pigs)
ggAcf(pigs)
Box.test(pigs, lag = 24, fitdf = 0, type = "Lj")

# White noise is a term that describes purely random data.
# We can test for white noise by looking at an ACF plot or by doing a Ljung-Box test
# A Ljung-Box test confirms the randomness of a series

# Stock prices and white noise
autoplot(goog)
autoplot(diff(goog))
ggAcf(diff(goog))
Box.test(diff(goog), lag = 10, type = "Ljung")

