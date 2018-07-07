# Time Series: a sequence of data in chronological order
# Time Series data is dated or time stamped in R
#
# A plot() function can be used to create a basic time series plot
# time is indexed in the horizontal axis
# observations are shown from left to right



#################################
### Exploring raw time series ###
#################################

# The most common first step when conducting time series analysis is to 
# display your time series dataset in a visually intuitive format.
#
# The most useful way to view raw time series data in R is to use the print() command, 
# which displays the Start, End, and Frequency of your data along with the observations.
#
# Another useful command for viewing time series data in R is the length() function, 
# which tells you the total number of observations in your data.

# Some datasets are very long, and previewing a subset of data is more suitable than displaying
# the entire series. The head() and tail() functions focus on the first and last few elements of a 
# given dataset respectively.

# In this lesson, we will examine a dataset called Nile.
# This time series dataset includes some metadata information. when calling print(Nile), note
# that Start = 1871 indicates that 1871 is the year of the first annual observation, and End = 1970
# indicates 1970 is the year of the last annual observation.

print(Nile)
length(Nile)
head(Nile, n=10)
tail(Nile, n=12)

# Another very useful way to explore any data is to generate a plot

plot(Nile, xlab = "Year", ylab = "River Volume (1e9 m^{3})", 
     main = "Annual River Nile Volume at Aswan, 1871-1970", type = "b")

# Sampling frequency: exact
# Time series data is exactly evenly spaced
# hourly temp measurement
#
# Sampling frequency: approximate
# Time series data is approximately evenly spaced
# temp measurement recorded every time you check your email
#
# Sampling frequency: missing values
# Time series data is evely spaced, but with missing values
# temp measurement recorded while you are awake

# Basic Assumptions:
#   consecutive observations are equally spaced
#   a discrete-time index is applied

# In addition to viewing your data and plotting over time, there are several additional 
# operations that can be performed on time series datasets
#
# The start() and end() functions return the time index of the first and last observations
# The deltat() function returns the fixed time interval between observations and the 
# frequency() function returns the number of observations per unit time
# The time() function calculates a vector of time indices, with one element for each time index on which the series was observed

start(AirPassengers)
end(AirPassengers)
time(AirPassengers)
deltat(AirPassengers)
frequency(AirPassengers)
cycle(AirPassengers)

# Sometimes it is useful to know the missing values locations, since we may want to impute 
# or estimate the missing values
#
# Use mean(___, na.rm = TRUE) to calculate the mean with all missing values removed
# Sometimes, it is common to replace missing values with the mean of the observed values

plot(AirPassengers)
mean(AirPassengers, na.rm = T)
AirPassengers[85:96] <- mean(AirPassengers, na.rm = T)
plot(AirPassengers)

# Basic Time Seires Objects
# Start with a vector of data
# Apply the ts() function to create a time series object
# they represent data that is approximately evenly spaced over the past
# We can use is.ts() to check whether an object is a time series object

data_vector <- runif(100, min=0, max=101)
plot(data_vector)
time_series <- ts(data_vector, start = 2004, frequency = 4)
plot(time_series)
is.ts(data_vector)
is.ts(time_series)

is.ts(EuStockMarkets)
start(EuStockMarkets)
end(EuStockMarkets)
frequency(EuStockMarkets)
plot(EuStockMarkets)
ts.plot(EuStockMarkets, col = 1:4, xlab = "Year", ylab = "Index Value", 
        main = "Major European Stock Indices, 1991-1998")

