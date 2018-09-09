#############################################################
### Correlation analysis and the autocorrelation function ###
#############################################################
# plot(a, b) function will produce a scatterplot when two time series names a and b are given as input
# plot(stock_A, stock_B) allows us to examine correlation between the two stocks
# ts.plot(cbind(stock_A, stock_B)) allows us to compare two time series objects

plot(EuStockMarkets)
# Use this code to convert prices to returns
returns <- EuStockMarkets[-1,] / EuStockMarkets[-1860,] - 1
# Convert returns to ts
returns <- ts(returns, start = c(1991, 130), frequency = 260)
plot(returns)
# Use this code to convert prices to log returns
logreturns <- diff(log(EuStockMarkets))
# Plot logreturns
plot(logreturns)

# Generate means from eu_percentreturns
colMeans(eu_percentreturns)
# Use apply to calculate sample variance from eu_percentreturns
apply(eu_percentreturns, MARGIN = 2, FUN = var)
# Use apply to calculate standard deviation from eu_percentreturns
apply(eu_percentreturns, MARGIN = 2, FUN = sd)
par(mfrow = c(2,2))
# Display a histogram of percent returns for each index
apply(eu_percentreturns, MARGIN = 2, FUN = hist, main = "", xlab = "Percentage Return")
# Display normal quantile plots of percent returns for each index
par(mfrow = c(2,2))
apply(eu_percentreturns, MARGIN = 2, FUN = qqnorm, main = "")
qqline(eu_percentreturns)

# Make a scatterplot of DAX and FTSE
plot(DAX, FTSE)
# Make a scatterplot matrix of eu_stocks
pairs(EuStockMarkets)
# Convert eu_stocks to log returns
logreturns <- diff(log(EuStockMarkets))
# Plot logreturns
plot(logreturns)
# Make a scatterplot matrix of logreturns
pairs(logreturns)

# Covariance and Correlation
# A positive covariance indicates a positive association between the two objects
# Covariance depends on the scale of the variables, so it is hard to interpret
#
# Correlation is a standardized version of covariance
# Correlation is always between -1 and +1

# Use cov() with DAX_logreturns and FTSE_logreturns
cov(DAX_logreturns, FTSE_logreturns)
# Use cov() with logreturns
cov(logreturns)
# Use cor() with DAX_logreturns and FTSE_logreturns
cor(DAX_logreturns, FTSE_logreturns)
# Use cor() with logreturns
cor(logreturns)

# Autocorrelation
# Autocorrelation helps with study how each time series observation is related to its recent past
#
# Autocorrelations are also called lagged correlations which are used to assess whether
# a time series is dependent on its past
# 
# For a time series x of length n we consider the n-1 pairs of observations one time unit apart
# The first such pair is (x[2],x[1]) and the next is (x[3],x[2])
# Each such pair is of the form (x[t],x[t-1]) where t is the observation index, which we 
# vary from 2 to n in this case
#
# The lag-1 autocorrelation of x can be estimated as the sample correlation of these 
# (x[t], x[t-1]) pairs
#
# Applying acf(..., lag.max = 1, plot = FALSE) to a series x automatically calculates the lag-1 autocorrelation

x_t0 <- x[-1]
x_t1 <- x[-n]
head(cbind(x_t0, x_t1))
# Plot x_t0 and x_t1
plot(x_t0, x_t1)
# View the correlation between x_t0 and x_t1
cor(x_t0, x_t1)
# Use acf with x
acf(x, lag.max = 1, plot = FALSE)
# Confirm that difference factor is (n-1)/n
cor(x_t1, x_t0) * (n-1)/n

# Autocorrelations can be estimated at many lags to better assess how a time series relates to its past
# We are typically most interested in how a series relates to its most recent past
#
# The acf(..., lag.max = ..., plot = FALSE) function will estimate all autocorrelations from 0, 1, 2,..., 
# up to the value specified by the argument lag.max
