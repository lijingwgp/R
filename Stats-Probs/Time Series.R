#######################
#### Introduction ##### 
#######################
## Judgmental Model
## -- Scenarios
## 
## Extrapolation Model (Quantitative)
## -- Time Series
##
## Econometric Model (Quantitative)
## -- Regression



################################
#### Extrapolation Methods ##### 
################################
## Trend regression
## Exponential smoothing
## Moving averages
## Auto-regression
##
## We are looking for patterns!
## Suppose k-period ahead forecast
## Y_t: measurement at time t
## Y_t-k: observed past; we want to forecast ahead to Y_t
## F_t-k,t: we observed t-k past data and want to corecast at time t
## E_t-k,t: forecast error
## -- = (Y_t) - (F_t-k,t)
##
## 2 steps in time series
## -- 1. model history
## -- 2. forecast future
## 
## MAE: mean absolute error
## -- 1/n * sum(E_t)
## RMSE
## -- sqrt(1/n * sum((E_t)^2))
## MAPE
## -- prop(1/n * sum(E_t))



############################
##### Example: Trivial ##### 
############################
## Random Model
## -- observations vary around mean with constant mu and constant variance
## -- observations are independent identical data points
## -- random time series is no time series
## Goal: Y_t = fitted + residual
## -- forecast + noise
##
## Runs test for randomness
## -- calculate x bar
## -- determine for each data point is either above the mean or below the mean
## -- See if residual is normally distributed (residual is randomly distributed if random series)
## -- hypothesis test



################################
##### Example: Random Walk ##### 
################################
## Auto-correlation
## -- time series that violates independence
## -- observations are correlated with each other
##
## LAG
## -- X is a vector of [x1, x2, ..., xn]
## -- lag_x (1) is X but in line with the second elements of X[]
## LAG Visualization
## -- on a graph, lag is the distance from a local optimum to another local optimum
## -- in other words, lag can be described in terms of cycles
##
## Series itself is not random
## but the vertical distances is

z = rnorm(250,0,2)
y = c()
y[1] = z[1]
for(i in 2:250){
  y[i] = y[i-1]+z[i]
}
plot(y)
ts.plot(y)

# compute lags
acf(y)

diff(y)
ts.plot(diff(y))
acf(diff(y))



####################################
##### Example: Auto Regression ##### 
####################################
## Goal: Y_t = previous + average change
## -- regress on t and check r^2
## -- we compute lag of 1...n
## -- compute subset of these lags with same length



####################################
##### Example: Moving Averages ##### 
####################################
## choose a span, say 6 month
## this is called smoothing function
## to eliminate the extremes

mean(z)
ts.plot(z)
abline(h=mean(z))
