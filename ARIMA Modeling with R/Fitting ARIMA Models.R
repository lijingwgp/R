########################
### AR and MA Models ###
########################
# in this section, we focus on fit ARMA model to stationary time series data
# the first problem is how to identify ARMA model from data

x <- arima.sim(list(order = c(1,0,0), ar = -.7), n = 200)
y <- arima.sim(list(order = c(0,0,1), ma = -.7), n = 200)

par(mfrow = c(1,2))
plot(x, main = "AR(1)")
plot(y, main = "MA(1)")

# we can't identidy model by just looking at the data
# we use acf() and/or pacf() to identify the model

# estimation
# estimation for time series is similar to using least squares for regression
#
# AR(2) with mean 50:

x <- arima.sim(list(order = c(2,0,0), 
                    ar = c(1.5, -.75)),
               n = 200) + 50
x_fit <- sarima(x, p = 2, d = 0, q = 0)
x_fit$ttable

y <- arima.sim(list(order = c(0,0,1), ma = -.7), n = 200)
y_fit <- sarima(y, p = 0, d = 0, q = 1)
y_fit$ttable


##############################
### Fitting an AR(1) Model ###
##############################
# Generate 100 observations from the AR(1) model
par(mfrow = c(1,1))
x <- arima.sim(model = list(order = c(1, 0, 0), ar = .9), n = 100) 
plot(x)
# plot the sample ACF and PACF pairs using the acf2() function
acf2(x)
# Fit an AR(1) to the data and examine the t-table
sarima(x, 1, 0, 0)


##############################
### Fitting an AR(2) Model ###
##############################
require(astsa)
x <- arima.sim(model = list(order = c(2,0,0), ar = c(1.5, -.75)), n = 200)
plot(x)
acf2(x)
sarima(x, 2, 0, 0)


##############################
### Fitting an MA(1) Model ###
##############################
x <- arima.sim(model = list(order = c(0,0,1), ma = -.8), n = 100)
plot(x)
acf2(x)
sarima(x,0,0,1)


##########################
### AR and MA together ###
##########################
x <- arima.sim(list(order = c(2,0,1), ar=c(1,-.9), ma=.8), n=250)
plot(x, main="ARMA(2,1)")
acf2(x)
sarima(x, 2, 0 ,1)


##########################################
### Model Choice and Residual Analysis ###
##########################################
# in practice, there are generally a couple of models that seems suitable to the data set
# the most popular measurement used to choose a model are AIC and BIC
# the goal is to find the smallest AIC or BIC

# residual analysis
# we want the residuals to be white guassian noise
#
# sarima() includes residual analysis graphic:
#   standardized residuals
#   sample ACF of residuals
#   normal qq plot
#   q-statistic p-values

# standardized residuals plot is used to identify patterns - it should behave as a white noise sequence
# acf plot can be used to access the correlation between the residuals - most of correlations should be in between the dashed blue lines
# qq plot access normality

dl_varve <- diff(log(varve))
plot(dl_varve)
sarima(dl_varve, 0, 0, 1)
sarima(dl_varve, 0, 0, 2)
sarima(dl_varve, 1,0,1)

plot(oil)
plot(diff(log(oil)))
acf2(diff(log(oil)))

# From the P/ACF pair, it is apparent that the correlations are small and the 
# returns are nearly noise. But it could be that both the ACF and PACF are tailing off. 
# If this is the case, then an ARMA(1,1) is suggested.

