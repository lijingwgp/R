rm(list=ls())
# Local regression is a statistical method for robustly fitting smoothing 
# curves without prior assumptions about the shape or form of the curve. 
# 'lowess' and 'loess' are two different functions in R which implement local 
# regression. As you've noted, 'loess' is the more recent program and it is 
# essentially equivalent to 'lowess' except that it has more features.
# 
# The R function 'loess' has three things which 'lowess' doesn't:
#   1. It accepts a formula specifying the model rather than the x and y matrices
#   2. It can be used with more than one predictor.
#   3. It accepts prior weights.
#    
# On the other hand, 'loess' is much slower than 'lowess' and occasionally 
# fails when 'lowess' succeeds, so both programs are kept in R.
# 
# When there is only one predictor variable and no prior weights, 'lowess' 
# and 'loess' are in principle exactly equivalent. However the default 
# settings of the two programs are very different. Here is an example in 
# which I force 'lowess' and 'loess' to do precisely the same  numerical 
# calculation:

y <- rnorm(1000)
x <- 1:1000
out.lowess <- lowess(x,y,f=0.3,iter=3,delta=0)
out.lowess$y[1:5]
out.loess <- loess(y~x,span=0.3,degree=1,family="symmetric",iterations=4,surface="direct")
fitted(out.loess)[1:5]

# Things to note here:
# 1. 'f' is the 'span' argument for 'lowess'
# 2. 'loess' does quadratic (degree=2) local regression by default instead of 
#    linear (degree=1)
# 3. Unless you specify family="symmetric", loess will fit the curve by least 
#    squares, i.e., won't do any robustness iterations at all.
# 4. lowess and loess count iterations in differently: 'iter' in lowess means 
#    the number of robustness iterations; 'iterations' in loess means the total 
#    number of iterations including the least squares fit, i.e., iterations=iter+1

# Robustness Iterations: At iterations beyond the first iteration, the local weights wi of 
# the previous iteration are replaced by riwi where ri is a weight that decreases as the 
# residual of the fitted value at the previous iteration at the point corresponding to di increases
