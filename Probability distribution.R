
# Probability functions in R

library(ggplot2)

  # What it does
  # -norm (normal distribution)
  # -binom (binomial distribution)
  # 
  # d- probability density (mass): y value given x value
  # dnorm
  # dbinom
  # p- probability given x value
  # pnorm
  # pbinom
  # q- x value given probability
  # qnorm
  # qbinom

# Probability density functions (d-): Return y value given x value

** NORMAL DISTRIBUTION **
  
  ## Density at 90 in a normal distribution with a mean of 124 and sd of 20
  dnorm(x = 90, mean = 124, sd = 20)
  
  ## Density at c(10,20,30) in a normal distribution with a mean of 124 and sd of 20
  dnorm(x = c(10,20,30), mean = 124, sd = 20)
  
  ## Graphing the distribution
  curve(dnorm(x, mean = 29, sd = 20), xlim = c(0, 55))
  abline(h = 0)

** Binomial distribution **
  
  ## Density (mass) at 3 in a binomial distribution with 5 trials with 
  #  success probability of 0.4
  dbinom(x = 3, size = 5, prob = 0.4)
  
  ## Density (mass) at c(1,2,3) in a binomial distribution with 5 trials 
  #  with success probability of 0.4
  dbinom(x = c(1,2,3), size = 5, prob = 0.4)
  
  ## Graphing the distribution: Use barplot() as it is a discrete distribution. 
  #  Give 0:5 (= c(0,1,2,3,4,5))
  barplot(dbinom(x = 0:5, size = 5, prob = 0.4), names.arg = 0:5)


  # Probability function (p-) and Quantile function (q-)
  # Probability function (p-): Given an x value, it returns the probability 
  # of having a value lower than x.
  # Quantile function (q-): Given a probability, it returns the x value 
  # at the upper boundary.

  # These two are doing the opposite actions.

  
## Normal distribution ##
## Probability of having a value *lower* than 90 in a normal distribution 
#  with a mean of 124 and sd of 20.
pnorm(q = 90, mean = 124, sd = 20, lower.tail = TRUE)

curve(dnorm(x, mean = 124, sd = 20), xlim = c(0, 200))
abline(h = 0)

sequence <- seq(100, 150, 0.1)
polygon(x = c(sequence,150,100),
        y = c(dnorm(sequence,124,20),0,0),
        col = "grey")


## Probability of having a value *higher* than 140 in a normal distribution 
#  with a mean of 124 and sd of 20.
1 - pnorm(q = 140, mean = 124, sd = 20, lower.tail = TRUE)
pnorm(q = 140, mean = 124, sd = 20, lower.tail = FALSE)

curve(dnorm(x, mean = 124, sd = 20), xlim = c(0, 200))
abline(h = 0)
sequence <- seq(140, 200, 0.1)
polygon(x = c(sequence,200,140),
        y = c(dnorm(c(sequence),124,20),0,0),
        col = "grey")


## X-axis value *below* which the probability (AUC) is 10%
qnorm(p = 0.10, mean = 124, sd = 20, lower.tail = TRUE)
curve(dnorm(x, mean = 124, sd = 20), xlim = c(0, 200))
abline(h = 0)
sequence <- seq(0, 98.36897, 0.1)
polygon(x = c(sequence,98.36897,0),
        y = c(dnorm(c(sequence),124,20),0,0),
        col = "grey")


## X-axis value *above* which the probability (AUC) is 20%
qnorm(p = 0.10, mean = 124, sd = 20, lower.tail = FALSE)
curve(dnorm(x, mean = 124, sd = 20), xlim = c(0, 200))
abline(h = 0)
sequence <- seq(149.631, 200, 0.1)
polygon(x = c(sequence,200,149.631),
        y = c(dnorm(c(sequence),124,20),0,0),
        col = "grey")

## Binomial distribution ##
## Definitions are confusing. It is probably safer to use summation of dbinom.
## Probability of having a value *lower* than or equal to 3 in a binomial 
#  distribution with 5 trials with success probability of 0.4. P[X <= x]. Include the "equal to" value.
pbinom(q = 3, size = 5, prob = 0.4, lower.tail = TRUE)
barplot(dbinom(x = 0:5, size = 5, prob = 0.4), names.arg = 0:5, 
        col = rep(c("grey","white"), c(4,2)))

## Probability of having a value *higher* than 3 in a binomial distribution 
#  with 5 trials with success probability of 0.4. P[X > x]. 
#  Be careful it excludes the probability at 3.
pbinom(q = 3, size = 5, prob = 0.4, lower.tail = FALSE)
barplot(dbinom(x = 0:5, size = 5, prob = 0.4), names.arg = 0:5, 
        col = rep(c("white","grey"), c(4,2)))

## Summation of individual probabilities may be more intuitive, since it's discrete
sum(dbinom(x = 3:5, size = 5, prob = 0.4))

## X-axis value *below (including the value)* which the summation of 
#  probabilities is greater than or equal to p.
qbinom(p = 0.3, size = 5, prob = 0.4, lower.tail = TRUE)
barplot(dbinom(x = 0:5, size = 5, prob = 0.4), names.arg = 0:5, 
        col = rep(c("grey","white"), c(2,4)))


## Summation of individual probabilities
cumsum(dbinom(x = 0:5, size = 5, prob = 0.4))
[1] 0.07776 0.33696 0.68256 0.91296 0.98976 1.00000

## Summation from the opposite direction.
cumsum(rev(dbinom(x = 0:5, size = 5, prob = 0.4)))
[1] 0.01024 0.08704 0.31744 0.66304 0.92224 1.00000
