###############################
##### Simulation Modeling #####
###############################

### Linear Congruential Generator
### x(i+1) = a*X(i) + c mod m
### m = 2^32, a = 1664525, c = 1013904223
x <- 1
x <- (1664525*x + 1013904223) %% 2^32
x

### News Vendor Problem
### Bookstore buys calenders for $7.50 and sells them for $10 each.
### After Feb 1st, all unsold calenders are returned for a refund of $2.50
### How many problem should the bookstore order?
# P(0.30) -- demand 100
# P(0.20) -- demand 150
# p(0.30) -- demand 200
# p(0.15) -- demand 250
# p(0.05) -- demand 300
d <- c(100,150,200,250,300)
p <- c(0.3,0.2,0.3,0.15,0.05)

cumsum(p)
cdf <- cumsum(p)
min(which(cdf >= runif(1)))
d[min(which(cdf >= runif(1)))]

idx <- function(prob){min(which(cdf >= prob))}
idx(0)
idx(0.95)

sapply(runif(10), idx)
demand <- d[sapply(runif(10), idx)]
demand

n <- 10                                          # randomly chosen 10 numbers
demand <- d[sapply(runif(n), idx)]               # determine demand for multiple location
calRevenue <- 10                                 # revenue is $10 for each sold
totalRevenue <- pmin(demand, 200) * calRevenue   # assume we purchased 200 from vendor, pmin(demand, 200)
                                                 # determine the exact quantity sold in each location
totalRevenue
totalCost <- 200 * 7.5                           # cost is $7.5 for each sold
excessInv <- 200 - demand                        # determine which location has excessive quantity
excessInv                                        # negative number indicates higher demand for that location
excessInv <- pmin(excessInv, 0)                  # determine which location has higher demand
excessInv <- pmax(excessInv, 0)                  # determine which location has lower demand
totalRefund <- excessInv * 2.5
totalProfit <- totalRevenue - totalCost + totalRefund
totalProfit
