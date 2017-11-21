x <- rnorm(1000)
hist(x)
hist(x, breaks = 100)
plot(density(x))
plot(ecdf(x))
qqnorm(x)
qqline(x)

# null hypothesis is that the underlying distribution of x is normal distribution
shapiro.test(x)
x2 <- x**2
hist(x2)
shapiro.test(x2)    # p-value is significant, so we reject the null hypothesis

# null hypothesis is that the two distributions are identical
ks.test(x, x2)
ks.test(x,pnorm)    # the power of shapiro test of testing normality is better
ks.test(x,pexp)

# exponential distribution
plot(hist(rexp(30)))
plot(hist(rexp(3000), breaks = 100))
