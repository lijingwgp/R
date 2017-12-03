## Simulating 100000 observations
n_samp <- 100000;

## The number of fish that is catched follows a poisson distribution
## with 7 fish per good day, 4 fish per normal day and 2 fish per bad day
##
## The wether condition follows a discrete distribution with probabilities of 
## good day is 0.1, normal day is 0.6, and bad day is 0.3 
weather <- sample(x=c(7,4,2), size=n_samp, prob=c(.1,.6,.3), replace=TRUE)

## Number of fish occured which (poisson distribution)
nFish <- sapply(weather, rpois, n=1)

## Number of yellowfin occured 
## 25% of occasions 0% are yellowfin
## 50% of occasions 25% are yellowfin
## 25% of occasions 35% are yellowfin
pctYellow <- sample(x=c(0,.25,.35), size=n_samp, prob=c(.25,.50,.35), replace=TRUE)

## pctYellow is a list of probabilities for the number of yellowfin from the group 
## of fish that is catched
## Number of yellowfin catched
nYellow <- round(nFish * pctYellow)

## Number of bluefin catched from the same group
nBlue <- pmax(nFish - nlegalYellow,0)

## Only fish weighing more than 20 lbs are allowed to catch
## "l" represent lbs
## Both number yellowfin and bluefin catched follows Normal distributions
## rnorm returns a list of probabilities that is,
## in a given day, a group of n catched yellowfin correspond to n probabilities
## legalYellow represent the total lbs catched for yellowfin
legalYellow <- unlist(lapply(sapply(nYellow,rnorm,mean=30,sd=18),function(l){sum(l[l>20])}))
legalBlue <- unlist(lapply(sapply(nBlue,rnorm,mean=35,sd=18),function(l){sum(l[l>20])}))

## A % of the fish represent the edible yield
## this is determined from a Beta distribution with an alpha=70
## and a beta=30
lbsYellow <- legalYellow * rbeta(n_samp,shape1=70,shape2=30)
lbsBlue <- legalBlue * rbeta(n_samp,shape1=70,shape2=30)

## When the weather is bad, there would be 2 fish catched
## Out of those 2 fish catched, the edible yield is 
## lbsYellow[which(weather==2)]*0.75
## 1 is the base
lbsYellow[which(weather==2)] <- lbsYellow[which(weather==2)]*0.75
lbsYellow[which(weather==7)] <- lbsYellow[which(weather==7)]*1.1
lbsBlue[which(weather==2)] <- lbsBlue[which(weather==2)]*0.75
lbsBlue[which(weather==7)] <- lbsBlue[which(weather==7)]*1.1

hist(c(lbsBlue,lbsYellow),breaks = 100,ylim=c(0,20000))
plot(density(c(lbsBlue,lbsYellow)))
summary(c(lbsBlue,lbsYellow))
