rm(list=ls())
# Here's a way we haven't used before:
Hitters = na.omit(Hitters)
set.seed(1)

train = sample(c(TRUE,FALSE), nrow(Hitters),rep=TRUE)
train = sample(nrow(Hitters)/2,nrow(Hitters)*0.8)
test = (!train)
test = (-train)

train = sample(nrow(Hitters),nrow(Hitters)*0.8)
test = setdiff(1:nrow(Hitters), train)
hitter.train = Hitters[train,]
hitter.test = Hitters[test,]
salary_test = Hitters$Salary[test]
