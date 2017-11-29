rm(list=ls())
# Here's a way we haven't used before:
Hitters=na.omit(Hitters)
set.seed(1)
train=sample(c(TRUE,FALSE), nrow(Hitters),rep=TRUE)
test=(!train) #test=(-train)

Hitters.train = Hitters[train,]
Hitters.test = Hitters[test,]
