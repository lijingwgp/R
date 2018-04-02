rm(list=ls())
##########################################################################################
installIfAbsentAndLoad <- function(neededVector) {
  for(thepackage in neededVector) {
    if( ! require(thepackage,character.only=T) )
    { install.packages(thepackage)}
    require(thepackage,character.only=T)
  }
}
needed <- c("doParallel","snow")
installIfAbsentAndLoad(needed)

ndf <- 2000
nobs <- 10000
## Create a list of ndf data frames, each 
## with 10 predictors, one response, and nobs observations
set.seed(5082)
newdocs <- vector("list",ndf)
for(i in 1:ndf) {
  my.matrix <- matrix(rnorm(nobs * 10), 
                      nrow=nobs, 
                      dimnames=list(NULL, paste('x', 1:10, sep='')))
	newdocs[[i]] <- data.frame(my.matrix, y=factor(sample(c(0,1), nobs, repl=T)))
}
head(newdocs[[1]])

## Divide the ndf indices of newdocs into 'nmult * ncores'
## MECE partitions of approximately equal size and return in
## a list d of length 'nslices' composed of indices.
ncores <- detectCores()
nmult <- 2
nslices <- nmult * ncores       #a nmult multiple of ncores (just for testing - not generally necessary)
d <- vector("list",nslices)
already <- numeric(0)
for(i in 1:(nslices - 1)) {
  v <- sample(setdiff(1:ndf,already),floor(ndf/nslices))
  d[[i]][1:length(v)] <- v
  already <- c(already,v)
}
v <- setdiff(1:ndf,already)
d[[nslices]][1:length(v)] <- v
str(d[[1]])
str(d[[nslices]])
##Register DoParallel for the multiprocessing core of foreach
##Note that Windows will use snow - others will use parallel
registerDoParallel(cores=ncores)
##Initiate parallel processing (change the %do% to %dopar% 
##for multui-thread, %do% for single-thread, to compare 
##times
#
#IMPORTANT NOTE: R implements multiprocessing by creating 
#new instances of R in which to run each parallel precess. 
#Consequently, packages required within the %dopar% loop 
#must be reloaded in each such instance. A vector of package
#names can be specified using the .packages= parameter of 
#the foreach() function. Note the "dot" in .packages= An
#example is included here, even though glmnet isn't
#required.
#
p.time.Multiprocessing <- system.time({
  error.rates <- foreach(i = 1:nslices, .packages='glmnet', .combine = rbind) %dopar% {
    results <- NULL
		for(j in  d[[i]]) {
			model <- glm(y ~ ., data=newdocs[[j]], family=binomial(logit))
			preds <- predict(model, type='response')
			results <- rbind(results, c(i, j, mean(ifelse(preds > .5, 1, 0) != newdocs[[j]]$y)) )
		}
    results
	}
})
error.rates <- error.rates[order(error.rates[,1], error.rates[, 2]),3]
p.time.Multiprocessing
length(error.rates)
min(error.rates)
mean(error.rates)

# Change %dopar% to %do% to disable multiprocessing

p.time.Uniprocessing <- system.time({
  error.rates <- foreach(i = 1:nslices, .combine = rbind) %do% {
    results <- NULL
    for(j in  d[[i]]) {
      model <- glm(y ~ ., data=newdocs[[j]], family=binomial(logit))
      preds <- predict(model, type='response')
      results <- rbind(results, c(i, j, mean(ifelse(preds > .5, 1, 0) != newdocs[[j]]$y)) )
    }
    results
  }
})
error.rates <- error.rates[order(error.rates[,1], error.rates[, 2]),3]
p.time.Uniprocessing
length(error.rates)
min(error.rates)
mean(error.rates)

# Disable Parallel Processing
registerDoSEQ()
