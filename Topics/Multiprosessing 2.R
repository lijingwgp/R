rm(list=ls())
##########################################################################################
installIfAbsentAndLoad<-function(neededVector) {
  for(thepackage in neededVector) {
    if( ! require(thepackage,character.only = TRUE) )
    { install.packages(thepackage)}
    require(thepackage,character.only = TRUE)
  }
}
needed<-c("doParallel","snow")
installIfAbsentAndLoad(needed)
##Create a list of 2000 items, each composed of a dataframe
##with 10 predictors, one response, and 1000 observations
newdocs<-vector("list",2000)
for(i in 1:2000) {
  my.matrix <- matrix(rnorm(1000 * 10), 
                      nrow=1000, 
                      dimnames=list(NULL, paste('x', 1:10, sep='')))
	newdocs[[i]]<-data.frame(my.matrix, y=factor(sample(c(0,1),1000,repl=T)))
}
head(newdocs[[1]])

## Divide the 2000 elements of newdocs into 'ncores' MECE partitions of approximately 
## equal size and return in a list d of length 'ncores'
ncores<-detectCores()
ndocs<-length(newdocs)
d<-vector("list",ncores)
already<-numeric(0)
for(i in 1:(ncores-1)) {
  v<-sample(setdiff(1:ndocs,already),floor(ndocs/ncores))
  d[[i]][1:length(v)]<-v
  already<-c(already,v)
}
v<-setdiff(1:ndocs,already)
d[[ncores]][1:length(v)]<-v

##Register DoParallel for the multiprocessing core of foreach
##Note that Windows will use snow - others will use parallel
registerDoParallel(cores=ncores)
##Initiate parallel processing (change the %do% to %dopar%
##for multui-thread, %do% for single-thread, to compare
##times
#
# IMPORTANT NOTE: R implements multiprocessing by creating 
# new instances of R in which to run each parallel precess. 
# Consequently, packages required within the %dopar% loop
# must be reloaded in each such instance. A vector of package
# names can be specified using the .packages= parameter of
# the foreach() function. Note the "dot" in .packages =
# An example is included in the Multiprocessing exercise.
#
error.rates <- rep(0,2000)
p.time.Multiprocessing <- system.time({
	foreach(i=1:ncores, .packages = c()) %dopar% {
		for(j in 1:2000) {
			result <- glm(y ~ ., data=newdocs[[j]], family=binomial(logit))
			preds <- predict(result, type='response')
			error.rates[j] <- mean(ifelse(preds > .5, 1, 0) != newdocs[[j]]$y)
		}
	}
})
p.time.Multiprocessing

error.rates <- rep(0,2000)
p.time.Uniprocessing <- system.time({
  foreach(i=1:ncores) %do% {
    for(j in 1:2000) {
      result <- glm(y ~ ., data=newdocs[[j]], family=binomial(logit))
      preds <- predict(result, type='response')
      error.rates[j] <- mean(ifelse(preds > .5, 1, 0) != newdocs[[j]]$y)
    }
  }
})
p.time.Uniprocessing
# Disable Parallel Processing
registerDoSEQ()