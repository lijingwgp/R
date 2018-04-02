rm(list=ls())
##########################################################################################
### Functions
##########################################################################################
installIfAbsentAndLoad <- function(neededVector) {
  for(thepackage in neededVector) {
    if( ! require(thepackage, character.only = TRUE) )
    { install.packages(thepackage)}
    require(thepackage, character.only = TRUE)
  }
}
needed <- c('randomForest', 'doParallel', 'snow')      
installIfAbsentAndLoad(needed)
###Get the data
churndata<-read.table("churndata.csv",sep=",",header=T)
###Clean up, change area code to a factor
data <- na.omit(churndata)
data$area<-factor(data$area)
nTimes <- 5
### Grow 1000-tree forest nTimes times WITHOUT 
### multiprocessing enabled, capturing timimng information 
### about the process in a variable called 
### p.time.UP.
p.time.UP <- system.time({
  for(i in 1:nTimes) {
    rf <- randomForest(formula=churn ~ .,
                       data=churndata,
                       ntree=1000, 
                       mtry=4,
                       importance=TRUE,
                       localImp=TRUE,
                       na.action=na.roughfix,
                       replace=FALSE)
  }
})    
p.time.UP

# Enable Parallel Processing
registerDoParallel(cores=detectCores())

### Regrow the 1000-tree forest nTimes times, again capturing
### timing information about the process in a variable called
### p.time.tryMP.

p.time.tryMP <- system.time({
  for(i in 1:nTimes) {
    rf <- randomForest(formula=churn ~ .,
                       data=churndata,
                       ntree=1000, mtry=4,
                       importance=TRUE,
                       localImp=TRUE,
                       na.action=na.roughfix,
                       replace=FALSE)
  }
})  
p.time.UP
p.time.tryMP

# The randomForest does not seem to use MP features when
# they are enabled.

#Use a foreach loop with %dopar% to regrow the 1000-tree 
#forest nTimes times, again capturing ## timing information 
#about the process in a variable called ## p.time.useMP.
#
#Important note: R implements multiprocessing by creating 
#new instances of R in which to run each parallel precess. 
#Consequently, packages required within the %dopar% loop
#must be reloaded in each such instance. A vector of package
#names can be specified using the .packages= parameter of
#the foreach() function. Note the "dot" in .packages =

p.time.useMP <- system.time({
  foreach(i=1:nTimes, .packages = "randomForest") %dopar% {
    rf <- randomForest(formula=churn ~ .,
                       data=churndata,
                       ntree=1000, mtry=4,
                       importance=TRUE,
                       localImp=TRUE,
                       na.action=na.roughfix,
                       replace=FALSE)
  }
})
p.time.UP
p.time.tryMP
p.time.useMP

# Disable Parallel Processing
registerDoSEQ()