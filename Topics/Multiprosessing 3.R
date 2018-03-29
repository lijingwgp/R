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
##########################################################
## Part 1 
##########################################################
### Grow 1000-tree forest nTimes times WITHOUT 
### multiprocessing enabled, capturing timing information 
### about the process in a variable called 
### p.time.UP. Here's the code to grow one forest:

p.time.UP <- system.time({
  for(i in 1:nTimes){
    rf <- randomForest(formula=churn ~ .,
                       data=data,
                       ntree=1000, 
                       mtry=4,
                       importance=TRUE,
                       localImp=TRUE,
                       na.action=na.roughfix,
                       replace=FALSE)
  }
})
p.time.UP


##########################################################
## Part 2 
##########################################################
# Enable Parallel Processing

registerDoParallel(cores=detectCores())


##########################################################
## Part 3 
##########################################################
### Regrow the 1000-tree forest nTimes times, again capturing
### timing information about the process in a variable called
### p.time.tryMP.

p.time.tryMP <- system.time({
  for(i in 1:nTimes){
    rf <- randomForest(formula=churn ~ .,
                       data=data,
                       ntree=1000, 
                       mtry=4,
                       importance=TRUE,
                       localImp=TRUE,
                       na.action=na.roughfix,
                       replace=FALSE)
  }
})
p.time.tryMP
registerDoSEQ()


##########################################################
#Part 4 
########################################################## 
#Does the randomForest function use MP features when they
#are enabled? (Yes/No)

p.time.UP
p.time.tryMP

#The randomForest function did not automatically use MP features 
#when they are enabled.


##########################################################
#Part 5 
########################################################## 
# Use a foreach() loop with %dopar% to regrow the 1000-tree 
# forest nTimes times, again capturing timing information 
# about the process in a variable called p.time.useMP.
#
# IMPORTANT NOTE: R implements multiprocessing by creating 
# new instances of R in which to run each parallel precess. 
# Consequently, packages required within the %dopar% loop
# must be reloaded in each such instance. A vector of package
# names can be specified using the .packages= parameter of
# the foreach() function. Note the "dot" in .packages =

ncores<-detectCores()
ndocs<-length(data)
d<-vector("list",ncores)
already<-numeric(0)
for(i in 1:(ncores-1)) {
  v<-sample(setdiff(1:ndocs,already),floor(ndocs/ncores))
  d[[i]][1:length(v)]<-v
  already<-c(already,v)
}
v<-setdiff(1:ndocs,already)
d[[ncores]][1:length(v)]<-v

registerDoParallel(cores=ncores)

p.time.useMP <- system.time({
  foreach(i=1:nTimes, .packages = 'randomForest') %dopar% {
    rf <- randomForest(formula=churn ~ .,
                       data=data,
                       ntree=1000, 
                       mtry=4,
                       importance=TRUE,
                       localImp=TRUE,
                       na.action=na.roughfix,
                       replace=FALSE)
    
  }
})
p.time.useMP
registerDoSEQ()


########################################################## 
#Part 6 
########################################################## 
# Display the time taken for this latest test. What was your
# (approximate) improvement factor?

p.time.UP
p.time.useMP
print(paste("The improvement factor is approximatly", round(p.time.UP[3]/p.time.useMP[3])))


########################################################## 
#Part 7 
########################################################## 
# Disable Parallel Processing
registerDoSEQ()
