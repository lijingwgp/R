rm(list=ls())
installIfAbsentAndLoad <- function(neededVector) {
  for(thispackage in neededVector) {
    if( ! require(thispackage, character.only = TRUE) )
    { install.packages(thispackage)}
    require(thispackage, character.only = TRUE)
  }
}
needed = c("ada")
installIfAbsentAndLoad(needed)
require(ada)

# read in the weather data build a classification tree to predict if its going to rain
weather<-read.table("myWeatherData.csv",sep=",",header=T)
names(weather)
discardcols <- names(weather) %in% c("Date", "Location","RISK_MM")
weather<- weather[!discardcols]
names(weather)
nobs <- nrow(weather)

# create a 80/20 split of training and test data using nobs 
set.seed(5082)
train <- sample(nobs, 0.8*nobs)
weather.train <- weather[train,]
test <- setdiff(1:nobs, train) 

# loop the trees and find error rate 
# Build an initial boosted tree
error <- rep(0,21)
for(i in 2:22){
  bm <- ada(formula=RainTomorrow ~ .,data=weather[train,],iter=50,bag.frac=0.5,
            control=rpart.control(maxdepth=i,cp=0.01,minsplit=20,xval=10))
  prtest <- predict(bm, newdata=weather[test,])
  t <- table(weather[test,"RainTomorrow"], prtest,dnn=c("Actual", "Predicted"))
  error[i-1] <- (t[1,2]+t[2,1])/sum(t)
}
plot(error,type = "l", main = "CV error rates to find best Max Tree Depth")
points(which.min(error), error[which.min(error)], col = "red", cex = 2, pch = 16)

# From this plot we see that a maxdepth of 2 gives us the lowest error test rates. 
# This makes sense if we need a slow learning algo. 

# So we should create a model with this max depth 
bm <- ada(formula=RainTomorrow ~ .,data=weather[train,],iter=50,bag.frac=0.5,
          control=rpart.control(maxdepth=2,cp=0.01,minsplit=20,xval=10))

# but lets keep a maxdepth of 30 to compare 
bm30 <- ada(formula=RainTomorrow ~ .,data=weather[train,],iter=50,bag.frac=0.5,
            control=rpart.control(maxdepth=30,cp=0.01,minsplit=20,xval=10))

# find the test error rate (% may be helpful, similar to our churn lab)
# test error rate 
prtest <- predict(bm, newdata=weather[test,])
#table(weather[test,"RainTomorrow"], prtest,dnn=c("Actual", "Predicted"))
round(100* table(weather[test,"RainTomorrow"], prtest,dnn=c("% Actual", "% Predicted"))/length(prtest),1)

# test error rate bm30
prtest <- predict(bm30, newdata=weather[test,])
#table(weather[test,"RainTomorrow"], prtest,dnn=c("Actual", "Predicted"))
round(100* table(weather[test,"RainTomorrow"], prtest,dnn=c("% Actual", "% Predicted"))/length(prtest),1)


#Notice that we have better training error with max depth 30, but worse test. 
