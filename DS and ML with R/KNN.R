require(ISLR)
str(Caravan)
summary(Caravan)
any(is.na(Caravan))

# the need for standardization
var(Caravan[,1])
var(Caravan[,2])

# scale
purchase <- Caravan[,86]
scaled.caravan <- scale(Caravan[,-86])
print(var(scaled.caravan[,1]))
print(var(scaled.caravan[,2]))

# train and test split
test.index <- 1:1000
test.data <- scaled.caravan[test.index,]
test.purchase <- purchase[test.index]
train.data <- scaled.caravan[-test.index,]
train.purchase <- purchase[-test.index]

# KNN
require(class)
set.seed(101)
predicted.purchase <- knn(train.data, test.data, train.purchase, k=1)
print(head(predicted.purchase))

# Performance
err.rate <- mean(test.purchase != predicted.purchase)
print(err.rate)

# choosing a k value
predicted.purchase <- NULL
err.rate <- NULL

for(i in 1:20){
  set.seed(101)
  predicted.purchase <- knn(train.data,test.data,train.purchase, k=i)
  err.rate[i] <- mean(test.purchase != predicted.purchase)
}
print(err.rate)

# visualize k using the elbow method
require(ggplot2)
k <- 1:20
temp <- data.frame(err.rate, k)
ggplot(data = temp, aes(k, err.rate)) + geom_point() + geom_line()
