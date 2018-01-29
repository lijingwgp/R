rm(list=ls())
##################### 
#### QUESTION 1 ##### 
##################### 
require(ISLR)
# set seed to 5072
set.seed(5072)

# use the full data set to perform a logistic regression with Direction as the 
# response variable and the five lag variables plus Volume as predictors.
glm.fit <- glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=Weekly, family=binomial)

# use the summary function to print results
summary(glm.fit)
print("It is found that lag2 is statistically significant")

# create and display a confusion matrix, assume that Down is the null hypothesis
glm.probs <- predict(glm.fit, type="response")   # type="response" outputs probabilities 
                                                 # P(Y = 1|X) instead of, for example, the logit.
contrasts(Weekly$Direction)
glm.pred <- rep("Down", nrow(Weekly))
glm.pred[glm.probs>.5] <- "Up"
mytable <- table(Weekly$Direction, glm.pred)     # for consistency, we will always put the actuals on the left 
mytable                                          # and the predictions on the top of the table

# compute the overall fraction of correct predictions
# compute the overall error rate
# type 1 and type 2 error rates
# the power of the model
# the precision of the model
mean(glm.pred==Weekly$Direction)
mean(glm.pred!=Weekly$Direction)
mytable[3]/(mytable[3]+mytable[1])
mytable[2]/(mytable[2]+mytable[4])
mytable[4]/(mytable[2]+mytable[4])
mytable[4]/(mytable[3]+mytable[4])

# fit a logistic regression model using training data period from 1990 to 2008,
# with lag2 as the only predictor variable
train <- (Weekly$Year < 2009)
weekly.test <- Weekly[!train, ]
glm.fit <- glm(Direction~Lag2, data=Weekly, family=binomial, subset=train)

# construct and display a confusion matrix. from this table, compute the same
# five performance statistics
direction.test <- Weekly$Direction[!train]
glm.probs <- predict(glm.fit, weekly.test, type="response")
glm.pred <- rep("Down", nrow(weekly.test))
glm.pred[glm.probs>.5] <- "Up"
mytable <- table(direction.test, glm.pred)     
mytable
mean(glm.pred==direction.test)
mean(glm.pred!=direction.test)
mytable[3]/(mytable[3]+mytable[1])
mytable[2]/(mytable[2]+mytable[4])
mytable[4]/(mytable[2]+mytable[4])
mytable[4]/(mytable[3]+mytable[4])

# repeat e and f using LDA
require(MASS)
lda.fit=lda(Direction~Lag2,data=Weekly,subset=train)
lda.fit
lda.pred=predict(lda.fit, weekly.test)
names(lda.pred)
lda.class=lda.pred$class
mytable <- table(direction.test,lda.class)
mytable
mean(lda.class==direction.test)
mean(lda.class!=direction.test)
mytable[3]/(mytable[3]+mytable[1])
mytable[2]/(mytable[2]+mytable[4])
mytable[4]/(mytable[2]+mytable[4])
mytable[4]/(mytable[3]+mytable[4])

# repeat e and f using QDA
qda.fit=qda(Direction~Lag2,data=Weekly,subset=train)
qda.fit
qda.class=predict(qda.fit,weekly.test)$class
mytable <- table(direction.test,qda.class)
mytable
mean(qda.class==direction.test)
mean(qda.class!=direction.test)
mytable[3]/(mytable[3]+mytable[1])
mytable[2]/(mytable[2]+mytable[4])
mytable[4]/(mytable[2]+mytable[4])
mytable[4]/(mytable[3]+mytable[4])

# repeat e and f using KNN with K=1
require(class)
train.X=data.frame(Weekly[train,3])
test.X=data.frame(weekly.test[,3])
train.y=as.vector(Weekly[train,9])
set.seed(5072)
knn.pred = knn(train.X, test.X, train.y, k=1)
mytable <- table(direction.test,knn.pred)
mytable
mean(knn.pred==direction.test)
mean(knn.pred!=direction.test)
mytable[3]/(mytable[3]+mytable[1])
mytable[2]/(mytable[2]+mytable[4])
mytable[4]/(mytable[2]+mytable[4])
mytable[4]/(mytable[3]+mytable[4])

# repeat e and f using knn with k=5
knn.pred = knn(train.X, test.X, train.y, k=5)
mytable <- table(direction.test,knn.pred)
mytable
mean(knn.pred==direction.test)
mean(knn.pred!=direction.test)
mytable[3]/(mytable[3]+mytable[1])
mytable[2]/(mytable[2]+mytable[4])
mytable[4]/(mytable[2]+mytable[4])
mytable[4]/(mytable[3]+mytable[4])

# based on the confusion matrices, which of these methods appears to provide the best results
print("LDA and logistic regression both yeilds better results in terms of test error rate")



rm(list=ls())
##################### 
#### QUESTION 2 ##### 
##################### 
# set the random seed to 5072
set.seed(5072)

# create a binary variable, mpg01 that contains a 1 if mpg contains a value above 
# its median, and a 0 otherwise.
require(ISLR)
mpg01 <- rep(0,length(Auto$mpg))
mpg01[Auto$mpg > median(Auto$mpg)] <- 1
auto <- data.frame(Auto,mpg01)
auto <- auto[-1]

# split the data into training set and test set
# the training set should be 80% of the total number of rows
train<-sample(1:nrow(auto),nrow(auto)*0.8)
test <- setdiff(1:nrow(auto),train)
auto.train<-auto[train,]
auto.test <-auto[test,]

# perform logistic regression on the training data to predict mpg01
# using cylinders, displacement and weight
glm.fit <- glm(mpg01~cylinders+displacement+weight, data=auto, family=binomial, subset = train)
summary(glm.fit)
glm.probs <- predict(glm.fit,auto.test,type = "response")
glm.pred <- rep(0,length(glm.probs))
glm.pred[glm.probs>0.5] <- 1
mytable <- table(auto.test$mpg01, glm.pred)     
mytable 
mean(glm.pred==auto.test$mpg01)
mean(glm.pred!=auto.test$mpg01)
mytable[3]/(mytable[3]+mytable[1])
mytable[2]/(mytable[2]+mytable[4])
mytable[4]/(mytable[2]+mytable[4])
mytable[4]/(mytable[3]+mytable[4])

# repeat d and e using LDA
require(MASS)
lda.fit <- lda(mpg01~cylinders+displacement+weight, data = auto, subset = train)
lda.pred <- predict(lda.fit, auto.test)
lda.class=lda.pred$class
mytable <- table(auto.test$mpg01,lda.class)
mytable 
mean(lda.class==auto.test$mpg01)
mean(lda.class!=auto.test$mpg01)
mytable[3]/(mytable[3]+mytable[1])
mytable[2]/(mytable[2]+mytable[4])
mytable[4]/(mytable[2]+mytable[4])
mytable[4]/(mytable[3]+mytable[4])

# repeat d and e using QDA
qda.fit=qda(mpg01~cylinders+displacement+weight, data = auto, subset = train)
qda.fit
qda.class=predict(qda.fit,auto.test)$class
mytable <- table(auto.test$mpg01,qda.class)
mytable
mean(qda.class==auto.test$mpg01)
mean(qda.class!=auto.test$mpg01)
mytable[3]/(mytable[3]+mytable[1])
mytable[2]/(mytable[2]+mytable[4])
mytable[4]/(mytable[2]+mytable[4])
mytable[4]/(mytable[3]+mytable[4])

# repeat d and e using KNN
require(FNN)
require(class)
train.x <- auto.train[,c(1,2,4)]
test.x <- auto.test[,c(1,2,4)]
train.y <- auto.train[,9]
test.error <- c()
for(i in 1:50){
  knn.pred <- knn(train.x,test.x,train.y,k=i)
  test.error[i] <- mean(knn.pred!=auto.test$mpg01)
}
print(paste("The best model occured when k =",which.min(test.error)))

# fit a KNN model with the optimal k value
knn.pred = knn(train.x, test.x, train.y, k=3)
mytable <- table(auto.test$mpg01,knn.pred)
mytable
mean(knn.pred==auto.test$mpg01)
mean(knn.pred!=auto.test$mpg01)
mytable[3]/(mytable[3]+mytable[1])
mytable[2]/(mytable[2]+mytable[4])
mytable[4]/(mytable[2]+mytable[4])
mytable[4]/(mytable[3]+mytable[4])

# based on the confusion matrices, which of these methods appear to provide 
# the best results on this data
print("QDA returns the best result since it has the highest overall correction rate")



rm(list=ls())
##################### 
#### QUESTION 3 ##### 
##################### 
# set the random seed to 5072
require(MASS)
set.seed(5072)

# create training and test sets in the ratio of 80/20
train <- sample(nrow(Boston),nrow(Boston)*0.8)
test <- setdiff(1:nrow(Boston), train)

# fit classification models in order to predict whether a given suburb has a 
# crime rate above or below the median by using nox, rad, and dis as predictors
crim_test <- rep(0, length(Boston$crim))
crim_test[Boston$crim > median(Boston$crim)] = 1
Boston <- data.frame(Boston,crim_test)
boston.train <- Boston[train,]
boston.test <- Boston[test,]
crim_test <- crim_test[test]

# logistic
glm.fit <- glm(crim_test~nox+rad+dis, data = Boston, family = binomial, subset = train)
glm.probs <- predict(glm.fit, boston.test, type = "response")
glm.pred <- rep(0, length(glm.probs))
glm.pred[glm.probs > 0.5] = 1
mytable <- table(crim_test, glm.pred)     
mytable 
mean(glm.pred==crim_test)
mean(glm.pred!=crim_test)
mytable[3]/(mytable[3]+mytable[1])
mytable[2]/(mytable[2]+mytable[4])
mytable[4]/(mytable[2]+mytable[4])
mytable[4]/(mytable[3]+mytable[4])
print("The overall correction rate of predicting crime rate above median is 0.7745")
print("The test error rate for a logistic regression model is 0.2254")

# LDA
lda.fit <- lda(crim_test~nox+rad+dis, data = Boston, family = binomial, subset = train)
lda.pred <- predict(lda.fit, boston.test)
lda.class = lda.pred$class
mytable <- table(crim_test,lda.class)
mytable 
mean(lda.class==crim_test)
mean(lda.class!=crim_test)
mytable[3]/(mytable[3]+mytable[1])
mytable[2]/(mytable[2]+mytable[4])
mytable[4]/(mytable[2]+mytable[4])
mytable[4]/(mytable[3]+mytable[4])
print("The test error rate for a LDA model is 0.2254")

# QDA
qda.fit = qda(crim_test~nox+rad+dis, data = Boston, subset = train)
qda.class = predict(qda.fit,boston.test)$class
mytable <- table(crim_test,qda.class)
mytable
mean(qda.class==crim_test)
mean(qda.class!=crim_test)
mytable[3]/(mytable[3]+mytable[1])
mytable[2]/(mytable[2]+mytable[4])
mytable[4]/(mytable[2]+mytable[4])
mytable[4]/(mytable[3]+mytable[4])
print("The test error rate for a QDA model is 0.2058 which is better than logistic, and LDA")

# KNN
require(class)
require(FNN)
boston.train.x <- boston.train[, c(5,8,9)]
boston.test.x <- boston.test[, c(5,8,9)]
boston.train.y <- boston.train[,15]
test.error <- c()

for(i in 1:50){
  knn.pred = knn(boston.train.x, boston.test.x, boston.train.y, k=i)
  test.error[i] <- mean(knn.pred!=crim_test)
}
print(paste("The best model occured when k =",which.min(test.error)))

knn.pred = knn(boston.train.x, boston.test.x, boston.train.y, k=1)
mytable <- table(crim_test,knn.pred)
mean(knn.pred==crim_test)
mean(knn.pred!=crim_test)
mytable[3]/(mytable[3]+mytable[1])
mytable[2]/(mytable[2]+mytable[4])
mytable[4]/(mytable[2]+mytable[4])
mytable[4]/(mytable[3]+mytable[4])
print("The KNN model produces a test error rate of 0.049 which is the lowest among LDA, QDA and logistic")



rm(list=ls())
##################### 
#### QUESTION 4 ##### 
##################### 
# generate a simulated data as follows:
set.seed(5072)
x <- rnorm(100)
y <- x-2*x^2+rnorm(100)

# create a data frame containing these x and y variables in named columns X and Y
data <- data.frame(x,y)
colnames(data) <- c("X", "Y")

# create a scatterplot X against Y
plot(data$X, data$Y)

# set the random seed to 123, then compute LOOCV errors
require(boot)
set.seed(123)
glm.fit <- glm(Y~X, data = data)
cv.glm(data, glm.fit)$delta
glm.fit = glm(Y ~ poly(X, 2), data = data)
cv.glm(data, glm.fit)$delta
glm.fit = glm(Y ~ poly(X, 3), data = data)
cv.glm(data, glm.fit)$delta
glm.fit = glm(Y ~ poly(X, 4), data = data)
cv.glm(data, glm.fit)$delta

# repeat the previous process using random seed 456
set.seed(456)
glm.fit <- glm(Y~X, data = data)
cv.glm(data, glm.fit)$delta
glm.fit = glm(Y ~ poly(X, 2), data = data)
cv.glm(data, glm.fit)$delta
glm.fit = glm(Y ~ poly(X, 3), data = data)
cv.glm(data, glm.fit)$delta
glm.fit = glm(Y ~ poly(X, 4), data = data)
cv.glm(data, glm.fit)$delta
print("Using random seeds 456 produces the sames results as using random seeds 123")
print("This is because LOOCV is a special case of Kth fold cross validation. It evaluates n folds of a single data point")

# which of the models had the smallest LOOCV error? Is this what you expected?
print("The second model has the smallest LOOCV error. This is as expected since we observed the relationship between X and Y is quadratic")

# comment on the statistical significance of the coefficient estimates that results 
# from fitting each of the models in d) using least squares. Do these results 
# agree with the conclusions drawn based on the cross-validation results?
summary(glm.fit)
print("p-value shows that both linear and quadratic terms are statistical significant which agress with the CV results")
