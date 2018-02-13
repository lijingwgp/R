rm(list=ls()) 
require(ISLR)
require(e1071)
##################### 
#### Part 1
##################### 
# a)
# use the following code to create a set of indices containing a random sample
# of 800 integers representing the training subset of set OJ
# and a set of test indices representing the remaining observations
set.seed(5082) 
n = dim(OJ)[1] 
train_inds = sample(1:n,800) 
test_inds = (1:n)[-train_inds] 

# b)
# fit a support vector classifier to the training data using cost = 0.01
# with Purchase as the response and other variables as predictors
# use the summary() to produce summary statistics and describe the results obtained
svc.fit <- svm(Purchase~., data=OJ[train_inds,], kernel = 'linear', cost = 0.01, scale = T)
summary(svc.fit)
print('There are two classes being identified. They are CH and MM.')
print('There are total of 446 support vectors. 223 of them are in the CH class, and the remaining is in the MM class')

# c)
# compute and display the training and test error rate
pred.train <- predict(svc.fit, OJ[train_inds,])
table(truth=OJ[train_inds,1], predict=pred.train)
mean(pred.train != OJ[train_inds,1])
pred.test <- predict(svc.fit, OJ[test_inds,])
table(OJ[test_inds,1], pred.test)
mean(pred.test != OJ[test_inds,1])
print("The training error rate of SVC is 0.169, and the test error rate of SVC is 0.152")

# d)
# use the tune() to select an optimal cost
# consider values in the range 0.01 to 10
svc.cv <- tune(svm, Purchase~., data = OJ, kernel = 'linear', 
               ranges=list(cost=seq(0.01,10,by=0.1)), scale = T)
summary(svc.cv)
svc.best <- svc.cv$best.model

# e)
# compute and display the training and test error rates using this new value for cost
pred.train <- predict(svc.best, OJ[train_inds,])
table(OJ[train_inds,1], pred.train)
mean(pred.train != OJ[train_inds,1])
pred.test <- predict(svc.best, OJ[test_inds,])
table(OJ[test_inds,1], pred.test)
mean(pred.test != OJ[test_inds,1])
print('The CV training error rate of SVC is 0.165, and the CV test error rate of SVC is 0.148')

# f)
# repeat part b through part e using a svm with radial kernel
# use the default value for gamma
svm.radial <- svm(Purchase~., data=OJ[train_inds,], kernel = 'radial', cost = 0.01, scale = T)
summary(svm.radial)
print('There are two classes being identified. They are CH and MM.')
print('There are total of 613 support vectors. 308 of them are in the CH class, and the remaining is in the MM class')

pred.train <- predict(svm.radial, OJ[train_inds,])
table(truth=OJ[train_inds,1], predict=pred.train)
mean(pred.train != OJ[train_inds,1])
pred.test <- predict(svm.radial, OJ[test_inds,])
table(OJ[test_inds,1], pred.test)
mean(pred.test != OJ[test_inds,1])
print('The training error rate of SVM with radial method is 0.381, the test error rate of SVM with radial method is 0.415')

svm.cv1 <- tune(svm, Purchase~., data = OJ, kernel = 'radial', 
                ranges=list(cost=seq(0.01,10,by=0.1)), scale = T)
summary(svm.cv1)
svm.best1 <- svm.cv1$best.model

pred.train <- predict(svm.best1, OJ[train_inds,])
table(OJ[train_inds,1], pred.train)
mean(pred.train != OJ[train_inds,1])
pred.test <- predict(svm.best1, OJ[test_inds,])
table(OJ[test_inds,1], pred.test)
mean(pred.test != OJ[test_inds,1])
print('The CV training error rate of SVM with radial method is 0.155, the CV test error rate of SVM with radial method is 0.137')

# g)
# repeat part b through part e using a svm with polynomial kernel
# use the default value for gamma
# set degree =2
svm.poly <- svm(Purchase~., data=OJ[train_inds,], kernel = 'polynomial', cost = 0.01, 
                scale = T, degree = 2)
summary(svm.poly)
print('There are two classes being identified. They are CH and MM.')
print('There are total of 614 support vectors. 309 of them are in the CH class, and the remaining is in the MM class')

pred.train <- predict(svm.poly, OJ[train_inds,])
table(truth=OJ[train_inds,1], predict=pred.train)
mean(pred.train != OJ[train_inds,1])
pred.test <- predict(svm.poly, OJ[test_inds,])
table(OJ[test_inds,1], pred.test)
mean(pred.test != OJ[test_inds,1])
print('The training error rate of SVM with poly method is 0.381, the test error rate of SVM with poly method is 0.415')

svm.cv2 <- tune(svm, Purchase~., data = OJ, kernel = 'polynomial', 
                ranges=list(cost=seq(0.01,10,by=0.1)), scale = T, degree = 2)
summary(svm.cv2)
svm.best2 <- svm.cv2$best.model

pred.train <- predict(svm.best2, OJ[train_inds,])
table(OJ[train_inds,1], pred.train)
mean(pred.train != OJ[train_inds,1])
pred.test <- predict(svm.best2, OJ[test_inds,])
table(OJ[test_inds,1], pred.test)
mean(pred.test != OJ[test_inds,1])
print('The CV training error rate of SVM with poly method is 0.158, the CV test error rate of SVM with poly method is 0.144')

# h)
# overall, which approach seems to give the best results on this data
print('Overall, SVM with radial based kernel relatively performs the best')
