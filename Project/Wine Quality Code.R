###############################
#### Exploratory Analysis ##### 
###############################
## The goal of this analysis is to generate a basic summary of the predictors
## that are selected. We would also like to test the assumptions for LDA
## and multicollinearity between predictors
rm(list=ls())
library(RcmdrMisc)
library(ResourceSelection)
library(abind)
library(e1071)
library(tcltk)
library(aplpack)
library(car)
library(MASS)
library(boot)
library(class)
library(boot)
library(FNN)
library(leaps)
library(glmnet)
library(dplyr)
library(ROCR)
library(ISLR)


## Read the data
## Categorize response variable into two classes
wine = read.csv('winequality.csv', header = T, sep = ',')  
quality01 <- rep(0, length(wine$quality))                      
quality01[wine$quality > 6] = 1                               
wine = data.frame(wine, quality01)


## Create another column as factors based on the classes from 'quality01'
wine <- within(wine, {
  quality_level <- factor(quality01, labels=c('normal','good'))
})
table(wine$quality_level)


## Having a look of the data
sum(is.na(wine))
head(wine)


## We picked three interesting predictors that we would like to have a look before fitting to a model
## They are alcohol level, pH values, and residual sugar level
numSummary(wine[,"alcohol", drop=FALSE], groups=wine$quality_level, 
           statistics=c("mean", "sd", "IQR", "quantiles"), quantiles=c(0,.25,.5,.75,1))
numSummary(wine[,"pH", drop=FALSE], groups=wine$quality_level, 
           statistics=c("mean", "sd", "IQR", "quantiles"), quantiles=c(0,.25,.5,.75,1))
numSummary(wine[,"residual.sugar", drop=FALSE], groups=wine$quality_level, 
           statistics=c("mean", "sd", "IQR", "quantiles"), quantiles=c(0,.25,.5,.75,1))
numSummary(wine[,"density", drop=FALSE], groups=wine$quality_level, 
           statistics=c("mean", "sd", "IQR", "quantiles"), quantiles=c(0,.25,.5,.75,1))

par(mfrow=c(2,2))
hist(wine$alcohol, breaks = 20, main = "Histogram of Alcohol Level",
     xlab = "Alcohol Level", freq = FALSE)
curve(dnorm(x, mean = mean(wine$alcohol), sd = sd(wine$alcohol)), add = T)
hist(wine$pH, breaks = 20, main = "Histogram of pH Levels", xlab = "pH Level", freq = FALSE)
curve(dnorm(x, mean = mean(wine$pH), sd = sd(wine$pH)), add = T)
hist(wine$residual.sugar, breaks = 20, main = "Histogram of Residual Sugar Level",
     xlab = "Residual Sugar Level", freq = FALSE)
curve(dnorm(x, mean = mean(wine$residual.sugar), sd = sd(wine$residual.sugar)), add = T)
hist(wine$density, breaks = 20, main = "Histogram of Density Level",
     xlab = "Density Level", freq = FALSE)
curve(dnorm(x, mean = mean(wine$density), sd = sd(wine$density)), add = T)

par(mfrow=c(2,2))
with(wine, plotMeans(alcohol, quality_level, error.bars="se", connect=TRUE))
with(wine, plotMeans(pH, quality_level, error.bars="se", connect=TRUE))
with(wine, plotMeans(residual.sugar, quality_level, error.bars="se", connect=TRUE))
with(wine, plotMeans(density, quality_level, error.bars="se", connect=TRUE))

anova.1 <- aov(alcohol ~ quality_level, data=wine)
summary(anova.1)
anova.2 <- aov(pH ~ quality_level, data=wine)
summary(anova.2)
anova.3 <- aov(residual.sugar ~ quality_level, data=wine)
summary(anova.3)
anova.4 <- aov(density ~ quality_level, data=wine)
summary(anova.4)

par(mfrow=c(2,2))
Boxplot(alcohol~quality_level, data=wine, id.method="y")
Boxplot(density~quality_level, data=wine, id.method="y")
Boxplot(pH~quality_level, data=wine, id.method="y")
Boxplot(residual.sugar~quality_level, data=wine, id.method="y")


## Testing for variances between groups
wine_tbl = tbl_df(wine) 
wine1 = filter(wine_tbl, quality01==0)
wine2=filter(wine_tbl, quality01==1)

a=wine1$alcohol
b=wine1$pH
c=wine1$residual.sugar
d=wine1$density
data = c(a,b,c,d)
groups = factor(rep(letters[1:4], each=1382))
bartlett.test(data,groups)

a=wine2$alcohol
b=wine2$pH
c=wine2$residual.sugar
d=wine2$density
data = c(a,b,c,d)
groups = factor(rep(letters[1:4], each=217))
bartlett.test(data,groups)
print("It is found that group variances are different which indicates that the assumption for LDA is violated")


## Testing multicollinearity
(location = which(cor(wine[,c("alcohol","chlorides","citric.acid","density","fixed.acidity",
                              "free.sulfur.dioxide","pH","residual.sugar","sulphates",
                              "total.sulfur.dioxide","volatile.acidity")], use="complete")>0.6))

scatterplotMatrix(~alcohol+chlorides+citric.acid+density+fixed.acidity+free.sulfur.dioxide+pH+
                    residual.sugar+sulphates+total.sulfur.dioxide+volatile.acidity,
                  reg.line=FALSE, smooth=FALSE, spread=FALSE, span=0.5, ellipse=FALSE, 
                  levels=c(.5, .9), id.n=0, diagonal = 'density', data=wine)

cor_test = as.matrix(cor(wine[,c("alcohol","chlorides","citric.acid","density","fixed.acidity",
                                 "free.sulfur.dioxide","pH","residual.sugar","sulphates",
                                 "total.sulfur.dioxide","volatile.acidity")], use="complete"))

cor_test[3,5]
cor_test[4,5]
cor_test[6,10]

scatterplotMatrix(~citric.acid+density+fixed.acidity+free.sulfur.dioxide+total.sulfur.dioxide,
                  reg.line=FALSE, smooth=FALSE, spread=FALSE, span=0.5, ellipse=FALSE, 
                  levels=c(.5, .9), id.n=0, diagonal = 'density', data=wine)
print("It is observed that there is correlation between citric.acid and fixed.acidity")
print("It is observed that there is correlation between density and fixed.acidity")
print("It is observed that there is correlation between free.sulfur.dioxide and total.sulfur.dioxide")



##########################
##### Model Selction #####
##########################
## In this section, we are going to compare models and observe which produces the 
## lowest test error rate
##
## Splitting the data into training and testing set
set.seed(1)
length(wine)
wine = wine[, c(-12,-14)]
train = sample(nrow(wine), nrow(wine)*0.8)
wine_test = wine[-train,-12]
quality_test = wine$quality01[-train]


## First, we perform LDA and QDA
## Note that LDA assumption has been violated as we observed from the exploratory section
##
## LDA
lda.fit=lda(quality01~.,data=wine,subset=train)
lda.fit
lda.pred=predict(lda.fit, wine_test)
lda.class = lda.pred$class
(mytable.lda <- table(quality_test,lda.class))
mean(lda.class==quality_test)
mean(lda.class!=quality_test)
mytable.lda[3]/(mytable.lda[3]+mytable.lda[1])
mytable.lda[2]/(mytable.lda[2]+mytable.lda[4])
mytable.lda[4]/(mytable.lda[2]+mytable.lda[4])
mytable.lda[4]/(mytable.lda[3]+mytable.lda[4])
print("The LDA model yields a 0.140 test error rate")
print("The test error rate produced by LDA is close to the one produced by KNN")


## QDA
qda.fit=qda(quality01~.,data=wine,subset=train)
qda.fit
qda.class=predict(qda.fit,wine_test)$class
(mytable.qda <- table(quality_test,qda.class))
mean(qda.class==quality_test)
mean(qda.class!=quality_test)
mytable.qda[3]/(mytable.qda[3]+mytable.qda[1])
mytable.qda[2]/(mytable.qda[2]+mytable.qda[4])
mytable.qda[4]/(mytable.qda[2]+mytable.qda[4])
mytable.qda[4]/(mytable.qda[3]+mytable.qda[4])
print("The QDA model yields a 0.178 test error rate")


## Next, we are going to examine non-parametric model, KNN since it does not
## make any assumptions on the shape of the model
train.x = wine[train, -12]
test.x = wine_test
train.y = wine[train, 12]
test.error = c()
for(i in 1:50){
  knn.pred <- knn(train.x,test.x,train.y,k=i)
  test.error[i] = mean(knn.pred!=quality_test)
}
print(paste("The best model occured when k =",which.min(test.error)))
knn.pred = knn(train.x, test.x, train.y, k=1)
(mytable.knn <- table(quality_test,knn.pred))
mean(knn.pred==quality_test)
mean(knn.pred!=quality_test)
mytable.knn[3]/(mytable.knn[3]+mytable.knn[1])
mytable.knn[2]/(mytable.knn[2]+mytable.knn[4])
mytable.knn[4]/(mytable.knn[2]+mytable.knn[4])
mytable.knn[4]/(mytable.knn[3]+mytable.knn[4])
print("The KNN model yields a 0.134 test error rate")


## Now, we will examine logistic regression
## Note that we have found multicollinearity exist among some predictors such as
## free.sulfur.dioxide, and citric.acid
## This issue could potentailly affect the accuracy of this model, thus overestimated test error rate
##
## Logistic model with all predictors
glm.fit.full = glm(quality01~., data = wine, family = binomial, subset = train)
summary(glm.fit.full)
glm.probs = predict(glm.fit.full, wine_test, type = "response")
glm.pred.full = rep(0, length(quality_test))
glm.pred.full[glm.probs > 0.5] = 1
(mytable = table(quality_test, glm.pred.full))
mean(glm.pred.full==quality_test)
mean(glm.pred.full!=quality_test)
print("The logistic model yields a 0.143 test error rate")


## We see that some of the variables are not significant, let's further investigate into that
## Recall there is multicolinearity issue among predictors
##
## citric.acid and fixed.acidity
## density and fixed.acidity
## free.sulfur.dioxide and total.sulfur.dioxide
##
## Now free.sulfur.dioxide, citric.acid and pH are excluded from our model
glm.fit.reduced = glm(quality01~fixed.acidity+volatile.acidity+residual.sugar+chlorides+
                        total.sulfur.dioxide+density+sulphates+alcohol,
                      data = wine, family = binomial, subset = train)
summary(glm.fit.reduced)
glm.probs = predict(glm.fit.reduced, wine_test, type = "response")
glm.pred.reduced = rep(0, length(quality_test))
glm.pred.reduced[glm.probs > 0.5] = 1
(mytable = table(quality_test, glm.pred.reduced))
mean(glm.pred.reduced==quality_test)
mean(glm.pred.reduced!=quality_test)
print("It is observed that test error rate for the reduced logistic model improved from 0.143 to 0.134")
print("The test error rate produced by this reduced logistic model is the same as the test error rate from the KNN model")


## Now, we will see if we can improve this reduced logistic model by eliminating
## possible outliers/high leverage points
par(mfrow = c(1,1))
influencePlot(glm.fit.reduced, id.method="noteworthy", id.n=2)
print("Influencial and/or high residual points are 1082,481,653,639,282")
wine = wine[-c(1082,481,653,639,282),]
set.seed(1)
train = sample(nrow(wine), nrow(wine)*0.8)
wine_test = wine[-train,-12]
quality_test = wine$quality01[-train]
glm.fit.reduced1 = glm(quality01~fixed.acidity+volatile.acidity+residual.sugar+chlorides+
                         total.sulfur.dioxide+density+sulphates+alcohol,
                       data = wine, family = binomial,subset = train)
summary(glm.fit.reduced1)
glm.probs = predict(glm.fit.reduced1, wine_test, type = "response")
glm.pred.reduced1 = rep(0, length(quality_test))
glm.pred.reduced1[glm.probs > 0.5] = 1
(mytable = table(quality_test, glm.pred.reduced1))
mean(glm.pred.reduced1==quality_test)
mean(glm.pred.reduced1!=quality_test)
print("The results improved from 0.134 to 0.112")


## Interaction terms
## Note that we still have fixed.acidity and density left in our previous model
## Thus, we are going to test the interaction between the two terms
scatterplot(wine$fixed.acidity,wine$density)

glm.fit.interaction <- glm(quality01~fixed.acidity*density+volatile.acidity+residual.sugar+chlorides+
                             total.sulfur.dioxide+sulphates+alcohol, family=binomial, data=wine, subset = train)
summary(glm.fit.interaction)
print("It is observed the interaction term does not contribute to our model")


## Transformation
## Now we are going to determine if there is any evidence that higher power terms
## of these predictors should be included to the model 
glm.fixed = glm(quality01~poly(fixed.acidity, 3), data = wine)
summary(glm.fixed)
glm.volatile = glm(quality01~poly(volatile.acidity, 3), data = wine)
summary(glm.volatile)
glm.sugar = glm(quality01~poly(residual.sugar, 3), data = wine)
summary(glm.sugar)
glm.chlorides = glm(quality01~poly(chlorides, 3), data = wine)
summary(glm.chlorides)
glm.total = glm(quality01~poly(total.sulfur.dioxide, 3), data = wine)
summary(glm.total)
glm.density= glm(quality01~poly(density, 3), data = wine)
summary(glm.density)
glm.sulphates= glm(quality01~poly(sulphates, 3), data = wine)
summary(glm.sulphates)
glm.alcohol= glm(quality01~poly(alcohol, 3), data = wine)
summary(glm.alcohol)
glm.fit.reduced2 = glm(quality01~fixed.acidity^3+volatile.acidity^2+residual.sugar^2+chlorides^3+
                         total.sulfur.dioxide^2+density^2+sulphates^2+alcohol^3, data = wine, family = binomial,subset = train)
summary(glm.fit.reduced2)
glm.probs = predict(glm.fit.reduced2, wine_test, type = "response")
glm.pred.reduced2 = rep(0, length(quality_test))
glm.pred.reduced2[glm.probs > 0.5] = 1
(mytable.log = table(quality_test, glm.pred.reduced2))
mean(glm.pred.reduced2==quality_test)
mean(glm.pred.reduced2!=quality_test)
mytable.log[3]/(mytable.log[3]+mytable.log[1])
mytable.log[2]/(mytable.log[2]+mytable.log[4])
mytable.log[4]/(mytable.log[2]+mytable.log[4])
mytable.log[4]/(mytable.log[3]+mytable.log[4])
print("The test error rate produced by the reduced logistic model with higher power terms is still 0.112")


## Now, the only model that we have not tested is the ridge regression
## Ridge regression
set.seed(1)
x = model.matrix(quality01~., wine)[,-1]
y = wine$quality01
cv.out.ridge = cv.glmnet(x, y, alpha=0)
plot(cv.out.ridge)
(bestlam = cv.out.ridge$lambda.min)
ridge.mod=glmnet(x, y,alpha=0,lambda=bestlam, thresh=1e-12,family = "binomial") 
ridge.pred=predict(ridge.mod, s=bestlam, newx=x[-train,], type = "class") 
mean(ridge.pred!=y[-train])
(ridge.coef = predict(ridge.mod,type="coefficients",s=bestlam)[1:12,])
print("The ridge model yields a test error rate, 0.094 which is close to the test error rate produced by the reduced logistic model when cv is performed")
print("The fact that lambda is small indicates that the shrinkage on all coefficients is small")
print("Thus, the model even with all predictors included has good classifying accuracy potentailly")



#############################
##### Feature Selection #####
#############################
## In this section, we are goint to choose a set of predictors that
## are most related to the response variable
##
## The Lasso
set.seed(1)
cv.out.lasso = cv.glmnet(x, y, alpha=1)
par(mfrow=c(1,1))
plot(cv.out.lasso)
(bestlam = cv.out.lasso$lambda.min)
lasso.mod=glmnet(x,y,alpha=1,lambda=bestlam,family = "binomial") 
lasso.pred = predict(lasso.mod,s=bestlam,newx = x[-train,],type = "class")
mean(lasso.pred!=y[-train])
print("The test error rate produced by the Lasso is 0.106")
lasso.coef=predict(lasso.mod, type="coefficients", s=bestlam)[1:12,]
lasso.coef
lasso.coef[lasso.coef!=0]
lasso.prob = predict(lasso.mod,s=bestlam,newx = x[-train,],type = "response")
lasso.prob.pred = rep(0, length(y[-train]))
lasso.prob.pred[lasso.prob > 0.5] = 1
(mytable.lasso = table(y[-train], lasso.prob.pred))
mean(lasso.prob.pred==y[-train])
mean(lasso.prob.pred!=y[-train])
mytable.lasso[3]/(mytable.lasso[3]+mytable.lasso[1])
mytable.lasso[2]/(mytable.lasso[2]+mytable.lasso[4])
mytable.lasso[4]/(mytable.lasso[2]+mytable.lasso[4])
mytable.lasso[4]/(mytable.lasso[3]+mytable.lasso[4])
print("The Lasso method suggest that citric.acid and pH should be excluded from the model")
print("Recall that the logistic model with all predictors include also have the same suggestion")
print("With further examination of the lasso model's output, it is observed that both free and total sulfur dioxide's coefficient resulted in very small numbers but in opposite sign")
print("This is a indication that both terms have a similar effect on the model in terms of maginitude, but in the opposite direction")
print("The fact that we have a small lambda value indicates that ")
print("The coefficient of predictor density is -176.529, we suspect that density level has the largest opposite effect on wine quality as compared to the rest of the predictors")
print("Recall from ANOVA, high quality wine tends to have high acohol level and low density level")
print("If we examine the lasso plot, one could reasonably suspect that we could potentially reduce the number of predictors included in the model by accepting a slightly higher test error rate")



##########################
##### Interpretation #####
##########################
## However, one draw back from using ridge regression is that the model iteself
## is almost impossible to interpret
##
## Thus, we will use PCA to determine components explain the most variance of the data
## Recall that components are different sets of linear combinations of predictors
pr.out = prcomp(wine[,-12], scale=T)
summary(pr.out)
pr.out$rotation
dim(pr.out$x)
par(mfrow=c(1,2))
plot(summary(pr.out)$importance[2,], type = "b", ylab = "Proportion of Variance Unexplained",
     xlab = "Principal Component")
plot(summary(pr.out)$importance[3,], type = "b", ylab = "Cumulative Proportion Explained",
     xlab = "Principal Component")
par(mfrow=c(1,1))
pr.out$rotation=-pr.out$rotation
pr.out$x=-pr.out$x
biplot(pr.out, scale=0)
print("We observe that ")
print("")



####################################
##### Improving Model Accuracy #####
####################################
## It is observed that logistic is a better model amongst LDA, QDA and KNN
## Therefore, we will perform a cross-validation method on the logistic model
glm.fit.cv.reduced = glm(quality01~fixed.acidity^3+volatile.acidity^2+residual.sugar^2+chlorides^3+
                           total.sulfur.dioxide^2+density^2+sulphates^2+alcohol^3, data = wine, family = binomial)
(error.reduced = cv.glm(wine, glm.fit.cv.reduced, K = 5)$delta)
print("It is found that using k fold cv on the reduced logistic model with higher power terms yields a 0.085 test error rate")


## Now we would like to perform a cv on the KNN model to see if we could further
## reduce its test error rate. Note that lowest test error rate occured when k=1
## This shows that our KNN has high variance which could potentially be a better classifier
## than the logistic model
n <- nrow(wine)
mydf <- wine[sample(1:n, n),]
numfolds <- 10
fold.indices <- cut(1:n, breaks=numfolds, labels=FALSE)
overall_correct.knn <- c()
overall_error.knn <- c()
type1.knn <- c()
type2.knn <- c()
accuracy.knn <- c()
power.knn <- c()

for(i in 1:numfolds){
  test.indices <- which(fold.indices == i)
  test.data <- wine[test.indices, -12]
  test.data.y <- wine[test.indices,12]
  train.data <- wine[-test.indices, -12]
  train.data.y <- wine[-test.indices, 12]
  knn.fit=knn(train.data, test.data, train.data.y, k=1)
  mytable.knn01 <- table(test.data.y,knn.fit)
  overall_correct.knn[i] <- mean(knn.fit==test.data.y)
  overall_error.knn[i] <- mean(knn.fit!=test.data.y)
  type1.knn[i] <- mytable.knn01[3]/(mytable.knn01[3]+mytable.knn01[1])
  type2.knn[i] <- mytable.knn01[2]/(mytable.knn01[2]+mytable.knn01[4])
  accuracy.knn[i] <- mytable.knn01[4]/(mytable.knn01[2]+mytable.knn01[4])
  power.knn[i] <- mytable.knn01[4]/(mytable.knn01[3]+mytable.knn01[4])
}
mean(overall_correct.knn) 
mean(overall_error.knn)
mean(type1.knn)
mean(type2.knn)
mean(accuracy.knn)
mean(power.knn)


## cv on the Lasso model
overall_correct.lasso <- c()
overall_error.lasso <- c()
type1.lasso <- c()
type2.lasso <- c()
accuracy.lasso <- c()
power.lasso <- c()

for(i in 1:numfolds){
  test.indices <- which(fold.indices == i)
  test.data <- model.matrix(quality01~., wine)[test.indices,-1]
  test.data.y <- wine[test.indices, 12]
  train.data <- model.matrix(quality01~., wine)[-test.indices,-1]
  train.data.y <- wine[-test.indices, 12]
  lasso.mod.cv <- glmnet(train.data,train.data.y,alpha=1,lambda=bestlam,family = "binomial") 
  lasso.prob.cv = predict(lasso.mod.cv,s=bestlam,newx = test.data,type = "response")
  lasso.prob.pred.cv = rep(0, length(test.data.y))
  lasso.prob.pred.cv[lasso.prob.cv > 0.5] = 1
  mytable.lasso01 = table(test.data.y, lasso.prob.pred.cv)
  overall_correct.lasso[i] <- mean(lasso.prob.pred.cv==test.data.y)
  overall_error.lasso[i] <- mean(lasso.prob.pred.cv!=test.data.y)
  type1.lasso[i] <- mytable.lasso01[3]/(mytable.lasso01[3]+mytable.lasso01[1])
  type2.lasso[i] <- mytable.lasso01[2]/(mytable.lasso01[2]+mytable.lasso01[4])
  accuracy.lasso[i] <- mytable.lasso01[4]/(mytable.lasso01[2]+mytable.lasso01[4])
  power.lasso[i] <- mytable.lasso01[4]/(mytable.lasso01[3]+mytable.lasso01[4])
}
mean(overall_correct.lasso) 
mean(overall_error.lasso)
mean(type1.lasso)
mean(type2.lasso)
mean(accuracy.lasso)
mean(power.lasso)
print("It is observed that overall the lasso model still maintain a lower test error rate after cv")


## ROC curve for the logistic and lasso model
par(mfrow=c(1,2))
glm.probs = predict(glm.fit.reduced2, wine_test, type = "response")
wine_test1 = wine[-train,12]
glm.probs = prediction(glm.probs, wine_test1)
eval.glm = performance(glm.probs, 'acc')
plot(eval.glm)
roc.glm = performance(glm.probs, 'tpr', 'fpr')
plot(roc.glm)

par(mfrow=c(1,2))
lasso.prob01 = predict(lasso.mod,s=bestlam,newx = x[-train,],type = "response")
lasso.prob01 = prediction(lasso.prob01, wine_test1)
eval.lasso = performance(lasso.prob01, 'acc')
plot(eval.lasso)
roc.lasso = performance(lasso.prob01, 'tpr', 'fpr')
plot(roc.lasso)