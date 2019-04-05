rm(list=ls()) 
require(rpart)
require(ada)
require(randomForest)
require(e1071)
require(doParallel)
require(snow)
require(glmnet)
require(gam)
require(splines)
require(ggfortify)
require(cluster)
####################### 
#### Data Preparation
#######################
bank <- read.csv("bank-additional.csv", sep=",", header = T)
sum(is.na(bank))
detecting_na = matrix(nrow = dim(bank)[1], ncol = dim(bank)[2])
for (i in 1:nrow(detecting_na)) {
  for (j in 1:ncol(detecting_na)) {
    if (bank[i,j] == "unknown") {
      detecting_na[i,j] = "True"
    }
  }
}
registerDoParallel(cores=detectCores())
#registerDoSEQ()



########################### 
#### Exploratory Analysis
###########################
## PCA
pca = prcomp(bank[,c(1,11,12,13,14,16,17,18,19,20)], scale = T, center = T)
summary(pca)
pca$rotation = -pca$rotation
pca$x = -pca$x
autoplot(pca, data = bank,loadings = TRUE,loadings.label = TRUE, loadings.label.size = 3.5, loadings.colour = 'blue')

print("The first loading vector puts equally more weights on employment variation rate, Euro interbank offered rate, number of employees. All of these are economic related variables")
print("The first loading vector puts more negative weights on variable previous which represent number of contacts before this campaign")
print("This may imply that when predicting whether a client is a term-deposit subsriber, the number of contacts before this campaign could have a opposite effect from all financial metrics")

print("The second loading vector puts relativly more weights on variable previous and all of the economic variables are centered around 0")
print("Also note that pdays has the largest negative weight in the second principal component. pdays represent number of days passed after the client was contacted from last campaign")
print("This may imply that when predicting whether a client is a term-deposite subsriber, the number of days passed after the client was contacted from last campaign largely have a oppsite effect from all other variables")

print("In general, we observe that the economic indicators are close to each other both in projected directions and projected length")
print("This shows possbile correlation between economy related variables")
print("We observe that age, duration, and campaign do not provide very much explaination on how the data population varies")

print("As a conclusion, we observe that economic variables dominate largely in the first demision. They explains the majority of the variances that are positively associated with whether a client being classified as a term-deposite subsriber")
print("However, when taking the variable previous into consideration together with economic variables, we suspect that when overall economic climate shows positive sign, the more contacts made before the current campaign the more negative effect it has on a client considering to become a term-deposite subsriber")

print("Variables that represents efforts made by the bank trying to reach out to a client before the current campaign largely explains the variances in the second dimension while putting economic indicators aside")
print("Given the fact that variable previous and pdays each has a positive and negative effect on classifying a client, we highly suspect that the more contacts made before the current campaign the more positive effect it will have on a client")
print("Similarly, the more days passed after a client was contacted from last campaign the more negative effect it will have on a client")

pca.var=pca$sdev^2
pve = pca.var / sum(pca.var)
plot(pve, xlab="Principal Component", ylab="Proportion of Variance Explained", ylim=c(0,1),type='b')
plot(cumsum(pve), xlab="Principal Component", ylab="Cumulative Proportion of Variance Explained", ylim=c(0,1),type='b')

## K-Means with PCA
kmax<-50
wss<-sapply(1:kmax,function(k){kmeans(bank[,c(1,11,12,13,14,16,17,18,19,20)],k,nstart=20)$tot.withinss})
plot(1:kmax,wss,type="b",pch=19, frame=FALSE, xlab="Number of Cluster K", ylab="Total within-clusters sum of squares")
abline(v=4, lty=2)
x <- cbind(pca$x[,1], pca$x[,2])
km.2 <- kmeans(x, 4, nstart=50)
plot(pca$x[,1:2], col=km.2$cluster)
points(km.2$centers, pch=16)



########################### 
#### Tree Based Models
###########################
## Classification Tree
time.tree <- system.time({
  cv.tree.parameter <- foreach(q=1:4, .packages = "rpart", .combine = rbind) %dopar% {
    err.tree <- NULL
    folds <- 12
    fold.indices <- cut(1:nrow(bank), breaks = folds, labels = FALSE)
    for(i in 1:10){
      for(j in 1:10){
        cv.temp = c()
        for(k in 1:10){
          fold.train <- sample(1:12,8)
          train.indices = c()
          for (z in 1:length(fold.train)) {
            temp <- which(fold.indices == fold.train[z])
            train.indices = append(train.indices, temp)
          }
          test.data <- bank[-train.indices,]
          train.data <- bank[train.indices,]
          original.tree <- rpart(y~., data = train.data, method = "class", parms = list(split="information"),
                                 control = rpart.control(cp = 0, minsplit = i, minbucket = j))
          min.xerror <- which.min(original.tree$cptable[,"xerror"])
          min.cp <- original.tree$cptable[min.xerror,"CP"]
          best.tree <- prune(original.tree, cp = min.cp)
          preds <- predict(best.tree, newdata = test.data, type = "class")
          table.classfication <- table(test.data$y, preds)
          cv.temp[k] <- (table.classfication[1,2] + table.classfication[2,1])/sum(table.classfication)
        }
        err.tree = rbind(err.tree, c(q,i,j,mean(cv.temp)))
      }
    }
    err.tree
  }
})
cv.tree.parameter <- cv.tree.parameter[order(cv.tree.parameter[,2],cv.tree.parameter[,3]),c(1,2,3,4)]
cv.tree.parameter.final <- aggregate(cv.tree.parameter, by=list(cv.tree.parameter[,2],cv.tree.parameter[,3]), FUN=mean)
cv.tree.parameter.final <- cv.tree.parameter.final[order(cv.tree.parameter.final[,6]),c(1:6)]
bestsplit = cv.tree.parameter.final[1,1]
bestbucket = cv.tree.parameter.final[1,2]
cv.tree.result = c()
folds <- 12
fold.indices <- cut(1:nrow(bank), breaks = folds, labels = FALSE)
for(i in 1:10){
  fold.train <- sample(1:12,8)
  train.indices = c()
  for (z in 1:length(fold.train)) {
    temp <- which(fold.indices == fold.train[z])
    train.indices = append(train.indices, temp)
  }
  test.data <- bank[-train.indices,]
  train.data <- bank[train.indices,]
  original.tree <- rpart(y~., data = train.data, method = "class", parms = list(split="information"),
                         control = rpart.control(cp = 0, minsplit = bestsplit, minbucket = bestbucket))
  min.xerror <- which.min(original.tree$cptable[,"xerror"])
  min.cp <- original.tree$cptable[min.xerror,"CP"]
  best.tree <- prune(original.tree, cp = min.cp)
  preds <- predict(best.tree, newdata = test.data, type = "class")
  table.classfication <- table(test.data$y, preds)
  cv.tree.result[i] <- (table.classfication[1,2] + table.classfication[2,1])/sum(table.classfication)
}
cv.tree.result.final <- c(time.tree[3], mean(cv.tree.result))

## Random Forest
numPred = seq(1,round(sqrt(length(bank)-1))+2,1)
time.rf <- system.time({
  cv.rf.parameter <- foreach(q=1:3, .packages = "randomForest", .combine = rbind) %dopar% {
    err.rf = NULL
    folds <- 12
    fold.indices <- cut(1:nrow(bank), breaks = folds, labels = FALSE)
    for (i in 1:length(numPred)){
      cv.temp = c()
      for(k in 1:5){
        fold.train <- sample(1:12,8)
        train.indices = c()
        for (z in 1:length(fold.train)) {
          temp <- which(fold.indices == fold.train[z])
          train.indices = append(train.indices, temp)
        }
        test.data <- bank[-train.indices,]
        train.data <- bank[train.indices,]
        original.rf <- randomForest(train.data$y~., data = train.data, mtry = numPred[i], localImp = TRUE,
                                    na.action = na.roughfix, replace=TRUE, ntree = 1000)
        min.err.idx <- which.min(original.rf$err.rate[,"OOB"])
        best.rf <- randomForest(train.data$y~., data = train.data, mtry = numPred[i], localImp = TRUE,
                                na.action = na.roughfix, replace=TRUE, ntree = min.err.idx)
        preds <- predict(best.rf, newdata = test.data, type = "class")
        table.rf <- table(test.data$y, preds)
        cv.temp[k] <- (table.rf[1,2] + table.rf[2,1])/sum(table.rf)
      }
      err.rf = rbind(err.rf, c(q,i,mean(cv.temp)))
    }
    err.rf
  }
})
cv.rf.parameter <- cv.rf.parameter[order(cv.rf.parameter[,2]),c(1,2,3)]
cv.rf.parameter.final <- aggregate(cv.rf.parameter, by=list(cv.rf.parameter[,2]), FUN=mean)
cv.rf.parameter.final <- cv.rf.parameter.final[order(cv.rf.parameter.final[,4]),c(1:4)]
bestmtry = numPred[cv.rf.parameter.final[1,1]]
cv.rf.result = c()
folds <- 12
fold.indices <- cut(1:nrow(bank), breaks = folds, labels = FALSE)
for(i in 1:5){
  fold.train <- sample(1:12,8)
  train.indices = c()
  for (j in 1:length(fold.train)) {
    temp <- which(fold.indices == fold.train[j])
    train.indices = append(train.indices, temp)
  }
  test.data <- bank[-train.indices,]
  train.data <- bank[train.indices,]
  original.rf <- randomForest(train.data$y~., data = train.data, mtry = bestmtry, localImp = TRUE,
                          na.action = na.roughfix, replace=TRUE, ntree = 1000)
  min.err.idx <- which.min(original.rf$err.rate[,"OOB"])
  best.rf <- randomForest(train.data$y~., data = train.data, mtry = bestmtry, localImp = TRUE,
                          na.action = na.roughfix, replace=TRUE, ntree = min.err.idx)
  preds <- predict(best.rf, newdata = test.data, type = "class")
  table.rf <- table(test.data$y, preds)
  cv.rf.result[i] <- (table.rf[1,2] + table.rf[2,1])/sum(table.rf)
}
cv.rf.result.final <- c(time.rf[3], mean(cv.rf.result))



###########################
#### Kerner Based Models
###########################
class(bank[,length(bank)])
## SVC
cost = seq(.01,10,length.out = 100)
time.svmLinear <- system.time({
  cv.svmLinear.parameter <- foreach(q=1:3, .packages = "e1071", .combine = rbind) %dopar% {
    err.svmLinear = NULL
    folds <- 12
    fold.indices <- cut(1:nrow(bank), breaks = folds, labels = FALSE)
    for(i in 1:length(cost)){
      cv.temp = c()
      for(k in 1:10){
        fold.train <- sample(1:12,8)
        train.indices = c()
        for (z in 1:length(fold.train)){
          temp <- which(fold.indices == fold.train[z])
          train.indices = append(train.indices, temp)
        }
        test.data <- bank[-train.indices,]
        train.data <- bank[train.indices,]
        svm.linear <- svm(y~., data=train.data, kernel = 'linear', cost = cost[i], scale = T)
        preds <- predict(svm.linear, newdata = test.data, type = "class")
        table.svmLinear <- table(test.data$y, preds)
        cv.temp[k] <- (table.svmLinear[1,2] + table.svmLinear[2,1])/sum(table.svmLinear)
      }
      err.svmLinear = rbind(err.svmLinear, c(q,i, mean(cv.temp)))
    }
    err.svmLinear
  }
})
cv.svmLinear.parameter <- cv.svmLinear.parameter[order(cv.svmLinear.parameter[,2]),c(1,2,3)]
cv.svmLinear.parameter.final <- aggregate(cv.svmLinear.parameter, by=list(cv.svmLinear.parameter[,2]), FUN=mean)
cv.svmLinear.parameter.final <- cv.svmLinear.parameter.final[order(cv.svmLinear.parameter.final[,4]),c(1:4)]
bestcostLinear = cost[cv.svmLinear.parameter.final[1,1]]
cv.svmLinear.result <- c()
folds <- 12
fold.indices <- cut(1:nrow(bank), breaks = folds, labels = FALSE)
for(i in 1:10){
  fold.train <- sample(1:12,8)
  train.indices = c()
  for (j in 1:length(fold.train)) {
    temp <- which(fold.indices == fold.train[j])
    train.indices = append(train.indices, temp)
  }
  test.data <- bank[-train.indices,]
  train.data <- bank[train.indices,]
  svm.linear <- svm(y~., data=train.data, kernel = 'linear', cost = bestcostLinear, scale = T)
  preds <- predict(svm.linear, newdata = test.data, type = "class")
  table.svmLinear <- table(test.data$y, preds)
  cv.svmLinear.result[i] <- (table.svmLinear[1,2] + table.svmLinear[2,1])/sum(table.svmLinear)
}
cv.svmLinear.result.final <- c(time.svmLinear[3], mean(cv.svmLinear.result))

## SVM-Poly
degree = seq(2,5,1)
gamma = seq(0.001,1,by = .05)
cost = seq(0.01,10,by = .5)
time.svmPoly <- system.time({
  cv.svmPoly.parameter <- foreach(q=1:2, .packages = "e1071", .combine = rbind) %dopar% {
    err.svmPoly = NULL
    folds <- 12
    fold.indices <- cut(1:nrow(bank), breaks = folds, labels = FALSE)
    for(a in 1:length(degree)){
      for(b in 1:length(gamma)){
        for(c in 1:length(cost)){
          cv.temp = c()
          for(k in 1:4){
            fold.train <- sample(1:12,6)
            train.indices = c()
            for (z in 1:length(fold.train)){
              temp <- which(fold.indices == fold.train[z])
              train.indices = append(train.indices, temp)
            }
            test.data <- bank[-train.indices,]
            train.data <- bank[train.indices,]
            svm.poly <- svm(y~., data=train.data, kernel = 'polynomial', cost = cost[c], 
                              degree = degree[a], gamma = gamma[b], scale = T)
            preds <- predict(svm.poly, newdata = test.data, type = "class")
            table.svmPoly <- table(test.data$y, preds)
            cv.temp[k] <- (table.svmPoly [1,2] + table.svmPoly [2,1])/sum(table.svmPoly )
          }
          err.svmPoly = rbind(err.svmPoly, c(q,a,b,c,mean(cv.temp)))
        }
      }
    }
    err.svmPoly
  }
})
cv.svmPoly.parameter <- cv.svmPoly.parameter[order(cv.svmPoly.parameter[,2],cv.svmPoly.parameter[,3],cv.svmPoly.parameter[,4]),c(1,2,3,4,5)]
cv.svmPoly.parameter.final <- aggregate(cv.svmPoly.parameter, by=list(cv.svmPoly.parameter[,2],cv.svmPoly.parameter[,3],cv.svmPoly.parameter[,4]), FUN=mean)
cv.svmPoly.parameter.final <- cv.svmPoly.parameter.final[order(cv.svmPoly.parameter.final[,8]),c(1:8)]
bestdegree = degree[cv.svmPoly.parameter.final[1,1]]
bestgammaPoly = gamma[cv.svmPoly.parameter.final[1,2]]
bestcostPoly = cost[cv.svmPoly.parameter.final[1,3]]
cv.svmPoly.result <- c()
folds <- 12
fold.indices <- cut(1:nrow(bank), breaks = folds, labels = FALSE)
for(i in 1:10){
  fold.train <- sample(1:12,8)
  train.indices = c()
  for (j in 1:length(fold.train)) {
    temp <- which(fold.indices == fold.train[j])
    train.indices = append(train.indices, temp)
  }
  test.data <- bank[-train.indices,]
  train.data <- bank[train.indices,]
  svm.poly <- svm(y~., data=train.data, kernel = 'polynomial', cost = bestcostPoly,
                    degree = bestdegree, gamma = bestgammaPoly, scale = T)
  preds <- predict(svm.poly, newdata = test.data, type = "class")
  table.svmPoly <- table(test.data$y, preds)
  cv.svmPoly.result[i] <- (table.svmPoly[1,2] + table.svmPoly[2,1])/sum(table.svmPoly)
}
cv.svmPoly.result.final <- c(time.svmPoly[3], mean(cv.svmPoly.result))

## SVM-Radial
gamma = seq(0.001,1,by = .05)
cost = seq(0.01,10,length.out = 40)
time.svmRadial <- system.time({
  cv.svmRadial.parameter <- foreach(q=1:3, .packages = "e1071", .combine = rbind)%dopar%{
    err.svmRadial = NULL
    folds <- 12
    fold.indices <- cut(1:nrow(bank), breaks = folds, labels = FALSE)
    for(a in 1:length(gamma)){
      for(b in 1:length(cost)){
        cv.temp = c()
        for(k in 1:3){
          fold.train <- sample(1:12,4)
          train.indices = c()
          for (z in 1:length(fold.train)){
            temp <- which(fold.indices == fold.train[z])
            train.indices = append(train.indices, temp)
          }
          test.data <- bank[-train.indices,]
          train.data <- bank[train.indices,]
          svm.radial <- svm(y~., data=train.data, kernel = 'radial', cost = cost[b],
                            gamma = gamma[a], scale = T)
          preds <- predict(svm.radial, newdata = test.data, type = "class")
          table.svmRadial <- table(test.data$y, preds)
          cv.temp[k] <- (table.svmRadial[1,2] + table.svmRadial[2,1])/sum(table.svmRadial)
        }
        err.svmRadial = rbind(err.svmRadial, c(q,a,b,mean(cv.temp)))
      }
    }
  err.svmRadial
  }
})
cv.svmRadial.parameter <- cv.svmRadial.parameter[order(cv.svmRadial.parameter[,2],cv.svmRadial.parameter[,3]),c(1,2,3,4)]
cv.svmRadial.parameter.final <- aggregate(cv.svmRadial.parameter, by=list(cv.svmRadial.parameter[,2],cv.svmRadial.parameter[,3]), FUN=mean)
cv.svmRadial.parameter.final <- cv.svmRadial.parameter.final[order(cv.svmRadial.parameter.final[,6]),c(1:6)]
bestgammaRadial = gamma[cv.svmRadial.parameter.final[1,1]]
bestcostRadial = cost[cv.svmRadial.parameter.final[1,2]]
cv.svmRadial.result <- c()
folds <- 12
fold.indices <- cut(1:nrow(bank), breaks = folds, labels = FALSE)
for(i in 1:5){
  fold.train <- sample(1:12,6)
  train.indices = c()
  for (j in 1:length(fold.train)) {
    temp <- which(fold.indices == fold.train[j])
    train.indices = append(train.indices, temp)
  }
  test.data <- bank[-train.indices,]
  train.data <- bank[train.indices,]
  svm.radial <- svm(y~., data=train.data, kernel = 'radial', cost = bestcostRadial,
                    gamma = bestgammaRadial, scale = T)
  preds <- predict(svm.radial, newdata = test.data, type = "class")
  table.svmRadial <- table(test.data$y, preds)
  cv.svmRadial.result[i] <- (table.svmRadial[1,2] + table.svmRadial[2,1])/sum(table.svmRadial)
}
cv.svmRadial.result.final <- c(time.svmRadial[3], mean(cv.svmRadial.result))               



#######################
#### Model Comparison
#######################
time.min <- min(cv.tree.result.final[1],cv.svmRadial.result.final[1],cv.rf.result.final[1],cv.svmLinear.result.final[1],
                cv.svmPoly.result.final[1])
time.max <- max(cv.tree.result.final[1],cv.svmRadial.result.final[1],cv.rf.result.final[1],cv.svmLinear.result.final[1],
                cv.svmPoly.result.final[1])
err.min <- min(cv.tree.result.final[2],cv.svmRadial.result.final[2],cv.rf.result.final[2],cv.svmLinear.result.final[2],
               cv.svmPoly.result.final[2])
err.max <- max(cv.tree.result.final[2],cv.svmRadial.result.final[2],cv.rf.result.final[2],cv.svmLinear.result.final[2],
               cv.svmPoly.result.final[2])
plot(x=NULL, y=NULL, xlim = range(time.min,time.max), ylim = range(err.min-0.001,err.max+0.001), ylab = "Error Rate", xlab = "Time Consumed",
     main = "Model Comparison")
points(cv.tree.result.final[1],cv.tree.result.final[2], pch=19, col='black', cex=1.8)
points(cv.rf.result.final[1],cv.rf.result.final[2], pch=19, col='blue', cex=1.8)
points(cv.svmLinear.result.final[1],cv.svmLinear.result.final[2], pch=19, col='red', cex=1.8)
points(cv.svmPoly.result.final[1],cv.svmPoly.result.final[2], pch=19, col='green', cex=1.8)
points(cv.svmRadial.result.final[1],cv.svmRadial.result.final[2], pch=19, col='brown', cex=1.8)
legend(1300,0.0945, legend = c("Decision Tree","Random Forest","SVC","SVM-Poly","SVM-Radial"),
       col = c("black","blue","red","green","brown"), pch = 19, cex = 0.9, box.lty = 0)



#####################
#### Learning Curve
#####################
## Learning curve for tree
data <- seq(6000,34000,2000)
yes.indices = which(bank[,21]=='yes')
no.indices = which(bank[,21]=='no')
bank.yes = bank[yes.indices,]
bank.no = bank[no.indices,]
ratio = summary(bank$y)[2]/summary(bank$y)[1]
time.tree.final <- system.time({
  cv.tree.result.final.bootstrap <- foreach(q=1:3, .packages = "rpart", .combine = rbind)%dopar%{
    err.tree.bootstrap <- NULL
    for(a in 1:length(data)){
      bank.sampled.no = bank.no[sample(1:nrow(bank.no),round(data[1]*(1-ratio)),replace = T),]
      bank.sampled.yes = bank.yes[sample(1:nrow(bank.yes),round(data[1]*ratio),replace = T),]
      bank.bootstrap = rbind(bank.sampled.no,bank.sampled.yes) 
      bank.bootstrap = bank.bootstrap[sample(1:nrow(bank.bootstrap)),]
      folds <- 10
      fold.indices <- cut(1:nrow(bank.bootstrap), breaks = folds, labels = FALSE)
      cv.temp = c()
      for(i in 1:folds){
        test.indices <- which(fold.indices == i)
        test.data <- bank.bootstrap[test.indices,]
        train.data <- bank.bootstrap[-test.indices,]
        original.tree <- rpart(y~., data = train.data, method = "class", parms = list(split="information"),
                               control = rpart.control(cp = 0, minsplit = bestsplit, minbucket = bestbucket))
        min.xerror <- which.min(original.tree$cptable[,"xerror"])
        min.cp <- original.tree$cptable[min.xerror,"CP"]
        best.tree <- prune(original.tree, cp = min.cp)
        preds <- predict(best.tree, newdata = test.data, type = "class")
        table.classfication <- table(factor(test.data$y,levels = c("no","yes")), factor(preds,levels = c("no","yes")))
        cv.temp[i] <- (table.classfication[1,2] + table.classfication[2,1])/sum(table.classfication)
      }
      err.tree.bootstrap = rbind(err.tree.bootstrap,c(q,a,mean(cv.temp)))
    }
    err.tree.bootstrap
  }
})
cv.tree.result.final.bootstrap <- cv.tree.result.final.bootstrap[order(cv.tree.result.final.bootstrap[,2]),c(1,2,3)]
cv.tree.result.final.bootstrap.final <- aggregate(cv.tree.result.final.bootstrap, by=list(cv.tree.result.final.bootstrap[,2]), FUN=mean)
plot(cv.tree.result.final.bootstrap.final[,4],type='l',main = "Learning Curve for Classification Tree")
    
## Learning curve for random forest
time.rf.final <- system.time({
  cv.rf.result.final.bootstrap <- foreach(q=1:3, .packages = "randomForest", .combine = rbind)%dopar%{
    err.rf.bootstrap <- NULL
    for(a in 1:length(data)){
      bank.sampled.no = bank.no[sample(1:nrow(bank.no),round(data[a]*(1-ratio)),replace = T),]
      bank.sampled.yes = bank.yes[sample(1:nrow(bank.yes),round(data[a]*ratio),replace = T),]
      bank.bootstrap = rbind(bank.sampled.no,bank.sampled.yes)
      bank.bootstrap = bank.bootstrap[sample(1:nrow(bank.bootstrap)),]
      cv.temp = c()
      folds <- 12
      fold.indices <- cut(1:nrow(bank.bootstrap), breaks = folds, labels = FALSE)
      for(i in 1:5){
        fold.train <- sample(1:12,8)
        train.indices = c()
        for (j in 1:length(fold.train)) {
          temp <- which(fold.indices == fold.train[j])
          train.indices = append(train.indices, temp)
        }
        test.data <- bank.bootstrap[-train.indices,]
        train.data <- bank.bootstrap[train.indices,]
        original.rf <- randomForest(train.data$y~., data = train.data, mtry = bestmtry, localImp = TRUE,
                                    na.action = na.roughfix, replace=TRUE, ntree = 1000)
        min.err.idx <- which.min(original.rf$err.rate[,"OOB"])
        best.rf <- randomForest(train.data$y~., data = train.data, mtry = bestmtry, localImp = TRUE,
                                na.action = na.roughfix, replace=TRUE, ntree = min.err.idx)
        preds <- predict(best.rf, newdata = test.data, type = "class")
        table.rf <- table(test.data$y, preds)
        cv.temp[i] <- (table.rf[1,2] + table.rf[2,1])/sum(table.rf)
      }
      err.rf.bootstrap = rbind(err.rf.bootstrap, c(q,a,mean(cv.temp)))
    }
    err.rf.bootstrap
  }
})
cv.rf.result.final.bootstrap <- cv.rf.result.final.bootstrap[order(cv.rf.result.final.bootstrap[,2]),c(1,2,3)]
cv.rf.result.final.bootstrap.final <- aggregate(cv.rf.result.final.bootstrap, by=list(cv.rf.result.final.bootstrap[,2]), FUN=mean)
plot(cv.rf.result.final.bootstrap.final[,4],type='l',main = "Learning Curve for RandomForest")

## Learning curve for SVC
time.svmLinear.final <- system.time({
  cv.svmLinear.result.final.bootstrap <- foreach(q=1:3, .packages = "e1071", .combine = rbind)%dopar%{
    err.svmLinear.bootstrap <- NULL
    for(a in 1:length(data)){
      bank.sampled.no = bank.no[sample(1:nrow(bank.no),round(data[a]*(1-ratio)),replace = T),]
      bank.sampled.yes = bank.yes[sample(1:nrow(bank.yes),round(data[a]*ratio),replace = T),]
      bank.bootstrap = rbind(bank.sampled.no,bank.sampled.yes) 
      bank.bootstrap = bank.bootstrap[sample(1:nrow(bank.bootstrap)),]
      folds <- 10
      fold.indices <- cut(1:nrow(bank.bootstrap), breaks = folds, labels = FALSE)
      cv.temp = c()
      for(i in 1:folds){
        test.indices <- which(fold.indices == i)
        test.data <- bank.bootstrap[test.indices,]
        train.data <- bank.bootstrap[-test.indices,]
        svm.linear <- svm(y~., data=train.data, kernel = 'linear', cost = bestcostLinear, scale = T)
        preds <- predict(svm.linear, newdata = test.data, type = "class")
        table.svmLinear <- table(test.data$y, preds)
        cv.temp[i] <- (table.svmLinear[1,2] + table.svmLinear[2,1])/sum(table.svmLinear)
      }
      err.svmLinear.bootstrap = rbind(err.svmLinear.bootstrap,c(q,a,mean(cv.temp)))
    }
    err.svmLinear.bootstrap
  }
})
cv.svmLinear.result.final.bootstrap <- cv.svmLinear.result.final.bootstrap[order(cv.svmLinear.result.final.bootstrap[,2]),c(1,2,3)]
cv.svmLinear.result.final.bootstrap.final <- aggregate(cv.svmLinear.result.final.bootstrap, by=list(cv.svmLinear.result.final.bootstrap[,2]), FUN=mean)
plot(cv.svmLinear.result.final.bootstrap.final[,4],type='l',main = "Learning Curve for SVC")

## Learning curve for SVM-Poly
time.svmPoly.final <- system.time({
  cv.svmPoly.result.final.bootstrap <- foreach(q=1:2, .packages = "e1071", .combine = rbind)%dopar%{
    err.svmPoly.bootstrap <- NULL
    for(a in 1:length(data)){
      bank.sampled.no = bank.no[sample(1:nrow(bank.no),round(data[a]*(1-ratio)),replace = T),]
      bank.sampled.yes = bank.yes[sample(1:nrow(bank.yes),round(data[a]*ratio),replace = T),]
      bank.bootstrap = rbind(bank.sampled.no,bank.sampled.yes) 
      bank.bootstrap = bank.bootstrap[sample(1:nrow(bank.bootstrap)),]
      folds <- 10
      fold.indices <- cut(1:nrow(bank.bootstrap), breaks = folds, labels = FALSE)
      cv.temp = c()
      for(i in 1:folds){
        test.indices <- which(fold.indices == i)
        test.data <- bank.bootstrap[test.indices,]
        train.data <- bank.bootstrap[-test.indices,]
        svm.poly <- svm(y~., data=train.data, kernel = 'polynomial', cost = bestcostPoly,
                        degree = bestdegree, gamma = bestgammaPoly, scale = T)
        preds <- predict(svm.poly, newdata = test.data, type = "class")
        table.svmPoly <- table(test.data$y, preds)
        cv.temp[i] <- (table.svmPoly[1,2] + table.svmPoly[2,1])/sum(table.svmPoly)
      }
      err.svmPoly.bootstrap = rbind(err.svmPoly.bootstrap,c(q,a,mean(cv.temp)))
    }
    err.svmPoly.bootstrap
  }
})
cv.svmPoly.result.final.bootstrap <- cv.svmPoly.result.final.bootstrap[order(cv.svmPoly.result.final.bootstrap[,2]),c(1,2,3)]
cv.svmPoly.result.final.bootstrap.final <- aggregate(cv.svmPoly.result.final.bootstrap, by=list(cv.svmPoly.result.final.bootstrap[,2]), FUN=mean)
plot(cv.svmPoly.result.final.bootstrap.final[,4],type='l',main = "Learning Curve for SVM-Poly")

## Learning Curve for SVM-Radial
time.svmRadial.final <- system.time({
  cv.svmRadial.result.final.bootstrap <- foreach(q=1:2, .packages = "e1071", .combine = rbind)%dopar%{
    err.svmRadial.bootstrap <- NULL
    for(a in 1:length(data)){
      bank.sampled.no = bank.no[sample(1:nrow(bank.no),round(data[a]*(1-ratio)),replace = T),]
      bank.sampled.yes = bank.yes[sample(1:nrow(bank.yes),round(data[a]*ratio),replace = T),]
      bank.bootstrap = rbind(bank.sampled.no,bank.sampled.yes) 
      bank.bootstrap = bank.bootstrap[sample(1:nrow(bank.bootstrap)),]
      folds <- 10
      fold.indices <- cut(1:nrow(bank.bootstrap), breaks = folds, labels = FALSE)
      cv.temp = c()
      for(i in 1:folds){
        test.indices <- which(fold.indices == i)
        test.data <- bank.bootstrap[test.indices,]
        train.data <- bank.bootstrap[-test.indices,]
        svm.radial <- svm(y~., data=train.data, kernel = 'radial', cost = bestcostRadial,
                          gamma = bestgammaRadial, scale = T)
        preds <- predict(svm.radial, newdata = test.data, type = "class")
        table.svmRadial <- table(test.data$y, preds)
        cv.temp[i] <- (table.svmRadial[1,2] + table.svmRadial[2,1])/sum(table.svmRadial)
      }
      err.svmRadial.bootstrap = rbind(err.svmRadial.bootstrap,c(q,a,mean(cv.temp)))
    }
    err.svmRadial.bootstrap
  }
})
cv.svmRadial.result.final.bootstrap <- cv.svmRadial.result.final.bootstrap[order(cv.svmRadial.result.final.bootstrap[,2]),c(1,2,3)]
cv.svmRadial.result.final.bootstrap.final <- aggregate(cv.svmRadial.result.final.bootstrap, by=list(cv.svmRadial.result.final.bootstrap[,2]), FUN=mean)
plot(cv.svmRadial.result.final.bootstrap.final[,4],type='l',main = "Learning Curve for SVM-Radial")



################################
#### Learning Curve Comparison
################################
err.min.final = min(cv.tree.result.final.bootstrap.final[,4],cv.rf.result.final.bootstrap.final[,4],cv.svmLinear.result.final.bootstrap.final[,4],
                    cv.svmPoly.result.final.bootstrap.final[,4],cv.svmRadial.result.final.bootstrap.final[,4])
err.max.final = max(cv.tree.result.final.bootstrap.final[,4],cv.rf.result.final.bootstrap.final[,4],cv.svmLinear.result.final.bootstrap.final[,4],
                    cv.svmPoly.result.final.bootstrap.final[,4],cv.svmRadial.result.final.bootstrap.final[,4])
plot(x=NULL, y=NULL, xlim = range(6000,34000), ylim = range(err.min.final,err.max.final+0.02), main = "Learning Curve Comparison",
     xlab = "Bootstrapped Data Size", ylab = "Error Rate")
lines(x=seq(6000,34000,2000), y=cv.tree.result.final.bootstrap.final[,4], type = 'l', lty = 1, lwd = 2.5, col = 'black')
lines(x=seq(6000,34000,2000), y=cv.rf.result.final.bootstrap.final[,4], type = 'l', lty = 2, lwd = 2.5, col = 'blue')
lines(x=seq(6000,34000,2000), y=cv.svmLinear.result.final.bootstrap.final[,4], type = 'l', lty = 3, lwd = 2.5, col = 'green')
lines(x=seq(6000,34000,2000), y=cv.svmPoly.result.final.bootstrap.final[,4], type = 'l', lty = 4, lwd = 2.5, col = 'brown')
lines(x=seq(6000,34000,2000), y=cv.svmRadial.result.final.bootstrap.final[,4], type = 'l', lty = 7, lwd = 2.5, col = 'purple')
legend(25500,0.115,legend = c("Decision Tree","Random Forest","SVC","SVM-Poly","SVM-Radial"),
       col = c("black","blue","green","brown","purple"), lty = c(1,2,3,4,7), cex = 0.8, box.lty = 0)



###################################
#### Learning Curve Vs. Real Data
###################################
bank.real <- read.csv("bank-additional-full.csv", sep=",", header = T)
sum(is.na(bank))
ratio.real = summary(bank.real$y)[2]/summary(bank.real$y)[1]
yes.indices.real = which(bank.real[,21]=='yes')
no.indices.real = which(bank.real[,21]=='no')
bank.yes.real = bank.real[yes.indices.real,]
bank.no.real = bank.real[no.indices.real,]
bank.sampled.no.real = bank.no.real[sample(1:nrow(bank.no.real),round(30000*(1-ratio.real))),]
bank.sampled.yes.real = bank.yes.real[sample(1:nrow(bank.yes.real),round(30000*ratio.real)),]
bank.sampled.combined.real = rbind(bank.sampled.no.real, bank.sampled.yes.real)
bank.sampled.combined.real = bank.sampled.combined.real[sample(1:nrow(bank.sampled.combined.real)),]

## Classification Tree Learning Curve Vs. Real Data
cv.tree.result.final.real <- foreach(q=1:3, .packages = "rpart", .combine = rbind)%dopar%{
  err.tree.real <- NULL
  cv.temp = c()
  folds <- 12
  fold.indices <- cut(1:nrow(bank.sampled.combined.real), breaks = folds, labels = FALSE)
  for(i in 1:10){
    fold.train <- sample(1:12,8)
    train.indices = c()
    for (j in 1:length(fold.train)) {
      temp <- which(fold.indices == fold.train[j])
      train.indices = append(train.indices, temp)
    }
    test.data <- bank.sampled.combined.real[-train.indices,]
    train.data <- bank.sampled.combined.real[train.indices,]
    original.tree <- rpart(y~., data = train.data, method = "class", parms = list(split="information"),
                           control = rpart.control(cp = 0, minsplit = bestsplit, minbucket = bestbucket))
    min.xerror <- which.min(original.tree$cptable[,"xerror"])
    min.cp <- original.tree$cptable[min.xerror,"CP"]
    best.tree <- prune(original.tree, cp = min.cp)
    preds <- predict(best.tree, newdata = test.data, type = "class")
    table.classfication <- table(test.data$y, preds)
    cv.temp[i] <- (table.classfication[1,2] + table.classfication[2,1])/sum(table.classfication)
  }
  err.tree.real = rbind(err.tree.real, c(q,mean(cv.temp)))
  err.tree.real
}

## Random Forest Learning Curve Vs. Real Data
cv.rf.result.final.real <- foreach(q=1:3, .packages = "randomForest", .combine = rbind)%dopar%{
  err.rf.real <- NULL
  cv.temp = c()
  folds <- 12
  fold.indices <- cut(1:nrow(bank.sampled.combined.real), breaks = folds, labels = FALSE)
  for(i in 1:5){
    fold.train <- sample(1:12,8)
    train.indices = c()
    for (j in 1:length(fold.train)) {
      temp <- which(fold.indices == fold.train[j])
      train.indices = append(train.indices, temp)
    }
    test.data <- bank.sampled.combined.real[-train.indices,]
    train.data <- bank.sampled.combined.real[train.indices,]
    original.rf <- randomForest(train.data$y~., data = train.data, mtry = bestmtry, localImp = TRUE,
                                na.action = na.roughfix, replace=TRUE, ntree = 1000)
    min.err.idx <- which.min(original.rf$err.rate[,"OOB"])
    best.rf <- randomForest(train.data$y~., data = train.data, mtry = bestmtry, localImp = TRUE,
                            na.action = na.roughfix, replace=TRUE, ntree = min.err.idx)
    preds <- predict(best.rf, newdata = test.data, type = "class")
    table.rf <- table(test.data$y, preds)
    cv.temp[i] <- (table.rf[1,2] + table.rf[2,1])/sum(table.rf)
  }
  err.rf.real = rbind(err.rf.real, c(q,mean(cv.temp)))
  err.rf.real
}

## SVC Learning Curve Vs. Real Data
cv.svmLinear.result.final.real <- foreach(q=1:3, .packages = "e1071", .combine = rbind)%dopar%{
  err.svmLinear.real <- NULL
  cv.temp = c()
  folds <- 12
  fold.indices <- cut(1:nrow(bank.sampled.combined.real), breaks = folds, labels = FALSE)
  for(i in 1:10){
    fold.train <- sample(1:12,8)
    train.indices = c()
    for (j in 1:length(fold.train)) {
      temp <- which(fold.indices == fold.train[j])
      train.indices = append(train.indices, temp)
    }
    test.data <- bank.sampled.combined.real[-train.indices,]
    train.data <- bank.sampled.combined.real[train.indices,]
    svm.linear <- svm(y~., data=train.data, kernel = 'linear', cost = bestcostLinear, scale = T)
    preds <- predict(svm.linear, newdata = test.data, type = "class")
    table.svmLinear <- table(test.data$y, preds)
    cv.temp[i] <- (table.svmLinear[1,2] + table.svmLinear[2,1])/sum(table.svmLinear)
  }
  err.svmLinear.real = rbind(err.svmLinear.real, c(q,mean(cv.temp)))
  err.svmLinear.real
}

## SVM-Poly Learning Curve Vs. Real Data
cv.svmPoly.result.final.real <- foreach(q=1:3, .packages = "e1071", .combine = rbind)%dopar%{
  err.svmPoly.real <- NULL
  cv.temp = c()
  folds <- 12
  fold.indices <- cut(1:nrow(bank.sampled.combined.real), breaks = folds, labels = FALSE)
  for(i in 1:5){
    fold.train <- sample(1:12,8)
    train.indices = c()
    for (j in 1:length(fold.train)) {
      temp <- which(fold.indices == fold.train[j])
      train.indices = append(train.indices, temp)
    }
    test.data <- bank.sampled.combined.real[-train.indices,]
    train.data <- bank.sampled.combined.real[train.indices,]
    svm.poly <- svm(y~., data=train.data, kernel = 'polynomial', cost = bestcostPoly,
                    degree = bestdegree, gamma = bestgammaPoly, scale = T)
    preds <- predict(svm.poly, newdata = test.data, type = "class")
    table.svmPoly <- table(test.data$y, preds)
    cv.temp[i] <- (table.svmPoly[1,2] + table.svmPoly[2,1])/sum(table.svmPoly)
  }
  err.svmPoly.real = rbind(err.svmPoly.real, c(q,mean(cv.temp)))
  err.svmPoly.real
}

## SVM-Radial Learning Curve Vs. Real Data
cv.svmRadial.result.final.real <- foreach(q=1:3, .packages = "e1071", .combine = rbind)%dopar%{
  err.svmRadial.real <- NULL
  cv.temp = c()
  folds <- 12
  fold.indices <- cut(1:nrow(bank.sampled.combined.real), breaks = folds, labels = FALSE)
  for(i in 1:5){
    fold.train <- sample(1:12,8)
    train.indices = c()
    for (j in 1:length(fold.train)) {
      temp <- which(fold.indices == fold.train[j])
      train.indices = append(train.indices, temp)
    }
    test.data <- bank.sampled.combined.real[-train.indices,]
    train.data <- bank.sampled.combined.real[train.indices,]
    svm.radial <- svm(y~., data=train.data, kernel = 'radial', cost = bestcostRadial,
                      gamma = bestgammaRadial, scale = T)
    preds <- predict(svm.radial, newdata = test.data, type = "class")
    table.svmRadial <- table(test.data$y, preds)
    cv.temp[i] <- (table.svmRadial[1,2] + table.svmRadial[2,1])/sum(table.svmRadial)
  }
  err.svmRadial.real = rbind(err.svmRadial.real, c(q,mean(cv.temp)))
  err.svmRadial.real
}



######################
#### Model Selection
######################
## Model Selection Approach 1
plot(x=NULL, y=NULL, xlim = range(time.min,time.max), ylim = range(err.min-0.001,err.max+0.008), ylab = "Error Rate", xlab = "Time Consumed",
     main = "Model Selection Approach 1")
points(cv.tree.result.final[1],cv.tree.result.final[2], pch=19, col='black', cex=1.8)
points(cv.rf.result.final[1],cv.rf.result.final[2], pch=19, col='blue', cex=1.8)
points(cv.svmLinear.result.final[1],cv.svmLinear.result.final[2], pch=19, col='red', cex=1.8)
points(cv.svmPoly.result.final[1],cv.svmPoly.result.final[2], pch=19, col='green', cex=1.8)
points(cv.svmRadial.result.final[1],cv.svmRadial.result.final[2], pch=19, col='brown', cex=1.8)
points(cv.tree.result.final[1],mean(cv.tree.result.final.real[,2]), pch=18, col='black', cex=2.1)
points(cv.rf.result.final[1],mean(cv.rf.result.final.real[,2]), pch=18, col='blue', cex=2.1)
points(cv.svmLinear.result.final[1],mean(cv.svmLinear.result.final.real[,2]), pch=18, col='red', cex=2.1)
points(cv.svmPoly.result.final[1],mean(cv.svmPoly.result.final.real[,2]), pch=18, col='green', cex=2.1)
points(cv.svmRadial.result.final[1],mean(cv.svmRadial.result.final.real[,2]), pch=18, col='brown', cex=2.1)
legend(1300,0.1, legend = c("Decision Tree","Random Forest","SVC","SVM-Poly","SVM-Radial"),
       col = c("black","blue","red","green","brown"), pch = 19, cex = 0.9, box.lty = 0)

## Model Selection Approach 2
plot(x=NULL, y=NULL, xlim = range(6000,34000), ylim = range(err.min.final,err.max.final+0.02), main = "Model Selection Approach 2",
     xlab = "Bootstrapped Data Size", ylab = "Error Rate")
lines(x=seq(6000,34000,2000), y=cv.tree.result.final.bootstrap.final[,4], type = 'l', lty = 1, lwd = 2.5, col = 'black')
lines(x=seq(6000,34000,2000), y=cv.rf.result.final.bootstrap.final[,4], type = 'l', lty = 2, lwd = 2.5, col = 'blue')
points(30000,mean(cv.tree.result.final.real[,2]), pch=1, col='black', cex=1.7)
points(30000,mean(cv.rf.result.final.real[,2]), pch=2, col='blue', cex=1.5)
legend(15000,0.115, legend = c("Decision Tree Bootstrap","Random Forest Bootstrap"),
       col = c("black","blue"),lty = c(1,2), cex = 0.72, box.lty = 0)
legend(16300,0.09, legend = c("Decision Tree Real Data","Random Forest Real Data"), pch = c(1,2),
       col = c("black","blue"),cex = 0.75, box.lty = 0)

plot(x=NULL, y=NULL, xlim = range(6000,34000), ylim = range(err.min.final,err.max.final+0.02), main = "Model Selection Approach 2",
     xlab = "Data Size", ylab = "Error Rate")
lines(x=seq(6000,34000,2000), y=cv.svmLinear.result.final.bootstrap.final[,4], type = 'l', lty = 3, lwd = 2.5, col = 'black')
lines(x=seq(6000,34000,2000), y=cv.svmPoly.result.final.bootstrap.final[,4], type = 'l', lty = 4, lwd = 2.5, col = 'red')
lines(x=seq(6000,34000,2000), y=cv.svmRadial.result.final.bootstrap.final[,4], type = 'l', lty = 7, lwd = 2.5, col = 'purple')
points(30000,mean(cv.svmLinear.result.final.real[,2]), pch=1, col='black', cex=1.7)
points(30000,mean(cv.svmPoly.result.final.real[,2]), pch=1, col='red', cex=1.7)
points(30000,mean(cv.svmRadial.result.final.real[,2]), pch=1, col='purple', cex=1.7)
legend(5000,0.035, legend = c("SVC Bootstrap","SVM-Poly Bootstrap","SVM-Radial Bootstrap"),
       col = c("black","red","purple"),lty = c(3,4,7), cex = 0.8, box.lty = 0)

## Confusion matrix the for tree model
folds <- 12
fold.indices <- cut(1:nrow(bank.real), breaks = folds, labels = FALSE)
cv.tree.matrix <- foreach(q=1:4, .packages = "rpart", .combine = rbind)%dopar%{
  err.tree.matrix <- NULL
  TP <- c()
  TN <- c()
  FP <- c()
  FN <- c()
  Overall <- c()
  for(i in 1:5){
    fold.train <- sample(1:12,8)
    train.indices = c()
    for (j in 1:length(fold.train)) {
      temp <- which(fold.indices == fold.train[j])
      train.indices = append(train.indices, temp)
    }
    test.data <- bank.real[-train.indices,]
    train.data <- bank.real[train.indices,]
    original.tree <- rpart(y~., data = train.data, method = "class", parms = list(split="information"),
                           control = rpart.control(cp = 0, minsplit = bestsplit, minbucket = bestbucket))
    min.xerror <- which.min(original.tree$cptable[,"xerror"])
    min.cp <- original.tree$cptable[min.xerror,"CP"]
    best.tree <- prune(original.tree, cp = min.cp)
    preds <- predict(best.tree, newdata = test.data, type = "class")
    table.classfication <- table(test.data$y, preds)
    Overall[i] <- (table.classfication[1,2] + table.classfication[2,1])/sum(table.classfication)
    TP[i] <- table.classfication[2,2]/sum(table.classfication[2,])
    TN[i] <- table.classfication[1,1]/sum(table.classfication[1,])
    FP[i] <- table.classfication[1,2]/sum(table.classfication[1,])
    FN[i] <- table.classfication[2,1]/sum(table.classfication[2,])
  }
  err.tree.matrix <- rbind(err.tree.matrix, c(q,mean(TP),mean(TN),mean(FP),mean(FN),mean(Overall)))
  err.tree.matrix
}
cv.tree.matrix = as.matrix(c(mean(cv.tree.matrix[,2]),mean(cv.tree.matrix[,3]),mean(cv.tree.matrix[,4]),mean(cv.tree.matrix[,5]),mean(cv.tree.matrix[,6])))
rownames(cv.tree.matrix) <- c("TP","TN","FP","FN","Err")

## Confusion matrix for SVM-Poly
folds <- 12
fold.indices <- cut(1:nrow(bank.real), breaks = folds, labels = FALSE)
cv.svmPoly.matrix <- foreach(q=1:4, .packages = "e1071", .combine = rbind)%dopar%{
  err.svmPoly.confusion <- NULL
  TP <- c()
  TN <- c()
  FP <- c()
  FN <- c()
  Overall <- c()
  for(i in 1:5){
    fold.train <- sample(1:12,8)
    train.indices = c()
    for (j in 1:length(fold.train)) {
      temp <- which(fold.indices == fold.train[j])
      train.indices = append(train.indices, temp)
    }
    test.data <- bank.real[-train.indices,]
    train.data <- bank.real[train.indices,]
    train.data <- bank.real[-test.indices,]
    svm.poly <- svm(y~., data=train.data, kernel = 'polynomial', cost = bestcostPoly,
                    degree = bestdegree, gamma = bestgammaPoly, scale = T)
    preds <- predict(svm.poly, newdata = test.data, type = "class")
    table.svmPoly <- table(test.data$y, preds)
    Overall[i] <- (table.svmPoly[1,2] + table.svmPoly[2,1])/sum(table.svmPoly)
    TP[i] <- table.svmPoly[2,2]/sum(table.svmPoly[2,])
    TN[i] <- table.svmPoly[1,1]/sum(table.svmPoly[1,])
    FP[i] <- table.svmPoly[1,2]/sum(table.svmPoly[1,])
    FN[i] <- table.svmPoly[2,1]/sum(table.svmPoly[2,])
  }
  err.svmPoly.confusion <- rbind(err.svmPoly.confusion, c(q,mean(TP),mean(TN),mean(FP),mean(FN),mean(Overall)))
  err.svmPoly.confusion
}
cv.svmPoly.matrix = as.matrix(c(mean(cv.svmPoly.matrix[,2]),mean(cv.svmPoly.matrix[,3]),mean(cv.svmPoly.matrix[,4]),mean(cv.svmPoly.matrix[,5]),mean(cv.svmPoly.matrix[,6])))
rownames(cv.svmPoly.matrix) <- c("TP","TN","FP","FN","Err")
  


####################
#### Business Case
####################
## Situation
print("40 percent of clients are term deposite subsribers")
print("We assume term deposit clients are more likely to buy other financial products")
print("Term deposit clients have monthly deposite of 2000 USD")
print("When a normal client became a term deposite client for the first time, we promote our financial products to them")
print("The probability of term deposite client will buy one of these financial product when they hear about them for the first time is 30 percent")
print("Then we follow up to these term deposite clients in a month to promote our financial products again")
print("The clients that are being followed up have 30 percent chance buying the financial products")
print("The probabilities of a client buying a financial product after the first promotion is independent from the probabilities of buying a financial product after the second promotion")
print("There is 20% of chance for the clients to get irrated and cancel the term deposit subsription")

## The Tree Model
step.size <- seq(0,1,by=0.01)
folds <- 12
fold.indices <- cut(1:nrow(bank.real), breaks = folds, labels = FALSE)
results1.table <- foreach(q=1:length(step.size), .packages = "rpart", .combine = rbind)%dopar%{
  results1.err <- NULL
  TP <- c()
  TN <- c()
  FP <- c()
  FN <- c()
  Overall <- c()
  for(i in 1:5){
    fold.train <- sample(1:12,8)
    train.indices = c()
    for (j in 1:length(fold.train)) {
      temp <- which(fold.indices == fold.train[j])
      train.indices = append(train.indices, temp)
    }
    test.data <- bank.real[-train.indices,]
    train.data <- bank.real[train.indices,]
    original.tree <- rpart(y~., data = train.data, method = "class", parms = list(split="information"),
                           control = rpart.control(cp = 0, minsplit = bestsplit, minbucket = bestbucket))
    min.xerror <- which.min(original.tree$cptable[,"xerror"])
    min.cp <- original.tree$cptable[min.xerror,"CP"]
    best.tree <- prune(original.tree, cp = min.cp)
    preds <- predict(best.tree, newdata = test.data, type = "prob")
    preds.vec <- rep("no", nrow(preds))
    preds.vec[preds[,2] >= step.size[q]] <- 'yes'
    table.combination <- table(factor(test.data$y, levels=c("no", "yes")), factor(preds.vec, levels=c("no", "yes")))
    Overall[i] <- (table.combination[1,2] + table.combination[2,1])/sum(table.combination)
    TP[i] <- table.combination[2,2]/sum(table.combination[2,])
    TN[i] <- table.combination[1,1]/sum(table.combination[1,])
    FP[i] <- table.combination[1,2]/sum(table.combination[1,])
    FN[i] <- table.combination[2,1]/sum(table.combination[2,])
  }
  results1.err <- rbind(results1.err, c(q,step.size[q],mean(TP),mean(TN),mean(FP),mean(FN),mean(Overall)))
  results1.err
}
results1.table <- cbind(results1.table, rep(0,nrow(results1.table)))
colnames(results1.table) <- c("Steps","Cutoff","TP","TN","FP","FN","Overall Error Rate","Expected Cost")
for (i in 1:nrow(results1.table)){
  results1.table[i,8] = 
    0.4*0.3*results1.table[i,3]*700 +
    0.4*0.7*results1.table[i,3]*-350 +
    0.4*0.3*results1.table[i,6]*720 +
    0.4*0.7*results1.table[i,6]*-50 +
    0.6*0.3*results1.table[i,5]*700 +
    0.6*0.7*results1.table[i,5]*-20 +
    0.6*0.3*results1.table[i,4]*0 +
    0.6*0.7*results1.table[i,4]*0
}
plot(seq(0,1,0.01), results1.table[,8], type = "l", xlab = "Cutoff Point", ylab = "Cost", main = "Decision Tree")  
  
## The SVM-Poly Model
step.size <- seq(0,1,by=0.01)
folds <- 4
fold.indices <- cut(1:nrow(bank.real), breaks = folds, labels = FALSE)
results2.table <- foreach(q=1:4, .packages = "e1071", .combine = rbind)%dopar%{
  table.temp <- NULL
  TP <- c()
  TN <- c()
  FP <- c()
  FN <- c()
  Overall <- c()
  test.indices <- which(fold.indices == q)
  test.data <- bank.real[test.indices,]
  train.data <- bank.real[-test.indices,]
  svm.poly <- svm(y~., data=train.data, kernel = 'polynomial', cost = bestcostPoly,
                  degree = bestdegree, gamma = bestgammaPoly, scale = T, probability = T)
  preds <- predict(svm.poly, test.data, probability = T)
  for(i in 1:length(step.size)){
    preds.vec <- rep("no", nrow(attr(preds, "probabilities")))
    preds.vec[attr(preds, "probabilities")[,2] >= step.size[i]] <- 'yes'
    table.combination <- table(factor(test.data$y, levels=c("no", "yes")), factor(preds.vec, levels=c("no", "yes")))
    Overall <- (table.combination[1,2] + table.combination[2,1])/sum(table.combination)
    TP <- table.combination[2,2]/sum(table.combination[2,])
    TN <- table.combination[1,1]/sum(table.combination[1,])
    FP <- table.combination[1,2]/sum(table.combination[1,])
    FN <- table.combination[2,1]/sum(table.combination[2,])
    row.temp <- c(step.size[i], TP, TN, FP, FN, Overall)
    table.temp <- rbind(table.temp, row.temp)
  }
  table.temp
}
results2.table <- results2.table[order(results2.table[,1]),c(1:6)]
results2.table <- aggregate(results2.table, by=list(results2.table[,1]), FUN=mean)
results2.table <- results2.table[,c(2:7)]
results2.table <- cbind(results2.table, rep(0,nrow(results2.table)))
colnames(results2.table) <- c("Cutoff","TP","TN","FP","FN","Overall Error Rate","Expected Cost")
for (i in 1:nrow(results2.table)){
  results2.table[i,7] =
    0.4*0.3*results2.table[i,2]*700 +
    0.4*0.7*results2.table[i,2]*-350 +
    0.4*0.3*results2.table[i,5]*720 +
    0.4*0.7*results2.table[i,5]*-50 +
    0.6*0.3*results2.table[i,4]*700 +
    0.6*0.7*results2.table[i,4]*-20 +
    0.6*0.3*results2.table[i,3]*0 +
    0.6*0.7*results2.table[i,3]*0
}
plot(seq(0,1,0.01), results2.table[,7], type = "l", xlab = "Cutoff Point", ylab = "Cost", main = "SVM - Polynomial")
