##############################
### Load required packages ###
##############################
rm(list=ls())
options(warn=-1)
installIfAbsentAndLoad  <-  function(neededVector) {
  if(length(neededVector) > 0) {
    for(thispackage in neededVector) {
      if(! require(thispackage, character.only = T)) {
        install.packages(thispackage)}
      require(thispackage, character.only = T)
    }
  }
}
needed <- c("rpart",  #this is the recursive partitioning package
            "rattle", #the fancyRpartPlot and asRules functions at 
                    #the end of this script are in the rattle package
            "ISLR","glmnet")    
installIfAbsentAndLoad(needed)


####################################
########## partition data ##########
### training, validate, test subsets 
###########  (60/20/20) ############
####################################
set.seed(527)
Hitters <- na.omit(Hitters)[,-c(14,15,20)]
n <- nrow(Hitters) 
trainrows <- sample(n, 0.6*n) 
validaterows <- sample(setdiff(seq_len(n), trainrows), 0.2*n) 
testrows <- setdiff(setdiff(seq_len(n), trainrows), validaterows)
train <- Hitters[trainrows,]
validate <- Hitters[validaterows,]
test <- Hitters[testrows,]


###################################
########## Generate Tree ##########
###################################
## we will use rpart() to construct decision trees
##
## note that the "method" argument indicates the type of model to be built
## and is dependent on the response variable
##
## for numeric variables, we use anova
## for classification, we use class
##
## rpart() has two arguments for tuning the algorithm, control and parms
## cp is the complexity parameter. cp = 0 allows us to build a complete tree
## minbucket is the minimum number of observations in any terminal node
## minsplit is the minimum number of observations that must exist in a node in order for a split to be attempted
##
## if only one of minbucket or minsplit is specified, the code either sets minsplit to
## minbucket*3 or minbucket to minsplit/3
rpart<-rpart(Salary ~ .,data=train, method="anova",
             parms=list(split="gini"),
             control=rpart.control(usesurrogate=0, 
                                   maxsurrogate=0,cp=0, minsplit=2,
                                   minbucket=1))
## inspect the cp table
printcp(rpart)
rpart$cptable
## useless
print(rpart)
summary(rpart)
asRules(rpart)


################################
########## Tree Plots ##########
################################
plot(rpart)
text(rpart, all=TRUE, use.n=TRUE)
title("Training Set's Regression Tree")
## the fancyRpartPlot() is what we rather use 
fancyRpartPlot(rpart, main="Fancy Plot")


##################################
########## Tree Pruning ##########
##################################
## plot the cp table
## note that cp is decreasing from infinity to 0 as the size of the tree
## grows from 1 to complete
plotcp(rpart)
## extract the CV errors column from the cp table
xerr<-rpart$cptable[,"xerror"]
## determines the lowest CV error
## this step locates the row index which corrospond to the lowest CV error from the cp table
minxerr<-which.min(xerr)
## this step determines the lowest cp that corrospond to the row where the lowest CV error occurs
mincp<-rpart$cptable[minxerr,"CP"]
## to summarize, we first find the lowest CV error from the cp table, then locate
## a specific row where that lowest CV error occurs. from there we look at that row and find
## the corrosponding lowest cp.
## once the lowest cp is determined, we are ready to prune the tree
rpart.prune<-prune(rpart,cp=mincp)
printcp(rpart.prune)
fancyRpartPlot(rpart.prune, main="Pruned Tree")


########################
######### MSE ##########
########################
predTest <- predict(rpart, newdata=test, type="vector")
tree.mse <- mean((test$Salary - predTest)^2)
predTest.p <- predict(rpart.prune, newdata=test, type="vector")
tree.prune.mse <- mean((test$Salary - predTest.p)^2)
cbind(tree.prune.mse,tree.mse)
