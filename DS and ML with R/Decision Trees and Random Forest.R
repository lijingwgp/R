require(rpart)
data("kyphosis")
str(kyphosis)
head(kyphosis)

# build a single decision tree model
tree <- rpart(Kyphosis ~ ., method = 'class', data = kyphosis)
printcp(tree)
plot(tree, uniform = T, main = 'Kyphosis Tree')
text(tree, use.n = T, all = F)
require(rpart.plot) # to make a better plot
prp(tree)

# build a rf model
require(randomForest)
model <- randomForest(Kyphosis ~ ., data = kyphosis)
print(model)
model$confusion
