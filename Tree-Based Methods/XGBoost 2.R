require(mlbench)
require(caret)
require(caTools)
require(doParallel)
c <- detectCores()
registerDoParallel(c)

# reading dataset
data("Sonar")

# inspecting the dataset
dim(Sonar)
head(Sonar,3)
summary(Sonar)
str(Sonar)

# data partition into training and testing
# p is the probability that a data point goes to the training set
ind = createDataPartition(Sonar$Class, p= 2/3, list = FALSE)
training <- Sonar[ind,]
testing <- Sonar[-ind,]

# using trainControl() function in caret
# controlParam <- trainControl(method = "cv",
#                              number = 5,
#                              savePredictions = TRUE,
#                              classProbs = TRUE)
controlParam <- trainControl(
  ## 10-fold CV
  method = "repeatedcv",
  number = 10,
  ## repeated ten times
  repeats = 10,
  allowParallel = T)

# xgboost model parameters
#   nrounds: number of iterations
#
#   max_depth: high value will create more deeper trees which may cause
#              overfitting. lower value may create underfitting. range: [1,infinity]
#
#   eta: learning rate. range: [0,1]
#
#   gamma: minimum loss reduction. the larger value will create more conservative tool. range: [0,infinity]
#
#   colsample_bytree: randomly choosing the number of columns out of all columns while
#                     tree building process. Higher value may create overfitting and lower value 
#                     may create underfitting.One needs to play with this value. range: (0,1]
#
#   min_child_weight: minimum sum of instance weight. it is like number of observations a terminal node has.
#                     If the tree partition step results in a leaf node with the sum of instance weight 
#                     less than min_child_weight, then the building process will give up further partitioning.
#                     range: [0,infinity]

# model parameters grid
parameterGrid <- expand.grid(eta = 0.1,
                             colsample_bytree = c(0.5,0.7),
                             max_depth = c(3,6),
                             nrounds = 1000,
                             gamma = 1,
                             min_child_weight = 2,
                             subsample = .6)
parameterGrid

# 5-fold cross-validation and model tuning
model <- caret::train(Class ~., data = training,
  #x = as.matrix(traindf %>% select(-Class)),
  #y = as.factor(traindf$Class),
  method = "xgbTree",
  trControl = controlParam,
  tuneGrid = parameterGrid
  )
model
model$bestTune
model$results
model$finalModel

# predictions on test data and confusion matrix
predictions <- predict(model, testing)
confusion <- confusionMatrix(predictions, testing$Class)

