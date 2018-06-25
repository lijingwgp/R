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
# trainControl() arguments
#   method: "boot","optimism_boot","boot_all","cv","repeatedcv","LOOCV","oob","adaptive_cv","adaptive_boot"
#   number: the number of folds or number of resampling iterations
#   repeats: for repeated k-fold cv only
#   p: for leave group out cv only, the training percentage
#   search: either "grid" or "random", describing how the tuning parameter grid is determined
#   verboselter: a logical for printing a training log
#   returnData: a logical for saving the data
#   returnResamp: indicates how much of the resampled summary metrics should be saved. "final", "all", "none"
#   savePredictions: an indicator of how much the hold-out predictions for each resample should be saved. "final", "all", "none"
#   classProbs: a logical; should class probabilities be computed for classfication models in each resample?
#   summaryFunction: a function to compute performance metrics across resamples
#   selectionFunction: the function used to select the optimal tuning parameter. For regression models, values of "RMSE" and "Rsquared" are applicable. 
#                      Classification models use either "Accuracy" or "Kappa" (for unbalanced class distributions)
#   preProcOptions: a list of options to pass to preProcess, "center", "scaling" is passed in via the preProc in train
#   sampling: a single character value describing  the type of additional sampling that is conducted after resampling. "none", "down", "up", "smote", "rose"

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

# 10-fold cross-validation and model tuning
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

# scatter plot of the AUC against max_depth and eta
ggplot(xgb_train_1$results, aes(x = as.factor(eta), y = max_depth, size = ROC, color = ROC)) + 
  geom_point() + 
  theme_bw() + 
  scale_size_continuous(guide = "none")


