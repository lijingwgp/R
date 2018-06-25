library(xgboost)
library(tidyverse)

###############################
### Setting up environment  ###
###############################
# read in our data & put it in a data frame
diseaseinfo <- read.csv("Outbreak_240817.csv", sep = ',')
set.seed(1234)
# shuffle row indices
diseaseinfo <- diseaseinfo[sample(1:nrow(diseaseinfo)),]



###########################################
### Preparing data & selecting features ###
###########################################
# in order to use xgboost, we cant just pass it a dataframe
# the core xgboost function requires data to be a matrix
head(diseaseinfo)
# to prepare our data, we have a number of steps we need to complete:
#   remove information about the target variable from the training data
#   reduce the amount of redundant information
#   convert categorical information to a numeric format
#   split dataset into testing and training subsets
#   convert the cleaned dataframe to a Dmatrix

# remove information about the target variable from the training data
diseaseinfo_humansRemoved <- diseaseinfo %>% select(-starts_with("human"))
diseaselabels <- diseaseinfo %>%
  select(humansAffected) %>%
  is.na() %>%
  magrittr::not()                  # switch TRUE and FALSE 
# (using function from the magrittr package)

# check out the first few lines
head(diseaselabels)                # of our target variable
head(diseaseinfo$humansAffected)   # of the origianl column

# reduce the amount of redundant information
# we also want to remove columns that have redundant information in them 
# or that we dont want to use to make predictions. 
# for example, I dont want to include latitude and longtitude in our training data
#
# We also need to remove variables that could be very informative due to chance, such as ID
#
# finally, I want to remove all the non-numeric variables, since a matrix can only hold numeric variables
diseaseinfo_numeric <- diseaseinfo_humansRemoved %>%
  select(-Id) %>%
  select(-c(longitude, latitude)) %>%
  select_if(is.numeric)
str(diseaseinfo_numeric)

# convert categorical information to a numeric format
region <- model.matrix(~country-1, diseaseinfo)
# sometimes more text processing is required to turn it into matrix
# for example:
head(diseaseinfo$speciesDescription)
# we can add "domestic" as a variable if the "speciesDescription" column has the word
diseaseinfo_numeric$is_domestic <- str_detect(diseaseinfo$speciesDescription, "domestic")
# we can also add a variable that only contains the species information in it
specieslist <- diseaseinfo$speciesDescription %>%
  # remove punctuation, some rows have parentheses
  str_replace("[[:punct:]]", "")%>%
  # extract the last work in each row
  str_extract("[a-z]*$")
# convert our list into a dataframe...
specieslist <- tibble(species = specieslist)
# and convert to a matrix using 1 hot encoding
# dont drop NA values
options(na.action = 'na.pass')
species <- model.matrix(~species-1, specieslist)

# So now, we have three seperate data frames all with numeric information in them
# We can bundle all these columns together and then should be able to convert them into a 
# matrix without a problem
diseaseinfo_numeric <- cbind(diseaseinfo_numeric, region, species)
diseaseinfo_matrix <- data.matrix(diseaseinfo_numeric)



#######################################################
### Split dataset into testing and training subsets ###
#######################################################
# we need to split our data into testing and training data
# train/test: 7/3
# train
train.indicies <- round(length(diseaselabels) * .7)
train_x <- diseaseinfo_matrix[1:train.indicies,]
train_y <- diseaselabels[1:train.indicies]
# test
test_x <- diseaseinfo_matrix[-(1:train.indicies),]
test_y <- diseaselabels[-(1:train.indicies)]



##################################################
### Convert the cleaned dataframe to a dmatrix ###
##################################################
# the very final step is to convert our matrixes to dmatrix objects
# this step isn't absolutely neccessary, but it will help our model train more quickly
# and you will need to do this if you ever want to train a model on multiple cores
dtrain <- xgb.DMatrix(data = train_x, label = train_y)
dtest <- xgb.DMatrix(data = test_x, label = test_y)



##########################
### Training our model ###
##########################
# lets start training one model then work on tweak parameters
# in order to train our model, we need to pass some information to the command:
#   training data
#   number of iterations
#   objective functions
model <- xgboost(data = dtrain, nrounds = 2, objective = "binary:logistic")
# we observe that for both the first and second rounds, we had the same error
# on the training data. this means we didnt see an improvement in the second iteration
#
# we are more interested in the accuracy on the testing data
pred <- predict(model, dtest)
err <- mean((as.numeric(pred) > .5) != test_y)



########################
### Tuning our model ###
########################
model_tuned <- xgboost(data = dtrain,
                       max.depth = 3,
                       nrounds = 2,
                       objective = "binary:logistic")
pred <- predict(model_tuned, dtest)
err <- mean(as.numeric(pred > 0.5) != test_y)
# because we weren't over-fitting to begin with--we don't really see a big change

# there are two things we can try to see if we improve our model performance
#   account for the fact that we have imbalanced classes
#   train for more rounds
negative_cases <- sum(train_y == FALSE)
positive_cases <- sum(test_y == TRUE)
model_tuned <- xgboost(data = dtrain,            
                       max.depth = 3,
                       nround = 10, 
                       # if we dont see an improvement in this many rounds, stop
                       early_stopping_rounds = 3,
                       objective = "binary:logistic",
                       # control for imbalanced classes
                       scale_pos_weight = negative_cases/positive_cases) 
pred <- predict(model_tuned, dtest)
print(err <- mean(as.numeric(pred > 0.5) != test_y))

# another technique that can help avoid over-fitting is adding a regularization term, gamma
# gamma is a measure of how much an additional split will need to reduce loss in order to be added 
# to the ensemble
#
# if a proposed model does not reduce loss by at least whatever-you-set-gamma-to, it won't be included
# here, I'll set it to one, which is fairly high
# by default gamma is zero
model_tuned <- xgboost(data = dtrain,            
                       max.depth = 3,
                       nround = 10, 
                       # if we dont see an improvement in this many rounds, stop
                       early_stopping_rounds = 3,
                       objective = "binary:logistic",
                       # control for imbalanced classes
                       scale_pos_weight = negative_cases/positive_cases,
                       # regularization term
                       gamma = 1) 



###########################
### Examining our model ###
###########################
# One way that we can examine our model is by looking at a representation 
# of the combination of all the decision trees in our model
#
# since all the trees have the same depth, we can stack them all on top of one
# another and pick the things that show up most often in each node
xgb.plot.multi.trees(feature_names = names(diseaseinfo_matrix), 
                     model = model)
# The top of the tree is on the left and the bottom of the tree is on the right
#
# For features, the number next to it is "quality", which helps indicate 
# how important this feature was across all the trees
# 
# Higher quality means a feature was more important. So we can tell that 
# is_domestic was by far the most important feature across all of our trees
# both because it's higher in the tree and also because it's quality score 
# is very high
#
# For the nodes with "Leaf", the number next to the "Leaf" is the 
# average value the model returned across all trees if a a certain observation ended up in that leaf
#
# Because we're using a logistic model here, it's telling us the log-odds rather than the probability
odds_to_probs <- function(odds){
  return(exp(odds)/ (1 + exp(odds)))
}
odds_to_probs(-0.59953)

# What if we want a quick way to see which features are most important? We can do that using by creating and then plotting the importance matrix
importance_matrix <- xgb.importance(names(diseaseinfo_matrix), model = model)
xgb.plot.importance(importance_matrix)

