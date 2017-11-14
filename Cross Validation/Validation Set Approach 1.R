###########################################################################################
## Partitioning a dataframe into train, validate, test sets of specified relative sizes ###
###########################################################################################
set.seed(5072)             #sets the random seed
nobs <- nrow(SandP) 
trainprop <- 0.7           #say we want a 70/20/10 split of rows for training, validation and test respectively
validateprop <- 0.2

#create a vector of random integers of training size from the vector 1:nobs
train  <-  sample(nobs, trainprop * nobs)

#create a vector of the remaining  integers, then create a vector of random integers
#of validate size by sampling from these
validate  <-  sample(setdiff(1:nobs, train), validateprop * nobs) 

#create a vector of the integers not in either training or validate
test <- setdiff(setdiff(1:nobs, train), validate)

#Create the data frames using the indices created in the three vectors above
trainset <- SandP[train,]
validateset <- SandP[validate,]
testset <- SandP[test,]

#Checks
nobs
nrow(trainset) / nobs
nrow(validateset) / nobs
nrow(testset) / nobs
set.seed(NULL)             #remove the random seed 
