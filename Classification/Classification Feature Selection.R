rm(list=ls())
options(warn=-1)   # Supress warning messages
##########################################################################################
### Functions
##########################################################################################
installIfAbsentAndLoad <- function(neededVector) {
  for(thispackage in neededVector) {
    if( ! require(thispackage, character.only = T) )
    { install.packages(thispackage)}
    require(thispackage, character.only = T)
  }
}
##############################
### Load required packages ###
##############################
#leaps contains the regsubset() function, glmnet contains the glmnet() function
needed <- c("glmulti", "ISLR")  
installIfAbsentAndLoad(needed)
rm(list=ls())
############################################################
####### Best Subset Selection For Classification ###########
############################################################

# Here we apply the best subset selection approach to the 
# Hitters data to predict whether a player's salary is above
# or below the median of all players' salaries on the basis
# of various statistics associated with performance in the
# previous year.

head(Hitters)
names(Hitters)
dim(Hitters)
hitters=na.omit(Hitters)
dim(hitters)
hitters$NewSalary <- rep(0, nrow(hitters))
hitters$NewSalary[hitters$Salary > median(hitters$Salary)] <- 1
hitters$NewSalary <- factor(hitters$NewSalary)
hitters.full <- hitters[setdiff(names(hitters),"Salary")]
hitters.truncated <- hitters[setdiff(names(hitters),c("Salary", names(hitters)[1:9]))]

# The glmulti() function (part of the glmulti package) performs best 
# subset selection by identifying the best model that contains a given number
# of predictors, where best is quantified using AIC (by default). 

# Begin by constructing a full model
glm.fit.truncated <- glm(NewSalary ~ .,
                         data = hitters.truncated, 
                         family=binomial)

# To compute the best subset, use method = 'h'
multifit.truncated <- glmulti(glm.fit.truncated, 
                              method = 'h',    # This specifies an exhaustive search
                              crit=aic,
                              level=1,         # This limits analysis to main effects only (no interactions)
                              family=binomial)
summary(multifit.truncated)
weightable(multifit.truncated)
plot(multifit.truncated)

# To perform subset selection when there is a large number
# of predictors, the glmulti() function supports a genetic
# algorithm approach to determing the best subset, as
# follows:
glm.fit.full <- glm(NewSalary ~ .,
                    data = hitters.full, 
                    family=binomial)

# To compute the best subset, use method = 'h'
multifit.full <- glmulti(glm.fit.full, 
                         method = 'g',    # This specifies a search using the built-in genetic algorithm
                         crit=aic,
                         level=1,         # This limits analysis to main effects only (no interactions)
                         family=binomial)

summary(multifit.full)
weightable(multifit.full)
plot(multifit.full)
