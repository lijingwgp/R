## The broom package takes the messy output of built-in functions in R,
## and turns them into tidy data frames.

## broom should be distinguished from packages like reshape2 and tidyr, 
## which rearrange and reshape data frames into different forms. 
## Those packages perform critical tasks in tidy data analysis but focus on 
## manipulating data frames in one specific format into another. 

## broom is designed to take format that is not in a data frame and convert it to a tidy data frame.
## This package provides three methods that do three distinct kinds of tidying.
##  tidy: constructs a dataframe that summarizes the model's statsitical findings.
##  augment: add columns to the original data that was modeled. This includes predictions, residuals, cluster assignments.
##  glance: construct a concise one-row summary of the model.

lmfit <- lm(mpg ~ wt, mtcars)
lmfit
summary(lmfit)
coef(summary(lmfit))

## Converting this summary to a dataframe so that we can do other modelings with this information
library(broom)
tidy(lmfit)

## Instead of viewing the coefficients, you might be interested in the fitted values and residuals
## for each of the original points in the regression
## The augment() function does this job
head(augment(lmfit))

## Finally, several important summary statistics are computed for the entire
## regression, such as R2 and F-stats. These can be access with the glance() function
glance(lmfit)

## These functions apply equally well to the output from glm()
glmfit <- glm(am~wt, mtcars, family = "binomial")
tidy(glmfit)
head(augment(glmfit))
glance(glmfit)

