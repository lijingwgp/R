rm(list=ls())
##################### 
#### QUESTION 1 #####
##################### 
## a) set the random seed to 5072
set.seed(5072)

## b) Using the rnorm() function, create a vector named x, containing 100 observations 
## drawn from a N(0, 1) distribution. This represents a feature space (or set of predictors)
x <- rnorm(100, mean = 0, sd = 1)

## c) using the rnorm function, create a vector named eps, containing 100 obs drawn from
## a N(0, 0.25) distribution
eps <- rnorm(100, mean = 0, sd = sqrt(0.25))

## d) Using x and eps, generate a vector named y according to the model 
## Y = -1 + 0.5X + error
y <- -1 + 0.5*x + eps

## e) displaying the length of y
length(y)

## f) indicate what the values of beta0 and beta1 are in this model
print("In this model, Beta0 is -1, and Beta1 is 0.5")

## g) create a scatterplot displaying the relationship between x and y
plot(x,y)

## h) indicate the type of relationship you observe, the degree of linearity, and the amount
## of variability
print("I observe a positive, relative strong linear relationship with moderate variability")

## i) fit a least squares linear model to predict y using x
ls.1 <- lm(y~x)

## j) what are Beta0 hat and Beta1 hat, and how do they compare with Beta0 and Beta1
coef(ls.1)
print("Beta0 hat and Beta1 hat in this model is -1 and 0.4352, they are the estimiates of 
      population Beta0 and Beta1. While Beta0 hat is exactly the same as population Beta0,
      Beta1 hat is moderate close to the population Beta1.")

## K) display the least squre line on the scatter plot in black
abline(ls.1, lty = 1, lwd = 2, col = "black")

## l) display the population regression line on the plot in red
y_true <- -1 + 0.5*x
true.1 <- lm(y_true~x)
abline(true.1, col="red", lty = 1, lwd = 2)

## m) use the legend command to create an appropriate legend
legend("topleft", legend = c("Least Squre Line","True Regression Line"), col = c(1,2), cex = 0.75, lty = c(1.1,1),lwd = c(2.5,2.5))

## n) fit a polynomial regression model that predicts y using x and x^2
x_square <- x^2
ls.square <- lm(y ~ x + x_square)

## o) determine whether ls.2 is better than ls.1
anova(ls.1, ls.square)
print("The null hypothesis is two models are equivelent. Since F-statistics is small 
       and p-value is greater than 0.05, we conclude that we failed
       to reject our null hypothesis.")

## p) repeat steps b) to m) with variance is 0.1
x <- rnorm(100, mean = 0, sd = 1)
eps <- rnorm(100, mean = 0, sd = sqrt(0.1))
y <- -1 + 0.5*x + eps
length(y)
print("In this model, Beta0 is -1, and Beta1 is 0.5")
plot(x,y)
print("I observe a positive, strong linear relationship with low variability")
ls.2 <- lm(y~x)
coef(ls.2)
print("Beta0 hat and Beta1 hat in this model is -1.02 and 0.4863, they are the estimiates of 
      population Beta0 and Beta1. This time, Beta0 hat is very close to the population Beta0, and
      Beta1 hat is also very close to the population Beta1, 0.5.")
abline(ls.1, lty = 1, lwd = 2, col = "black")
y_true <- -1 + 0.5*x
true.1 <- lm(y_true~x)
abline(true.1, col="red", lty = 1, lwd = 2)
legend("topleft", legend = c("Least Squre Line","True Regression Line"), col = c(1,2), cex = 0.75, lty = c(1.1,1),lwd = c(2.5,2.5))

## q) change variance to 0.5
x <- rnorm(100, mean = 0, sd = 1)
eps <- rnorm(100, mean = 0, sd = sqrt(0.5))
y <- -1 + 0.5*x + eps
length(y)
print("In this model, Beta0 is -1, and Beta1 is 0.5")
plot(x,y)
print("I observe a positive, weak linear relationship with high variability")
ls.3 <- lm(y~x)
coef(ls.3)
print("Beta0 hat and Beta1 hat in this model is -1.08 and 0.3827, they are the estimiates of 
      population Beta0 and Beta1. This time, neither Beta0 hat nor Beta1 is close to the population Beta0, and
      Beta1.")
abline(ls.3, lty = 1, lwd = 2, col = "black")
y_true <- -1 + 0.5*x
true.1 <- lm(y_true~x)
abline(true.1, col="red", lty = 1, lwd = 2)
legend("topleft", legend = c("Least Squre Line","True Regression Line"), col = c(1,2), cex = 0.75, lty = c(1.1,1),lwd = c(2.5,2.5))

## r) constrast the closeness of the fit to the population regression line among all 
## three levels of population variance esp.
print("In the first graph which has 0.25 variance, the predicted regression line intersects with the population regression line around X=0. 
      This distance begins increase towards the two ends of x axis.")
print("In the second graph which has 0.1 variance, the predicted regression line is almost identical to the population regression line across all X values")
print("In the third graph which has 0.5 variance, the predicted regression line intersects with the population regression line close to X=-1, then the two quickly departs from each other at other X values from both ends.")

## s) Display the 95% confidence internval for Beta0 and Beta1 based on the original data set, 
## the noisier data set, and the less noisy data set. 
confint(ls.1, level = .95)
confint(ls.2, level = .95)
confint(ls.3, level = .95)

## t) comment on the reason why the width of the confidence intervals are as observed.


rm(list=ls())
##################### 
#### QUESTION 2 #####
##################### 
## a) execute the following commands
set.seed (5072) 
x1=runif (100) 
x2 = 0.5 *  x1 + rnorm (100) /10 
y= 2+2* x1 + 0.3* x2 + rnorm (100) 

## b) On a single comment line, indicate what the values of ??0, ??1 and ??2 are in this 
## linear model.
print("Beta0 is 2, Beta1 is 2 and Beta2 is 0.3")

## c) display the Pearson correlation coefficients of y, x1 and x2.
correlation_matrix <- matrix(c(x1,x2,y), nrow = 100, ncol = 3)
cor(correlation_matrix)

## d) create scatterplots displaying the relationship between y, x1 and x2 
## (recall the pairs() function) 
pairs(y~x1+x2)

## e) comment of the correlations among these variables
print("x1 and x2 is strongly positive correlated. x1 and y is weakly positive correlated. x2 and y is also weakly positive correlated.")

## f) using this data, fit a least squares regression model called lm.fit.both to predict y using x1 and x2.
lm.fit.both <- lm(y~x1+x2)

## g) display the values of Beta0 hat, Beta1 hat, and Beta2 hat.
coef(lm.fit.both)

## h) comment on the statistical significance of Beta0 hat, Beta1 hat, and Beta2 hat.
summary(lm.fit.both)
print("both the intercept, and x1's coefficient are significant since their p-value is smaller than 0.05.")

## i) can you reject the null hypothesis H0: ??1 = 0? How about the null hypothesis H0: ??2 = 0? 
print("since the p-value of Beta1 hat is statistically significant, we have sufficient evidence to reject the null hypothesis of Beta1 being 0")
print("since the p-value of Beta2 hat is greater than 0.05, we failed to reject the null hypothesis of Beta2 being 0")

## j) fit a least squares regression model called lm.fit.justx1 to predict y using only x1.
lm.fit.justx1 <- lm(y~x1)

## k) can you reject the null hypothesis H0: ??1 = 0? 
summary(lm.fit.justx1)
print("since the p-value of Beta1 hat is statistically significant, we have sufficient evidence to reject the null hypothesis of Beta1 being 0.")

## l) fit a least squares regression model called lm.fit.justx2 to predict y using only x2. 
lm.fit.justx2 <- lm(y~x2)

## m) can you reject the null hypothesis H0: ??2 = 0? 
summary(lm.fit.justx2)
print("since the p-value of Beta2 hat is statistically significant, we have sufficient evidence to reject the null hypothesis of Beta2 being 0")

## n) do the results contradict?
print("the results from lm.fit.both contradicts with the results from lm.fit.justx2. This happened because x1 and x2 have a high collinearity.
      In other words, x2 being a predictor in the lm.fit.both model can be linearly predicted from x1 with a substantial degree of accuracy.")

## o) excute the following command.
x1=c(x1 , 0.1) 
x2=c(x2 , 0.8) 
y=c(y,6) 

## p) re-fit the linear models from f)-m) using this new data.
lm.fit.both <- lm(y~x1+x2)
coef(lm.fit.both)
summary(lm.fit.both)

print("both the intercept, and x1's coefficient are significant since their p-value is smaller than 0.05.
      However, the significance level of Beta1 hat reduced to 0.04 which is just little less than 0.05")
print("since the p-value of Beta1 hat is statistically significant, we have sufficient evidence to reject the null hypothesis of Beta1 being 0")
print("since the p-value of Beta2 hat is greater than 0.05, we failed to reject the null hypothesis of Beta2 being 0")

lm.fit.justx1 <- lm(y~x1)
summary(lm.fit.justx1)
print("since the p-value of Beta1 hat is statistically significant, we have sufficient evidence to reject the null hypothesis of Beta1 being 0.")

lm.fit.justx2 <- lm(y~x2)
summary(lm.fit.justx2)
print("since the p-value of Beta2 hat is statistically significant, we have sufficient evidence to reject the null hypothesis of Beta2 being 0")

## q) comment on the effects this new observation has on each of the models
print("This new observation has effect on both Beta1 hat and Beta2 hat. It increase the significance level of x2 and reduced the significance level of x1")

## r) in each model, is this new observation (point 101) an outlier? A high-leverage point? Both? 
par(mfrow=c(2,2))
plot(lm.fit.both)
print("point 101 is both a leverage point and an outlier since it's cook's distance is greater than 1 and its residule value is higher than the rest of the group.")


rm(list=ls())
##################### 
#### QUESTION 3 #####
#####################
## a) Set the seed to 5072. For each predictor, 
## fit a simple linear regression model to predict the response.
set.seed (5072) 
require(MASS)

values_table <- data.frame()
for(i in 2:14){
  lm.boston <- lm(Boston$crim~Boston[,i])
  values <- c(summary(lm.boston)$fstatistic[1],anova(lm.boston)$'Pr(>F)'[1],coef(lm.boston)[1], coef(lm.boston)[2])
  values_table <- rbind(values_table, values)
}
colnames(values_table) <- c("F-Stats","P-value","Intercept","Beta1")
row.names(values_table) <- c("zn","indus","chas","nox","rm","age","dis","rad","tax","ptratio","black","lstat","medv")
values_table

## b) in which of the models is there a statistically significant association 
values_table <- values_table[-3,]
values_table
print("all predictors, but chas, are significant when they are used in individual simple regression to predict crime rate")

## c) change the graphics window to a 4x3 grid, then for each significant predictor, plot the xvalues on the x-axis and the y 
## values on the y-axis and line produced by the least-squares linear model.
row_names <- row.names(values_table)
par(mfrow=c(4,3))
par(mar=c(2,2,2,2))
for(i in row_names){
  lm.boston <- lm(Boston$crim ~ Boston[i][,1])
  plot(Boston[i][,1], Boston$crim, main = i)
  abline(lm.boston,col="red", lty = 1, lwd = 2)
}

## d) Fit a multiple regression model to predict the response using all of the predictors
lm.fit.all <- lm(crim~., data = Boston)

## e) Create a statement in R to display only those predictors which are significant at a level 
x=data.frame(coef(summary(lm.fit.all)))
a=x[x[,4]<=0.05,]
a

## f) Compare your results from (a) to your results from (d) as follows
par(mfrow = c(1,1))
values_table <- data.frame()
for(i in 2:14){
  lm.boston <- lm(Boston$crim~Boston[,i])
  values <- c(summary(lm.boston)$fstatistic[1],anova(lm.boston)$'Pr(>F)'[1],coef(lm.boston)[1], coef(lm.boston)[2])
  values_table <- rbind(values_table, values)
}
colnames(values_table) <- c("F-Stats","P-value","Intercept","Beta1")
row.names(values_table) <- c("zn","indus","chas","nox","rm","age","dis","rad","tax","ptratio","black","lstat","medv")
x <- x[-1,]
plot(values_table[,4],x[,1])

## g) Is there evidence of non-linear association between any of the predictors and the response? 
polynorm <- data.frame()
for(i in 2:14){
  x <- Boston[,i]
  x_square <- x^2
  x_cub <- x^3
  lm.fit.simple <- lm(Boston$crim~x)
  lm.fit.multiple <- lm(Boston$crim~x+x_square+x_cub)
  row_vec <- data.frame()
  row_vec[1,1] <- names(Boston[i])
  row_vec[1,2] <- anova(lm.fit.simple,lm.fit.multiple)$F[2]
  row_vec[1,3] <- anova(lm.fit.simple,lm.fit.multiple)$'Pr(>F)'[2]
  polynorm <- rbind(polynorm, row_vec)
}
polynorm <- na.omit(polynorm)
polynorm[order(polynorm[,3]),]
colnames(polynorm) <- c("predictor","fstat","pvalueofFstat")
polynorm <- polynorm[-12,]
polynorm
print("All predictors but one variable, black, has non-linear association with the response variable")
