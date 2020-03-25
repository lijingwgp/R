# read data
df <- read.csv('student-mat.csv', sep = ';')
df
any(is.na(df))
str(df)

# explore the data
require(ggplot2)
require(ggthemes)
require(dplyr)
require(corrgram)
require(corrplot)
num.cols <- sapply(df, is.numeric)  #num data only
cor.data <- cor(df[,num.cols])  #calculate correlation
corrplot(cor.data, method = 'color')
corrgram(cor.data, order = T, lower.panel = panel.shade, upper.panel = panel.pie)
ggplot(df, aes(x=G3)) + geom_histogram(bins = 20, alpha=0.5, fill='blue')

# train and test split
require(caTools)
set.seed(101)
sample <- sample.split(df$G3, SplitRatio = .7)
train <- df[sample,]
test <- subset(df, sample==FALSE)

# linear regression model
m1 <- lm(G3 ~ ., data=train)
summary(m1)
res <- residuals(m1)
res <- as.data.frame(res)
ggplot(res, aes(x=res)) + geom_histogram(fill='blue', alpha=.5)

# model validation
plot(m1)
predictions <- predict(m1, test)
results <- cbind(predictions, test$G3)
colnames(results) <- c('predicted', 'actual')
results <- as.data.frame(results)
results

# take care of negative values
min(results$predicted)
to_zero <- function(x){
  if (x<0){
    return(0)
  }else{
    return(x)
  }
}
results$predicted <- sapply(results$predicted, to_zero)

# MSE
mse <- mean((results$actual - results$predicted)^2)
