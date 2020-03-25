# load data
train <- read.csv('titanic_train.csv')
test <- read.csv('titanic_test.csv')
str(train)

# explore the data first
require(Amelia) #useful for investigating NA
missmap(train, main = 'Missing Map', col=c('white','blue'), legend = F)
require(ggplot2)
ggplot(train, aes(Survived)) + geom_bar()
ggplot(train, aes(Pclass)) + geom_bar(aes(fill=factor(Pclass)))
ggplot(train, aes(Sex)) + geom_bar(aes(fill=factor(Sex)))
ggplot(train, aes(Age)) + geom_histogram(bins = 20, alpha=.5, fill='blue')
ggplot(train, aes(Fare)) + geom_histogram(fill='light blue', color='black')
ggplot(train, aes(Pclass, Age)) + geom_boxplot(aes(group=Pclass, fill=factor(Pclass), alpha=.5)) +
  scale_y_continuous(breaks = seq(min(0), max(80), by=4)) + theme_bw()

# imputation
impute_age <- function(age, class){
  out <- age
  for (i in 1:length(age)){
    if (is.na(age[i])){
      if (class[i] == 1){
        out[i] = 37
      } else if (class[i] == 2){
        out[i] = 29
      } else{
        out[i] = 24
      }
    } else {
      out[i] = age[i]
      }
  }
  return(out)
}
age_new <- impute_age(train$Age, train$Pclass)
train$Age <- age_new
age_new <- impute_age(test$Age, test$Pclass)
test$Age <- age_new
missmap(train, main = 'Missing Map', col=c('white','blue'), legend = F)

# some preprocessing
require(dplyr)
train <- select(train, -c(PassengerId, Name, Ticket))
test <- select(test, -c(PassengerId, Name, Ticket))
train$Survived <- factor(train$Survived)
train$Pclass <- factor(train$Pclass)
train$Parch <- factor(train$Parch)
train$SibSp <- factor(train$SibSp)
test$Pclass <- factor(test$Pclass)
test$Parch <- factor(test$Parch)
test$SibSp <- factor(test$SibSp)

# logistic model
m1 <- glm(Survived~., family = binomial(link = 'logit'), data = train)
summary(m1)
probs <- predict(m1, test, type = 'response')
preds <- ifelse(probs>.5,1,0)
#err <- mean(preds != train$Survived)
#1-err
#table(train$Survived, probs>0.5)