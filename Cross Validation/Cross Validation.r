rm(list=ls())
require(boot)
my_table <- read.csv('Accepted.csv', sep = ',', stringsAsFactors = FALSE)
my_newtable <- read.csv('NewData.csv', sep = ',', stringsAsFactors = FALSE)

rows <- nrow(my_table)
cv.error <- c()

for(i in 1:10) {
  glm_fit <- glm(Accepted ~ poly(Applications, i), data=my_table)
  cv.error[i] <- cv.glm(my_table, glm_fit, K=5)$delta[1]
}

which.min(cv.error)
print("The lowest estimated test MSE occured when predictor variable has one degree")

glm_fit <- glm(Accepted ~ Applications, data = my_table)
summary(glm_fit)

mean(predict(glm_fit, my_newtable)-my_newtable$Accepted)
cv.error[1]

