rm(list = ls())
library(dplyr)
library(ggplot2)
library(car)
set.seed(2018)
test <- read.csv("patient profile with replacement.csv", sep = ",")
test <- mutate(test, totalEngage = AWV+OfficeVisit+Physical) 
test <- mutate(test, totalAllowedAmount = TotalOPAllowedAmt + TotalProfAllowedAmt + TotalIPAllowedAmt)

## Proofs of choosing specific columns
glimpse(test)
#scatterplot(test$pulsepredicteddollarsamt, test$totalEngage, smoother = F, reg.line = T, boxplots = F)
ggplot(test, aes(x = pulsepredicteddollarsamt, y = totalEngage)) + 
  geom_point(stat = "identity") + xlab("Predicted Medical Cost") + ylab("Total Engagement") +
  ggtitle("Total Engagement vs. Predicted Medical Cost")
#scatterplot(test$IPAdmissionsnum, test$totalEngage, smoother = F, reg.line = T, boxplots = F)
ggplot(test, aes(x = IPAdmissionsnum, y = totalEngage)) + 
  geom_point(stat = "identity") + xlab("Number of Hospital Admissions") + ylab("Total Engagement") +
  ggtitle("Total Engagement vs. Number of Hospital Admissions")
#scatterplot(test[-c(1262,2331),"EDVisitsnum"], test[-c(1262,2331),"totalEngage"], smoother = F, reg.line = F, boxplots = F)
ggplot(test[-c(1262,2331),], aes(x = EDVisitsnum, y = totalEngage)) + 
  geom_point(stat = "identity") + xlab("Number of Emergency Room Visits") + ylab("Total Engagement") +
  ggtitle("Total Engagement vs. Number of Emergency Room Visits")
#scatterplot(test$totalAllowedAmount, test$totalEngage)
ggplot(test, aes(x = totalAllowedAmount, y = totalEngage)) + 
  geom_point(stat = "identity") + xlab("Total Allowed Amount") + ylab("Total Engagement") +
  ggtitle("Total Engagement vs. Total Allowed Amount")
#scatterplot(test$UniqueDrugcountnum, test$totalEngage)
ggplot(test, aes(x = UniqueDrugcountnum, y = totalEngage)) + 
  geom_point(stat = "identity") + xlab("Number of Medicines Prescribed") + ylab("Total Engagement") +
  ggtitle("Total Engagement vs. Number of Medicines Prescribed")
#scatterplot(test$sickNum,test$totalEngage)
ggplot(test, aes(x = sickNum, y = totalEngage)) + 
  geom_point(stat = "identity") + xlab("Number of Sicknesses") + ylab("Total Engagement") +
  ggtitle("Total Engagement vs. Number of Sicknesses")

## Generating Z-Scores
test <- test[,c(26,29,30,35,56)]
test[which(test$totalAllowedAmount > 100000), "totalAllowedAmount"] <- 100000
z <- scale(test, center = T, scale = T)
head(z)

## Kmeans Clustering
km.out <- kmeans(z, 6, nstart = 50)
km.out$size
print("There are 516 patients in Cluster 1, 229 patients in Cluster 2, 3807 patients in Cluster 3, 
      1436 patients in Cluster 4, and 12 patients in Cluster 5.")
z <- cbind(z, "Cluster" = km.out$cluster)
clusterCenters <- as.data.frame(km.out$centers)
clusterCenters

## Plot the values
plot(1:5, clusterCenters[1,], type = 'b', ylim = c(0, 18), col = 1, pch = 19,
     xlab = "Parameters", ylab = "Values (Cluster Center Coordinates)")
lines(1:5, clusterCenters[2,], type = 'b', col = 2, pch = 19)
lines(1:5, clusterCenters[3,], type = 'b', col = 3, pch = 19)
lines(1:5, clusterCenters[4,], type = 'b', col = 4, pch = 19)
lines(1:5, clusterCenters[5,], type = 'b', col = 6, pch = 19)
axis(side = 3, at = 1:5, labels = c("sickNum", "predictedDollarAmt", "numHospitalStay", "drugNum", "totalAllowedAmt"))
legend("topright", legend = c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5"),
       col = c(1:4, 6,8), lty = 1, lwd = 2, cex = 0.6)


## Another Segmentation Model Based on Difference between Predicted and Actual Medical Spend
testTbl <- test %>% group_by(cancerind, asthmaind, hypertenind, chfind, diabetesind, copdind) %>% 
  summarise(counts = n(), avgPredAmt = mean(pulsepredicteddollarsamt), avgTotalAmt = mean(totalAllowedAmount)) %>%
  mutate(Difference = avgTotalAmt - avgPredAmt)

