rm(list=ls())
require(RcmdrMisc)
require(dplyr)
require(ggplot2)
require(lattice)
patient = read.csv("exploratory demographics.csv", sep = ',')
patient = patient[,-c(1,2)]
str(patient)
patient <- within(patient, {
  age1 <- factor(age1, labels=c('0','1','2','3','4','5','6','7','8'), ordered = FALSE)
})
patient <- within(patient, {
  newPatient <- factor(newPatient, labels=c('Current','New'), ordered = FALSE)
})
patient <- within(patient, {
  HighRiskInd <- factor(HighRiskInd, labels=c('No','Yes'), ordered = FALSE)
})
patient <- tbl_df(patient)
patient <- mutate(patient, totalEngage = OfficeVisit+Physical+AWV)
str(patient)



## Basic demographic information
## Racial background
local({
  race <- patient %>% group_by(racecode) %>% summarise(counts = n()) %>%
    mutate(percent = round(counts/sum(counts)*100))
  print(race)
  ggplot(race, aes(x = racecode, y = percent))+
    geom_bar(stat="identity", fill = "#FF6666", width = 0.5) + ylab("% of Population") + xlab("Racial Background") +
    ggtitle("% of Population Grouped By Racial Background")+ geom_text(aes(label = percent, y = percent + 0.05),position = position_dodge(0.9),
                                                                       vjust = -0.2)
})
## Gender
local({
  gender <- patient %>% group_by(sexcode) %>% summarise(counts = n()) %>%
    mutate(percent = round(counts/sum(counts)*100))
  print(gender)
  ggplot(gender, aes(x = sexcode, y = percent))+
    geom_bar(stat="identity", fill = "#FF6666", width = 0.4) + ylab("% of Population") + xlab("Gender") +
    ggtitle("% of Population Grouped By Gender")+ geom_text(aes(label = percent, y = percent + 0.05),position = position_dodge(0.9),
                                                            vjust = -0.2)
})
## Age
local({
  age <- patient %>% group_by(age1) %>% summarise(counts = n()) %>%
    mutate(percent = round(counts/sum(counts)*100))
  print(age)
  ggplot(age, aes(x = age1, y = percent))+
    geom_bar(stat="identity", fill = "#FF6666") + ylab("% of Population") + xlab("Age Group") +
    ggtitle("% of Population Grouped By Age Group")+ geom_text(aes(label = percent, y = percent + 0.05),position = position_dodge(0.9),
                                                               vjust = -0.2)
})
## High risk population
local({
  highrisk <- patient %>% group_by(HighRiskInd) %>% summarise(counts = n()) %>%
    mutate(percent = round(counts/sum(counts)*100))
  print(highrisk)
  ggplot(highrisk, aes(x = HighRiskInd, y = percent))+
    geom_bar(stat="identity", fill = "#FF6666", width = 0.5) + ylab("% of Population") + xlab("High Risk") +
    ggtitle("% of Population Grouped by High Risk Indicator") + geom_text(aes(label = percent, y = percent + 0.05),position = position_dodge(0.9),
                                                                              vjust = -0.2)
})
## New patient population
local({
  newpatient <- patient %>% group_by(newPatient) %>% summarise(counts = n()) %>%
    mutate(percent = round(counts/sum(counts)*100))
  print(newpatient)
  ggplot(newpatient, aes(x = newPatient, y = percent))+
    geom_bar(stat="identity", fill = "#FF6666", width = 0.5) + ylab("% of Population") + xlab("New Patient") +
    ggtitle("% of Population Grouped by New Patient Indicator") + geom_text(aes(label = percent, y = percent + 0.05),position = position_dodge(0.9),
                                                                            vjust = -0.2)
})



## Part 1
## More sickness indicates high-risk
local({
  sickness.highrisk <- patient %>% group_by(sickNum,HighRiskInd) %>% summarise(counts = n()) %>%
    mutate(percent = counts/sum(counts))
  print(sickness.highrisk)
  ggplot(sickness.highrisk, aes(x = sickNum, y = counts, fill = HighRiskInd))+
    geom_bar(stat="identity") + ylab("Number of Population") + xlab("Number of Sickness") +
    ggtitle("More sickness indicates more likely being identified as high-risk")
})
## More female are in the high-risk group
local({
  gender.highrisk <- patient %>% group_by(HighRiskInd,sexcode) %>% summarise(counts = n()) %>%
    mutate(percent = counts/sum(counts))
  print(gender.highrisk)
  ggplot(gender.highrisk, aes(x = HighRiskInd, y = counts, fill = sexcode))+
    geom_bar(stat="identity", width = 0.45) + ylab("Number of Population") + xlab("High-Risk Indicator") +
    ggtitle("More female are in the high-risk group")
})
## There are more female than male when it comes to engage with a doctor
local({
  engage.gender <- patient %>% group_by(totalEngage,sexcode) %>% summarise(counts = n()) %>%
    mutate(percent = counts/sum(counts))
  print(engage.gender, n=40)
  ggplot(engage.gender, aes(x = totalEngage, y = counts, fill = sexcode))+
    geom_bar(stat="identity") + ylab("Number of Population") + xlab("Number of Engagement") +
    ggtitle("More female when engagement increases")
  
})
## High risk patients tends to engage more often with the doctor than non high risk patients
## Especially when a high risk patient who has more sickness
xyplot(totalEngage ~ sickNum | HighRiskInd, groups=HighRiskInd, type="p", 
       pch=16, auto.key=list(border=TRUE), par.settings=simpleTheme(pch=16), 
       scales=list(x=list(relation='same'), y=list(relation='same')), data=patient, col = c("blue","green"))
## As patients get older, they would have more sickness and
## possibly they will engage more often with the doctor
local({
  .Table <- with(patient, table(sickNum,age1))
  cat("\nFrequency table:\n")
  rownames(.Table) <- c("Sicknum 0","Sicknum 1","Sicknum 2","Sicknum 3","Sicknum 4","Sicknum 5","Sicknum 6")
  colnames(.Table) <- c("Age 0","Age 1","Age 2","Age 3","Age 4","Age 5","Age 6","Age 7","Age 8")
  print(.Table)
  cat("\nPercentages by column:\n")
  temp <- cbind(round(100*.Table[,1]/sum(.Table[,1])),round(100*.Table[,2]/sum(.Table[,2])),round(100*.Table[,3]/sum(.Table[,3])),
                round(100*.Table[,4]/sum(.Table[,4])),round(100*.Table[,5]/sum(.Table[,5])),round(100*.Table[,6]/sum(.Table[,6])),
                round(100*.Table[,7]/sum(.Table[,7])),round(100*.Table[,8]/sum(.Table[,8])),round(100*.Table[,9]/sum(.Table[,9])))
  colnames(temp) <- c("Age 0","Age 1","Age 2","Age 3","Age 4","Age 5","Age 6","Age 7","Age 8")
  rownames(temp) <- c("Sicknum 0","Sicknum 1","Sicknum 2","Sicknum 3","Sicknum 4","Sicknum 5","Sicknum 6")
  print(temp)
})
with(patient, plotMeans(sickNum, age1, error.bars="se", connect=TRUE, main = "Avg. Number of Diseases as Age Increases"))
with(patient, plotMeans(totalEngage, age1, error.bars="se", connect=TRUE, main = "Avg. Number of Engagements as Age Increases"))



## Part 2
## Number of patients that each practice office receives
provider.tbl <- patient %>% group_by(Practice) %>% summarise(count=n())
provider.tbl <- cbind(provider.tbl, round(100*provider.tbl[,2]/sum(provider.tbl[,2]),2))
colnames(provider.tbl) <- c("Practice","Patient","Percent")
##
dist.tbl <- patient[,11:17]
actualPrac <- patient[,28]
dist.tbl <- cbind(dist.tbl, actualPrac)
dist.tbl <- na.omit(dist.tbl)
actualPrac <- dist.tbl[,8]
dist.tbl <- dist.tbl[,-8]
min_dist = c()
for (i in 1:nrow(dist.tbl)){
  dist_list <- dist.tbl[i,]
  min_dist[i] = which.min(dist_list)
}
test <- rep(0, nrow(dist.tbl))
test[min_dist==1] <- 1
sum(test == actualPrac)/length(which(actualPrac == 1))
print("48% of patient choose practice 1 office because that's the closest to them")
test <- rep(0, nrow(dist.tbl))
test[min_dist==2] <- 2
sum(test == actualPrac)/length(which(actualPrac == 2))
print("69% of patient choose practice 2 office because that's the closest to them")
test <- rep(0, nrow(dist.tbl))
test[min_dist==3] <- 3
sum(test == actualPrac)/length(which(actualPrac == 3))
print("65% of patient choose practice 3 office because that's the closest to them")
test <- rep(0, nrow(dist.tbl))
test[min_dist==4] <- 4
sum(test == actualPrac)/length(which(actualPrac == 4))
print("96% of patient choose practice 4 office because that's the closest to them")
test <- rep(0, nrow(dist.tbl))
test[min_dist==5] <- 5
sum(test == actualPrac)/length(which(actualPrac == 5))
print("81% of patient choose practice 5 office because that's the closest to them")
test <- rep(0, nrow(dist.tbl))
test[min_dist==6] <- 6
sum(test == actualPrac)/length(which(actualPrac == 6))
print("0% of patient choose practice 6 office because that's the closest to them")
test <- rep(0, nrow(dist.tbl))
test[min_dist==7] <- 7
sum(test == actualPrac)/length(which(actualPrac == 7))
print("40% of patient choose practice 7 office because that's the closest to them")
## 
patient$HighRiskInd = factor(patient$HighRiskInd, labels=c('0','1'), ordered = FALSE)
patient$HighRiskInd = as.numeric(as.character(patient$HighRiskInd))
highrisk.tbl <- patient %>% group_by(Practice) %>% summarise(count=sum(HighRiskInd))
highrisk.tbl <- cbind(highrisk.tbl, round(100*highrisk.tbl[,2]/sum(highrisk.tbl[,2]),2))
colnames(highrisk.tbl) <- c("Practice","High-Risk Patient","Percent")
##
patient$HighRiskInd = factor(patient$newPatient, labels=c('0','1'), ordered = FALSE)
patient$newPatient = as.numeric(as.character(patient$newPatient))
newpatient.tbl <- patient %>% group_by(Practice) %>% summarise(count=sum(newPatient))
newpatient.tbl <- cbind(newpatient.tbl, round(100*newpatient.tbl[,2]/sum(newpatient.tbl[,2]),2))
colnames(highrisk.tbl) <- c("Practice","New Patient","Percent")



## Backup information about demographics
# ## race vs. high risk
# local({
#   .Table <- xtabs(~racecode+HighRiskInd, data=patient)
#   cat("\nFrequency table:\n")
#   colnames(.Table) <- c("Not At-Risk","At-Risk")
#   a <- barchart(.Table, col = c("light blue","light green"))
#   print(a)
#   print(.Table)
#   cat("\nPercentages by column:\n")
#   temp <- cbind(round(100*.Table[,1]/sum(.Table[,1])),round(100*.Table[,2]/sum(.Table[,2])))
#   colnames(temp) <- c("Not At-Risk","At-Risk")
#   print(temp)
# })
# ## age vs. high risk
# local({
#   .Table <- xtabs(~age1+HighRiskInd, data=patient)
#   cat("\nFrequency table:\n")
#   colnames(.Table) <- c("Not At-Risk","At-Risk")
#   rownames(.Table) <- c("Age 0","Age 1","Age 2","Age 3","Age 4","Age 5","Age 6","Age 7","Age 8")
#   a <- barchart(.Table, col = c("light blue","light green"))
#   print(a)
#   print(.Table)
#   cat("\nPercentages by column:\n")
#   temp <- cbind(round(100*.Table[,1]/sum(.Table[,1])),round(100*.Table[,2]/sum(.Table[,2])))
#   colnames(temp) <- c("Not At-Risk","At-Risk")
#   rownames(temp) <- c("Age 0","Age 1","Age 2","Age 3","Age 4","Age 5","Age 6","Age 7","Age 8")
#   print(temp)
# })
# ## new patient vs. high risk
# local({
#   .Table <- xtabs(~newPatient+HighRiskInd, data=patient)
#   cat("\nFrequency table:\n")
#   colnames(.Table) <- c("Not At-Risk","At-Risk")
#   rownames(.Table) <- c("Current","New")
#   a <- barchart(.Table, col = c("light blue", "light green"))
#   print(a)
#   print(.Table)
#   cat("\nPercentages by column:\n")
#   temp <- cbind(round(100*.Table[,1]/sum(.Table[,1])),round(100*.Table[,2]/sum(.Table[,2])))
#   colnames(temp) <- c("Not At-Risk","At-Risk")
#   rownames(temp) <- c("Current","New")
#   print(temp)
# })
# ## age vs. new patient
# local({
#   .Table <- xtabs(~age1+newPatient, data=patient)
#   cat("\nFrequency table:\n")
#   colnames(.Table) <- c("Current","New")
#   rownames(.Table) <- c("Age 0","Age 1","Age 2","Age 3","Age 4","Age 5","Age 6","Age 7","Age 8")
#   a<- barchart(.Table, col = c("light blue", "light green"))
#   print(a)
#   print(.Table)
#   cat("\nPercentages by column:\n")
#   temp <- cbind(round(100*.Table[,1]/sum(.Table[,1])),round(100*.Table[,2]/sum(.Table[,2])))
#   colnames(temp) <- c("Current","New")
#   rownames(temp) <- c("Age 0","Age 1","Age 2","Age 3","Age 4","Age 5","Age 6","Age 7","Age 8")
#   print(temp)
# })
# ## gender vs. new patient
# local({
#   .Table <- xtabs(~sexcode+newPatient, data=patient)
#   cat("\nFrequency table:\n")
#   colnames(.Table) <- c("Current","New")
#   a<- barchart(.Table, col = c("light blue", "light green"))
#   print(a)
#   print(.Table)
#   cat("\nPercentages by column:\n")
#   temp <- cbind(round(100*.Table[,1]/sum(.Table[,1])),round(100*.Table[,2]/sum(.Table[,2])))
#   colnames(temp) <- c("Current","New")
#   print(temp)
# })
# ## race vs. new patient
# local({
#   .Table <- xtabs(~racecode+newPatient, data=patient)
#   cat("\nFrequency table:\n")
#   colnames(.Table) <- c("Current","New")
#   a<- barchart(.Table, col = c("light blue", "light green"))
#   print(a)
#   print(.Table)
#   cat("\nPercentages by column:\n")
#   temp <- cbind(round(100*.Table[,1]/sum(.Table[,1])),round(100*.Table[,2]/sum(.Table[,2])))
#   colnames(temp) <- c("Current","New")
#   print(temp)
# })
# ## sickNum vs. newPatient
# local({
#   .Table <- with(patient, table(sickNum,newPatient))
#   cat("\nCounts:\n")
#   colnames(.Table) <- c("Current","New")
#   rownames(.Table) <- c("Sicknum 0","Sicknum 1","Sicknum 2","Sicknum 3","Sicknum 4","Sicknum 5","Sicknum 6")
#   a <- barchart(.Table, col = c("light blue","light green"))
#   print(a)
#   print(.Table)
#   cat("\nPercentages by column:\n")
#   temp <- cbind(round(100*.Table[,1]/sum(.Table[,1])),round(100*.Table[,2]/sum(.Table[,2])))
#   colnames(temp) <- c("Current","New")
#   rownames(temp) <- c("Sicknum 0","Sicknum 1","Sicknum 2","Sicknum 3","Sicknum 4","Sicknum 5","Sicknum 6")
#   print(temp)
# })
# ## sickNum vs. gender
# local({
#   .Table <- with(patient, table(sickNum,sexcode))
#   cat("\nFrequency table:\n")
#   rownames(.Table) <- c("Sicknum 0","Sicknum 1","Sicknum 2","Sicknum 3","Sicknum 4","Sicknum 5","Sicknum 6")
#   colnames(.Table) <- c("Female","Male")
#   print(.Table)
#   cat("\nPercentages by column:\n")
#   temp <- cbind(round(100*.Table[,1]/sum(.Table[,1])),round(100*.Table[,2]/sum(.Table[,2])))
#   colnames(temp) <- c("Female","Male")
#   rownames(temp) <- c("Sicknum 0","Sicknum 1","Sicknum 2","Sicknum 3","Sicknum 4","Sicknum 5","Sicknum 6")
#   print(temp)
# })
# ## sickNum vs. race
# local({
#   .Table <- with(patient, table(sickNum,racecode))
#   cat("\nFrequency table:\n")
#   rownames(.Table) <- c("Sicknum 0","Sicknum 1","Sicknum 2","Sicknum 3","Sicknum 4","Sicknum 5","Sicknum 6")
#   print(.Table)
#   cat("\nPercentages by column:\n")
#   temp <- cbind(round(100*.Table[,1]/sum(.Table[,1])),round(100*.Table[,2]/sum(.Table[,2])),round(100*.Table[,3]/sum(.Table[,3])),
#                 round(100*.Table[,4]/sum(.Table[,4])),round(100*.Table[,5]/sum(.Table[,5])))
#   rownames(temp) <- c("Sicknum 0","Sicknum 1","Sicknum 2","Sicknum 3","Sicknum 4","Sicknum 5","Sicknum 6")
#   colnames(temp) <- c("American Native","Asian","Black","Other Race","White")
#   print(temp)
# })
# ## totalEngage vs. age
# local({
#   .Table <- with(patient, table(totalEngage,age1))
#   cat("\nFrequency table:\n")
#   colnames(.Table) <- c("Age 0","Age 1","Age 2","Age 3","Age 4","Age 5","Age 6","Age 7","Age 8")
#   print(.Table)
#   cat("\nPercentages by column:\n")
#   temp <- cbind(round(100*.Table[,1]/sum(.Table[,1])),round(100*.Table[,2]/sum(.Table[,2])),round(100*.Table[,3]/sum(.Table[,3])),
#                 round(100*.Table[,4]/sum(.Table[,4])),round(100*.Table[,5]/sum(.Table[,5])),round(100*.Table[,6]/sum(.Table[,6])),
#                 round(100*.Table[,7]/sum(.Table[,7])),round(100*.Table[,8]/sum(.Table[,8])),round(100*.Table[,9]/sum(.Table[,9])))
#   colnames(temp) <- c("Age 0","Age 1","Age 2","Age 3","Age 4","Age 5","Age 6","Age 7","Age 8")
#   print(temp)
# })
# ## totalEngage vs. race
# local({
#   .Table <- with(patient, table(totalEngage,racecode))
#   cat("\nFrequency table:\n")
#   print(.Table)
#   cat("\nPercentages by column:\n")
#   temp <- cbind(round(100*.Table[,1]/sum(.Table[,1])),round(100*.Table[,2]/sum(.Table[,2])),round(100*.Table[,3]/sum(.Table[,3])),
#                 round(100*.Table[,4]/sum(.Table[,4])),round(100*.Table[,5]/sum(.Table[,5])))
#   colnames(temp) <- c("American Native","Asian","Black","Other Race","White")
#   print(temp)
# })
# ## totalEngage vs. high risk
# local({
#   .Table <- with(patient, table(totalEngage,HighRiskInd))
#   cat("\nCounts:\n")
#   colnames(.Table) <- c("Not At-Risk","At-Risk")
#   print(.Table)
#   cat("\nPercentages by column:\n")
#   temp <- cbind(round(100*.Table[,1]/sum(.Table[,1])),round(100*.Table[,2]/sum(.Table[,2])))
#   colnames(temp) <- c("Not At-Risk","At-Risk")
#   print(temp)
# })
# ## totalEngage vs. new patient
# local({
#   .Table <- with(patient, table(totalEngage,newPatient))
#   cat("\nCounts:\n")
#   colnames(.Table) <- c("Current","New")
#   print(.Table)
#   cat("\nPercentages by column:\n")
#   temp <- cbind(round(100*.Table[,1]/sum(.Table[,1])),round(100*.Table[,2]/sum(.Table[,2])))
#   colnames(temp) <- c("Current","New")
#   print(temp)
# })



## Backup graphics
# with(patient, discretePlot(sickNum, by=HighRiskInd, scale="percent"))
# with(patient, discretePlot(sickNum, by=newPatient, scale="frequency"))
# with(patient, discretePlot(totalEngage, by=HighRiskInd, scale="percent"))
# densityPlot(totalEngage~HighRiskInd, data=patient, bw="SJ", adjust=1, 
#             kernel="gaussian")
# with(patient, discretePlot(totalEngage, by=newPatient, scale="percent"))
# densityPlot(totalEngage~newPatient, data=patient, bw="SJ", adjust=1, 
#             kernel="gaussian")
# xyplot(totalEngage ~ sickNum | newPatient, groups=newPatient, type="p", 
#        pch=16, auto.key=list(border=TRUE), par.settings=simpleTheme(pch=16), 
#        scales=list(x=list(relation='same'), y=list(relation='same')), data=patient, col = c("blue","green"))
# stripchart(totalEngage ~ racecode, vertical=TRUE, method="jitter", 
#            ylab="total engagement", data=patient)



# ## Backup tables
# sick.tbl <- patient %>% group_by(Practice) %>% summarise(total = sum(sickNum))
# sick.tbl <- cbind(sick.tbl, round(100*sick.tbl[,2]/sum(sick.tbl[,2]),2))
# colnames(sick.tbl) <- c("Practice","Sickness","Percent")
# engage.tbl <- patient %>% group_by(Practice) %>% summarise(totalEngage = sum(totalEngage))
# engage.tbl <- cbind(engage.tbl, round(100*engage.tbl[,2]/sum(engage.tbl[,2]),2))
# colnames(engage.tbl) <- c("Practice","Engagement","Percent")
