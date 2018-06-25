rm(list=ls())
require(dplyr)
insurance = read.csv("exploratory insurance.csv", sep = ',')
glimpse(insurance)
insurance <- mutate(insurance, totalEngage = AWV+OfficeVisit+Physical)
insurance <- mutate(insurance, totalAllowedAmt = TotalOPAllowedAmt+TotalProfAllowedAmt+TotalIPAllowedAmt)
insurance$Practice <- as.factor(insurance$Practice)
insurance$newPatient <- as.factor(insurance$newPatient)
insurance$HighRiskInd <- as.factor(insurance$HighRiskInd)
insurance$age1 <- as.factor(insurance$age1)
summary(insurance[, c("chronicind", "cancerind", "asthmaind", "hypertenind", "chfind", "diabetesind", "copdind")])



## Part 1
## chronic vs. Age
chronicByAge <- insurance %>%
  group_by(chronicind, age1) %>%
  summarise(counts = n()) %>%
  mutate (percent = counts/sum(counts))
chronicByAge

# ggplot(chronicByAge, aes(x = chronicind, y = counts, fill = age1))+
#   geom_bar(stat = "identity", width = 0.6) + ylab("Number of Patients") + xlab("Presence of Chronic Disease") +
#   ggtitle("Number of Patients in Each Age Group By Presence of Chronic Disease")
ggplot(chronicByAge, aes(x = age1, y = counts, fill = chronicind))+
  geom_bar(stat = "identity") + ylab("Number of Patients") + xlab("Age Group") +
  ggtitle("Presence of Chronic Disease By Age Group")

## chronic vs. race  
# chronicByRace <- insurance %>%
#   group_by(chronicind, racecode) %>%
#   summarise(counts = n()) %>%
#   mutate (percent = counts/sum(counts))
# chronicByRace
# 
# ggplot(chronicByRace, aes(x = racecode, y = counts, fill = chronicind))+
#   geom_bar(stat = "identity")

## chronic vs. Gender  
# chronicByGender <- insurance %>%
#   group_by(chronicind, sexcode) %>%
#   summarise(counts = n()) %>%
#   mutate (percent = counts/sum(counts))
# chronicByGender
# 
# ggplot(chronicByGender, aes(x = chronicind, y = counts, fill = sexcode))+
#   geom_bar(stat = "identity") + ylab("Number of Patients") + xlab("Presence of Chronic Disease") +
#   ggtitle("Gender By Presence of Chronic Disease")
# ggplot(chronicByGender, aes(x = sexcode, y = counts, fill = chronicind))+
#   geom_bar(stat = "identity") + ylab("Number of Patients") + xlab("Gender") +
#   ggtitle("Presence of Chronic Disease By Gender")

## chronic vs. High Risk
chronicByHighrisk <- insurance %>%
  group_by(chronicind, HighRiskInd) %>%
  summarise(counts = n()) %>%
  mutate (percent = counts/sum(counts))
chronicByHighrisk

# ggplot(chronicByHighrisk, aes(x = chronicind, y = counts, fill = HighRiskInd))+
#   geom_bar(stat = "identity", width = 0.6) + ylab("Number of Patients") + xlab("Presence of Chronic Disease") +
#   ggtitle("High Risk By Presence of Chronic Disease")
ggplot(chronicByHighrisk, aes(x = HighRiskInd, y = counts, fill = chronicind))+
  geom_bar(stat = "identity", width = 0.5) + ylab("Number of Patients") + xlab("High Risk") +
  ggtitle("Presence of Chronic Disease By High Risk")

## chronic vs. Total Engage
chronicByTotalEngage <- insurance %>%
  group_by(chronicind, totalEngage) %>%
  summarise(counts = n()) %>%
  mutate (percent = counts/sum(counts))
chronicByTotalEngage

# ggplot(chronicByTotalEngage, aes(x = chronicind, y = counts, fill = totalEngage))+
#   geom_bar(stat = "identity", width = 0.6) + ylab("Number of Patients") + xlab("Presence of Chronic Disease") +
#   ggtitle("Total Number of Engagement By Presence of Chronic Disease")
ggplot(chronicByTotalEngage, aes(x = totalEngage, y = counts, fill = chronicind))+
  geom_bar(stat = "identity") + ylab("Number of Patients") + xlab("Number of Total Engagement") +
  ggtitle("Presence of Chronic Disease By Total Number of Engagement")

# # Chronic vs. NewPatient
# chronicByNewPatient <- insurance %>%
#   group_by(chronicind, newPatient) %>%
#   summarise(counts = n()) %>%
#   mutate (percent = counts/sum(counts))
# chronicByNewPatient
# 
# ggplot(chronicByNewPatient, aes(x = chronicind, y = counts, fill = newPatient))+
#   geom_bar(stat = "identity")
# ggplot(chronicByNewPatient, aes(x = newPatient, y = counts, fill = chronicind))+
#   geom_bar(stat = "identity")



## Part 2
## Disease
temp1 <- insurance %>% group_by(cancerind) %>% summarise(counts = n())
temp2 <- insurance %>% group_by(asthmaind) %>% summarise(counts = n())
temp3 <- insurance %>% group_by(hypertenind) %>% summarise(counts = n())
temp4 <- insurance %>% group_by(chfind) %>% summarise(counts = n())
temp5 <- insurance %>% group_by(diabetesind) %>% summarise(counts = n())
temp6 <- insurance %>% group_by(copdind) %>% summarise(counts = n())

tbl <- data.frame()
tbl <- cbind(temp1[,2],temp2[,2],temp3[,2],temp4[,2],temp5[,2],temp6[,2])

tbl2 <- t(tbl)
rownames(tbl2) <- c("cancerind", "asthmaind", "hypertenind", "chfind", "diabetesind", "copdind")
colnames(tbl2) <- (c("No", "Yes"))
df <- data.frame(rownames(tbl2), tbl2[,2])
colnames(df) <- c("disease", "counts")
ggplot(df, aes(y=counts, x=disease)) + 
  geom_bar(position="dodge", stat="identity", fill = "#FF6666", width = 0.5) +
  ylab("Number of Patients") + xlab("Type of Disease") +
  ggtitle("Number of Patients By Type of Disease")

## Predicted Medical Spend
temp1 <- insurance %>% group_by(cancerind) %>% summarise(avgMedicalSpend = mean(pulsepredicteddollarsamt))
temp2 <- insurance %>% group_by(asthmaind) %>% summarise(avgMedicalSpend = mean(pulsepredicteddollarsamt))
temp3 <- insurance %>% group_by(hypertenind) %>% summarise(avgMedicalSpend = mean(pulsepredicteddollarsamt))
temp4 <- insurance %>% group_by(chfind) %>% summarise(avgMedicalSpend = mean(pulsepredicteddollarsamt))
temp5 <- insurance %>% group_by(diabetesind) %>% summarise(avgMedicalSpend = mean(pulsepredicteddollarsamt))
temp6 <- insurance %>% group_by(copdind) %>% summarise(avgMedicalSpend = mean(pulsepredicteddollarsamt))
tbl <- data.frame()
tbl <- cbind(temp1,temp2,temp3,temp4,temp5,temp6)
tbl2 <- t(tbl)
tbl2 <- tbl2[c(2,4,6,8,10,12),]
rownames(tbl2) <- c("cancerind", "asthmaind", "hypertenind", "chfind", "diabetesind", "copdind")
colnames(tbl2) <- (c("No", "Yes"))
disease <- c(rep("cancer",2), rep("asthma", 2), rep("hyperten", 2), rep("chf", 2), rep("diabetes", 2), rep("copd",2))
status <- c(rep(c("No", "Yes"), 6))
avgPredAmt <- c(716.61, 2039.37, 761.62, 1097.42, 626.79, 1189.10, 737.14, 5753.64, 709.48, 1659.58, 761.49, 1743.58)
df2 <- data.frame(disease, status, avgPredAmt)
ggplot(df2, aes(fill=status, y=avgPredAmt, x=disease)) + 
  geom_bar(position="dodge", stat="identity") +
  ylab("Avg. Predicted Spent") + xlab("Type of Disease") +
  ggtitle("Average Total Predicted Medical Spend By Type of Disease")

# ## Average Total OutPatient Allowed Amount
# temp1 <- insurance %>% group_by(chronicind) %>% summarise(AvgTotalOPAmt = mean(TotalOPAllowedAmt))
# temp2 <- insurance %>% group_by(cancerind) %>% summarise(AvgTotalOPAmt = mean(TotalOPAllowedAmt))
# temp3 <- insurance %>% group_by(asthmaind) %>% summarise(AvgTotalOPAmt = mean(TotalOPAllowedAmt))
# temp4 <- insurance %>% group_by(hypertenind) %>% summarise(AvgTotalOPAmt = mean(TotalOPAllowedAmt))
# temp5 <- insurance %>% group_by(chfind) %>% summarise(AvgTotalOPAmt = mean(TotalOPAllowedAmt))
# temp6 <- insurance %>% group_by(diabetesind) %>% summarise(AvgTotalOPAmt = mean(TotalOPAllowedAmt))
# temp7 <- insurance %>% group_by(copdind) %>% summarise(AvgTotalOPAmt = mean(TotalOPAllowedAmt))
# 
# disease <- c(rep("cancer",2), rep("asthma", 2), rep("hyperten", 2), rep("chf", 2), rep("diabetes", 2), rep("copd",2))
# status <- c(rep(c("No", "Yes"), 6))
# avgOPAmt <- c(1269.90, 3205.82, 1291.35, 2994.41, 1046.44, 2221.02, 1328.99, 4710.59, 1285.99, 2285.46, 1291.12, 6239.20)
# df3 <- data.frame(disease, status, avgOPAmt)
# ggplot(df3, aes(fill=status, y=avgOPAmt, x=disease)) + 
#   geom_bar(position="dodge", stat="identity")

# ## Avrage Total InPatient Allowed Amount
# temp1 <- insurance %>% group_by(chronicind) %>% summarise(AvgTotalIPAmt = mean(TotalIPAllowedAmt))
# temp2 <- insurance %>% group_by(cancerind) %>% summarise(AvgTotalIPAmt = mean(TotalIPAllowedAmt))
# temp3 <- insurance %>% group_by(asthmaind) %>% summarise(AvgTotalIPAmt = mean(TotalIPAllowedAmt))
# temp4 <- insurance %>% group_by(hypertenind) %>% summarise(AvgTotalIPAmt = mean(TotalIPAllowedAmt))
# temp5 <- insurance %>% group_by(chfind) %>% summarise(AvgTotalIPAmt = mean(TotalIPAllowedAmt))
# temp6 <- insurance %>% group_by(diabetesind) %>% summarise(AvgTotalIPAmt = mean(TotalIPAllowedAmt))
# temp7 <- insurance %>% group_by(copdind) %>% summarise(AvgTotalIPAmt = mean(TotalIPAllowedAmt))
# 
# avgIPAmt <- c(249.76, 1126.84, 642.79, 3511.33, 756.37, 1048.79, 542.52, 1400.76, 696.78, 10284.52, 711.15, 1535.80, 741.27, 2779.92)
# df4 <- data.frame(disease, status, avgIPAmt)
# ggplot(df3, aes(fill=status, y=avgIPAmt, x=disease)) + 
#   geom_bar(position="dodge", stat="identity")
# 
# ## Average Total Professional Allowed Amount
# temp1 <- insurance %>% group_by(chronicind) %>% summarise(AvgTotalProfAmt = mean(TotalProfAllowedAmt))
# temp2 <- insurance %>% group_by(cancerind) %>% summarise(AvgTotalProfAmt = mean(TotalProfAllowedAmt))
# temp3 <- insurance %>% group_by(asthmaind) %>% summarise(AvgTotalProfAmt = mean(TotalProfAllowedAmt))
# temp4 <- insurance %>% group_by(hypertenind) %>% summarise(AvgTotalProfAmt = mean(TotalProfAllowedAmt))
# temp5 <- insurance %>% group_by(chfind) %>% summarise(AvgTotalProfAmt = mean(TotalProfAllowedAmt))
# temp6 <- insurance %>% group_by(diabetesind) %>% summarise(AvgTotalProfAmt = mean(TotalProfAllowedAmt))
# temp7 <- insurance %>% group_by(copdind) %>% summarise(AvgTotalProfAmt = mean(TotalProfAllowedAmt))
# avgProfAmt <- c(1455.35, 3650.53, 2368.76, 11170.00, 2667.77, 4914.17, 2208.40, 4278.74, 2661.37, 14766.73, 2527.37, 5193.16, 2623.10, 12652.65)
# df5 <- data.frame(disease, status, avgProfAmt)
# ggplot(df5, aes(fill=status, y=avgProfAmt, x=disease)) + 
#   geom_bar(position="dodge", stat="identity")

## Average Total Allowed Amount
temp1 <- insurance %>% group_by(cancerind) %>% summarise(AvgTotalProfAmt = mean(TotalProfAllowedAmt))
temp2 <- insurance %>% group_by(asthmaind) %>% summarise(AvgTotalProfAmt = mean(TotalProfAllowedAmt))
temp3 <- insurance %>% group_by(hypertenind) %>% summarise(AvgTotalProfAmt = mean(TotalProfAllowedAmt))
temp4 <- insurance %>% group_by(chfind) %>% summarise(AvgTotalProfAmt = mean(TotalProfAllowedAmt))
temp5 <- insurance %>% group_by(diabetesind) %>% summarise(AvgTotalProfAmt = mean(TotalProfAllowedAmt))
temp6 <- insurance %>% group_by(copdind) %>% summarise(AvgTotalProfAmt = mean(TotalProfAllowedAmt))
avgTotalAmt <- c(2368.76, 11170, 2667.77, 4914.17, 2208.40, 4278.74, 2661.37, 14766.727, 2572.365, 5193.16, 2623.10, 12652.65)
df6 <- data.frame(disease, status, avgTotalAmt)
ggplot(df6, aes(fill=status, y=avgTotalAmt, x=disease)) + 
  geom_bar(position="dodge", stat="identity")  +
  ylab("Avg. Allowed Amount") + xlab("Type of Disease") +
  ggtitle("Average Total Allowed Amount By Type of Disease")

## Number of Hospital Stays
test <- insurance %>% filter(cancerind == "YES" & IPAdmissionsnum != 0) %>% 
  group_by(cancerind, IPAdmissionsnum) %>% summarise(counts = n())
test1 <- insurance %>% filter(cancerind == "YES") %>% summarise(counts = n())
test1.final <- (test[1,3]+test[2,3]+test[3,3]+test[4,3])/test1
test <- insurance %>% filter(asthmaind == "YES" & IPAdmissionsnum != 0) %>%
  group_by(asthmaind, IPAdmissionsnum) %>% summarise(counts = n())
test2 <- insurance %>% filter(asthmaind == "YES") %>% summarise(counts = n())
test2.final <- (test[1,3]+test[2,3])/test2
test <- insurance %>% filter(hypertenind == "YES" & IPAdmissionsnum != 0) %>%
  group_by(hypertenind, IPAdmissionsnum) %>% summarise(counts = n())
test3 <- insurance %>% filter(hypertenind == "YES") %>% summarise(counts = n())
test3.final <- (test[1,3]+test[2,3]+test[3,3]+test[4,3])/test3
test <- insurance %>% filter(chfind == "YES" & IPAdmissionsnum != 0) %>%
  group_by(chfind, IPAdmissionsnum) %>% summarise(counts = n())
test4 <- insurance %>% filter(chfind == "YES") %>% summarise(counts = n())
test4.final <- (test[1,3]+test[2,3]+test[3,3])/test4
test <- insurance %>% filter(diabetesind == "YES" & IPAdmissionsnum != 0) %>%
  group_by(diabetesind, IPAdmissionsnum) %>% summarise(counts = n())
test5 <- insurance %>% filter(diabetesind == "YES") %>% summarise(counts = n())
test5.final <- (test[1,3]+test[2,3]+test[3,3]+test[4,3])/test5
test <- insurance %>% filter(copdind == "YES" & IPAdmissionsnum != 0) %>%
  group_by(copdind, IPAdmissionsnum) %>% summarise(counts = n())
test6 <- insurance %>% filter(copdind == "YES") %>% summarise(counts = n())
test6.final <- (test[1,3]+test[2,3]+test[3,3])/test5

hospitalStays <- c()
hospitalStays <- rbind(test1.final, test2.final, test3.final, test4.final, test5.final,test6.final)
hospitalStays <- cbind(hospitalStays,c("Cancer","Asthma","Hypertension","Heart Failure","Diabetes","COPD"))
colnames(hospitalStays) <- c("Percentages","Disease")
ggplot(hospitalStays, aes(x=Disease, y=Percentages)) + 
  geom_bar(stat = "identity", fill = '#FF6666', width = 0.6) +
  ylab("Percentage of Patients with Hospital Admissions") + xlab("Type of Disease") +
  ggtitle("Percentage of Patients Admitted to Hospital")

## EDVisit
test <- insurance %>% filter(cancerind == "YES" & EDVisitsnum != 0) %>% 
  group_by(cancerind, EDVisitsnum) %>% summarise(counts = n())
test1 <- insurance %>% filter(cancerind == "YES") %>% summarise(counts = n())
test1.final <- (test[1,3]+test[2,3]+test[3,3]+test[4,3])/test1
test <- insurance %>% filter(asthmaind == "YES" & EDVisitsnum != 0) %>%
  group_by(asthmaind, EDVisitsnum) %>% summarise(counts = n())
test2 <- insurance %>% filter(asthmaind == "YES") %>% summarise(counts = n())
test2.final <- (test[1,3]+test[2,3]+test[3,3]+test[4,3])/test2
test <- insurance %>% filter(hypertenind == "YES" & EDVisitsnum != 0) %>%
  group_by(hypertenind, EDVisitsnum) %>% summarise(counts = n())
test3 <- insurance %>% filter(hypertenind == "YES") %>% summarise(counts = n())
test3.final <- (test[1,3]+test[2,3]+test[3,3]+test[4,3]+test[5,3]+test[6,3]+test[7,3]+test[8,3]+test[9,3])/test3
test <- insurance %>% filter(chfind == "YES" & EDVisitsnum != 0) %>%
  group_by(chfind, EDVisitsnum) %>% summarise(counts = n())
test4 <- insurance %>% filter(chfind == "YES") %>% summarise(counts = n())
test4.final <- (test[1,3]+test[2,3]+test[3,3]+test[4,3])/test4
test <- insurance %>% filter(diabetesind == "YES" & EDVisitsnum != 0) %>%
  group_by(diabetesind, EDVisitsnum) %>% summarise(counts = n())
test5 <- insurance %>% filter(diabetesind == "YES") %>% summarise(counts = n())
test5.final <- (test[1,3]+test[2,3]+test[3,3]+test[4,3]+test[5,3]+test[6,3])/test5
test <- insurance %>% filter(copdind == "YES" & EDVisitsnum != 0) %>%
  group_by(copdind, EDVisitsnum) %>% summarise(counts = n())
test6 <- insurance %>% filter(copdind == "YES") %>% summarise(counts = n())
test6.final <- (test[1,3]+test[2,3]+test[3,3]+test[4,3]+test[5,3])/test5

EDVisit <- c()
EDVisit <- rbind(test1.final, test2.final, test3.final, test4.final, test5.final,test6.final)
EDVisit <- cbind(EDVisit,c("Cancer","Asthma","Hypertension","Heart Failure","Diabetes","COPD"))
colnames(EDVisit) <- c("Percentages","Disease")
ggplot(EDVisit, aes(x=Disease, y=Percentages)) + 
  geom_bar(stat = "identity", fill = '#FF6666', width = 0.6) +
  ylab("Percentage of Patients with ER Visits") + xlab("Type of Disease") +
  ggtitle("Percentage of Patients with ER Visits")

## Avergae Total Engagement
temp2 <- insurance %>% group_by(cancerind) %>% summarise(AvgTotalEngage = mean(totalEngage))
temp3 <- insurance %>% group_by(asthmaind) %>% summarise(AvgTotalEngage = mean(totalEngage))
temp4 <- insurance %>% group_by(hypertenind) %>% summarise(AvgTotalEngage = mean(totalEngage))
temp5 <- insurance %>% group_by(chfind) %>% summarise(AvgTotalEngage = mean(totalEngage))
temp6 <- insurance %>% group_by(diabetesind) %>% summarise(AvgTotalEngage = mean(totalEngage))
temp7 <- insurance %>% group_by(copdind) %>% summarise(AvgTotalEngage = mean(totalEngage))
avgTotalEngage <- c(8.20, 9.62, 8.11, 12.22, 7.15, 11.40, 8.22, 14.14, 7.97, 12.22, 8.17, 14.79)
df6 <- data.frame(disease, status, avgTotalEngage)
ggplot(df6, aes(fill=status, y=avgTotalEngage, x=disease)) + 
  geom_bar(position="dodge", stat="identity")  +
  ylab("Avg. Number of Engagements") + xlab("Type of Disease") +
  ggtitle("Average Total Number of Engagement By Type of Disease")

## Avergae Number of Drug
temp2 <- insurance %>% group_by(cancerind) %>% summarise(AvgDrugNum = mean(UniqueDrugcountnum))
temp3 <- insurance %>% group_by(asthmaind) %>% summarise(AvgDrugNum = mean(UniqueDrugcountnum))
temp4 <- insurance %>% group_by(hypertenind) %>% summarise(AvgDrugNum = mean(UniqueDrugcountnum))
temp5 <- insurance %>% group_by(chfind) %>% summarise(AvgDrugNum = mean(UniqueDrugcountnum))
temp6 <- insurance %>% group_by(diabetesind) %>% summarise(AvgDrugNum = mean(UniqueDrugcountnum))
temp7 <- insurance %>% group_by(copdind) %>% summarise(AvgDrugNum = mean(UniqueDrugcountnum))
avgDrugNum <- c(0.94, 1.17, 0.93, 1.56, 0.68, 1.72, 0.94, 3.09, 0.88, 1.94, 0.92, 3.47)
df6 <- data.frame(disease, status, avgDrugNum)
ggplot(df6, aes(fill=status, y=avgDrugNum, x=disease)) + 
  geom_bar(position="dodge", stat="identity") +
  ylab("Avg. Number of Drug Prescriptions") + xlab("Type of Disease") +
  ggtitle("Average Number of Drugs Prescribed By Type of Disease")



## Part 3
crossTbl <- insurance %>% group_by(cancerind, asthmaind, hypertenind, chfind, diabetesind, copdind) %>% 
  summarise(counts = n(), avgPredAmt = mean(pulsepredicteddollarsamt), avgTotalAmt = mean(totalAllowedAmount)) %>%
  mutate(Difference = avgTotalAmt - avgPredAmt)