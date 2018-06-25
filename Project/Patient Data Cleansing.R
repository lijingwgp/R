rm(list=ls())
require(dplyr)
require(tibble)
require(tidyr)


## Load dataset
visit = read.csv("Visit.csv", sep = ',')    # this csv file the is original csv generated from visit history file 
demographics = read.csv("Demographics.csv", sep = ',')
chronicconditions = read.csv("Chronicconditions.csv", sep = ',')
providers = read.csv("Providers.csv", sep = ',')
payer = read.csv("Payerfile.csv", sep = ',')


## Join chronicconditions and demographics
## Create a patient profile table
chronicconditions$PatientID = as.numeric(chronicconditions$PatientID)
demographics$PatientID = as.numeric(demographics$PatientID)
profile = merge(demographics, chronicconditions, by = "PatientID", all = TRUE)


## Create a column that calculate total number of sickness a patient has
profile.tbl <- tbl_df(profile)
profile.tbl <- mutate(profile.tbl, sickNum = AfibInd+AsthmaInd+CADInd+CHFInd+COPDInd+DMInd+DYSInd
       +HTNInd+RAInd)


## Join providers and patient profile
providers$NPI = as.numeric(providers$NPI)
profile.tbl$NPI = as.numeric(profile.tbl$NPI)
profile.tbl = merge(profile.tbl, providers, by = "NPI", all = TRUE)
profile.tbl = profile.tbl[,c(2,1,3:25)]
print("Note that there are rows with NPI, but no patientID")
print("It means some doctors ID appeared in the provider table, but not in the demographics table")
profile.tbl = profile.tbl[-(which(is.na(profile.tbl$PatientID))),]


## Join payers and patient profile
payer$PatientID = as.numeric(payer$PatientID)
profile.tbl = merge(profile.tbl, payer, by = "PatientID", all = FALSE)
print("Note there are 10 patientID appeared in the demographics table but not in the payers table")
print("Some patientID appeared more than once due to the reason that some people have more than one payerID")
print("I manually insupected that all duplicated pairs of rows have the exact same records across all fileds except for the payerID column")
print("We decided to remove the duplicated observations")
profile.tbl = profile.tbl[!duplicated(profile.tbl$PatientID),]


## Join visit history with patient profile
visit$PatientID <- as.character(visit$PatientID)
visit.tbl <- tbl_df(visit)
visit.tbl.test <- visit.tbl %>%
  filter(Procedure %in% c("AWV", "OfficeVisit", "Physical")) %>%
  group_by(PatientID, Procedure) %>%
  summarise(counts = n())
visit.tbl.test2 <- spread(visit.tbl.test, Procedure, counts)
visit.tbl.test2$AWV[is.na(visit.tbl.test2$AWV)] = 0
visit.tbl.test2$OfficeVisit[is.na(visit.tbl.test2$OfficeVisit)] = 0
visit.tbl.test2$Physical[is.na(visit.tbl.test2$Physical)] = 0
visit.tbl.test2$PatientID <- as.numeric(visit.tbl.test2$PatientID)
profile.tbl <- merge(profile.tbl, visit.tbl.test2, by ="PatientID", all = FALSE)
print("Note that there are 18 records appear in the original patient profile table but not in the visit history table")


## Clean the patient profile accords to the assumptions
## Race column
profile.tbl$racecode <- as.character(profile.tbl$racecode)
lookup <- c("Black or African American" = "African American", "Asian Indian" = "Asian", 
            "Chinese" = "Asian", "Other Pacific Islander" = "Asian", "Pakistani" = "Asian", "English" = "Caucaisan", 
            "German" = "Caucaisan", "Polish" = "Caucaisan", "Healy Lake" = "Other Race", "Patient Declined" = "Other Race", 
            "American Indian or Alaska Native" = "Native American", "Mexican American Indian" = "Native American",
            "Other Race" = "Other Race", "Asian" = "Asian", "Black" = "African American", "White" = "Caucaisan")
profile.tbl$racecode <- lookup[profile.tbl$racecode]
profile.tbl$racecode[is.na(profile.tbl$racecode)] = "Other Race"
## Age
profile.tbl$age1 <- findInterval(profile.tbl$age, c(10,20,30,40,50,60,70,80,90))
## Remove rows with no NPI
profile.tbl <- profile.tbl[-(which(is.na(profile.tbl$NPI))),]
## LegacyID
profile.tbl <- profile.tbl %>% mutate(newPatient = case_when(
  is.na(LegacyID) ~ 1,
  TRUE ~ 0 
))
##asthmaind
profile.tbl$asthmaind <- as.character(profile.tbl$asthmaind)
profile.tbl$asthmaind <- na_if(profile.tbl$asthmaind, "")
profile.tbl <- profile.tbl %>% mutate(asthmaind = recode(asthmaind, .missing = "NO"))
##hypertenind
profile.tbl$hypertenind <- as.character(profile.tbl$hypertenind)
profile.tbl$hypertenind <- na_if(profile.tbl$hypertenind, "")
profile.tbl <- profile.tbl %>% mutate(hypertenind = recode(hypertenind, .missing = "NO"))
##chfind
profile.tbl$chfind <- as.character(profile.tbl$chfind)
profile.tbl$chfind <- na_if(profile.tbl$chfind, "")
profile.tbl <- profile.tbl %>% mutate(chfind = recode(chfind, .missing = "NO"))
##duabetesind
profile.tbl$diabetesind <- as.character(profile.tbl$diabetesind)
profile.tbl$diabetesind <- na_if(profile.tbl$diabetesind, "")
profile.tbl <- profile.tbl %>% mutate(diabetesind = recode(diabetesind, .missing = "NO"))
##copdind
profile.tbl$copdind <- as.character(profile.tbl$copdind)
profile.tbl$copdind <- na_if(profile.tbl$copdind, "")
profile.tbl <- profile.tbl %>% mutate(copdind = recode(copdind, .missing = "NO"))
## Distance columns
## Detect majority's choice of which doctor to choose
dist.tbl <- profile.tbl[,8:14]
actualPrac <- profile.tbl[,25]
dist.tbl <- cbind(dist.tbl, actualPrac)
dist.tbl <- na.omit(dist.tbl)
actualPrac <- dist.tbl[,8]
dist.tbl <- dist.tbl[,-8]
min_dist = c()
for (i in 1:nrow(dist.tbl)){
  dist_list <- dist.tbl[i,]
  min_dist[i] = which.min(dist_list)
}
sum(min_dist == actualPrac)/nrow(profile.tbl)
print("It is observed that there is about 53% of people that choose the closest practice office from their location")
## Prepare two datasets, one with no missing records in the distance columns
## The other one has missing records being replaced with values determined by using our assumptions
profile2 = profile.tbl[-(which(is.na(profile.tbl$Prac1Dist))),]
doc.location = profile.tbl$Practice[which(is.na(profile.tbl$Prac1Dist))]
dist.tbl <- profile.tbl[,8:14]
dist.tbl <- dist.tbl[which(is.na(profile.tbl$Prac1Dist)),]
for(i in 1:length(doc.location)){
  dist.tbl[i,doc.location[i]] <- 0
}
dist.tbl$Prac1Dist[is.na(dist.tbl$Prac1Dist)] = 3000
dist.tbl$Prac2Dist[is.na(dist.tbl$Prac2Dist)] = 3000
dist.tbl$Prac3Dist[is.na(dist.tbl$Prac3Dist)] = 3000
dist.tbl$Prac4Dist[is.na(dist.tbl$Prac4Dist)] = 3000
dist.tbl$Prac5Dist[is.na(dist.tbl$Prac5Dist)] = 3000
dist.tbl$Prac6Dist[is.na(dist.tbl$Prac6Dist)] = 3000
dist.tbl$Prac7Dist[is.na(dist.tbl$Prac7Dist)] = 3000
profile1 <- merge(profile.tbl, dist.tbl, by = "row.names", all = TRUE)
profile1$Prac1Dist.x[is.na(profile1$Prac1Dist.x)] = profile1$Prac1Dist.y[!is.na(profile1$Prac1Dist.y)]
profile1$Prac2Dist.x[is.na(profile1$Prac2Dist.x)] = profile1$Prac2Dist.y[!is.na(profile1$Prac2Dist.y)]
profile1$Prac3Dist.x[is.na(profile1$Prac3Dist.x)] = profile1$Prac3Dist.y[!is.na(profile1$Prac3Dist.y)]
profile1$Prac4Dist.x[is.na(profile1$Prac4Dist.x)] = profile1$Prac4Dist.y[!is.na(profile1$Prac4Dist.y)]
profile1$Prac5Dist.x[is.na(profile1$Prac5Dist.x)] = profile1$Prac5Dist.y[!is.na(profile1$Prac5Dist.y)]
profile1$Prac6Dist.x[is.na(profile1$Prac6Dist.x)] = profile1$Prac6Dist.y[!is.na(profile1$Prac6Dist.y)]
profile1$Prac7Dist.x[is.na(profile1$Prac7Dist.x)] = profile1$Prac7Dist.y[!is.na(profile1$Prac7Dist.y)]
write.csv(profile1,"patient profile with replaced distance.csv")

