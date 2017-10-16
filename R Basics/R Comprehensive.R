rm(list=ls())
#####################################################
### Functions
#####################################################
installIfAbsentAndLoad<-function(neededVector) {
  for(thispackage in neededVector) {
    if( ! require(thispackage, character.only = T) )
    { install.packages(thispackage)}
    require(thispackage, character.only = T)
  }
}
##############################
### Load required packages ###
##############################
needed <- c("corrplot", "xlsx", "RODBC", "jsonlite")
installIfAbsentAndLoad(needed)
library(corrplot)
library(xlsx)
library(RODBC)
library(jsonlite)
############################################
### Script for R-Lab: Level Setting in R ###
############################################
# Create a vector using the scan() function
mynumbers <- scan("VectorData.csv",sep=",")
mycountries <- scan("Countries.txt", sep="\t",what="character")
# Retrieve vector elements
newvec <- mycountries[c(2:5,7,90,194)]
newvec
length(newvec)
# The Stock Market Data File SandPReturns.txt contains daily
# returns for the S&P 500 stock index for roughly six years. 
# Each row contains the date, open, high, low, close, volume,
# adjusted close, and close lagged from 1 to 5 days. The file
# is tab-delimited.
#############################################
#### Read file and create a data         ####
#### frame, bypassing the transformation ####
#### of character variables to nominal   ####
#### factors                             ####
#############################################
SandP <- read.table("SandPReturns.txt", sep="\t", header=T, stringsAsFactors=F)
#############################################
#### Display and manipulate dataframes:  ####
#### names, subsetting, rownames, dates  ####
#### and time (POSIX), other type        #### 
#### conversions, factors - ordinal      ####               
#### and nominal                         ####
#############################################
head(SandP)
names(SandP) <- tolower(gsub(".", "", names(SandP), fixed=T))  #change all the names to lower case
str(SandP)                                                     #display the structure of the object
SandP$date <- as.Date(SandP$date, "%m/%d/%Y")                  #change chr variables to dates
###############################################
#### Change the data type of the           ####
#### timeofhigh variable to POSIXlt (this  #### 
#### is one of several date-time data      #### 
#### formats approved by international     ####
#### standards groups - it is used exactly #### 
#### the same as the as.Date() function,   #### 
#### except that the format string adds    #### 
#### a blank and then %H:%M at the end.    ####
###############################################
SandP$timeofhigh <- strptime(SandP$timeofhigh, "%m/%d/%Y %H:%M")
head(SandP)         #Display first few rows
head(SandP, 10)     #Display first 10 rows
tail(SandP, 3)      #Display last 3 rows
dim(SandP)
nrow(SandP)         #Use nrow() or dim() for dataframes...length() gives # of cols for a dataframe
summary(SandP)
###############################################
#### The pairs() function produces a       ####
#### matrix of scatter plots               #### 
####                                       #### 
#### The cor() function produces a matrix  ####
#### contains all of the pairwise          #### 
#### correlations among the predictors     #### 
#### in a data set                         #### 
###############################################
pairs(SandP[c(5, 9:13)])    
cor(SandP[c(5, 9:13)]) 
###############################################
#### Use the corrplot function to plot a   #### 
#### Correlation Matrix (Need to           #### 
#### install.packages("corrplot")          ####
###############################################
corrplot(cor(SandP[c(5, 9:13)]), mar=c(0, 0, 1, 0))
plot(SandP$volume)
##########  Recoding  #########################
#### Add a factor named direction with     #### 
#### levels Down and Up indicating whether #### 
#### the market had a positive or          ####
#### negative return that day.             ####
###############################################
SandP$direction[SandP$close > SandP$lag1] <- "Up"
SandP$direction[SandP$close < SandP$lag1] <- "Down"
SandP$direction[SandP$close == SandP$lag1] <- "No Change"
SandP$direction <- factor(SandP$direction, ordered=TRUE, levels=c("Down","Up"))
str(SandP)
head(SandP)
nrow(SandP[SandP$direction == "No Change",])
#################################
#### Table operations ###########
#################################
actuals <- read.table("DirectionActuals.txt", header=T)
predicted <- read.table("DirectionPredicted.txt", header=T)
head(actuals)
head(predicted)
#### The table() function returns a matrix (not a dataframe, so need a comma to specity a column, as in mytable{,"Down"])
mytable1 <- table(actuals[,1], predicted[,1])
mytable1
#### Compute the error various ways
(mytable1["Up", "Down"] + mytable1["Down", "Up"]) / sum(mytable1)
#### Checks
sum(predicted[1] != actuals) / nrow(actuals)
1 - mean(actuals == predicted[1])
mean(actuals != predicted[1])
#Compute the success rate 3 ways
(mytable1["Up", "Up"] + mytable1["Down", "Down"]) / sum(mytable1)
mean(actuals == predicted[1])
1 - mean(actuals != predicted[1])
#Types of errors
#################################
#### Types of errors  ###########
#################################
#### False Positives (a.k.a. Type 1 errors)
mytable1["Down", "Up"]
#### False Positive Error Rate (a.k.a. 1-Specificity)
mytable1["Down", "Up"] / sum(mytable1["Down",])
#### False Negatives (a.k.a. Type 2 errors)
mytable1["Up", "Down"]
#### False Negative Error Rate (a.k.a. 1-Sensitivity)
mytable1["Up", "Down"] / sum(mytable1["Up",])
#### Specificity: 1-False Positive Error Rate
1 - mytable1["Down", "Up"] / sum(mytable1["Down",])
#### Power (a.k.a. Sensitivity, Recall): 1-False Negative Error Rate
1 - mytable1["Up", "Down"] / sum(mytable1["Up",])
#### Other measures (Negative prediction proportion, precision)
#### Negative prediction proportion
mytable1["Down", "Down"] / sum(mytable1[, "Down"])
#### Precision
mytable1["Up", "Up"] / sum(mytable1[, "Up"])
#######################
### Comparing models ##            
#######################
mytable2 <- table(actuals[,1], predicted[,2])
mytable3 <- table(actuals[,1], predicted[,3])
# Model 1 error rate
(mytable1["Up", "Down"] + mytable1["Down", "Up"])/sum(mytable1)
# Model 2 error rate
(mytable2["Up","Down"] + mytable2["Down", "Up"]) / sum(mytable2)
# Model 3 error rate
(mytable3["Up", "Down"] + mytable3["Down", "Up"]) / sum(mytable3)
# Which model is better (trick question!)
###########################################################################################
## Partitioning a dataframe into train, validate, test sets of specified relative sizes ###
###########################################################################################
set.seed(5072)             #sets the random seed
nobs <- nrow(SandP) 
trainprop <- 0.7           #say we want a 70/20/10 split of rows for training, validation and test respectively
validateprop <- 0.2
#create a vector of random integers of training size from the vector 1:nobs
train  <-  sample(nobs, trainprop * nobs)
#create a vector of the remaining  integers, then create a vector of random integers
#of validate size by sampling from these
validate  <-  sample(setdiff(1:nobs, train), validateprop * nobs) 
#create a vector of the integers not in either training or validate
test <- setdiff(setdiff(1:nobs, train), validate)
#Create the data frames using the indices created in the three vectors above
trainset <- SandP[train,]
validateset <- SandP[validate,]
testset <- SandP[test,]
#Checks
nobs
nrow(trainset) / nobs
nrow(validateset) / nobs
nrow(testset) / nobs
set.seed(NULL)             #remove the random seed 
########################################################################
#### Reading Data from Excel, SQL Server and JSON File Structures ######
########################################################################
### From Excel
myframe  <-  read.xlsx("SomeData.xlsx", sheetName="Data", startRow=2) 
head(myframe)
nrow(myframe)
### From SQL Server
### Need to set up DSN in Windows...Control Panel,System, Admin Tools, ODBC Data Sources (64-bit), 
### System DSN, Add button, SQL Server, Finish button and go through dialog - choose a name to use 
### in the following command:
myconn  <- odbcConnect("myRDataSQLServer")
demandstats  <-  sqlFetch(myconn, "DemandStatsFrontList")   #Reads a database table and creates a data frame
str(demandstats)
head(demandstats)
myQuery = "SELECT Products.Name AS 'ProductFamily', SUM(resultsCustomerDemandScenario_1.Quantity) AS 'TotalDemand'
FROM resultsCustomerDemandScenario_1 INNER JOIN Products ON resultsCustomerDemandScenario_1.Product = Products.Product
GROUP BY Products.Name, Products.Product Order By Products.Product"
anotherTable <- sqlQuery(myconn, myQuery)    #Executes the SQL provided and creates a data frame
anotherTable
close(myconn)     #Good hygene
### From JSON
mydataframe <- fromJSON("data.json.txt")
head(mydataframe)
#################################
#### Basic Plotting #############
#################################
salesdata <- read.table("Advertising.csv", sep=",", header=T)
attach(salesdata)
###Set up a 1x3 graphics window
par(mfrow=c(1, 3))
###Plot TV versus Sales ###
plot(TV, Sales)
abline(lm(Sales ~ TV))
###Plot Newspaper versus Sales ###
plot(Newspaper,Sales)
abline(lm(Sales ~ Newspaper))
###Plot Radio versus Sales ###
plot(Radio, Sales)
abline(lm(Sales ~ Radio))
detach(salesdata)
#Reset the graphics window to 1x1
par(mfrow=c(1, 2))
attach(myframe)
## Histogram
hist(maxMeanDemandAtPeak, col="blue")
## Box Plots
plot(Case,maxMeanDemandAtPeak,col="red", "blue")
detach(myframe)
search()
par(mfrow=c(1, 1))
####################################
#### Extract data from a list -   ##
#### named elements and otherwise ##
####################################
mylist <- lm(mtcars$mpg ~ mtcars$hp)
str(mylist)
mylist[1]            #The entire first list element, as one object
mylist[1][2]         #Can't refer to second item...it's one single thing
str(mylist[1])       #It's a list
mylist[[1]]          #The contents of the first list element
str(mylist[[1]])     #The list elements
mylist[[1]][2]       #The second item in the first list element
mylist[3:5]          #A subset of the list
str(mylist[3:5][3])  #It is also a list
mylist[3:5][[3]][22] #The 22nd item in the list
#### Using names
mtcars$mpg - mylist$fitted.values   #displays the residuals
mylist$coefficients["mtcars$hp"]
