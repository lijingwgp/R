# CHAPTER 2 SCRIPTS: DATA ANALYSIS USING GRAPHICAL DISPLAYS
###################################################
### boxplot
### put a boxplot and a histgram in a single 2x1 plot
### boxplot group by "ocean"
###################################################
data("USmelanoma", package = "HSAUR2")
boxplot(USmelanoma$mortality, ylab="mortality")
boxplot(USmelanoma$mortality, ylab="mortality", horizontal=TRUE)

par(mfrow=c(1,1))
lim <- range(USmelanoma$mortality) * c(0.9, 1.1)
boxplot(USmelanoma$mortality, ylim=lim, horizontal=TRUE, xlab="Mortality")
hist(USmelanoma$mortality, xlim = lim, xlab="", main="", axes=TRUE, ylab="")
axis(1)

boxplot(USmelanoma$mortality~USmelanoma$ocean, ylim=lim, xlab="Mortality")


###################################################
### define limits
### create density line
### draw histgram with density lines
### create legend
###################################################
xr = range(USmelanoma$mortality)* c(0.9, 1.1)
dyes=density(USmelanoma$mortality[USmelanoma$ocean=="yes"])
dno=density(USmelanoma$mortality[USmelanoma$ocean=="no"])
plot(dyes, lty=1, xlim=xr, ylim=c(0,0.018), main="")
lines(dno, lty=2)
legend("topleft", lty=1:2, legend=c("Coastal State","Land State"), bty="n")


###################################################
### create a scatterplot matrix
### subsetting a specific cluster of the data
### reorder mortality by decending
###################################################
scatterplotMatrix(~latitude+longitude+mortality | ocean, reg.line=lm, smooth=TRUE, spread=FALSE, span=0.5, diagonal='density',
                  by.groups=TRUE, data=USmelanoma)

d <- subset(USmelanoma, subset = latitude<35 & ocean == "yes")
ix = order(d$mortality, decreasing=TRUE) 
d[ix,] 


###################################################
### load the chinese health and family dataset
### create a bar chart for the variable "R_happy"
### create a stacked bar chart
### create a stacked bar chart which involves one categorical variable and one numeric variable
### create a conditional density plot
###################################################
data("CHFLS", package = "HSAUR2")
barplot(table(CHFLS$R_happy), xlab = "R_happy", ylab = "Frequency")
plot(R_happy ~ R_health, data=CHFLS)
plot(R_happy ~ log(R_income+1), data=CHFLS) 

par(mfrow=c(1,1))
plot(R_happy ~ log(R_income + 1), data = CHFLS)
cdplot(R_happy ~ log(R_income + 1), data = CHFLS)
xyplot(jitter(log(A_income + 0.5)) ~ jitter(log(R_income + 0.5)) | R_edu, data = CHFLS)



# EXERCISES
###################################################
### load the household data in HSAUR2
### create a new variable which is the sum of all four commodity groups
### create four new variables where each of the four commodity is divided by the total expenditure
### create scatterplots of each of the five variables plotted against the total expenditure
###################################################
data("household", package = "HSAUR2")
household$total <- with(household, housing+food+goods+service)
household$food_prop <- with(household, food/total)
household$goods_prop <- with(household, goods/total)
household$housing_prop <- with(household, housing/total)
household$service_prop <- with(household, service/total)

par(mfrow=c(2,1))
scatterplot(service_prop~total, reg.line=lm, smooth=TRUE, spread=TRUE, boxlplots=FALSE, span=0.5, xlab="Total Expenditures"
            , ylab = "Proportional Service Expenses", data = household)
scatterplot(food_prop~total, reg.line=lm, smooth=TRUE, spread=TRUE, boxlplots=FALSE, span=0.5, xlab="Total Expenditures"
            , ylab = "Proportional Food Expenses", data = household)
scatterplotMatrix(~service_prop+food_prop+housing_prop+goods_prop+total | gender, data = household)


###################################################
### load the suicides2 data from HSAUR2
### create side by side box plots for the data from different age groups
###################################################
data("suicides2", package = "HSAUR2")
View(suicides2)
par(mfrow=c(1,5))
boxplot(suicides2$A25.34, ylim=c(0,90), xlab="25-34", ylab="suicides per 100000")
boxplot(suicides2$A35.44, ylim=c(0,90), xlab="35-44")
boxplot(suicides2$A45.54, ylim=c(0,90), xlab="45-54")
boxplot(suicides2$A55.64, ylim=c(0,90), xlab="55-64")
boxplot(suicides2$A65.74, ylim=c(0,90), xlab="65-74")


###################################################
### load USstates data from HSAUR2
### construct a scatterplot matrix of the data labeling the points by state name
### construct a plot of life expectancy and homicide rate conditional on average per capita income
###################################################
data("USstates", package = "HSAUR2")
View(USstates)
par(mfrow=c(1,1))
plot(Graduates~Income, data=USstates)
text(Graduates~Income, rownames(USstates), pos=1, offset=0.5, cex=0.7, data = USstates)

USstates$apci <- with(USstates, Income/Population)
USstates$apci <- as.factor(with(USstates, ifelse(apci>(median(apci)),"high","low")))
xyplot(Homicide~Life.Expectancy | apci, data=USstates)

