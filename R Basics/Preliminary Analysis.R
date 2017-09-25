## DATA OBJECTS IN R
###################################################
### make data object "Forbes2000" (data frame) available
### list objects in workspace:
### print to default device Forbes2000 data frame
### what is structure of columns (variables) in Forbes 2000?
### what is name of class of Forbes2000 object?
### What is the dimension?
### How many rows? How many columns?
### what are names of variables in Forbes2000?
###################################################
data("Forbes2000", package = "HSAUR2")
ls()
print(Forbes2000)
str(Forbes2000)
class(Forbes2000)
dim(Forbes2000)
nrow(Forbes2000)
ncol(Forbes2000)
names(Forbes2000)


###################################################
### what is the class of a single variable?
### how many unique entries "rank"?
###################################################
class(Forbes2000[,"rank"])
length(Forbes2000)
1: length(Forbes2000)
length(Forbes2000[,"rank"])
1: length(Forbes2000[,"rank"])


###################################################
### what is the class of the "name" object?
### how many entries in the "name column?
### what is first element of name vector?
###################################################
class(Forbes2000[,"name"])
length(Forbes2000[,"name"])
Forbes2000[,"name"][1]


###################################################
### what is the class of the category vector?
### what are the number of levels of the factor variable category?
### list the unique levels of the factor category
### show me frequencies of levels of category factor
###################################################
class(Forbes2000[,"category"])
nlevels(Forbes2000[,"category"])
levels(Forbes2000[,"category"])
table(Forbes2000[,"category"])


###################################################
### what is the class of the object sales (variable)?
### what is the median, the mean, and range of sales?
### the summary method shows quite a few different statistics
###################################################
class(Forbes2000[,"sales"])
median(Forbes2000[,"sales"])
mean(Forbes2000[,"sales"])
range(Forbes2000[,"sales"])
summary(Forbes2000[,"sales"])



## BASIC DATA MANIPULATION
###################################################
### take 'name' column and store it in variable 'companies'
### what is the first (and largest) entry in 'companies'?
### list all companies greater than 3
###################################################
companies <- Forbes2000$name
companies[1]
companies[(4:2000)]


###################################################
### extract variables "name" "sales" "profits" "assets" from top 3
### order the companies' sales from low to high
### what companies have three lowest sales?
### what companies have three highest sales?
### number of companies have assets > 1000
### what companies have assets > 1000?
###################################################
Forbes2000[1:3, c("name", "sales", "profits", "assets")]
order_sales <- order(Forbes2000$sales)
companies[order_sales[1:3]]
companies[order_sales[1998:2000]]
table(Forbes2000$assets > 1000)
Forbes2000[Forbes2000$assets > 1000,
           c("name", "sales", "profits", "assets")]


###################################################
### list "name" "sales" "profits" "assets" for companies with missing profits data
### tell me how many cases are complete or not
###################################################
na_profits <- is.na(Forbes2000$profits)
table(na_profits)
Forbes2000[na_profits,
           c("name", "sales", "profits", "assets")]
table(complete.cases(Forbes2000))



## COMPUTING WITH DATA
###################################################
### summary prints quite a bit of information
###################################################
head(Forbes2000)
summary(Forbes2000)
lapply(Forbes2000, summary)
table(Forbes2000) # helpful for category variables
str(Forbes2000) # helpful when data contains multiple data format
library(psych)
describe(Forbes2000)


###################################################
### compare median profits in each of the 27 categories
### sort categories by median profits
### three categories with highest median profits
###################################################
mprofits <- tapply(Forbes2000$profits,
                   Forbes2000$category, median, na.rm = TRUE)
print(sort(mprofits))
rev(sort(mprofits))[1:3]



## CREATING FUNCTIONS
###################################################
### create a function, call it iqr
### compare output of your iqr function with R IQR function
###################################################
iqr <- function(x, na.rm = TRUE) {
  q <- quantile(x, prob = c(0.25, 0.75), names = FALSE, na.rm = TRUE)
  return(diff(q))
}
xdata <- rnorm(100)
xdata[1] <- NA
iqr(xdata, na.rm = TRUE)
IQR(xdata, na.rm = TRUE)



## SIMPLE GRAPHICS
###################################################
### draw some histograms
###################################################
par(mfrow=c(1,1))
hist(Forbes2000$marketvalue)
hist(log(Forbes2000$marketvalue))


###################################################
### plot log of market value by log of sales
###################################################
plot(log(marketvalue) ~ log(sales), data = Forbes2000,
     pch = ".")


###################################################
### code chunk number 88: AItR-Forbes2000-marketvalue-sales-shading
###################################################
plot(log(marketvalue) ~ log(sales), data = Forbes2000,
     col = rgb(0,0,0,0.1), pch = 16)



## EXERCISES
###################################################
### calculate the median profit for the companies in the US
### calculate the median profit for the companies in the UK, France, and Germany
###################################################
median(Forbes2000$profits[Forbes2000$country == "United States"], na.rm = TRUE)
median(Forbes2000$profits[Forbes2000$country == "United Kingdom"], na.rm = TRUE)
median(Forbes2000$profits[Forbes2000$country == "France"], na.rm = TRUE)
median(Forbes2000$profits[Forbes2000$country == "Germany"], na.rm = TRUE)


###################################################
### find all German companies with negative profit
###################################################
NEGpro <- Forbes2000[Forbes2000$profits < 0,]
NEGpro$name[NEGpro$country == "Germany"]


###################################################
### to which business category do most of the Bermuda island companies belong?
###################################################
summary(Forbes2000$category[Forbes2000$country == "Bermuda"])


###################################################
### ex 1.4
###################################################
highpro <- order(Forbes2000$profits, na.last = TRUE, decreasing = TRUE)
highprocompany <- Forbes2000[highpro,]
top50 <- highprocompany[1:50,]
names(top50)
plot(log(top50$sales)~log(top50$assets))
text(log(top50$sales)~log(top50$assets), labels = abbreviate(top50$country, minlength = 3), pos = 1)


###################################################
### find the average value of sales for the companies 
### in each country in the data set, and the number of companies
### in each country with profits above 5 billion US dollars
###################################################
avg <- tapply(Forbes2000$sales, Forbes2000$country, mean, na.rm=TRUE)
Forbes2000[Forbes2000$profits > 5,]

