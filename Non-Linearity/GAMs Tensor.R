# Another example using another package called mgcv
# See R for Everyone, section 23.3 
install.packages('mgcv') 
library(mgcv)
# make vector of column names
creditNames <- c("Checking", "Duration", "CreditHistory", "Purpose",
                 "CreditAmount", "Savings", "Employment",
                 "InstallmentRate", "GenderMarital", "OtherDebtors",
                 "YearsAtResidence", "RealEstate", "Age",
                 "OtherInstallment", "Housing", "ExistingCredits",
                 "Job", "NumLiable", "Phone", "Foreign", "Credit")
# use read.table to read the file
# specify that headers are not included
# the col.names are from creditNames
theURL <- "http://archive.ics.uci.edu/ml/machine-learning-databases/statlog/german/german.data"
credit <- read.table(theURL, sep=" ", header=FALSE,
                     col.names=creditNames, stringsAsFactors=FALSE)
head(credit)

# before
head(credit[, c("CreditHistory", "Purpose", "Employment", "Credit")])
creditHistory <- c(A30="All Paid", A31="All Paid This Bank",
                   A32="Up To Date", A33="Late Payment",
                   A34="Critical Account")

purpose <- c(A40="car (new)", A41="car (used)",
             A42="furniture/equipment", A43="radio/television",
             A44="domestic appliances", A45="repairs", A46="education",
             A47="(vacation - does not exist?)", A48="retraining",
             A49="business", A410="others")

employment <- c(A71="unemployed", A72="< 1 year", A73="1 - 4 years",
                A74="4 - 7 years", A75=">= 7 years")

credit$CreditHistory <- creditHistory[credit$CreditHistory]
credit$Purpose <- purpose[credit$Purpose]
credit$Employment <- employment[credit$Employment]

# code credit as good/bad
credit$Credit <- ifelse(credit$Credit == 1, "Good", "Bad")
# make good the base levels
credit$Credit <- factor(credit$Credit, levels=c("Good", "Bad"))
# after
head(credit[, c("CreditHistory", "Purpose", "Employment", "Credit")])
# fit a logistic GAM
n <- nrow(credit)
train <- sample(1:n, n * 0.8)
test <- -train

# apply a tensor product on CreditAmount and a spline on Age
creditGam <- gam(Credit ~ te(CreditAmount) + s(Age) + CreditHistory + Employment,
                 data=credit, subset=train, family=binomial(link="logit"))
summary(creditGam)
plot(creditGam, select=1, se=TRUE, shade=TRUE)
plot(creditGam, select=2, se=TRUE, shade=TRUE)
pred <- predict(creditGam, newdata=credit[test,], type='response')
class.preds <- ifelse(pred > 0.5, "Bad","Good")
class.preds <- factor(class.preds, levels=c("Good", "Bad"))
table(credit[test,'Credit'],class.preds)