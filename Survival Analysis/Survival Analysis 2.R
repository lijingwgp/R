#############################
##### Survival Analysis #####
#############################

### load the data
library(survival)
library(ranger)
library(ggplot2)
library(dplyr)
library(ggfortify)

# the data we will use here is called 'veteran' from the survival package
# this dataset contains two treatment, randomized trial for lung cancer.

data(veteran)
head(veteran)

# The variables in veteran are: 
#   * trt: 1 = standard, 2 = test 
#   * celltype: 1 = squamous, 2 = small cell, 3 = adeno, 4 = large 
#   * time: survival time in days 
#   * status: censoring status 
#   * karno: Karnofsky performance score (100=good) 
#   * diagtime: months from diagnosis to randomization 
#   * age: in years 
#   * prior: prior therapy 0 = no, 10 = yes



### Kaplan Meier Analysis
# the first thing to do is use 'Surv()' to build the standard survival object.
# variable 'time' records survival time; 'status' indicates whether the patient's
# death was observed (status=1) or was censored (status=0). 
#
# note that a "+" after the time in the print out indicates censoring.

km <- with(veteran, Surv(time, status))

# to begin our analysis, we first create a surv object then use the survfit()
# function to produce the KM estimates of the probability of survival over time.
#
# the 'times' parameter of the summary() function gives some control over which 
# times to print. here it's set to print the estimate for 1, 30, 60, 90 days, and
# every 90 days thereafter.

km_fit <- survfit(km~1, data=veteran)
summary(km_fit, times = c(1,30,60,90*(1:10)))
autoplot(km_fit)

# next, we look at survival curves by treatment
km_trt_fit <- survfit(km~trt, data = veteran)
autoplot(km_trt_fit)

# to show one more exploratory plot, we will look at survival by age.
# first, create a new data column which is a categorical feature 'AG' that has
# values 'LT60', and 'GT60'. respectively, they describe veterans younger
# and older than 60.
#
# we will also make 'trt' and 'prior' into factor variables. but note, survfit()
# works fine without this extra step.

vet = veteran %>% mutate(AG = ifelse(age<60, 'LT60', 'OV60'),
                         trt = factor(trt, labels = c('standard','test')),
                         prior = factor(prior, labels = c('No','Yes')))
km_AG_fit <- survfit(km~AG, data = vet)
autoplot(km_AG_fit)



### Cox Proportional Hazards Model
# now we will fit a cph model that makes use of all of the covariates in the dataset

cox <- coxph(km ~ trt+celltype+karno+diagtime+age+prior, data = vet)
summary(cox)
cox_fit <- survfit(cox)
autoplot(cox_fit)

# note that the model flags small cell type, adeno cell type, etc. as significant.
# however, some caution needs to be exercised in interpreting these results.
# we should further more investigate into the assumptions for cox model.
#
# instead, we should be interested in the concordance statistic. it is the 
# probability of agreement for any two randomly chosen observations, where
# in this case agreement means that the observation with the shorter survival
# time of the two also has the larger risk score.
#
# A value of 1 signifies perfect agreement, .6-.7 is a common result for 
# survival data, .5 is an agreement that is no better than chance, and 
# .3-.4 is the performance of some stock market analysts.



### Aalen's Additive Regression Model
# to show how the effects of the covariates change over time, we could fit 
# Aalen's additive regression model for censord data to the veteran data.
# 
# one reason to use Aalen's additive regression model is that users may not 
# fully understand its complexities nor be able to check assumtions.

aa_fit <- aareg(km~trt+celltype+karno+diagtime+age+prior, data=vet)
aa_fit
autoplot(aa_fit)



### Random Forest Model
# finally, the last model we will explore is the random forest model using 
# the ranger() function.
# ranger() builds a model for each observation in the dataset. the following code
# builds the model using the same variables used in the cox model above, and
# plot twenty random curves, along with a curve that represents the global
# average for all of the patients.

r_fit <- ranger(km~trt+celltype+karno+diagtime+age+prior,
                data = vet,
                mtry=4,
                importance = 'permutation',
                splitrule = 'extratrees',
                verbose = TRUE)

# average the survival models
death_times <- r_fit$unique.death.times
surv_prob <- data.frame(r_fit$survival)
avg_prob <- sapply(surv_prob, mean)

# Plot the survival models for each patient
plot(r_fit$unique.death.times,r_fit$survival[1,], 
     type = "l", 
     ylim = c(0,1),
     col = "red",
     xlab = "Days",
     ylab = "survival",
     main = "Patient Survival Curves")
cols <- colors()
for (n in sample(c(2:dim(vet)[1]), 20)){
  lines(r_fit$unique.death.times, r_fit$survival[n,], type = "l", col = cols[n])
}
lines(death_times, avg_prob, lwd = 2)
legend(500, 0.7, legend = c('Average = black'))

# the following code illustrates how ranger() ranks variable importance
vi <- data.frame(sort(round(r_fit$variable.importance, 4), decreasing = TRUE))
names(vi) <- "importance"
head(vi)

# note that ranger() flags 'karno' and 'celltype' as the two most important
# also note that the importance results just give variable names and not level
# names. 
# the result produced by ranger() met with the results from cox model. 

# my personal believe that the major use for tree-based models for survival
# data will be to deal with very large data sets. finally, let's compare 
# the three models we have examine so far.

# Set up for ggplot
kmi <- rep("KM",length(km_fit$time))
km_df <- data.frame(km_fit$time,km_fit$surv,kmi)
names(km_df) <- c("Time","Surv","Model")

coxi <- rep("Cox",length(cox_fit$time))
cox_df <- data.frame(cox_fit$time,cox_fit$surv,coxi)
names(cox_df) <- c("Time","Surv","Model")

rfi <- rep("RF",length(r_fit$unique.death.times))
rf_df <- data.frame(r_fit$unique.death.times,avg_prob,rfi)
names(rf_df) <- c("Time","Surv","Model")

plot_df <- rbind(km_df,cox_df,rf_df)

p <- ggplot(plot_df, aes(x = Time, y = Surv, color = Model))
p + geom_line()
