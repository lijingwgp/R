##################################
##### Survival Analysis in R #####
##################################

# with the concept in mind, we can now start analyze an actual dataset and try to 
# answer some of the questions above. 
#
# load required packages

library(survival)
library(survminer)
library(dplyr)

# the next step is to load the dataset and examine its structure. we will use
# the 'ovarian' dataset here. this dataset comprises a cohort of ovarian cancer
# patients and respective clinical information, including:
#   - the time patients were tracked until they either died or were lost, 
#   - whether patients were censored or not, 
#   - patient age, treatment group assignment, presence of residual disease and performance status. 
# 
# if the variable names are too confusing, we can consult with the help page

data("ovarian")
glimpse(ovarian)
help("ovarian")

# the 'futime' column holds the survival times which is the response variable.
# 'fustat' on the other hand, tells you if an individual patients' survival time is censored. 
# the 26 patients in this dataset received either one of two therapy regiments (rx)
# and the attending physician assessed the regression of tumors and patients'
# performance at some point.

# furthermore, if we want to use patients age as a predictive variable, we need
# to dichotomize continuous to binary values. but what cutoff should we choose
# for that? let's look at the overall distribution of age values.

ovarian$rx <- factor(ovarian$rx, levels = c("1","2"),
                     labels = c("A","B"))
ovarian$resid.ds <- factor(ovarian$resid.ds, levels = c("1","2"),
                           labels = c("no","yes"))
ovarian$ecog.ps <- factor(ovarian$ecog.ps, levels = c("1","2"),
                          labels = c("good","bad"))
hist(ovarian$age)

# the obviously bi-modal distribution suggests a cutoff of 50 years

ovarian <- ovarian %>% mutate(age_group = ifelse(age>50, "old", "young"))
ovarian$age_group <- factor(ovarian$age_group)

# now we are prepared to create a survival object which is basically a compiled
# version of the 'futime' and 'fustat' columns that can be interpreted by the 
# survfit function
#
# a '+' behind survival time indicates censored data points.
#
# fit survival data using the Kaplan-Meier method

surv_obj <- Surv(time = ovarian$futime, event = ovarian$fustat)
surv_obj

# the next step is to fit the KM curves. we can easily do that by passing the
# surv_obj to the 'survfit' function. we can also stratify the curve depending
# on the treatment regimen 'rx' that patients were assigned to

fit1 <- survfit(surv_obj~rx, data = ovarian)
summary(fit1)

# we can examine the corresponding survival curve by passing the survival object
# to the ggsurvplot function. the 'pval = TRUE' argument is very userful, because
# it plots the p-value of a log rank test as well

ggsurvplot(fit1, data = ovarian, pval = TRUE)

# the log rank test p-value is 0.3 indicates a non-significant result if we 
# consider p < 0.05 to indicate statistical significance. in other words, none
# of the treatments examined were significant superior although patients
# receiving treatment B are doing better in the first month of follow-up
#
# now, let's examine other variables

fit2 <- survfit(surv_obj~resid.ds, data = ovarian)
ggsurvplot(fit2, data = ovarian, pval = TRUE)

# the KM plots stratified according to residual disease status look a bit different: 
# the curves diverge early and the log rank test is almost significant. 
# we might want to argue that a follow up study with an increased sample size
# could validate these results.

# there is more systematic way to look at the different covariates.
# cox proportional hazards models allow us to include covariates. 
#
# we can build cox proportional hazard models using the 'coxph' function and 
# visualize them using the 'ggforest' function.
#
# these type of plot is called a forest plot. it shows so-called hazard rations
# which are derived from the model for all covariates that we included in the formula
# in 'coxph'.
#
# briefly, an hazard ratio > 1 indicates an increased risk of death if a 
# specific condition is met by a patient. an hr < 1 indicates a decreased risk

fit.coxph <- coxph(surv_obj~rx+resid.ds+age_group+ecog.ps, data = ovarian)
ggforest(fit.coxph, data = ovarian)

# a hazard ratio of 0.25 for treatment groups tells you that patients who
# received treatment B hav a reduced risk of dying compared to patients who
# received treatment A.
# 
# also note that the respective 95% confidence interval is 0.071 - 0.89
# and this result is significant.
