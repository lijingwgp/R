##########################################
### install and load required packages ###
##########################################

library(survival)
library(survminer)
data('lung')
head(lung)

# inst: institution code
# time: survival time in days
# status: censoring status, 1=censored, 2=dead
# age: in years
# sex: male=1, female=2
# ph.ecog: ECOG performance score, 0=good, 5=dead
# ph.karno: Karnofsky performance score, 0=bad, 100=good
# pat.karno: karnofsky performance score as rated by patient
# meal.cal: calories consumed at meals
# wt.loss: weight loss in last six months


###############################################
### compute survival curves using survfit() ###
###############################################

# the survfit() function can be used to compute KM survival estimate. 
# its main argument include:
#   - a survival object 
#   - and the data containing the variables

temp1 <- survfit(Surv(time, status)~sex, data = lung)
summary(temp1)$table

d <- data.frame(subjects = temp1$n,
                time = temp1$time,
                n.risk = temp1$n.risk,
                n.event = temp1$n.event,
                n.censor = temp1$n.censor,
                surv = temp1$surv,
                upper = temp1$upper,
                lower = temp1$lower)
head(d)

# the survfit() function returns a list of variables:
#   - n: total nubmer of subjects in each curve
#   - time: the time points on the curve
#   - n.risk: number of subjects at risk at time t
#   - n.event: number of events that occurred at time t
#   - n.censor: the number of censored subjects, who exit the risk set, 
#     without an event, at time t
#   - lower, upper: lower and upper confidence limits for the curve.
#   - strata: indicates stratification of curve estimation. if strata is not NULL,
#     there are multiple curves in the result.

temp2 <- surv_summary(temp1)
attr(temp2, "table")


################################
### visualize survival curve ###
################################

# we will use ggsurplot() function to produce the survival curves for 
# two groups of subjects
#
# it's possible to show that:
#   - 95% conf limits using conf.int=TRUE
#   - the p-value of the log-rank test using pval=TRUE
#   - horizontal / vertical line at median survival using surv.median.line
#     "hv", "h", "v"
#   - to show the risk table using risk.table=TRUE

ggsurvplot(temp1,
           pval = TRUE, conf.int = TRUE,
           risk.table = TRUE,         # Add risk table
           risk.table.col = "strata", # Change risk table color by groups
           linetype = "strata",       # Change line type by groups
           surv.median.line = "hv",   # Specify median survival
           ggtheme = theme_bw(),      # Change ggplot2 theme
           palette = c("#E7B800", "#2E9FDF"))

ggsurvplot(
  temp1,                     # survfit object with calculated statistics.
  pval = TRUE,               # show p-value of log-rank test.
  conf.int = TRUE,           # show confidence intervals for 
                             # point estimaes of survival curves.
  conf.int.style = "step",   # customize style of confidence intervals
  xlab = "Time in days",     # customize X axis label.
  break.time.by = 200,       # break X axis in time intervals by 200.
  ggtheme = theme_light(),   # customize plot and risk table with a theme.
  risk.table = "abs_pct",    # absolute number and percentage at risk.
  risk.table.y.text.col = T, # colour risk table text annotations.
  risk.table.y.text = FALSE, # show bars instead of names in text annotations
                             # in legend of risk table.
  ncensor.plot = TRUE,       # plot the number of censored subjects at time t
  surv.median.line = "hv",   # add the median survival pointer.
  legend.labs = 
    c("Male", "Female"),     # change legend labels.
  palette = 
    c("#E7B800", "#2E9FDF")  # custom color palettes.
)

# The horizontal axis (x-axis) represents time in days, and the vertical axis 
# (y-axis) shows the probability of surviving or the proportion of people surviving.
# 
# The lines represent survival curves of the two groups.
# A vertical drop in the curves indicates an event. The vertical tick mark on 
# the curves means that a patient was censored at this time.

# At time zero, the survival probability is 1.0 (or 100% of the participants are alive).
#
# At time 250, the probability of survival is approximately 0.55 (or 55%) for 
# sex=1 and 0.75 (or 75%) for sex=2.
# 
# The median survival is approximately 270 days for sex=1 and 426 days for 
# sex=2, suggesting a good survival for sex=2 compared to sex=1. But to evaluate
# whether this difference is statistically significant requires a formal 
# statistical test, which we will discuss in the next section.

# note that, three often used transformations ben be specified using the 
# argument 'fun':
#   - log: log transformation of the survival function
#   - event: plots cumulative events. also known as the cumulative incidence
#   - cumhaz: plots the cumulative hazard function

ggsurvplot(temp1,
           conf.int = TRUE,
           risk.table.col = "strata", # Change risk table color by groups
           ggtheme = theme_bw(),      # Change ggplot2 theme
           palette = c("#E7B800", "#2E9FDF"),
           #xlim = c(0, 600)
           fun = "cumhaz")

# the cumulative hazard is commonly used to estimate the hazard probability.
# it is defined as H(t) = -log(survival function) = -log(S(t)).
# it can be interpreted as the cumulative force of mortality. 
# it corresponds to the number of events that would be expected for each by time t.


##################################################
### log-rank test comparing two survive curves ###
##################################################

# the log-rank test is the most widely used method of comparing two or more
# survive curves. 
#   - null: there is no difference in survival between two groups
#   - alternative: there is

# the log-rank test is non-parametric, which makes no assumptions about the 
# survival distributions. 

# survdiff() function can be used to compute log-rank test

temp3 <- survdiff(Surv(time, status)~sex, data = lung)
temp3

# the function returns a list of components include:
#   - n: the number of subjects in each group
#   - obs: the weighted observed number of events in each group
#   - exp: the weighted expected number of events
#   - chisq: the chi-square statistics for a test of equality
#   - strata: optionally, the nubmer of subjects contained in each stratum

# note that in our example, the p-value = 0.0013 indicates that the sex groups
# differ significantly in survival.


###################################
### fit complex survival curves ###
###################################

# in this section, we will compute survival curves using the combination
# of multiple factors

data("colon")
colon$adhere <- factor(colon$adhere)
temp4 <- survfit(Surv(time, status)~sex+rx+adhere, data = colon)
ggsur <- ggsurvplot(temp4, fun = 'event', conf.int = TRUE, ggtheme = theme_bw())
ggsur$plot + theme_bw() + theme(legend.position = "right") + facet_grid(rx~adhere)
