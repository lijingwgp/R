#######################################
### univariate cox regression model ###
#######################################


library(survival)
library(survminer)

# the coxph() function can be used to compute the cox proportional hazards model
#   - formula: is a linear model with a survival object as the response variable
#   - data: a dataframe
#   - method: is used to sprcify how to handle ties. the default is 'efron'.
#     other options would be 'breslow', and 'exact'.

data('lung')
head(lung)

# we will fit a cox regression model using the following:
#   - age
#   - sex
#   - ph.ecog
#   - wt.loss

# univariate
res.cox <- coxph(Surv(time, status)~sex, data = lung)
summary(res.cox)

# the cox regression results can be interpreted as follow:
#
#   - statistical significance: this is a wald statistic, which evaluate whether
#     the coefficient of a given variable is statistically significantly different
#     from 0.

#   - coefficients: a positive sign means that the hazard is higher for subjects
#     with higher values of that variable. note that the cox model gives the 
#     hazard ratio for the second group relative to the first group, that is
#     female versus male. the coefficient for sex = -0.53 indicates that females
#     have lower risk of death than males.
#
#   - hazard ratios: the exponentiated coefficients, give the effect size of 
#     of covariates. so being female (sex=2) reduces the hazard by a factor of 0.59
#
#   - conf intervals: this is a upper and lower bounds of the hazard ratio
#
#   - global significance: finally, the output gives p-values for three alternative
#     tests for overall significance of the model: the likelihood-ratio, wald test, 
#     and logrank statistics. these three methods are equivalent. if sample size
#     is small, the likelihood ratio test is preferred.

covariates <- c("age","sex","ph.karno","ph.ecog","wt.loss")
univ_formulas <- sapply(covariates, function(x) as.formula(paste('Surv(time,status)~',x)))
univ_models <- lapply(univ_formulas, function(x) {coxph(x, data = lung)})
univ_results <- lapply(univ_models,
                       function(x){ 
                         x <- summary(x)
                         p.value<-signif(x$wald["pvalue"], digits=2)
                         wald.test<-signif(x$wald["test"], digits=2)
                         beta<-signif(x$coef[1], digits=2);#coeficient beta
                         HR <-signif(x$coef[2], digits=2);#exp(beta)
                         HR.confint.lower <- signif(x$conf.int[,"lower .95"], 2)
                         HR.confint.upper <- signif(x$conf.int[,"upper .95"],2)
                         HR <- paste0(HR, " (", 
                                      HR.confint.lower, "-", HR.confint.upper, ")")
                         res<-c(beta, HR, wald.test, p.value)
                         names(res)<-c("beta", "HR (95% CI for HR)", "wald.test", 
                                       "p.value")
                         return(res)
                         #return(exp(cbind(coef(x),confint(x))))
                       })
res <- t(as.data.frame(univ_results, check.names = FALSE))
as.data.frame(res)

# note that in the analysis above, each factor is assessed through separate
# univariate cox regressions.


#########################################
### multivariate cox regression model ###
#########################################

res.cox <- coxph(Surv(time, status) ~ age + sex + ph.ecog, data =  lung)
summary(res.cox)

# the p-value for all three overall tests are significant, indicating that
# the model is significant. these test evaluate the omnibus null hypothesis that 
# all of the betas are 0. 

# note that age fails to be significant.
# the hazard ratio for sex is 0.58, indicating a strong relationship between the
# patients' gender and decreased risk of death. in other words, holding other
# covariates constant, being female (sex=2) reduces the hazard by a factor of
# 0.58. 


################################################################
### visualizing the estimated distribution of survival times ###
################################################################

ggsurvplot(survfit(res.cox), palette = "#2E9FDF", ggtheme = theme_minimal(),
           data = lung)

# we may wish to display how estimated survival depends upon the value of 
# a covariate of interest. let's assume we want to assess the sex column
#
# we could construct a new data frame with two rows, one for each value of sex
# covariate. all other covariates are fixed to their average values or
# their lowest level. for dummy covariates, the average value is the proportion
# coded 1 in the data set. 
# this new data set is passed to survfit via the newdata argument.

sex_df <- with(lung, data.frame(sex = c(1, 2), age = rep(mean(age, na.rm = TRUE), 2),
              ph.ecog = c(1, 1)))
fit <- survfit(res.cox, newdata = sex_df)
ggsurvplot(fit, conf.int = TRUE, legend.labs=c("Sex=1", "Sex=2"),
           ggtheme = theme_minimal(), data = sex_df)
