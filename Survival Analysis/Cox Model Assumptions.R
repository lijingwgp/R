#####################################
### Diagnostics for the Cox model ###
#####################################

# The Cox proportional hazards model makes several assumptions. Here, we will 
# discuss three types of diagonostics for the Cox model:
#
#   1. testing the proportional hazards assumption
#   2. examing influential observations (or outliers)
#   3. detecting nonlinearity in relationship between log hazard and the covariates

# In order to check these model assumptions, residual method as used. The common
# residuals for the Cox model include:
#
#   1. Schoenfeld residuals to check the proportional assumption
#   2. Deviance residual to examine influential observations
#   3. Martingale residual to assess nonlinearity



#############################
### Computing and Testing ###
#############################

library(survival)
library(survminer)
data('lung')

res.cox <- coxph(Surv(time, status)~age+sex+wt.loss, data = lung)
res.cox

### testing proportional hazard assumption
# this can be checked using statistical test and graphical diagnostic 
# based on the scaled Schoenfeld residuals.
#
# in prinsiple, the Schoenfeld residuals are independent of time. a plot that
# shows a non-random pattern against time is evidence of violation to the assumption.
#
# in other words, the proportional hazard assumption is supported by a non-significant
# relationship between residuals and time.
#
# cox.zph() provides a convenient solution.

test.ph <- cox.zph(res.cox)
test.ph

# from the output above, the test is not significant for each of the covariates,
# and the global test is also not statistically significant.

# it's possible to do a graphical diagnostic using the function ggcoxzph(),
# which produces, for each covariate, graphs of the scaled Schoenfeld residuals
# against the transformed time.

ggcoxzph(test.ph)

# in the figure above, the solid line is a smoothing spline fit to the plot, 
# with the dashed lines representing a +/- 2 standard error bank about the fit.
# from a graphical inspection, there is no pattern with time.

# a violations of proportional assumption can be resolved by:
#   - adding covariate*time interaction
#   - stratification

### testing influential observations
# we can use the following:
#   the deviance residuals 
#   the dfbeta values
# the function ggcoxdiagnostics() provides a convenient solution

ggcoxdiagnostics(res.cox, type = 'deviance', linear.predictions = TRUE)
ggcoxdiagnostics(res.cox, type = 'dfbeta', linear.predictions = TRUE)

# the produced graph show that comparing the magnitudes of the largest dfbeta values
# to the regression coefficients suggests that none of the observations is 
# terribly influential, even though some of the dfbeta values for age and wt.loss
# are large compared with the others.

# It’s also possible to check outliers by visualizing the deviance residuals. 
# These residuals should be roughtly symmetrically distributed about zero 
# with a standard deviation of 1.
#   - Positive values correspond to individuals that “died too soon”.
#   - Negative values correspond to individual that “lived too long”.
#   - Very large or small values are outliers, which are poorly predicted by the model.

### testing non linearity
# plotting the Martingale residuals against continuous covariates is a common approach
# used to detect nonlinearity or, to assess the functional form of a covariate.
#
# nonlinearity is not an issue for categorical variables, so we ony examine
# plots of martingale residuals and partial residuals against a continuous variable
#
# Martingale residuals may present any value in the range (-inf, +1)
#   - a value of martinguale residuals near 1 represents individuals that "died too soon"
#   - and large negative values corresponding to individuals that "lived too long"

ggcoxfunctional(Surv(time, status) ~ age + log(age) + sqrt(age), data = lung)

# This might help to properly choose the functional form of continuous 
# variable in the Cox model. Fitted lines with lowess function should be 
# linear to satisfy the Cox proportional hazards model assumptions.
