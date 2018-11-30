########################
##### Introduciton #####
########################

# A/B testing is a power way to trail a new design or program 
# changes before making final decisions.

# A/B testing is a framework for a user to test new ideas on 
# existing design. In addition, it involves run experiment, 
# statistically analyze results, updating to winning idea and 
# then continue to cycle again. 

# A/B testing is not something only run occationally in a very
# long time. It is a continous effort to maximaize things like
# conversion rates or customer usage time. 

# All in all, A/B testing is just experimental design. It could 
# be used in not only web development, although that's the popular
# one, but virtually every business situation whenever the user
# has a particular business idea in mind. 

# To perform A/B testing, we need two groups. A control group and a 
# test group. The hypothesis is that the changes made to the test
# group will make people to act in a way that is previously designed.

# Preliminary dataset
library(tidyverse)
click_data <- read_csv("click_data.csv")
click_data
# we can see that the dataset has two columns: visit_date, and clicked_adopt_today
# we would like to find the oldest and most recent date in the visit_date column.
min(click_data$visit_date)
max(click_data$visit_date)



#####################################
##### Baseline Conversion Rates #####
#####################################

# A baseline value is the current value before any experimental 
# changes happened.

# For example, let's frame a hypothesis
#
# "Using a photo of a cat wearing a hat will result in more
# ADOPT TODAY! clicks."
#
# In this hypothesis, in order to calculate the baseline metric
# we need to be precise about what do we mean by "more"

# Monthly current conversion rate is our baseline numbers

library(lubridate)
click_data_sum <- click_data %>% group_by(month(visit_date)) %>%
  summarize(conversion_rate = mean(clicked_adopt_today))

ggplot(click_data_sum, aes(x='month(visit_date)', y=conversion_rate)) + 
  geom_point()+geom_line()



####################################################
##### Designing an Experiment - Power Analysis #####
####################################################

# Now that we have a good sense of our baseline numbers, we are
# ready to design our experiments. Here, we use seasonality and
# Power Analysis to figure out how long we need to run the experiment.

# Based on our prior knowledge, the annual historical conversion rate
# is rougthly 28%. From our baseline number, we also observe that summer 
# likely to have higher conversion rate. So what does this mean to our experiment?
# 
# It means that it would be bad to run the control condition on August 
# and run the test condition on September because the control group may 
# look better simply due to seasonality. 
#
# This is why we would almost like to run the A/B testing of two groups samutaniously.
# User also need to bare in mind that the control group conversion rate
# is likely to be different through out the year.

# With these knowledge, we would use Power Analysis to determine how long
# should we let the experiment to run. 
#
# Experiment length is another critical part of a Power Analysis.
# If the duration is too long, then we may be facing risk of wasting resource,
# and if we stop too soon, we may not get enough effect.
#
# One way to solve this issue, is to use Power Analysis as it will tell use
# how large a sample size we need to have in order to make sure that the effect
# is real. Once we have a sample size, we can figure out how long we will need
# to run the experiment to get the number of the required data points.
#
# Running Power Analysis is also good because it helps us think which statistical
# test we need to run before the A/B testing take place. 
#
# To recap... when running a Power Analysis, we should know a few things beforehand.
#     1. statistical test we plan to run on
#     2. baseline value for the current control condition
#     3. desired value for the test condition
#     4. proportion of the data from the test condition (ideally 0.5)
#     5. significant threshold/alpha level where effect significant (generally 0.05)
#     6. power, or 1-beta which is the probability correctly rejecting null hypothesis 
#        (generally 0.8)

library(powerMediation)

# Now we will decision which statistical test we plan to run
# since the value of the dependent variable is binary, "click or didn't click"
# we will run a logistic regression.

# To run a Power Analysis for logistic regression, we will use the function below
# p1 is the control group condition. In our case is the control group's conversion
# rate. p2 is the test group condition, our goal we want to achieve.

total_sample_size <- SSizeLogisticBin(p1 = 0.2, p2 = 0.3, B = 0.5, alpha = 0.05, power = 0.8)
total_sample_size   # this is the total sample size for two groups
total_sample_size/2   # this is the sample size per group
