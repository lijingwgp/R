####################
### Introduction ###
####################
# this chapter will show how to use visualisation and transformation to explore the data
# in a systematic way, a task that statisticans call exploratory data analysis, EDA
#
# EDA is an iterative cycle that we:
#   1. generate questions about the data
#   2. search for answers by visualising, transforming, and modeling the data
#   3. use what we learn to refine our questions and/or generate new questions

# EDA is not a formal process with a strict set of rules. More than anything, 
# EDA is a state of mind. During the initial phases of EDA you should feel free to 
# investigate every idea that occurs to you. Some of these ideas will pan out, 
# and some will be dead ends. 
#
# As your exploration continues, you will home in on a few particularly 
# productive areas that you'll eventually write up and communicate to others.

require(tidyverse)



#################
### Questions ###
#################
# our goal during EDA is to develop an understanding of your data. The easiest 
# way to do this is to use questions as tools to guide your investigation. 
# When you ask a question, the question focuses your attention on a specific 
# part of your dataset and helps you decide which graphs, models, or 
# transformations to make.

# EDA is fundamentally a creative process. And like most creative processes, 
# the key to asking quality questions is to generate a large quantity of questions
#
# Each new question that you ask will expose you to a new aspect of your data and 
# increase your chance of making a discovery. You can quickly drill down into the 
# most interesting parts of your data-and develop a set of thought-provoking 
# questions-if you follow up each question with a new question based on what you 
# find.

# There is no rule about which questions you should ask to guide your research. 
# However, two types of questions will always be useful for making discoveries 
# within your data:
#   1. what type of variation occurs within my variables?
#   2. what type of covariation occurs between my variables?



#################
### Variation ###
#################
# Variation is the tendency of the values of a variable to change from 
# measurement to measurement. if you measure any continuous variable twice, 
# you will get two different results. This is true even if you measure quantities 
# that are constant, like the speed of light. Each of your measurements will 
# include a small amount of error that varies from measurement to measurement. 

# How you visualise the distribution of a variable will depend on whether the 
# variable is categorical or continuous. Categorical variables are usually saved as
# factors or character vectors. To examine the distribution of a categorical 
# variable, use a bar chart:

ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut))
diamonds %>% count(cut)

# continues variables
ggplot(data = diamonds) + 
  geom_histogram(mapping = aes(x=carat), binwidth = .5)
# or
diamonds %>% 
  count(cut_width(carat, 0.5))

# if wish to overlay multiple histograms in the same plot, you should use geom_freqpoly()
# instead of displaying the counts with bars, geom_freqpoly() uses lines instead

ggplot(data = diamonds, mapping = aes(x = carat, colour = cut)) +
  geom_freqpoly(binwidth = 0.1)

# typical values
# clusters of similar values suggest that subgroups exist in your data
ggplot(data = diamonds, mapping = aes(x = carat)) +
  geom_histogram(binwidth = 0.01)

ggplot(data = faithful, mapping = aes(x = eruptions)) + 
  geom_histogram(binwidth = 0.25)

# unusual values
# Outliers are observations that are unusual; data points that don't seem to fit 
# the pattern. Sometimes outliers are data entry errors; other times outliers 
# suggest important new science. 
#
# When you have a lot of data, outliers are sometimes difficult to see in a 
# histogram. For example, take the distribution of the y variable from the 
# diamonds dataset. The only evidence of outliers is the unusually wide limits 
# on the x-axis.

ggplot(diamonds) + 
  geom_histogram(mapping = aes(x = y), binwidth = 0.5)

# There are so many observations in the common bins that the rare bins are so short that you can't see them
# To make it easy to see the unusual values, we need to zoom to small values of 
# the y-axis with coord_cartesian()

ggplot(diamonds) + 
  geom_histogram(mapping = aes(x = y), binwidth = .5) + 
  coord_cartesian(ylim = c(0, 50))
