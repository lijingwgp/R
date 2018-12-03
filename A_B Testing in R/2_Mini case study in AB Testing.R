#############################
##### Analyzing results #####
#############################

library(tidyverse)
experiment_data <- read_csv("experiment_data.csv")
experiment_data$visit_date <- as.Date(experiment_data$visit_date, "%m/%d/%y")
experiment_data

experiment_data_sum <- experiment_data %>% group_by(visit_date, condition)
  summarize(conversion_rate = mean(click_adopt_today))
  
ggplot(experiment_data_sum,
       aes(x = visit_date,
           y = conversion_rate,
           color = condition,
           group = condition)) +
  geom_point() +
  geom_line()

experiment_results <- glm(clicked_adopt_today ~ condition,
                          family = "binomial",
                          data = experiment_data_sum) %>% tidy()
experiment_results



###########################################
##### Designing follow-up experiments #####
###########################################

# It's important to take small steps and only experiment 
# the important ideas one at a time.
# Also note that the experiment groups should be compared to 
# their own control group.
# 
# Always avoid confounding variables by always test small 
# changes that can be measured

# Now, there is an assumption error in which we ran our power 
# analysis and expectations for the 1st condition in January,
# but our follow-up experiment was run in August.
#
# Let's refer back to the conversion rate by month graph from previous.
# As discussed before, the conversion rate for August is very different
# from the conversion rate for January. But remember though, this
# is for our original control condition, the picture of a cat with no hat.


