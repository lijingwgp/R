####################
### Introduction ###
####################

# Visualisation is an important tool for insight generation, but it is rare that you get the data in exactly 
# the right form you need. Often you'll need to create some new variables or summaries, or maybe you just 
# want to rename the variables or reorder the observations in order to make the data a little easier to work with.

require(nycflights13)
require(tidyverse)

nycflights13::flights

# dplyr basics
# There are five key dplyr functions that allow us to solve the vast majority of the data challenges
#   1. filter - pick observations by their values
#   2. arrange - reorder the rows
#   3. select - pick variables by their names
#   4. mutate - create new variables with the functions of existing variables
#   5. summarise - collapse many values down to a single summary

# These can all be used in conjunction with group_by() which changes the scope of each function from operating \
# on the entire dataset to operating on it group-by-group.



###################
### Filter Rows ###
###################

# filter() allows you to subset observations based on their values.

(feb2 <- filter(flights, month==2, day==2))

# selecting with comparison operators
# >,>=,<,<=,!=
#
# Multiple arguments to filter() are combined with "and". Every expression must be true in order for a 
# row to be included in the output: &,|,!

filter(flights, month == 11 | month == 12)

# This will select every row where x is one of the values in y.

filter(flights, month %in% c(11, 12))

# R also has && and ||. Don't use them here! You'll learn when you should use them in conditional execution.
# Also note that filter() only includes rows where the condition is TRUE; it excludes both FALSE and NA values. 
# If you want to preserve missing values, ask for them explicitly:

df <- tibble(x = c(1, NA, 3))
filter(df, x > 1)
filter(df, is.na(x) | x > 1)



####################
### Arrange Rows ###
####################

# arrange() works similarly to filter() except that instead of selecting rows, it changes their order.
# It takes a data frame and a set of column names to order by. If you provide more than one column name, 
# each additional column will be used to break ties in the values of preceding columns.

arrange(flights, year, month, day)

# Use desc() to re-order by a column in descending order:

arrange(flights, desc(dep_delay))

# Missing values are always sorted at the end

arrange(df, x)
arrange(df, desc(x))



######################
### Select Columns ###
######################

# select() allows you to find useful subset using operations based on the names of the variables.

# Select columns by name

select(flights, year, month, day)

# Select all columns between year and day (inclusive)

select(flights, year:day)

# Select all columns except those from year to day (inclusive)

select(flights, -(year:day))

# There are a number of helper functions you can use within select()
#   - start_with()
#   - end_with()
#   - contains()
#   - matches() for regex
#   - num_range('x', 1:3) matches x1, x2, x3

# use rename(), which is a variant of select(), to rename columns

rename(flights, tail_num = tailnum)

# Another option is to use select() in conjunction with the everything() helper. 
# This is useful if you have a handful of variables you'd like to move to the start of the data frame.

select(flights, time_hour, air_time, everything())



################################
### Add New Vars with Mutate ###
################################

# Besides selecting sets of existing columns, it's often useful to add new columns that are functions 
# of existing columns.

select(flights, year:day, ends_with("delay"), distance, air_time) %>% 
  mutate(gain = dep_delay - arr_delay, speed = distance / air_time * 60) %>%
  mutate(hours = air_time / 60, gain_per_hour = gain / hours)

# If you only want to keep the new variables, use transmute()

transmute(flights,
          gain = dep_delay - arr_delay,
          hours = air_time / 60,
          gain_per_hour = gain / hours)

# Other useful creation functions
#   +,-,*,/,^, sum(), mean()
#   %/% integer division, %% remainder
#   log(),log2(),log10()
#   lead(),lag()
#   cumsum(),cumprod(),cummin(),cummax(),cummean()
#   <,<=,>,>=,!=,==
#   min_rank(),desc(),row_number(),dense_rank(),percent_rank(),cume_dist(),ntile()



##########################################
### Grouped Summaries with summarise() ###
##########################################

# The last key verb is summarise(). It collapses a data frame to a single row.

summarise(flights, delay = mean(dep_delay, na.rm = TRUE))

# summarise() is not terribly useful unless we pair it with group_by(). 
# This changes the unit of analysis from the complete dataset to individual groups.
# Then, when you use the dplyr verbs on a grouped data frame they'll be automatically applied "by group".

group_by(flights, year, month, day) %>%
  summarise(delay = mean(dep_delay, na.rm = TRUE))

# Combining multiple operations with the pipe

delays <- flights %>% 
  group_by(dest) %>% 
  summarise(
    count = n(),
    dist = mean(distance, na.rm = TRUE),
    delay = mean(arr_delay, na.rm = TRUE)) %>% 
  filter(count > 20, dest != "HNL")

# Whenever you do any aggregation, it's always a good idea to include either a count (n()), or a count of
# That way you can check that you're not drawing conclusions based on very small amounts of data.

not_cancelled <- flights %>% 
  filter(!is.na(dep_delay), !is.na(arr_delay))

delays <- not_cancelled %>% 
  group_by(tailnum) %>% 
  summarise(
    delay = mean(arr_delay))

ggplot(data = delays, mapping = aes(x = delay)) + 
  geom_freqpoly(binwidth = 10)

# The story is actually a little more nuanced. We can get more insight if we draw a scatterplot of number of 
# flights vs. average delay.

delays <- not_cancelled %>% 
  group_by(tailnum) %>% 
  summarise(
    delay = mean(arr_delay, na.rm = TRUE),
    n = n())

ggplot(data = delays, mapping = aes(x = n, y = delay)) + 
  geom_point(alpha = 1/10)

# When looking at this sort of plot, it's often useful to filter out the groups with the smallest numbers of 
# observations, so you can see more of the pattern and less of the extreme variation in the smallest groups.

delays %>% 
  filter(n > 25) %>% 
  ggplot(mapping = aes(x = n, y = delay)) + 
  geom_point(alpha = 1/10)

# Another example

batting <- as_tibble(Lahman::Batting)
batters <- batting %>% 
  group_by(playerID) %>% 
  summarise(
    ba = sum(H, na.rm = TRUE) / sum(AB, na.rm = TRUE),
    ab = sum(AB, na.rm = TRUE))
batters %>% 
  filter(ab > 100) %>% 
  ggplot(mapping = aes(x = ab, y = ba)) +
  geom_point() + 
  geom_smooth(se = FALSE)

# Useful summary functions
#   - measure of location: mean(), median()
#   - measure of spread: sd(), IQR(), mad()
#   - measure of rank: min(), quantile(), max()
#   - measure of position: first(), nth(), last()
#   - counts: n(), n_distinct()
#   - Counts and proportions of logical values: sum(x > 10), mean(y == 0)



###################################
### Grouped mutates and filters ###
###################################

# Grouping is most useful in conjunction with summarise(), but you can also do convenient operations with 
# mutate() and filter():

# Find the worst members of each group

flights_sml %>% 
  group_by(year, month, day) %>%
  filter(rank(desc(arr_delay)) < 10)

# Find all groups bigger than a threshold

popular_dests <- flights %>% 
  group_by(dest) %>% 
  filter(n() > 365)
popular_dests

# Standardise to compute per group metrics

popular_dests %>% 
  filter(arr_delay > 0) %>% 
  mutate(prop_delay = arr_delay / sum(arr_delay)) %>% 
  select(year:day, dest, arr_delay, prop_delay)
