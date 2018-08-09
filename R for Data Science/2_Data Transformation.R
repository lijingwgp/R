####################
### Introduction ###
####################
require(nycflights13)
require(tidyverse)

# take careful note of the conflicts message that's printed when you load the 
# tidyverse. It tells you that dplyr overwrites some functions in base R. 
# If you want to use the base version of these functions after loading dplyr, 
# you'll need to use their full names: stats::filter() and stats::lag().

# dataset nycflights13
# To explore the basic data manipulation verbs of dplyr, we'll use 
# nycflights13::flights. This data frame contains all 336,776 flights that departed 
# from New York City in 2013.

nycflights13::flights
class(flights)

# In this chapter you are going to learn the five key dplyr functions
#   filter: pick observations by their values
#   arrange: reorder the rows
#   select: pick variables by their names
#   mutate: create new variables
#   summarise: collapse many values down to a single summary
#
# these can be used in conjunction with group_by() which changes the scope of each 
# function from operating on the entire dataset to operating on it group-by-group.

# all verbs work similarly
#   the first argument is a data frame
#   the subsequent argument disbribe what to do
#   the result is a new data frame



#################################
### Filter rows with filter() ###
#################################
# filter() allows you to subset observations based on their values

(jan1 <- filter(flights, month==1, day==1))

# comparison operators:
#   >, >=, <, <=, !=
#
# logical operators:
#   &, |, !
#
# when we have more values to filter and other complicated subsetting

filter(flights, month>=7)
filter(flights, month==11 | month==12)
filter(flights, month %in% c(11,12,4,3,2,1))
filter(flights, !(arr_delay>120 | dep_delay>120))

# missing values
# any operation involving an unknown value will also be unknown.

NA > 5
10 == NA
NA / 2

# filter() only includes rows where the condition is TRUE; it excludes both FALSE 
# and NA values. If you want to preserve missing values, ask for them explicitly

df <- tibble(x = c(1,NA,2))
filter(df, x>1)
filter(df, is.na(x) | x>1)



###################################
### Arrange rows with arrange() ###
###################################
# arrange() works similarly to filter() except that instead of selecting rows, 
# it changes their order.
# 
# If you provide more than one column name, each additional column will be used to 
# break ties in the values of preceding columns

arrange(flights, year, month, day)

# Use desc() to re-order by a column in descending order

arrange(flights, desc(dep_delay))

# Missing values are always sorted at the end

arrange(df, x)



####################################
### Select columns with select() ###
####################################
# select() allows you to rapidly zoom in on a useful subset using operations 
# based on the names of the variables.

# Select columns by name
select(flights, year, month, day)
# Select all columns between year and day (inclusive)
select(flights, year:day)
# Select all columns except those from year to day (inclusive)
select(flights, -(year:day))

# There are a number of helper functions you can use within select():
#   starts_with()
#   ends_with()
#   contains()
#   matches(): this one is for regular expression
#   num_range("x", 1:3): matcohes x1, x2, and x3

# rename() can be used to rename variables

rename(flights, tail_num = tailnum)

# Another option is to use select() in conjunction with the everything() helper. 
# This is useful if you have a handful of variables you'd like to move to 
# the start of the data frame.

select(flights, time_hour, air_time, everything())



#######################################
### Add new variables with mutate() ###
#######################################
# Besides selecting sets of existing columns, it's often useful to add new 
# columns that are functions of existing columns. That's the job of mutate().
#
# mutate() always adds new columns at the end of your dataset so we'll start by 
# creating a narrower dataset so we can see the new variables.
#
# Note that you can refer to columns that you've just created:

flights_sml <- flights %>% dplyr::select(year:day, ends_with("delay"), distance, air_time)
flights_sml %>% mutate(gain = dep_delay-arr_delay, speed = distance/air_time*60)
flights_sml %>% mutate(gain = dep_delay-arr_delay, hours = air_time/60, gain_per_hour = gain/hours)

# If you only want to keep the new variables, use transmute()

flights %>% transmute(gain = dep_delay-arr_delay, hours = air_time/60, gain_per_hour = gain/hours)

# other useful creation functions:
#   +, -, *, /, ^
#   x/sum(x), y-mean(y)
#   %/%
#   log(), log2(), log10()
#   lead(), lag(), x-lag(x), x!=lag(x)
#   cumsum(), cumprod(), cummin(), cummax(), cummean()
#   <, <=, >, >=, !=



##########################################
### Grouped summaries with summarise() ###
##########################################
# The last key verb is summarise(). It collapses a data frame to a single row.

flights %>% summarise(delay = mean(dep_delay, na.rm = T))

# summarise() itself is not terribly useful unless we pair it with group_by()
# This changes the unit of analysis from the complete dataset to individual groups. 
# Then, when you use the dplyr verbs on a grouped data frame they'll be 
# automatically applied "by group".
#
# Together group_by() and summarise() provide one of the tools that you'll use 
# most commonly when working with dplyr: grouped summaries.

by_day <- flights %>% group_by(year, month, day)
summarise(by_day, delay = mean(dep_delay, na.rm = T))

delays <- flights %>% 
  group_by(dest) %>%
  summarise(
    count = n(),
    dis = mean(distance, na.rm = T),
    delay = mean(arr_delay, na.rm = T)
  ) %>%
  filter(count > 20, dest != "HNL")

# counts
# Whenever you do any aggregation, it's always a good idea to include either a 
# count (n()), or a count of non-missing values (sum(!is.na(x))). That way you 
# can check that you're not drawing conclusions based on very small amounts of 
# data.

not_cancelled <- flights %>% 
  filter(!is.na(dep_delay), !is.na(arr_delay))
delays <- not_cancelled %>%
  group_by(tailnum) %>%
  summarise(
    delay = mean(arr_delay, na.rm = T),
    n = n()
  )
ggplot(data = delays, mapping = aes(x = n, y = delay)) + 
  geom_point(alpha = 1/10)

# When looking at this sort of plot, it's often useful to filter out the groups 
# with the smallest numbers of observations, so you can see more of the pattern 
# and less of the extreme variation in the smallest groups.

delays %>%
  filter(n > 25) %>%
  ggplot(mapping = aes(x=n, y=delay)) + geom_point(alpha = 1/10)

# Let's look at how the average performance of batters in baseball is related 
# to the number of times they're at bat.

batting <- as_tibble(Lahman::Batting)
batter <- batting %>% group_by(playerID) %>% summarise(
  ba = sum(H, na.rm = T) / sum(AB, na.rm = T),
  ab = sum(AB, na.rm = T)
)
batter %>% 
  filter(ab > 100) %>%
  ggplot(mapping = aes(x=ab, y=ba)) + 
  geom_point() + 
  geom_smooth(se = F)

# useful summary functions
# R provides many other useful summary functions:

not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(
    avg_delay1 = mean(arr_delay),
    avg_delay2 = mean(arr_delay[arr_delay > 0]) # the average positive delay
  )
not_cancelled %>% 
  group_by(dest) %>% 
  summarise(distance_sd = sd(distance)) %>% 
  arrange(desc(distance_sd))
not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(
    first = min(dep_time),
    last = max(dep_time)
  )
not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(
    first_dep = first(dep_time), 
    last_dep = last(dep_time)
  )
not_cancelled %>% 
  group_by(year, month, day) %>% 
  mutate(r = min_rank(desc(dep_time))) %>% 
  filter(r %in% range(r))
not_cancelled %>% 
  group_by(dest) %>% 
  summarise(carriers = n_distinct(carrier)) %>% 
  arrange(desc(carriers))
not_cancelled %>% 
  count(dest)
not_cancelled %>% 
  count(tailnum, wt = distance)
not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(n_early = sum(dep_time < 500))
not_cancelled %>% 
  group_by(year, month, day) %>% 
  summarise(hour_perc = mean(arr_delay > 60))

# grouping by multiple variables
# When you group by multiple variables, each summary peels off one level 
# of the grouping. That makes it easy to progressively roll up a dataset

daily <- flights %>% group_by(year, month, day) 
(per_day <- daily %>% summarise(flights = n()))
(per_month <- per_day %>% summarise(flights = sum(flights)))
(per_year <- per_month %>% summarise(flights = sum(flights)))

# ungrouping
# If you need to remove grouping, and return to operations on ungrouped data, 
# use ungroup().

daily %>%
  ungroup() %>%
  summarise(flights = n())



###################################
### Grouped mutates and filters ###
###################################
# Grouping is most useful in conjunction with summarise(), but you can also do 
# convenient operations with mutate() and filter()

flights_sml %>%
  group_by(year, month, day) %>%
  filter(rank(desc(arr_delay)) < 10)
popular_dests <- flights %>% 
  group_by(dest) %>% 
  filter(n() > 365)
popular_dests %>% 
  filter(arr_delay > 0) %>% 
  mutate(prop_delay = arr_delay / sum(arr_delay)) %>% 
  select(year:day, dest, arr_delay, prop_delay)








