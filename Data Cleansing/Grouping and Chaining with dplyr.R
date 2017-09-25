# grouping and chaining with dplyr
# recall that summarize() is most powerful when applied to grouped data
# the main idea behind grouping data is that you want to break up your dataset into groups of rows based on the values of one 
# or more variables. the group_by() function is designed for this
library(dplyr)                # load package
cran <- tbl_df(mydf)          # put in tabular dataframe
rm("mydf")                    # remove the orignial dataframe
cran                          # print data in console

by_package <- group_by(cran, package)         # group by package, print the data everything looks the same but now any operation 
                                              # applied will take place on a per package basis


# recall that when we applied mean(size) to the orignal tbl_df via summarize(), it returned a single number -- the mean of all
# values in the size column. wouldn't it be much more interesting to look at the mean download size for each unique package?
summarize(by_package, mean(size))             # apply mean(size) via summarize() based on package


# now compute four values in a chaining format
# the 'count' created with n() contains total number of row for each package
# the 'unique' created with n_distinct() gives the total number of unique dowloads for each package measured by ip
# the 'countires' provides the number of countries in whcih each package was downloaded
# the 'avg_bytes" contains mean download size for each package
pack_sum <- summarize(by_package,
                      count = n(),
                      unique = n_distinct(ip_id),
                      countries = n_distinct(country),
                      avg_bytes = mean(size))
pack_sum


# now let's start isolating the top 1% of packages, based on the total number of downloads as measured by 'count'
# we need to know the value of 'count' that splits the data into the top 1% and bottom 99% of packages based on total downloads
quantile(pack_sum$count, probs = 0.99)                # determine 99% percentail
top_counts <- filter(pack_sum, count>679)             # select the top 1%
top_counts                                            # view data
View(top_counts)                                      # view all data
top_counts_sorted <- arrange(top_counts, desc(count)) # rearrange the data based on 'count' column in descending order
View(top_counts_sorted)                               # view all data

quantile(pack_sum$unique, probs = 0.99)               # determine 99% percentail unique download
top_unique <- filter(pack_sum, unique>465)            # select top 1% unique download
View(top_unique)                                      # view all data
top_unique_sorted <- arrange(top_unique,desc(unique)) # rearrange
View(top_unique_sorted)                               # view all data


# chaining
# chaining calls in a way that is compact and readable while still accomplishing the desired result
# avg_bytes is a tie breaker with smaller avg_bytes will have higher ranking
by_package <- group_by(cran, package)
pack_sum <- summarize(by_package,
                      count = n(),
                      unique = n_distinct(ip_id),
                      countries = n_distinct(country),
                      avg_bytes = mean(size))

top_countries <- filter(pack_sum, countries > 60)
result1 <- arrange(top_countries, desc(countries), avg_bytes)


# to achieve the same results but avoid saving intermediate steps, we need to embed function calls within one another
# %>% is a special chaining operator
# the benefit of this operator is that it allows us to chain the function calls in a linear fashion
result3 <-
  cran %>%
  group_by(package) %>%
  summarize(count = n(),
            unique = n_distinct(ip_id),
            countries = n_distinct(country),
            avg_bytes = mean(size)
  ) %>%
  filter(countries > 60) %>%
  arrange(desc(countries), avg_bytes)

print(result3)


# build a chain of dplyr commands
cran %>%
  select(ip_id, country, package, size) %>%
  mutate(size_mb = size / 2^20) %>%
  filter(size_mb <= 0.5) %>%
  arrange(desc(size_mb))