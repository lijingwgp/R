# manipulating data with dplyr
mydf <- read.csv(path2csv, stringsAsFactors = FALSE)    # import data
dim(mydf)               # exmine dimensions do the data
head(mydf)              # brief summary of the data


# the first step of working with dplyr is to load the data into 'tbl_df' format
cran <- tbl_df(mydf)    # load data into the tbl_df format and create a new data frame
rm("mydf")              # remove the original data frame from workspace
                        # the major advantage to using tbl_df over a regular data frame is the printing


# dplyr supplies firve 'verbs' that cover most fundamental data manipulation tasks: select(), filter(), arrange(), mutate(), and summarize()
select(cran, ip_id, package, country)         # select only these variables from the cran dataset
select(cran, r_arch:country)                  # select all columns starting from r_arch and ending with country
select(cran, country:r_arch)                  # also works the same i reverse order
select(cran, -time)                           # we can also specify the columns we want to throw away
select(cran, -(X:size))                       # select a subset of columns 


# use filter() to select a subset of rows
filter(cran, package == "swirl")                    # select all rows for which the package variable is equal to "swirl"
filter(cran, r_version == "3.1.1", country == "US") # multiple conditions
filter(cran, country == "IN", r_version <= "3.0.2") # multiple conditions
filter(cran, country == "US"|country == "IN")       # "either or" statement
filter(cran, size > 100500, r_os == "linux-gnu")    # mutilple conditions
filter(cran, !is.na(r_version))                     # return all rows which r_version is not NA


# in herent in select() was also the ability to arrange our selected columns in any order we please
cran2 <- select(cran, size:ip_id)             # create a subset
arrange(cran2, ip_id)                         # ip_id in ascending order
arrange(cran2, desc(ip_id))                   # ip_id in decending order
arrange(cran2, package, ip_id)                # sort by multiple values
arrange(cran2, country, desc(r_version), ip_id)


# it is common to crate a new variable based on the value of one or more variable already in dataset
cran3 <- select(cran, ip_id, package, size)   # create a subset
mutate(cran3, size_mb = size / 2^20)          # create a new column based on calculations
mutate(cran3, size_mb = size / 2^20, size_gb = size_mb / 2^10)
mutate(cran3, correct_size = size + 1000)


# summarize()
summarize(cran, avg_bytes = mean(size))       # creating a new column using summarize()