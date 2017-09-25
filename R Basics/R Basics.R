###################################################
### make a sequence of number 1 to 3
###################################################
1:3
c(1,2,3)
-c(1,2,3)
seq(from = 1, to = 3, by = 1)
seq(from = 1, to = 3, length = 30)
rep(0, times = 5)
rep(c(0, 1, 2, 3), times = 5)
rep(c(0, 1, 2, 3), each = 5)


###################################################
### make a numerical vector
### length of a numerical vector
### make a character vector
### assign values to character
### create a matrix
### create a dataframe
###################################################
num_vect <- c(0.5, 55, -10, 6)
my_vector <- 1:20
length(my_vector)
tf <- num_vect<1
num_vect >= 6

my_char <- c("my", "name", "is")
paste(my_char, collapse = " ")
paste("hello", "world", collapse = " ")
paste(1:3, c("x", "y", "z"), sep = "")
paste(letters, 1:4, sep = "-")

vect <- c(foo=11, bar=2, norf=NA)
names(vect)
vect1 <- c(11,2,NA)
names(vect1) <- c("foo", "bar", "norf")
identical(vect, vect1)

my_matrix <- matrix(1:20, nrow=4, ncol=5)
patients <- c("bill", "gina", "kelly", "sean")
cbind(patients, my_matrix)
my_data <- data.frame(patients, my_matrix)
cnames <- c("patients", "age", "weight", "bp", "rating", "test")
colnames(my_data) <- cnames


###################################################
### create a vector with missing values
### multiply missing values
### create a vector with 1000 draws from a standard normal distribution
### select 100 elements at random from y
### create a vector with missing values
### subsetting vectors
### subsetting vectors with specific entries
### leaving first two entries out of the vector
### extract all missing values from vector
### conditional subsetting
###################################################
x <- c(44, NA, 5, NA)
x*3
y <- rnorm(1000)
my_data <- sample(c(x, y), 100)
is.na(my_data)

x <- c(-0.7, -1.9, -0.5, NA, NA, NA, NA, -0.12, NA, 1.6, 2.1, 5, NA, 78, 21, 11)
x[1:10]
x[c(1,3,5)]
x[-c(1,2)]
x[is.na(x)]
x[!is.na(x) & x>10]


###################################################
### logic expressions
###################################################
TRUE == TRUE
(FALSE == TRUE) == FALSE
6 < 7
10 <= 10
5 != 7
!(5 == 7)
FALSE & FALSE
# left operand is recycles across every element in the right 
TRUE & c(TRUE, FALSE, TRUE)
# left perand is only evaluated with the first member of the right 
FALSE && c(TRUE, FALSE, TRUE)
# across entire vector
FALSE | c(TRUE, FALSE, TRUE)
# only evaluate the first element from the right
FALSE || c(TRUE, FALSE, FALSE)
# all 'and' operators are evalueated before 'or' operators
# first the left and right of the 'and' operators are evaluated
# 6 is not equal to 8 and 4 is greater than 3.9, so the resulting epxression 'true && true' is true
# then the left operand of 'or' operator is evaluated: 5 is not greater than 8 so the entire expression is reduced to 
# false || true
5 > 8 || 6 != 8 && 4 > 3.9
isTRUE(6 < 4)
identical('twins', 'twins')
# xor() function takes two arguments. if onw argument evaluates to TRUE and one argument evaluates to FALSE,
# then this function will return TRUE, otherwise it will reture FALSE
xor(5==6, !FALSE)
ints <- sample(10)
ints > 5
any(TRUE, FALSE, FALSE, FALSE)
all(TRUE, FALSE, FALSE, FALSE)


###################################################
### simulation
###################################################
sample(1:6, 4, replace = FALSE) # replace enables duplicates

LETTERS # pre-built in variable
letters
sample(letters) # sample function can also permute or rearrange elements

flips <- sample(c(0,1), 100, replace = TRUE, prob = c(0.3,0.7)) # flip a unfair coin 100 times 
sum(flips)

rbinom(1, size=100, prob=0.7) # number of heads in 100 flips of unfair coin
rbinom(100, size = 1, prob = 0.7)

rnorm(100) # randomly generate 100 numbers from normal distribution
rnorm(100, 50, 10) # mean of 50, sd of 10

rpois(5, 10) # one group of 5 numbers with mean 10 from poisson distribution
replicate(100, rpois(5,10)) # 100 groups

