# Collatz conjecture
# if n is even then n/2
# if n is odd then 3n + 1
# sequence always finishes at 1

# what number < 1000000 produces largest chain of numbers to arrive at 1

# define functions for handling even and odd numbers
even <- function(n) n/2
odd <- function(n) (3 * n) + 1

# define function to calculate next value in string
coll <- function(x){if (x%%2==0) even(x) else odd(x)}

# count the number of iterations to converge to 1
cfunc <- function(z){
ct = 0 # initialize counter
while (z != 1){z=(coll(z)); 
  ct = ct + 1 # increment counter
  }
# print(z) # all numbers arrive at 1
# print(ct) # prints number of iterations to reach 1
}

# set vector with range desired for calculation
# 1 to 1 million, for example
# and place in dataframe
nums <- c(1:100000)
df <- data.frame(unlist(nums))
colnames(df)[1] <-"nums"

# calculate individual string lengths
sls <- lapply(nums, cfunc)

# combine colums into dataframe of number and string length
df <- cbind(df, unlist(sls))
colnames(df)[2] <-"string.length"

# find 5 greatest string lengths and numbers
fdf <- df[order(df$string.length, decreasing = TRUE), ]

# table of highest strings and their values
ftab <- head(fdf, 10)
ftab

# build a graphic
plot(fdf)
