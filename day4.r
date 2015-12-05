# --- Day 4: The Ideal Stocking Stuffer ---
#  
#  Santa needs help mining some AdventCoins (very similar to bitcoins) to use as gifts 
#  for all the economically forward-thinking little girls and boys.
#
#  To do this, he needs to find MD5 hashes which, in hexadecimal, start with at least five zeroes. 
#  The input to the MD5 hash is some secret key (your puzzle input, given below) followed by a 
#  number in decimal. To mine AdventCoins, you must find Santa the lowest positive number 
#  (no leading zeroes: 1, 2, 3, ...) that produces such a hash.
#
# For example:
#  
#  - If your secret key is abcdef, the answer is 609043, because the 
#     MD5 hash of abcdef609043 starts with five zeroes (000001dbbfa...), 
#     and it is the lowest such number to do so.
#  
#  - If your secret key is pqrstuv, the lowest number it combines with 
#     to make an MD5 hash starting with five zeroes is 1048970; that is, 
#     the MD5 hash of pqrstuv1048970 looks like 000006136ef....
#

# Load util
source("util.r")

##
# Challenge 1
#
# Input is yzbqklnj

test1 <- "abcdef"
test2 <- "pqrstuv"

input <- "yzbqklnj"

library(digest)
find5zerohash <- function(secret, leadingzeroes) {
  check <- paste(rep("0", leadingzeroes), sep = "", collapse = "")
  i <- 1
  while(substr(digest(paste(secret, i, sep = ""), algo = "md5", serialize = F), 1, leadingzeroes) != check) {
    i <- i + 1
  }
  return(i)
}

assert(expected = 609043, actual = find5zerohash(test1, 5))
assert(expected = 1048970, actual = find5zerohash(test2, 5))

# Correct result 282749
print(paste("The leading number to yield 5 zeroes in beginning of md5 is:", find5zerohash(input, 5)))

##
# Challenge 2
#
# Find the leading number which yields the 6 first leading zeroes in the md5 hash

# Correct result 9962624
print(paste("The leading number to yield 6 zeroes in beginning of md5 is:", find5zerohash(input, 6)))
