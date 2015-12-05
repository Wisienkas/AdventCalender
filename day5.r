# --- Day 5: Doesn't He Have Intern-Elves For This? ---
#
# Santa needs help figuring out which strings in his text file are naughty or nice.
#
# A nice string is one with all of the following properties:
#
# It contains at least three vowels (aeiou only), like aei, xazegov, 
# or aeiouaeiouaeiou.
# 
# It contains at least one letter that appears twice in a row, like xx, 
# abcdde (dd), or aabbccdd (aa, bb, cc, or dd).
# 
# It does not contain the strings ab, cd, pq, or xy, even if they are 
# part of one of the other requirements.
#
# For example:
#
#     ugknbfddgicrmopn is nice because it has at least three vowels (u...i...o...), 
#     a double letter (...dd...), and none of the disallowed substrings.
#
#     aaa is nice because it has at least three vowels and a double letter, even 
#     though the letters used by different rules overlap.
#
#     jchzalrnumimnmhp is naughty because it has no double letter.
#
#     haegwjzuvuyypxyu is naughty because it contains the string xy.
#
#     dvszwmarrgswjxmb is naughty because it contains only one vowel.

# Load util
source("util.r")

# Reading input
data <- read.csv("data/day5.txt", header = F)
# convert to string rather than factors
data[] <- lapply(data, as.character)

## 
# Challenge 1
#
# How many Strings are nice

vowelFilter <- c("a", "e", "i", "o", "u")
naughtyFilter <- c("ab", "cd", "pq", "xy")

has.double.letter <- function(string) {
  char_arr <- rawToChar(charToRaw(string), multiple = T)
  char_arr_first <- char_arr[1:length(char_arr) - 1]
  char_arr_second <- char_arr[2:length(char_arr)]
  
  return(sum(char_arr_first == char_arr_second) > 0)
}

get.filter.count <- function(filter, subject) {
  return (sum(
    sapply(rawToChar(charToRaw(subject), multiple = T), FUN = function(x) {
      any(
        sapply(filter, char = x, function(x, char) {
          grepl(x, char)
        })
      )
    })
  ))
}

is.naughty <- function(filter, subject) {
  return (any(
        sapply(filter, string = subject, function(x, string) {
          grepl(x, string)
        })
    ))
}

is.nice <- function(string, threshold) {
  if(get.filter.count(vowelFilter, string) >= threshold) {
    if(has.double.letter(string)) {
      if(!is.naughty(naughtyFilter, string)) {
        return(T)
      }
    }
  }
  return(F)
}

# Tests
# 1
assert(expected = 3, actual = get.filter.count(vowelFilter, "ugknbfddgicrmopn"))
assert(expected = F, actual = is.naughty(naughtyFilter, "ugknbfddgicrmopn"))
assert(expected = T, actual = has.double.letter("ugknbfddgicrmopn"))
assert(expected = T, actual = is.nice("ugknbfddgicrmopn", 3))
# 2
assert(expected = 3, actual = get.filter.count(vowelFilter, "aaa"))
assert(expected = F, actual = is.naughty(naughtyFilter, "aaa"))
assert(expected = T, actual = has.double.letter("aaa"))
assert(expected = T, actual = is.nice("aaa", 3))
# 3 
assert(expected = F, actual = has.double.letter("jchzalrnumimnmhp"))
assert(expected = F, actual = is.nice("jchzalrnumimnmhp", 3))
# 4 
assert(expected = T, actual = is.naughty(naughtyFilter, "haegwjzuvuyypxyu"))
assert(expected = F, actual = is.nice("haegwjzuvuyypxyu", 3))
# 5
assert(expected = 1, actual = get.filter.count(vowelFilter, "dvszwmarrgswjxmb"))
assert(expected = F, actual = is.nice("dvszwmarrgswjxmb", 3))

# Correct Result is 258
print(paste("The amount of nice strings:", sum(apply(data, 1, FUN = function(x) is.nice(string = as.character(x), threshold = 3)))))

# --- Part Two ---
# 
# Realizing the error of his ways, Santa has switched to a better model of determining 
# whether a string is naughty or nice. None of the old rules apply, as they are all clearly ridiculous.
# 
# Now, a nice string is one with all of the following properties:
#  
# It contains a pair of any two letters that appears at least twice in the string without overlapping, 
#   like xyxy (xy) or aabcdefgaa (aa), but not like aaa (aa, but it overlaps).
# It contains at least one letter which repeats with exactly one letter between them, like xyx, 
#   abcdefeghi (efe), or even aaa.
#
# For example:
#  
#   qjhvhtzxzqqjkmpb is nice because is has a pair that appears twice (qj) 
#     and a letter that repeats with exactly one letter between them (zxz).
#
#   xxyxx is nice because it has a pair that appears twice and a letter that 
#     repeats with one between, even though the letters used by each rule overlap.
#
#   uurcxstgmygtbstg is naughty because it has a pair (tg) but no repeat with a single letter between them.
#
#   ieodomkazucvgmuy is naughty because it has a repeating letter with one between (odo), but no pair that appears twice.

##
# Challenge 2
# 
# How many strings are nice under these new rules

get.pairs <- function(subject) {
  string <- rawToChar(charToRaw(subject), multiple = T)
  string_start <- string[1:(length(string) - 1)]
  string_end <- string[2:length(string)]
  
  return(paste(string_start, string_end, sep = ""))
}


get.double.occurence <- function(subject) {
  
  pairs <- get.pairs(subject)
  
  sum <- 0
  for(i in 3:nchar(subject)) {
    matches <- sum(pairs[i - 2] == get.pairs(substr(subject, i, nchar(subject))))
    if(!is.na(matches)) sum <- sum + matches
  }
  return(sum)
}


get.one.betweeners <- function(subject) {
  string <- rawToChar(charToRaw(subject), multiple = T)
  string_start <- string[1:(length(string) - 2)]
  string_end <- string[3:length(string)]
  
  return(sum(string_start == string_end))
}

is.nice2 <- function(subject) {
  return(get.one.betweeners(subject) > 0 && get.double.occurence(subject) > 0)
}

# Tests
# 1
assert(expected = 1, get.double.occurence("qjhvhtzxzqqjkmpb"))
assert(expected = 2, get.one.betweeners("qjhvhtzxzqqjkmpb"))
assert(expected = T, is.nice2("qjhvhtzxzqqjkmpb"))

# 2
assert(expected = 1, get.double.occurence("xxyxx"))
assert(expected = 1, get.one.betweeners("xxyxx"))
assert(expected = T, is.nice2("xxyxx"))

# 3
assert(expected = 2, get.double.occurence("uurcxstgmygtbstg"))
assert(expected = 0, get.one.betweeners("uurcxstgmygtbstg"))
assert(expected = F, is.nice2("uurcxstgmygtbstg"))

# 4
assert(expected = 0, get.double.occurence("ieodomkazucvgmuy"))
assert(expected = 1, get.one.betweeners("ieodomkazucvgmuy"))
assert(expected = F, is.nice2("ieodomkazucvgmuy"))

# Correct Result is 53
print(paste("Given new rules, the amount of nice strings is:", sum(apply(data, 1, is.nice2))))
