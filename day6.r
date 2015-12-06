# --- Day 6: Probably a Fire Hazard ---
#  
# Because your neighbors keep defeating you in the holiday house 
# decorating contest year after year, you've decided to deploy 
# one million lights in a 1000x1000 grid.
#
# Furthermore, because you've been especially nice this year, 
# Santa has mailed you instructions on how to display the ideal 
# lighting configuration.
#
# Lights in your grid are numbered from 0 to 999 in each direction; 
# the lights at each corner are at 0,0, 0,999, 999,999, and 999,0. 
# The instructions include whether to turn on, turn off, or toggle 
# various inclusive ranges given as coordinate pairs. Each coordinate 
# pair represents opposite corners of a rectangle, inclusive; a 
# coordinate pair like 0,0 through 2,2 therefore refers to 9 lights
# in a 3x3 square. The lights all start turned off.
#
# To defeat your neighbors this year, all you have to do is set up 
# your lights by doing the instructions Santa sent you in order.
# 
# For example:
#  
#   turn on 0,0 through 999,999 would turn on (or leave on) every light.
# 
#   toggle 0,0 through 999,0 would toggle the first line of 1000 lights, 
#     turning off the ones that were on, and turning on the ones that were off.
#
#   turn off 499,499 through 500,500 would turn off (or leave off) the 
#     middle four lights.

# Load util
source("util.r")
library(stringr)

# Read in raw data
data <- read.csv("data/day6.txt", header = F)
last <- data[,3]
first <- data[,1]
mixed <- data[,2]

first_digit <- gsub(pattern = "[^0-9]", x = first, replacement = "")
mixed_digit <- str_split(gsub(pattern = "[^0-9]", x = mixed, replacement = " "), pattern = "\\s+")
mixed_digit <- data.frame(matrix(unlist(mixed_digit), nrow=length(mixed_digit), byrow=T))

numbers <- cbind(data.frame(matrix(first_digit)), mixed_digit, last)
names(numbers) <- c("x1_1", "x1_2", "x2_1", "x2_2")

from <- numbers[, 1:2]
from[, 1] <- as.integer(as.character(from[, 1])) + 1
from[, 2] <- as.integer(as.character(from[, 2])) + 1

to <- numbers[, 3:4]
to[, 1] <- as.integer(as.character(to[, 1])) + 1
to[, 2] <- as.integer(as.character(to[, 2])) + 1

x <- rep(x = 0, times = 1000)

lightarray <- outer(x, x)

on_list <- grep("on", first)
off_list <- grep("off", first)
toggle_list <- grep("toggle", first)

##
# Challenge 1
#
# After following the instructions, how many lights are lit?

for(i in 1:nrow(data)) {
  rowRange <- from[i,1]:to[i,1]
  colRange <- from[i,2]:to[i,2]
  part <- lightarray[rowRange, colRange]
  if(i %in% on_list) {
    part <- 1
  } else if(i %in% off_list) {
    part <- 0
  } else if(i %in% toggle_list) {
    part <- ifelse(part == 1, 0, 1)
  }
  lightarray[from[i,1]:to[i,1], from[i,2]:to[i,2]] <- part
}

# Correct Answer 543903
print(paste("The amount of lights lit after all instructions:", sum(lightarray)))

##
# Challenge 2
#
# --- Part Two ---
#
# You just finish implementing your winning light pattern when you 
# realize you mistranslated Santa's message from Ancient Nordic Elvish.
#
# The light grid you bought actually has individual brightness controls; 
# each light can have a brightness of zero or more. The lights all start at zero.
#
# The phrase turn on actually means that you should increase the brightness 
# of those lights by 1.
#
# The phrase turn off actually means that you should decrease the brightness 
# of those lights by 1, to a minimum of zero.
#
# The phrase toggle actually means that you should increase the brightness 
# of those lights by 2.
#
# What is the total brightness of all lights combined after following 
# Santa's instructions?
#
# For example:
#  
#   turn on 0,0 through 0,0 would increase the total brightness by 1.
#
#   toggle 0,0 through 999,999 would increase the total brightness by 2000000.

lightarray <- outer(x, x)

for(i in 1:nrow(data)) {
  rowRange <- from[i,1]:to[i,1]
  colRange <- from[i,2]:to[i,2]
  part <- lightarray[rowRange, colRange]
  if(i %in% on_list) {
    part <- part + 1
  } else if(i %in% off_list) {
    part <- ifelse(part > 0, part - 1, 0)
  } else if(i %in% toggle_list) {
    part <- part + 2
  }
  lightarray[from[i,1]:to[i,1], from[i,2]:to[i,2]] <- part
}

# Correct Answer 14687245
print(paste("The Total brightness after all instructions:", sum(lightarray)))
