# --- Day 3: Perfectly Spherical Houses in a Vacuum ---
#  
#  Santa is delivering presents to an infinite two-dimensional grid of houses.
#
# He begins by delivering a present to the house at his starting location, and then 
# an elf at the North Pole calls him via radio and tells him where to move next. 
# Moves are always exactly one house to the north (^), south (v), east (>), or west (<). 
# After each move, he delivers another present to the house at his new location.
#
# However, the elf back at the north pole has had a little too much eggnog, and so 
# his directions are a little off, and Santa ends up visiting some houses more than once. 
# How many houses receive at least one present?
#
# For example:
#  
#  > delivers presents to 2 houses: one at the starting location, and one to the east.
#
#  ^>v< delivers presents to 4 houses in a square, including twice to the house at his 
#    starting/ending location.
#
#  ^v^v^v^v^v delivers a bunch of presents to some very lucky children at only 2 houses.

# Load util
source("util.r")

# Read in data
data <- read.csv("data/day3.txt", header = F)
txt_sequence <- as.character(data[1,1])

char_arr <- rawToChar(charToRaw(txt_sequence), multiple = T)

##
# Challenge 1
#
# How many unique houses receives presents?

directionforce <- function(char) {
  return(lapply(char, function(x) switch (x,
    "^" = return(c(0, 1)),
    "v" = return(c(0, -1)),
    "<" = return(c(-1, 0)),
    ">" = return(c(1, 0))
  )))
}

deliverycoords <- function(char) {
  df <- data.frame()
  for(force in directionforce(char)) {
    if(nrow(df) == 0) {
      df <- rbind(c(x = force[1], y = force[2]))
    }
    else {
      df <- rbind(df, c(x = force[1] + df[nrow(df), 1], y = force[2] + df[nrow(df), 2]))
    }
  }
  return(df)
}

result <- deliverycoords(char_arr)

# Correct result 2565
print(paste("Santa elves has been at:", nrow(unique(result)), "unique houses"))

##
# Challenge 2
#
# --- Part Two ---
#
# The next year, to speed up the process, Santa creates a robot version of himself, 
# Robo-Santa, to deliver presents with him.
#
# Santa and Robo-Santa start at the same location (delivering two presents to the 
# same starting house), then take turns moving based on instructions from the elf, 
# who is eggnoggedly reading from the same script as the previous year.
#
# This year, how many houses receive at least one present?
#
# For example:
#  
#  ^v delivers presents to 3 houses, because Santa goes north, and then Robo-Santa goes south.
#
#  ^>v< now delivers presents to 3 houses, and Santa and Robo-Santa end up back where they started.
#
#  ^v^v^v^v^v now delivers presents to 11 houses, with Santa going one direction and Robo-Santa going the other.
#
# How many unique houses receives presents?

char_robot <- char_arr[seq(from = 1, to = length(char_arr), by = 2)]
char_santa <- char_arr[seq(from = 2, to = length(char_arr), by = 2)]

result_santa <- deliverycoords(char_santa)
result_robot <- deliverycoords(char_robot)

print(paste("Santa has been at:", nrow(unique(result_santa)), "unique houses"))
print(paste("Santa-Robot has been at:", nrow(unique(result_robot)), "unique houses"))

result <- rbind(result_santa, result_robot)

# Correct Result 2639
print(paste("Sanata and Santa-Robot has been at:", nrow(unique(result)), "unique houses"))