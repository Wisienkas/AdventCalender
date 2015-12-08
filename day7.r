# --- Day 7: Some Assembly Required ---
#   
#  This year, Santa brought little Bobby Tables a set of wires 
#   and bitwise logic gates! Unfortunately, little Bobby is a 
#   little under the recommended age range, and he needs help 
#   assembling the circuit.
#
#  Each wire has an identifier (some lowercase letters) and 
#   can carry a 16-bit signal (a number from 0 to 65535). 
#   A signal is provided to each wire by a gate, another wire, 
#   or some specific value. Each wire can only get a signal 
#   from one source, but can provide its signal to multiple 
#   destinations. A gate provides no signal until all of its 
#   inputs have a signal.
#
#  The included instructions booklet describes how to connect 
#   the parts together: x AND y -> z means to connect wires x 
#   and y to an AND gate, and then connect its output to wire z.
#
# For example:
#  
#  123 -> x means that the signal 123 is provided to wire x.
#
#  x AND y -> z means that the bitwise AND of wire x and wire 
#   y is provided to wire z.
#
#  p LSHIFT 2 -> q means that the value from wire p is 
#   left-shifted by 2 and then provided to wire q.
#
#  NOT e -> f means that the bitwise complement of the value 
#   from wire e is provided to wire f.
#
#  Other possible gates include OR (bitwise OR) and RSHIFT 
#   (right-shift). If, for some reason, you'd like to emulate 
#   the circuit instead, almost all programming languages 
#   (for example, C, JavaScript, or Python) provide operators 
#   for these gates.
#
# For example, here is a simple circuit:
#
# 123 -> x
# 456 -> y
# x AND y -> d
# x OR y -> e
# x LSHIFT 2 -> f
# y RSHIFT 2 -> g
# NOT x -> h
# NOT y -> i
# 
# After it is run, these are the signals on the wires:
#
# d: 72
# e: 507
# f: 492
# g: 114
# h: 65412
# i: 65079
# x: 123
# y: 456

# Read data
data <- file("data/day7.txt")
data <- readLines(data)

##
# Challenge 1
library(stringr)
library(bit)
# To cast integer to bit
bit16 <- function(x) {
  x = x %% 2^16
  i <- 0
  string <- numeric(16)
  while(x > 0) {
    string[16 - i] <- x %% 2
    x <- x %/% 2
    i <- i + 1 
  }
  return(string)
}
# To cast back to integer
int16 <- function(x) {
  mult <- 2^15
  string <- numeric(1)
  for(i in 1:length(x)) {
    if(x[i]) string <- string + mult
    mult <- mult %/% 2
  }
  return(string)
}

shift <- function(x, offset) {
  offset <- offset %% length(x)
  if(offset == 0) return x
  if(offset < 0) return leftshift(x, offset * -1)
  if(offset > 0) return rightshift(x, offset)
}

# Moves bit to left, drop at end
#
# Example with 4: 
# 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0
# LEFT 2
# 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0 0
leftshift <- function(x, offset) {
  offset <- offset %% length(x)
  return(c(x[-(1:offset)], rep(0, times = offset)))
}

# Moves bit to right, drop at end
#
# Example with 4: 
# 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0
# RIGHT 2
# 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1
rightshift <- function(x, offset) {
  print(paste("x: ", length(x), "offset", offset))
  offset <- offset %% length(x)
  return(c(rep(0, times = offset), x[1:(length(x) - offset)]))
}

not <- function(x) {
  return (ifelse(x == 0, 1, 0))
}

and <- function(x, y) {
  return (ifelse(x == y, 1, 0))
}

or <- function(x, y) {
  return ifelse(x + y > 0, 1, 0)
}

# Split command and target
split_data <- str_split(data, "->")
split_data <- lapply(split_data, str_trim)
split_data <- data.frame(matrix(unlist(split_data), nrow=length(split_data), byrow=T))
names(split_data) <- c("wiring", "target")

commands <- split_data

numb <- data.frame(matrix(unlist(rep(rep(bit16(0)), times = nrow(split_data))), nrow=nrow(split_data), byrow=T))
valueList <- cbind(unique(split_data[2]), numb)

# Will return the id's of the command
getWires <- function(command) {
  splitted <- unlist(str_split(command, " "))
  commandIndex <- grep(pattern = "[a-z]", x = splitted)
  return(splitted[commandIndex])
}

getNumbers <- function(command) {
  splitted <- unlist(str_split(command, " "))
  commandIndex <- grep(pattern = "[1-9]", x = splitted)
  return(splitted[commandIndex])
}

getCommand <- function(command) {
  splitted <- unlist(str_split(command, " "))
  commandIndex <- grep(pattern = "[A-Z]", x = splitted)
  return(splitted[commandIndex])
}
##
## HANDLING OUTCOMES
## 
handleAND <- function(wires, target) {
  target.index <- which(valueList[,1] == target)
  wires.index <- which(valueList[,1] == wires)
  valueList[target.index, 2:17] <- and(valueList[wires.index[1], 2:17], valueList[wires.index[2], 2:17])
  return()
}

handleNOT <- function(wire, target) {
  target.index <- which(valueList[,1] == target)
  wire.index <- which(valueList[,1] == wire)
  valueList[target.index, 2:17] <- and(valueList[wire.index, 2:17])
  return()
}

handleOR <- function(wires, target) {
  target.index <- which(valueList[,1] == target)
  wires.index <- which(valueList[,1] == wires)
  valueList[target.index, 2:17] <- or(valueList[wires.index[1], 2:17], valueList[wires.index[2], 2:17])
  return()
}

handleRSHIFT <- function(wire, number, target) {
  target.index <- which(valueList[,1] == target)
  wire.index <- which(valueList[,1] == wire)
  valueList[target.index, 2:17] <- rightshift(valueList[wire.index, 2:17], number[1])
  return()
}

handleLSHIFT <- function(wire, number, target) {
  target.index <- which(valueList[,1] == target)
  wire.index <- which(valueList[,1] == wire)
  valueList[target.index, 2:17] <- leftshift(valueList[wire.index, 2:17], number[1])
  return()
}

handleCommand <- function(commandline, target) {
  target.index <- which(valueList[,1] == target)
  wires <- getWires(commandline)
  command <- getCommand(commandline)
  numbers <- getNumbers(commandline)
  
  if(length(command) > 0) {
    switch(command[1],
      AND = handleAND(wires, target),
      NOT = handleNOT(wires, target),
      RSHIFT = handleRSHIFT(wires, numbers, target),
      LSHIFT = handleLSHIFT(wires, numbers, target),
      OR = handleOR(wires, target)
    )
  } else if(length(wires) == 1) {
    wires.index <- which(valueList[,1] == wires)
    valueList[target.index, 2:17] <- valueList[wires.index, 2:17]
  } else if(length(numbers) == 1) {
    valueList[target.index, 2:17] <- bit16(numbers)
  }
}