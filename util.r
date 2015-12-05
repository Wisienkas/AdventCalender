assert <- function(expected, actual) {
  if(expected != actual) print(paste("The Expected:", expected, "was not equal the actual:", actual))
}