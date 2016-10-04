# Function to estimate height from a list of diameters
# It randomizes the height instead of having a closed solution
get.height <- function(DBH){
  #Height allometric equation from ?
  HEIGHT <- exp(0.93687 + 0.55204*log(DBH)) + rnorm(n = length(DBH), mean = 1, sd = 1)
  return(HEIGHT)
}

