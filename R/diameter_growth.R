# Function to estimate diameter growth from the normal distribution.
# Does not assign new diameter. For that see diameter_assign
# this function could also be altered to allow for temporal correlation of growth
diameter_growth <- function(stand){
  return (mapply(rnorm,
                 n = 1, 
                 mean = stand$DBH.GROWTH.MEAN,
                 sd = stand$DBH.GROWTH.SD))
}