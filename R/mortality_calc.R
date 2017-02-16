#' NATURAL MORTALITY 
#'
#' Returns a T/F list of killed trees in the stand based on X probabilities. Smaller trees have a higher probability of mortality
#'
#' @param stand The table of trees in the stand
#'
#' @references
#' 
#' @return an array of boolean values: TRUE: the tree if it is killed by natural causes
#'
#' @examples
#' source('startup.R')
#' stand <- stand.randomizer()
#' mortality.calc(stand)
#' 
mortality.calc <- function(stand){
  
  #cretes an empty array of FALSE with the length of the stand
  natural.dead <- rep(x = c(FALSE), times = length(stand$DBH))
  natural.dead [stand$DBH < 10] <- sample(x = c(TRUE, FALSE), prob = c(0.03, 0.97), replace = TRUE, size = sum(stand$DBH < 10))
  natural.dead [stand$DBH >= 10] <- sample(x = c(TRUE, FALSE), prob = c(0.01, 0.99), replace = TRUE, size = sum(stand$DBH >= 10))
  return(natural.dead)
}
