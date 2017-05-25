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
#' forest <- stand.randomizer()
#' mortality.calc(forest)
#' 
mortality.calc <- function(forest){
  
  #cretes an empty array of FALSE with the length of the stand
  natural.dead <- rep(x = c(FALSE), times = nrow(forest))
  
  #trees smaller than 10cm have a 3% of mortality probability
  natural.dead [forest$DBH < 10] <- sample(x = c(TRUE, FALSE), prob = c(0.03, 0.97), replace = TRUE, size = sum(forest$DBH < 10))
  
  #trees larger than 10cm have a 1% of mortality probability
  natural.dead [forest$DBH >= 10] <- sample(x = c(TRUE, FALSE), prob = c(0.01, 0.99), replace = TRUE, size = sum(forest$DBH >= 10))
  
  return(natural.dead)
}