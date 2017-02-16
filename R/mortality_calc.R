#' NATURAL MORTALITY 
#'
#' Returns a T/F list of killed trees in the stand based on X probabilities.
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
  
  #There is 1% probability of dying in a year
  random.dead.trees <- sample(x = c(TRUE, FALSE), 
                         size = nrow(stand),
                         replace = TRUE, 
                         prob = c(0.01, 0.99))
  
  return(random.dead.trees)
}
