#' HURRICANE MORTALITY 
#'
#' Calculates a probability for catastrophic hurricane every 10 years for each tree
#' and kills a probability of around X% of all trees
#'
#' @param stand The table of trees in the stand
#'
#' @references
#' Whigham et al. 1991; Sanchez Sanchez 1999; Bonilla-Moheno 2010; McGroddy et al. 2013
#' 
#' @return an array of boolean values: TRUE: the tree if it is killed by the hurricane
#'
#' @examples
#' source('startup.R')
#' stand <- stand.randomizer()
#' hurricane.mortality(stand)
#' 
hurricane.mortality <- function(stand){
  #cretes an empty array of FALSE with the length of the stand
  hurricane.dead <- rep(x = c(FALSE), times = length(stand$DBH))
  hurricane.year <- sample(x = c(TRUE, FALSE), prob = c(0.1, 0.9), size = 1)
  
  if (hurricane.year){
    hurricane.dead [stand$DBH < 20] <- sample(x = c(TRUE, FALSE), prob = c(0.5, 0.9), replace = TRUE, size = sum(stand$DBH < 20))
    hurricane.dead [stand$DBH >= 20] <- sample(x = c(TRUE, FALSE), prob = c(0.25, 0.9), replace = TRUE, size = sum(stand$DBH >= 20))
  }
  #remove trees that were inside rectangles
  return(hurricane.dead)
}