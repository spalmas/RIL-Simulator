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
#' @return an array of boolean values with length equal to the number of trees in the stand. TRUE: the tree if it is killed by the hurricane
#'
#' @examples
#' source('startup.R')
#' stand <- stand.randomizer()
#' hurricane.mortality(stand)
#' 
hurricane.mortality <- function(stand){
  
  #10% of hurricane probability
  hurricane.year <- sample(x = c(TRUE, FALSE), prob = c(0.1, 0.9), size = 1)
  
  #empty FALSE vector
  hurricane.dead <- rep(x = c(FALSE), times = nrow(stand))   #cretes an empty array of FALSE with the length of the stand
  
  if (hurricane.year){
    
    hurricane.dead [stand$DBH <  10] <- sample(x = c(TRUE, FALSE), prob = c(0.2, 0.8), replace = TRUE, size = sum(stand$DBH < 10))
    hurricane.dead [stand$DBH >= 10] <- sample(x = c(TRUE, FALSE), prob = c(0.1, 0.9), replace = TRUE, size = sum(stand$DBH >= 10))
  }
  #returns a list of dead trees
  return(hurricane.dead)
}
