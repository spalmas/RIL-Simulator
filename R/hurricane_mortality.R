#' HURRICANE MORTALITY 
#'
#' Calculates a probability for catastrophic hurricane every 10 years for each tree
#' and kills a probability of around X% of all trees
#'
#' @param forest Complete list of forest trees
#'
#' @references
#' Whigham et al. 1991; Sanchez Sanchez 1999; Bonilla-Moheno 2010; McGroddy et al. 2013
#' 
#' @return an array of boolean values with length equal to the number of trees in the stand. TRUE: the tree if it is killed by the hurricane
#'
#' @examples
#' source('startup.R')
#' forest <- forest.randomizer()
#' hurricane.mortality(forest)
#' 
hurricane.mortality <- function(forest){
  
  #Hurricane mortality probabilities. Already with
  small.mortality <- sample(x = c(0, 0.1, 0.2, 0.3),   #For hurricane 3,4,5, small trees. Big trees are + 0.1
                           prob = c(0.55, 0.2, 0.15, 0.1),  size = 1)
  
  #empty FALSE vector
  hurricane.dead <- rep(x = c(FALSE), times = nrow(forest))   #cretes an empty array of FALSE with the length of the stand
  
  if (small.mortality > 0){
    
    hurricane.dead [forest$DBH <  20] <- sample(x = c(TRUE, FALSE), 
                                                prob = c(small.mortality, 1 - small.mortality),
                                                replace = TRUE,
                                                size = sum(forest$DBH < 20))
    
    hurricane.dead [forest$DBH >= 20] <- sample(x = c(TRUE, FALSE),
                                                prob = c(small.mortality + .1, 1-small.mortality - 0.1), 
                                                replace = TRUE,
                                                size = sum(forest$DBH >= 20))
  }
  #returns a list of dead trees
  return(hurricane.dead)
}
