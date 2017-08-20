#' HURRICANE MORTALITY 
#'
#' Assigns trees mortality from hurricanes using the small.mortality estimated in the simulator
#'
#' @param forest Complete list of forest trees
#' @param small.mortality Probability of dying for small trees. COmes from the simulator.
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
hurricane.mortality <- function(forest, small.mortality){
    #empty FALSE vector
  hurricane.dead <- rep(x = c(FALSE), times = nrow(forest))   #creates an empty array of FALSE with the length of the stand
  
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
