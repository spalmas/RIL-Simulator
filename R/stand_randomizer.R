#' STAND RANDOMIZED
#'
#' Creates a simulated one hectare stand based on probabilities 
#'
#' @param stand The table of trees in the stand
#'
#' @references
#' 
#' @return dataframe with SPECIES.CODE, DBH, HEIGHT, UNDER.BOSQUETE, COORD.X AND COORD.Y
#'
#' @examples
#' source('startup.R')
#' stand <- stand.randomizer()
#' head(stand)
stand.randomizer <- function(){
  n.trees <- rpois(n = 1, lambda = 70)     #Random number of trees
  species.options <- c('SM', 'LL', 'MB', 'MZ', 'PU', 'XX') #options of species
  SPECIES.CODE <- sample(species.options, size = n.trees, replace = TRUE)  #A list of n.trees species
  DBH <- 15 *rexp(n = n.trees)  #random diameter list
  HEIGHT <- get.height(DBH)
  UNDER.BOSQUETE <- rep(x = FALSE, times = n.trees)
  
  COORD.X <- runif(n = n.trees, min = 0, max=99)
  COORD.Y <- runif(n = n.trees, min = 0, max=99)

  return(data.frame(SPECIES.CODE, DBH, HEIGHT, UNDER.BOSQUETE, COORD.X, COORD.Y))
}