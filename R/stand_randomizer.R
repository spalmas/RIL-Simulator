# Function to randomize initial stand list
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
