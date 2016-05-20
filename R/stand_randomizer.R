# Function to randomize initial stand list
stand.randomizer <- function(){
  n.trees <- rpois(n = 1, lambda = 70)     #Random number of trees
  species.options <- c('SM', 'LL', 'MB', 'MZ', 'PU', 'XX') #options of species
  SPECIES.CODE <- sample(species.options, size = n.trees, replace = TRUE)  #A list of n.trees species
  DBH <- 15 *rexp(n = n.trees)  #random diameter list
  HEIGHT <- exp(0.93687 + 0.55204*log(DBH))
  COORD.X <- sample(1:100, size = n.trees)  #random x locations
  COORD.Y <- sample(1:100, size = n.trees)  #random y locations
  
  return(data.frame(SPECIES.CODE, DBH, HEIGHT, COORD.X, COORD.Y))
}
