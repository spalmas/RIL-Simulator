#' FOREST RANDOMIZER
#'
#' Creates a simulated forest based on species and sizes probabilities and rotation years
#'
#' @param lambda mean trees per hectare
#' @param ROTATIONYEARS how many cutting areas are in the forest. There will be this number of one hectare plots/
#'
#' @references
#' 
#' @return dataframe with ACA, SPECIES.CODE, DBH, HEIGHT, UNDER.BOSQUETE, COORD.X AND COORD.Y
#'
#' @examples
#' forest <- forest.randomizer(ROTATIONYEARS = 1)
#' head(forest)
forest.randomizer <- function(lambda = 70, ROTATIONYEARS = 1){

  ACA.n.trees <- rpois(n = ROTATIONYEARS, lambda = lambda)     #Random number of trees in each ACU
  
  #list of length sum(ACU.n.trees) with the ACU code for each tree based on the randomized ACU.n.trees
  ACA <- rep(x = 0:(ROTATIONYEARS-1), times = ACA.n.trees)
  
  #Randomization of species
  species.options <- c('SWMA', 'LYLA', 'MEBR', 'MAZA', 'POUN', 'XXXX') #options of species
  SPECIES.CODE <- species.options %>% sample(size = sum(ACA.n.trees), replace = TRUE)  #A list of n.trees species
  
  DBH <- 15 *rexp(n = sum(ACA.n.trees))  #random diameter list  #where did this distribution came from?
  D.DBH <- rep(x = c(NA), times = sum(ACA.n.trees))
  HEIGHT <- DBH %>% get.height()
  UNDER.BOSQUETE <- rep(x = FALSE, times = sum(ACA.n.trees))
  COORD.X <- runif(n = sum(ACA.n.trees), min = 0, max=99)
  COORD.Y <- runif(n = sum(ACA.n.trees), min = 0, max=99)
  
  df <- tibble(ACA, SPECIES.CODE, DBH, D.DBH, HEIGHT, UNDER.BOSQUETE, COORD.X, COORD.Y)
  df$AGB <- get.agb(forest = df)
  
  return(df)
}
