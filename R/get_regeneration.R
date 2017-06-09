#' GET REGENERATION
#'
#' Long Description: Estimates new trees that are added to the forest. 
#' The parameters of regeneration are mean and standard deviation number of recruits 
#' in one hectare by species and depending in the percentage of canopy level.
#' 
#'
#' @param forest list of trees in forest
#' @param canopy.cover is the mean for all canopy covers
#'
#' @references
#' Ninguna por ahora

#' @return a table new trees that should r bind with forest.
#'
#' @examples
#' source('startup.R')
#' forest <- forest.randomizer(ROTATIONYEARS = 2)
#' get.regeneration(forest)

get.regeneration <- function(forest, canopy.cover){
  #subset data to only that canopy cover and existing species
  canopy.cover  <- 999
  regen.params.subset <- regen.params %>% filter(CANOPY.COVER == canopy.cover,
                                                 SPECIES.CODE %in% forest$SPECIES.CODE)

  #randomization of normal regeneration value
  regen.n <- mapply(rnorm,
                       n = 1,
                       mean = regen.params.subset$REG.N.HA.MEAN,
                       sd = regen.params.subset$REG.N.HA.SD)

  #There is no negative recruitment
  regen.n[regen.n < 0] <- 0  #no negative regeneration, just in case.
  
  #Convert number of recruits to integers
  regen.n <- round(regen.n)

  #total number of recruits
  forest.regen.n <- sum(regen.n)
  
  #Creating list of species
  SPECIES.CODE <- rep(x = regen.params.subset$SPECIES.CODE, times = regen.n)
  ACA <- sample(x = unique(forest$ACA), replace = TRUE, size = forest.regen.n) #Randomized ACU, all with same probabilities
  DBH <- 10 + (rexp(n = length(SPECIES.CODE)))/2   #
  HEIGHT <- get.height(DBH)
  #HEIGHT[HEIGHT < 0] <- 5  #No trees under 5 cm, maybe we need a change in distribution NOT NEEDED?
  DIAMETER.GROWTH <- rep(x = 0, times = )   #no initial DIAMETER.GROWTH. Just added for the column
  #regen.table$VOLUME <- get.volume(regen.table)
  AGB <- get.agb(DBH = DBH, HEIGHT = HEIGHT)
  UNDER.BOSQUETE <- rep(x = c(FALSE), times = forest.regen.n)
  COORD.X <- runif(n = forest.regen.n, min = 0, max = 99)
  COORD.Y <- runif(n = forest.regen.n, min = 0, max = 99)
  HARVESTED <- rep(x = c(FALSE), times = forest.regen.n)
  
  #regen.table$VOLUME <- NA

  #delete row names to avoid problems later
  #row.names(regen.table) <- NULL
  
  return(data.frame(ACA, SPECIES.CODE, DBH, HEIGHT, AGB, UNDER.BOSQUETE, COORD.X, COORD.Y, HARVESTED, row.names = NULL))
}

