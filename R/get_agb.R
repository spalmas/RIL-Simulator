#' Function to estimate Above Ground Biomass of the trees from Cairns Paper
#'
#' Gets the AGB for a table of trees
#'
#' @param forest: table of trees with columns of DBH and HEIGHT 
#'
#' @references  Cairns paper
#' 
#' @return AGB in megagrams of Carbon of dried biomass
#'
#' @examples
#' source('startup.R')
#' stand <- stand.randomizer()
#' stand$AGB <- get.agb(stand) 
#' stand$DIAMETER.GROWTH <- 0
#' stand <- rbind(stand, get.regeneration(stand))
#' get.agb(stand)

get.agb <- function(forest = NULL, DBH = NA, HEIGHT = NA){
  if (is.null(forest)){
    return(0.47*(exp(-2.173+0.868 * log(DBH^2 * HEIGHT)+ 0.0939 / 2)) / 1000)
  }
  return(0.47*(exp(-2.173+0.868 * log(forest$DBH^2 * forest$HEIGHT)+ 0.0939 / 2)) / 1000)
}