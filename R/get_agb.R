#' Function to estimate Above Ground Biomass of the trees from Cairns Paper
#'
#' Gets the AGB for a table of trees
#'
#' @param stand: table of trees with columns of DBH and HEIGHT 
#'
#' @references  Cairns paper
#' 
#' @return AGB in megagrams of Carbon of dried biomass
#'
#' @examples
#' source('startup.R')
#' stand <- stand.randomizer()
#' stand <- get.regeneration(stand)
#' get.agb(stand)

get.agb <- function(stand){
  return(0.47*(exp(-2.173+0.868 * log(stand$DBH^2 * stand$HEIGHT)+ 0.0939 / 2)) / 1000)
}

get.agb(trees.tab)
