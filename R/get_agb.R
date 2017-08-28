#' Function to estimate Above Ground Biomass of the trees from Chave Paper
#'
#' Gets the AGB for a table of trees
#'
#' @param forest: table of trees with columns of DBH and HEIGHT 
#'
#' @references  Chave 2009, 2014, Zanne 2009 
#' 
#' @return AGB in Mg of carbon
#'
#' @examples
#' source('startup.R')
#' forest <- forest.randomizer()
#' forest$AGB <- get.agb(forest) 

get.agb <- function(forest = NULL, DBH = NA, HEIGHT = NA, SPECIES.CODE. = NA){
  if (is.null(forest)){
    wd <- species$wd[match(SPECIES.CODE, species$SPECIES.CODE)]
    AGB <- 0.0673*(wd*DBH^2*HEIGHT)^0.976 / 1000
  } else{
    wd <- species$wd[match(forest$SPECIES.CODE, species$SPECIES.CODE)]
    AGB <- 0.0673*(wd*forest$DBH^2*forest$HEIGHT)^0.976 / 1000
  }
  
  return(AGB)
}

