#' GETS A LIST OF DIAMETER INCREMENTS FOR THE SPECIES
#'
#' Depending on the species, it randomizes the growth of the trees from the normal distribution.
#' It also considers previous correlations of growth
#'
#' @param stand The table of trees in the stand
#'
#' @references
#' 
#' @return A list of dbh changes
#'
#' @examples
#' source('startup.R')
#' stand <- stand.randomizer()
#' get.diameter.growth(stand)
get.diameter.growth <- function(stand){
  #Diameter equation table from the MIZE and NEGREROS paper
  diameter.eqs <- data.frame(SPECIES.CODE = c('SM', 'LL', 'MB', 'MZ', 'PU', 'XX'),
                       DBH.GROWTH.MEAN = c(0.22, 0.44, 0.20, 0.15, 0.11, 0.15),
                       DBH.GROWTH.SD = c(0.27, 0.35, 0.12, 0.11, 0.08, 0.15))

  #List of means and standard deviations of diameter growth
  list.mean <- diameter.eqs$DBH.GROWTH.MEAN[match(stand$SPECIES.CODE, diameter.eqs$SPECIES.CODE )]  #Parameter b0 for diameter growth equation
  list.sd <- diameter.eqs$DBH.GROWTH.SD[match(stand$SPECIES.CODE, diameter.eqs$SPECIES.CODE )]  #parameter b1 for diameter growth equation

  # Random growth generator
  list.D.DBH <- mapply(FUN = rnorm,
                       n = 1,
                       mean = list.mean,
                       sd = list.sd)

  #there are no negative growth rates
  list.D.DBH[list.D.DBH < 0 ] <- 0


  return(list.D.DBH)
}
