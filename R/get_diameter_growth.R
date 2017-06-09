#' GETS A LIST OF DIAMETER INCREMENTS FOR THE SPECIES
#'
#' Depending on the species, it randomizes the growth of the trees from the normal distribution.
#' It also considers previous correlations of growth
#'
#' @param forest The table of trees in the stand
#'
#' @references
#' 
#' @return A list of dbh changes
#'
#' @examples
#' source('startup.R')
#' forest <- forest.randomizer(ROTATIONYEARS = 3)
#' forest <- get.diameter.growth(forest)

get.diameter.growth <- function(forest){
  
  #Diameter equation table from the MIZE and NEGREROS paper or others
  diameter.eqs <- data.frame(SPECIES.CODE = c('SM', 'SM', 'LL', 'MB', 'MZ', 'PU', 'XX'),
                             UNDER.BOSQUETE = c(F, T, F, F, F, F, F),
                             DBH.GROWTH.MEAN = c(0.22, 0.31 ,0.44, 0.20, 0.15, 0.11, 0.15),
                             DBH.GROWTH.SD = c(0.27, 0.02, 0.35, 0.12, 0.11, 0.08, 0.15)
                            )
  
  #Adding grwoth parameteres column to the tree list
  forest <- left_join(forest, diameter.eqs, by = c('SPECIES.CODE', 'UNDER.BOSQUETE'))
  
  #str(forest)
  #randomizing for all trees
  D.DBH <- mapply(FUN = rlnorm,
                  n = 1,
                  mean = forest$DBH.GROWTH.MEAN,
                  sd = forest$DBH.GROWTH.SD) %>% 
    log()   #converting
  
  #Assgning those without previous DBH
  forest$D.DBH[is.na(forest$D.DBH)] <- D.DBH[is.na(forest$D.DBH)]
  
  #Growth rates as a small deviation from previous growth
  forest$D.DBH[!is.na(forest$D.DBH)] <-  forest$D.DBH[!is.na(forest$D.DBH)] +
    mapply(FUN = runif,
           n =  sum(!is.na(forest$D.DBH)),
           min = -0.05,
           max =  0.05)
  
  return(forest %>% select(-DBH.GROWTH.MEAN, -DBH.GROWTH.SD))
}
