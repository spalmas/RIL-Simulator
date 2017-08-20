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
  
  #Diameter equation table from the MIZE and NEGREROS paper or others in cm per year
  #diameter.eqs <- tibble(SPECIES.CODE = c('SWMA', 'SWMA', 'LYLL', 'MEBR', 'MAZA', 'POUN', 'XXXX'),
  #                           UNDER.BOSQUETE = c(F, T, F, F, F, F, F),
  #                           DBH.GROWTH.MEAN = c(0.22, 0.31 ,0.32, 0.20, 0.15, 0.11, 0.15),
  #                           DBH.GROWTH.SD = c(0.27, 0.02, 0.35, 0.12, 0.11, 0.08, 0.15)
  #                          )
  
  #forest.e <- forest
  
  
  #Adding grwoth parameteres column to the tree list
  forest <- left_join(forest, diameter.eqs, by = c('SPECIES.CODE', 'UNDER.BOSQUETE'))
  
  #If there is no data found for that species, use this.
  # These values are equal to those of XXXX
  forest$mean[is.na(forest$mean)] <- 0.18
  forest$sd[is.na(forest$sd)] <- 0.22
  
  #str(forest)
  #randomizing for all trees
  D.DBH <- mapply(FUN = rlnorm,
                  n = 1,
                  mean = forest$mean,
                  sd = forest$sd) %>% 
    log()   #converting
  
  #Growth rates as a small deviation from previous growth
  #Only if any D.DBH is not NA, otherwise it causes errors
  
  if(any(!is.na(forest$D.DBH))){
    forest$D.DBH[!is.na(forest$D.DBH)] <-  forest$D.DBH[!is.na(forest$D.DBH)] +
      mapply(FUN = runif,
             n =  sum(!is.na(forest$D.DBH)),
             min = -0.05,
             max =  0.05)
  }
  
  #Assgning those without previous DBH (recruits, enriched)
  forest$D.DBH[is.na(forest$D.DBH)] <- D.DBH[is.na(forest$D.DBH)]
  
  
  return(forest %>% select(-mean, -sd))
  #forest <- forest %>% select(-mean, -sd)
}
