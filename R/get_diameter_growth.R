#' UPDATES THE FOREST TABLE TO INCLUDE NEW DBH AND D.DBH
#'
#' Depending on the species, it randomizes the growth of the trees from the normal distribution.
#' It also considers previous correlations of growth
#'
#' @param forest The table of trees in the stand
#'
#' @references
#' 
#' @return A new forest table with the new DBH and D.DBH
#'
#' @examples
#' source('startup.R')
#' forest <- forest.randomizer(ROTATIONYEARS = 1)
#' forest <- get.diameter.growth(forest)
get.diameter.growth <- function(forest){
  #Adding grwoth parameteres column to the tree list
  forest <- left_join(forest, diameter.eqs, by = c('SPECIES.CODE', 'UNDER.BOSQUETE'))
  
  #If there is no data found for that species, use this.
  # These values are equal to those of XXXX
  forest$mean[is.na(forest$mean)] <- 0.18
  forest$sd[is.na(forest$sd)] <- 0.22
  
  #str(forest)
  #randomizing for all trees
  D.DBH <- mapply(FUN = rlnorm, n = 1, mean = forest$mean, sd = forest$sd) %>% log()  
  
  #Growth rates as a small deviation from previous growth
  #Only if any D.DBH is not NA, otherwise it causes errors
  if(any(!is.na(forest$D.DBH))){
    forest$D.DBH[!is.na(forest$D.DBH)] <- forest$D.DBH[!is.na(forest$D.DBH)] + 
      mapply(FUN = runif, n =  sum(!is.na(forest$D.DBH)), min = -0.05, max =  0.05)
  }
  
  #Assgning those without previous D.DBH (recruits, enriched)
  forest$D.DBH[is.na(forest$D.DBH)] <- D.DBH[is.na(forest$D.DBH)]
  
  #updating DBH
  forest$DBH <- forest$DBH + forest$D.DBH
  
  return(forest %>% select(-mean, -sd))
}
