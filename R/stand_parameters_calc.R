#Calculate stand parameters 
stand_parameters_calc <- function(stand){
  BA <- sum(pi()*(stand$DBH/2)^2)
  Biomass <- sum(stand$biomass)
  
}
