
# Function to estimate Above Ground Biomass of the trees
# Chave 2014 equation


agb.calc <- function(trees){
  #AGB <- sum(0.0673 * (0.7 * trees$DBH^2 * trees$HEIGHT)^0.976) 
  AGB <- 0.47*sum(exp(-2.173+0.868*log(trees$DBH^2*trees$HEIGHT)+0.0939/2)) # Cairns paper
  return (AGB)
}

