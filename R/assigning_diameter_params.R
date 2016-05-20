#this functions assigns the growth distribution parameters to each tree of the stand
assigning_diameter_params <- function(stand, diameter.eqs){
  stand['DBH.GROWTH.PARAMS.ROW'] <- match(stand$SPECIES.CODE, diameter.eqs$SPECIES.CODE ) #getting a list of the order in diameter equation table 
  stand['DBH.GROWTH.MEAN'] <- diameter.eqs$DBH.GROWTH.MEAN[stand$DBH.GROWTH.PARAMS.ROW]  #Parameter b0 for diameter growth equation
  stand['DBH.GROWTH.SD'] <- diameter.eqs$DBH.GROWTH.SD[stand$DBH.GROWTH.PARAMS.ROW]  #parameter b1 for diameter growth equation
  return (stand)
}