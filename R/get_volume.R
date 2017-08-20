#' Function that receives a table of trees and estimates their volume based on X
#'
#' @param A table of trees with the columns: SPECIES.CODE, DBH, HEIGHT
#' 
#' @references 
#' Condit? Chave?
#' 
#' @return A list of volumes for each tree. This can be added as another column in the table of trees.
#'

get.volume <- function(stand){
  
  #dataframe of species code names and its prices. In a good simulation model,
  #this should be allowed to be input by the user. Maybe later
  
  #This equation is in form b0 + b1*DHB^2*H
  parameters <- data.frame(SPECIES.CODE = c('SWMA', 'LYLA', 'MEBR', 'MAZA', 'POUN', 'DURAS', 'BLANDAS', 'BE', 'CO', 'XXXX'),
                       B0 = c(0.01711, 0.00842, 0.00842, 0.00842, 0.00842, 0.00842, 0.01247, 0.03139, 0.07055, 0.00842),
                       B1 = c(0.000041591, 0.000050894, 0.000050894, 0.000050894, 0.000050894, 0.000050894,0.000047554, 0.000038954,0.000047705,0.000050894))
  
  #Calculating volume
  return(parameters$B0[ match(stand$SPECIES.CODE, parameters$SPECIES.CODE ) ] +
    parameters$B1[ match(stand$SPECIES.CODE, parameters$SPECIES.CODE ) ] * stand$DBH^2 * stand$HEIGHT)

}

