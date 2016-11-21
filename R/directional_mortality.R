#' Winching mortality 
#'
#' The functions finds which trees are inside a rectangle from the position of the harvested tree
#' to the trail. It creates a rectangle 6m wide from the trail to the harvested tree and it returns
#' an array of boolean values: TRUE: the tree is inside the rectangle and FALSE if not. 
#' The winching distance avoids nmortality within that distance from the harvested tree.
#'
#' @param stand The table of trees in the stand
#' @param w.dist The winching distance that was input in the simulation
#' @param harvested table of harvested trees
#'
#' @references
#' Ninguna por ahora
#' 
#' @return an array of boolean values: TRUE: the tree is inside the rectangle and FALSE if not.
#'
#' @examples
#' source('startup.R')
#' stand <- stand.randomizer()
#' intensity <- 'Normal'
#' rotation <- 20
#' y <- 20
#' w.dist <- 5
#' harvested <- get.harvest(stand, intensity)
#' stand <- stand[!rownames(stand) %in% rownames(harvested),]   #Removing harvested trees from the stand
#' killed.directional <- directional.mortality(stand = stand, harvested = harvested)
#' killed.directional
#' killed.trees <- stand[killed.directional,]


directional.mortality <- function(stand, harvested){
  vecinity <- rep(x = c(FALSE), times = nrow(stand))
  
  if (nrow(harvested) != 0){    #If there are trees harvested
    for (i in 1:nrow(harvested)){    #For each harvested tree
      vecinity.i <- rep(x = c(FALSE), times = nrow(stand))
      for (t in 1:nrow(stand)){
        # t <- 1
        # i <- 1
        vecinity.i[t] <- (stand$COORD.X[t]-harvested$COORD.X[i])**2 + (stand$COORD.Y[t]-harvested$COORD.Y[i])**2 < (harvested$HEIGHT[i]/2)**2
      }
      vecinity <- vecinity | vecinity.i 
    }
  }
  
  #only killing small trees
  vecinity.small <- vecinity & (stand$DBH < 15)
  
  #There is a 50/50 change of mortality in those small trees
  vecinity.small[vecinity.small] <- sample(x = c(TRUE,FALSE), 
                                                   prob = c(0.5, 0.5), 
                                                   replace = TRUE,
                                                   size = length(vecinity.small[vecinity.small]))
    

  #remove trees that were inside rectangles
  return(vecinity.small)
}