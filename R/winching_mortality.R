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
#' sy <- 25
#' it <- 5    #General simulation parameters
#' rotation <- 20
#' intensity <- 'Normal'
#' enrich.bosquete <- TRUE
#' w.dist <- 5
#' dir.felling <- TRUE
#' improved.trail <- TRUE
#' lower.impact <- TRUE
#' stand <- stand.randomizer()
#' y <- 20
#' harvested <- get.harvest(stand, intensity, y, rotation)
#' inside <- winching.mortality(stand = stand, w.dist = w.dist, harvested = harvested)
#' inside
#' killed.trees <- stand[(inside & stand$DBH < 15),]


winching.mortality <- function(stand, w.dist, harvested){
  #y =20
  
  #harvested <- get.harvest(stand, intensity, y, rotation) #harvesting the stand and store harvested trees
  #w.dist <- 00
  
  #cretes an empty array of FALSE with the length of the stand
  inside <- rep(x = c(FALSE), times = nrow(stand))
  
  if (nrow(harvested) != 0){    #If there are trees harvested
    for (i in 1:nrow(harvested)){    #For each harvested tree
      #Creates a buffer rectangle from the tree to the road.
      rectangle <- matrix(c(harvested$COORD.X[i]+3, harvested$COORD.Y[i] - w.dist,
                            harvested$COORD.X[i]-3, 0,
                            harvested$COORD.X[i]+3, 0,
                            harvested$COORD.X[i]-3, harvested$COORD.Y[i] - w.dist),
                          ncol = 2, byrow = TRUE)
      #Check if some trees are inside the rectangle and add them to the total list of trees that will be killed
      #in.out from mgcv package
      inside <- inside | in.out(bnd = rectangle, x = matrix(data = c(stand$COORD.X, stand$COORD.Y), ncol = 2))
    }
  }

  #remove trees that were inside rectangles
  return(inside)
}