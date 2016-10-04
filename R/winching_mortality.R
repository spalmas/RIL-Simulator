winching.mortality <- function(stand, w.dist, harvested){
  #y =20
  #harvested <- get.harvest(stand, intensity, y, rotation) #harvesting the stand and store harvested trees
  #w.dist <- 00
  inside <- rep(x = c(FALSE), times = nrow(stand))
  
  if (nrow(harvested) != 0){
    for (i in 1:nrow(harvested)){
      #Creates a buffer rectangle from the tree to the road.
      rectangle <- matrix(c(harvested$COORD.X[i]+3, harvested$COORD.Y[i] - w.dist,
                            harvested$COORD.X[i]-3, 0,
                            harvested$COORD.X[i]+3, 0,
                            harvested$COORD.X[i]-3, harvested$COORD.Y[i] - w.dist),
                          ncol = 2, byrow = TRUE)
      #Check if some trees are inside the rectangle and add them to the total list of trees that will be killed
      inside <- inside | in.out(bnd = rectangle, x = matrix(data = c(stand$COORD.X, stand$COORD.Y), ncol = 2))
    }
  }
  
  #remove trees that were inside rectangles
  return(inside)
}