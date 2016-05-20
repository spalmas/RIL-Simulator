
#fuction to compute euclidean distance
euc.dist <- function(x1, y1, x2, y2){
  return(sqrt((x2-x1)^2 + (y2-y1)^2))
}

#function to return a list of distances between pairs of points
distance  <- function(harvested){
  pairs <- combn(1:nrow(harvested), m= 2)
  distance.list <- rep(NA, ncol(pairs))
  for (i in 1:ncol(pairs)){
    #calculating euclidean distnace
    distance <- euc.dist(x1 = harvested$COORD.X[pairs[1,i]],
                         y1 = harvested$COORD.Y[pairs[1,i]],
                         x2 = harvested$COORD.X[pairs[2,i]],
                         y2 = harvested$COORD.Y[pairs[2,i]])
    
    distance.list[i] <- distance  #adding distance to list
  }
  
  #Creating a table with the pairs, distance. Ordered by less to more
  distance.list <- cbind(t(pairs), distance.list)[order(distance.list),]
  return(distance.list)
}

