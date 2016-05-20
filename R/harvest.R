harvest <- function(stand, intensity, y, rotation){
  if (y%%rotation == 0 | y == 0){    #only if the year is a rotation
    if (intensity == 'Normal'){
    #harvesting 50% of the harvestable trees. Randomized
    harvested <- stand[(stand$DBH > 55 & stand$SPECIES.CODE == 'SM') |
                         (stand$DBH > 35 & stand$SPECIES.CODE != 'SM'), ]
    
    #Get a randomized list of the harvestable trees (50% probabilities)
    random.trees <- sample(x = c(TRUE, FALSE), 
                           size = nrow(harvested),
                           replace = TRUE, prob = c(0.5, 0.5))
    
    harvested <- harvested[random.trees,]
    
  } else if (intensity == 'High'){
    
    #harvesting all tree sizes over 35 cm, or 55 cm for caoba
    harvested <- stand[(stand$DBH > 55 & stand$SPECIES.CODE == 'SM') |
                         (stand$DBH > 35 & stand$SPECIES.CODE != 'SM'), ]
  }
    
    return(harvested)
  }
}
