get.harvest <- function(stand, intensity, y, rotation){
  if (y%%rotation == 0 | y == 0){    #only if the year is a rotation (year 0 or year rotation.
    if (intensity == 'Normal'){    #harvesting the 50% largest trees
    
    #A table of the harvestable trees  
    harvestable.stand <- stand[(stand$DBH > 55 & stand$SPECIES.CODE == 'SM') |
                         (stand$DBH > 35 & stand$SPECIES.CODE != 'SM'), ]
    
    #Get a list of the biggest 50% harvestable trees
    biggest.half <- order(harvestable.stand$DBH, decreasing = TRUE)[1:round(nrow(harvestable.stand)/2)]
    harvested <- harvestable.stand[biggest.half,]
    
  } else if (intensity == 'High'){
    
    #harvesting all tree sizes over 35 cm, or 55 cm for caoba
    harvested <- stand[(stand$DBH > 55 & stand$SPECIES.CODE == 'SM') |
                         (stand$DBH > 35 & stand$SPECIES.CODE != 'SM'), ]
  }
    
    return(harvested)
  }
}
