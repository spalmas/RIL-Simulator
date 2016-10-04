#regeneration simulation of a stand

#parameters of regeneration. These are mean and standard deviation number of recruits
#in one hectare by species and depending in the percentage of canopy level.
#canopy level 999 is the mean for all canopy covers. This could be used if there
#is no knowledge on the canopy cover
regen.params <- read.csv('Data/regenation_params.csv', sep = ',')


get.regeneration <- function(stand, canopy.cover){
  #subset data to only that canopy cover
  canopy.cover  <- 999
  regen.params.subset <- regen.params[regen.params$CANOPY.COVER == canopy.cover,]
  #regen.params.subset <- regen.params[regen.params$CANOPY.COVER == 999,]
  
  #subset to only species of trees already found in the stand.
  #There is no recruitment of new species in the stand
  #This also serves as to limit the species in the simulation
  regen.params.subset <- regen.params.subset[regen.params.subset$SPECIES.CODE %in% stand$SPECIES.CODE,]
  
  #randomization of normal regeneration value
  regen.n <- mapply(rnorm,
                       n = 1,
                       mean = regen.params.subset$REG.N.HA.MEAN,
                       sd = regen.params.subset$REG.N.HA.SD)
  
  #There is no negative recruitment
  regen.n[regen.n <0] <- 1  #at least one tree regenerated
  
  #Convert number of recruits in integers
  regen.n <- round(regen.n)
  
  #Adding species names and creating a table
  regen.table <- data.frame(SPECIES.CODE = regen.params.subset$SPECIES.CODE, REGEN.N = regen.n)
  
  
  regen.table <- data.frame(SPECIES.CODE = unlist(mapply(rep,
                                          x = as.character(regen.table$SPECIES.CODE),
                                          times = regen.table$REGEN.N)))
  
  #adding the columns as in the normal stand table
  regen.table$DBH <- rnorm(n = nrow(regen.table), mean = 5, sd = 1)
  regen.table$HEIGHT <- get.height(regen.table$DBH)
  regen.table$DIAMETER.GROWTH <- get.diameter.growth(regen.table)   #randomized diameter growth
  #regen.table$DBH <- regen.table$DBH + regen.table$DIAMETER.GROWTH #assign new diameter
  regen.table$VOLUME <- get.volume(regen.table)
  regen.table$AGB <- get.agb(regen.table)
  regen.table$UNDER.BOSQUETE <- FALSE
  regen.table$COORD.X <- runif(n = sum(regen.n), min = 0, max = 99)
  regen.table$COORD.Y <- runif(n = sum(regen.n), min = 0, max = 99)
  

  
    #regen.table$DIAMETER.GROWTH <- NA
  #regen.table$VOLUME <- NA
  #regen.table$AGB <- NA
  
  #delete row names to avoid problems later
  row.names(regen.table) <- NULL
  
  
  
  return(regen.table)
}

