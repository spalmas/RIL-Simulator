#' GET REGENERATION
#'
#' Long Description: Estimates new trees that are added to the forest. 
#' The parameters of regeneration are mean and standard deviation number of recruits 
#' in one hectare by species and depending in the percentage of canopy level.
#' 
#'
#' @param forest list of trees in forest
#' @param canopy.cover is the mean for all canopy covers
#' @param BA Basal area of plot
#'
#' @references
#' Ninguna por ahora

#' @return a table new trees that should r bind with forest.
#'
#' @examples
#' source('startup.R')
#' forest <- forest.randomizer(ROTATIONYEARS = 2)
#' get.regeneration(forest)

get.regeneration <- function(forest){
  
  list.canopy.cover <- unique(regen.params$CANOPY.COVER)[2:9]  #list of possible canopy.cover values
  
  regen.table <- tibble(SPECIES.CODE = NA, ACA = NA, DBH = NA, HEIGHT = NA, AGB = NA, UNDER.BOSQUETE = NA,
                        COORD.X = NA, COORD.Y = NA, HARVESTED = NA)
  for (ACA. in unique(forest$ACA)){
    #ACA. <- 0
    ACA.stand <- forest %>% filter(ACA == ACA.)
    BA.stand <- sum(pi*(ACA.stand$DBH/200)^2)
    #Basal area goes from 0 to 10 and canopy cover from 50 to 85
    canopy.cover.alt <- 50 + (BA.stand) * (85-50) /(10 - 0)  #Where does the BA falls in the canopy cover category
    canopy.cover.alt <- list.canopy.cover[which.min(abs(list.canopy.cover - canopy.cover.alt)) ]
    regen.params.subset <- regen.params %>% filter(CANOPY.COVER == canopy.cover.alt,
                                                   SPECIES.CODE %in% forest$SPECIES.CODE)
    
    #randomization of normal regeneration value
    regen.n <- mapply(rnorm,
                      n = 1,
                      mean = regen.params.subset$REG.N.HA.MEAN,
                      sd = regen.params.subset$REG.N.HA.SD)
    
    #There is no negative recruitment
    regen.n[regen.n < 0] <- 0  #no negative regeneration, just in case.
    
    #Convert number of recruits to integers
    regen.n <- round(regen.n)
    
    #total number of recruits
    forest.regen.n <- sum(regen.n)

    
    if (forest.regen.n > 0){
      
      #Creating list of species
      SPECIES.CODE <- rep(x = regen.params.subset$SPECIES.CODE, times = regen.n)
      ACA <- ACA.
      DBH <- 10 + (rexp(n = length(SPECIES.CODE)))/2   #
      HEIGHT <- get.height(DBH)
      #HEIGHT[HEIGHT < 0] <- 5  #No trees under 5 cm, maybe we need a change in distribution NOT NEEDED?
      DIAMETER.GROWTH <- rep(x = NA, times = forest.regen.n)   #no initial DIAMETER.GROWTH. Just added for the column
      #regen.table$VOLUME <- get.volume(regen.table)
      AGB <- get.agb(DBH = DBH, HEIGHT = HEIGHT, SPECIES.CODE. = SPECIES.CODE)
      UNDER.BOSQUETE <- rep(x = c(FALSE), times = forest.regen.n)
      COORD.X <- runif(n = forest.regen.n, min = 0, max = 99)
      COORD.Y <- runif(n = forest.regen.n, min = 0, max = 99)
      HARVESTED <- rep(x = c(FALSE), times = forest.regen.n)
      regen.ACA <- tibble(ACA, SPECIES.CODE, DBH, HEIGHT, AGB, UNDER.BOSQUETE, COORD.X, COORD.Y, HARVESTED)
      regen.table <- rbind(regen.table, regen.ACA)
    }
  }
  regen.table <- regen.table %>% filter(!is.na(ACA))
  
  #xx <- forest %>% group_by(ACA) %>% summarise(BA = sum(pi*(DBH/200)^2))
  #xx$BA %>% hist()
  return(regen.table)
}

