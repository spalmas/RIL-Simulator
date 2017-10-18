#' GET REGENERATION
#'
#' Long Description: Estimates new trees that are added to the forest. 
#' The parameters of regeneration are mean and standard deviation number of recruits 
#' in one hectare by species and depending in the percentage of canopy level.
#' 
#'
#' @param forest list of trees in forest
#' @param area Area of the forest plots or ACAs
#'
#' @references
#' Ninguna por ahora

#' @return a table new trees that should r bind with forest.
#'
#' @examples
#' source('startup.R')
#' forest <- forest.randomizer(ROTATIONYEARS = 2)
#' get.regeneration(forest)

get.regeneration <- function(forest, area = 10000){
  CF <- 10000/area
  list.canopy.cover <- unique(regen.params$CANOPY.COVER)[2:9]  #list of possible canopy.cover values
  
  regen.table <- tibble(SPECIES.CODE = NA, ACA = NA)
  for (ACA. in unique(forest$ACA)){
    #ACA. <- 0
    ACA.stand <- forest %>% filter(ACA == ACA.)
    BA <- CF * sum(pi*(ACA.stand$DBH^2/40000)) # by hectare
    #Basal area goes from 0 to 7 in inf, canopy cover from 50 to 85
    #CCx <- (CCmax - CCmin)(BAx - BAmin)/(BAmax - BAmin)
    canopy.cover.alt <- 50 + (BA-1) * (85-50) /(40 - 0)  #Where does the BA falls in the canopy cover category
    canopy.cover.alt <- list.canopy.cover[which.min(abs(list.canopy.cover - canopy.cover.alt)) ]
    regen.params.subset <- regen.params %>% filter(CANOPY.COVER == canopy.cover.alt,
                                                   SPECIES.CODE %in% forest$SPECIES.CODE)
    
    #randomization of normal regeneration value. By ha
    regen.n <- mapply(rnorm, n = 1,
                      mean = regen.params.subset$REG.N.HA.MEAN / 4,  #over four because it is simulated annualy, instead of every four years
                      sd = regen.params.subset$REG.N.HA.SD)
    
    #There is no negative recruitment
    regen.n[regen.n < 0] <- 0  #no negative regeneration, just in case.
    
    #Convert number of recruits to integers. In units of area 
    regen.n <- round(regen.n)
    #regen.n <- round(regen.n/CF)
    
    #total number of recruits
    forest.regen.n <- sum(regen.n)

    #only if there are new trees in the ACA
    if (forest.regen.n > 0){
      #Creating list of species
      SPECIES.CODE <- rep(x = regen.params.subset$SPECIES.CODE, times = regen.n)
      #HEIGHT[HEIGHT < 0] <- 5  #No trees under 5 cm, maybe we need a change in distribution NOT NEEDED?
      #regen.table$VOLUME <- get.volume(regen.table)
      regen.ACA <- tibble(ACA = ACA., SPECIES.CODE)
      regen.table <- rbind(regen.table, regen.ACA)
    }
  }
  
  regen.table$DBH <- 3 + (rexp(n = nrow(regen.table)))/2   #a minimum of 3
  regen.table$AGB <- NA
  regen.table$DIAMETER.GROWTH <- NA
  regen.table$UNDER.BOSQUETE <- NA
  regen.table$COORD.X <- runif(n = nrow(regen.table), min = 0, max = 99)
  regen.table$COORD.Y <- runif(n = nrow(regen.table), min = 0, max = 99)
  regen.table$HEIGHT <- get.height(regen.table$DBH)
  regen.table$HARVESTED <- NA
  
  regen.table <- regen.table %>% filter(!is.na(ACA))
  
  #xx <- forest %>% group_by(ACA) %>% summarise(BA = sum(pi*(DBH/200)^2))
  #xx$BA %>% hist()
  return(regen.table)
}

