#' HARVEST RETURN LIST
#'
#' It returns a list of the harvested trees depending on the chosen intensity and XXXX
#'
#' @param stand The table of trees in the stand
#' @param intensity The table of trees in the stand
#'
#' @references
#' 
#' @return TRUE/FALSE array of harvested trees
#'
#' @examples
#' source('startup.R')
#' forest <- forest.randomizer(ROTATIONYEARS = 25)
#' forest$HARVESTED <- get.harvest(forest = forest, intensity = 'All', y = 1, rotation = 25)
#' forest[forest$HARVESTED,]

get.harvest <- function(forest, intensity, y = y, rotation = rotation){
  
  if (intensity == 'No logging'){
    return(rep(c(FALSE), times = nrow(forest)))
    }
  
  intensity.table <- data.frame(intensity = c('No logging', 'Low', 'BAU', 'High', 'All'),
                                perc_intensity = c(0, .25, .50, .75, 1))


  #Getting the percent of trees to be harvested
  perc_intensity <-  intensity.table$perc_intensity[intensity.table$intensity == intensity]
  
  #Getting a harvestable list depending on minimum diameters (or some other condition, maybe price?)
  #harvestable <-  (forest$DBH > 55 & forest$SPECIES.CODE == 'SM')  |
  #  (forest$DBH > 35 & forest$SPECIES.CODE != 'SM') |
  #  (y == ACU | (y == (ACU + rotation)) | (y == (ACU + 2 * rotation)))
  
  #Getting a list 
  forest <-  forest %>% mutate(
    harvestable = ((DBH > 55 & SPECIES.CODE == 'SM') | (DBH > 35 & SPECIES.CODE != 'SM')) &
    ((y == ACU) | (y == (ACU + rotation)) | (y == (ACU + 2 * rotation)))
    )
  
  #Getting a harvestable stand table
  harvestable.stand <- forest[forest$harvestable,]
  
  #Getting the percent of trees to be harvested
  n.harvestable <-  perc_intensity * nrow(harvestable.stand) %>% floor()
  #n.harvestable <-  nrow(harvestable.stand) %>% floor() just for trying
  
  #Finding the rownames of the n biggest trees
  biggest.n <- rownames(harvestable.stand)[order(harvestable.stand$DBH, decreasing = TRUE)[1:n.harvestable]]
  
  #TRUE/FALSE of harvested tree from stand
  harvested <- rownames(forest) %in% biggest.n
  
  return(harvested)
}
