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
#' forest <- forest.randomizer(ROTATIONYEARS = 2)
#' harvested.list <- get.harvest(forest = forest, intensity = 'All', ACA. = 0)
#' forest[harvested.list,]

get.harvest <- function(forest, intensity, ACA.){
  
  if (intensity == 'No logging'){
    return(rep(c(FALSE), times = nrow(forest)))
    }
  
  intensity.table <- data.frame(intensity = c('No logging', 'Low', 'BAU', 'High', 'All'),
                                perc_intensity = c(0, .25, .50, .75, 1))


  #Getting the percent of trees to be harvested
  perc_intensity <-  intensity.table$perc_intensity[intensity.table$intensity == intensity]
  
  #adding a column with a harvestable list depending on minimum diameters and ACA
  forest <-  forest %>% mutate(
    harvestable = ((DBH > 55 & SPECIES.CODE == 'SM') | (DBH > 35 & SPECIES.CODE != 'SM')) & (ACA == ACA.)
    )
  
  #Getting the number of trees to be harvested based on the intensity and rounded down
  n.harvestable <-  ((forest$harvestable %>% sum) * perc_intensity) %>% floor()

  #TRUE/FALSE which of the harvestable trees are the n.harvestable biggest trees
  biggest.harvested <- forest[forest$harvestable,]$DBH %>% order(decreasing = TRUE) <= n.harvestable
  
  #TRUE/FALSE. Getting rownames of the trees to be harvested
  harvested <- rownames(forest) %in% which(forest$harvestable)[biggest.harvested]
  
  return(harvested)
}
