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
#' forest <- forest.randomizer(ROTATIONYEARS = 10)
#' harvested.list <- get.harvest(forest = forest, intensity = 'All', ACA. = 0)
#' harvested <- forest[harvested.list,]
#' do.enrichment(harvested = harvested, ACA  = 1)

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
  
  #Getting a harvestable stand table #splyr erases row numbers
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
