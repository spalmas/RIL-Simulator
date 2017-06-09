#' Enrichment process
#'
#' It receives the list of harvested trees and randomizes the number and sizes of trees planted inside that bosquete\
#' It only plants Swietenia macrophylla seedlings (SM). These are small in size but relatively homogeneous.
#'
#' @param enrich.bosquete Boolean. TRUE if the simulation includes enrichment 
#' @param harvested Table of harvested trees
#'
#' @references
#' Ninguna por ahora

#' @return a table with seedlings planted inside the bosquetes
#'
#' @examples
#' source('startup.R')
#' forest <- forest.randomizer(ROTATIONYEARS = 10)
#' forest$HARVESTED <- get.harvest(forest = forest, intensity = 'All', ACA. = 0)
#' harvested <- forest[forest$HARVESTED,]
#' do.enrichment(harvested = harvested, ACA  = 1)
#' 
do.enrichment <- function(harvested, ACA){
  #If the ejidos enrich their bosquetes the number of bosquet. 
  #One bosquete will be created for each 3 trees harvested
  
    #Estimating the number of new seedlings
  n.seedlings <- harvested %>% nrow() %>% prod(., 1/3) %>%     #one bosquete at for each 3 harvested trees 
    rlnorm(n = ., mean = 0.1, sd = 0.039) %>%  #randomized area (ha), n is decimal, but it rounds as floor
    log() %>% 
    sum() %>%   #adding the area of all randomized bosquetes
    prod(., rnorm(n = 1, mean = 2000, sd = 600), .7 ) %>%    #a mean of 2000 seedlings per hectare and reducing to 70% because not all area of bosquete is used
    round()   #integer number of new seedlings.

  #Creating a table with n.seedlings mahoganies
  enrich.table <- as_tibble(rep(x = 'SM', times = n.seedlings)) 
  colnames(enrich.table) <- 'SPECIES.CODE'
  
  #adding new variables. Should be the same as forest randomizer
  enrich.table <- enrich.table %>% 
    mutate(
      ACA = ACA,
      DBH = log(rlnorm(n = nrow(enrich.table), mean = 1, sd = .15)),
      HEIGHT = log(rlnorm(n = nrow(enrich.table) , mean = 1, sd = .15)),   #should change to allometric equations
      D.DBH = NA,#no initial growth
      UNDER.BOSQUETE = TRUE,
      COORD.X =  runif(n = nrow(enrich.table), min = 0, max = 99),
      COORD.Y =  runif(n = nrow(enrich.table), min = 0, max = 99),
      HARVESTED = FALSE
    )
      
  enrich.table$AGB = get.agb(forest = enrich.table)
  
  #retunning a list of enrichment plants
  return(enrich.table)

}
