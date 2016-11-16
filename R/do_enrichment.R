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
#' rotation <- 10
#' intensity <- 'Normal'
#' enrich.bosquete <- TRUE
#' w.dist <- 5
#' stand <- stand.randomizer()
#' harvested <- get.harvest(stand, intensity, y, rotation)
#' do.enrichment(enrich.bosquete = TRUE, harvested = harvested)
#' 
do.enrichment <- function(enrich.bosquete, harvested){
  #If the ejidos enrich their bosquetes the number of bosquet. 
  #One bosquete will be created for each 3 trees harvested
  bos.n <- floor(nrow(harvested) / 3) #3 harvested trees per bosquete. Rounding down
  
  # it sums the area of bosquetes
  area.bosquete <- sum(rnorm(n = bos.n, mean = .1, sd = 0.039))  #randomized total cleared area for bosquetes
  if (area.bosquete < 0 ){area.bosquete <- 0.1}  #to avoid negative area (change to other distribution?)
  
  #estimating the number of total added seedlings to the hectare 
  n.seedlings <- area.bosquete * rnorm(n = 1, mean = 2000, sd = 600)
  if (n.seedlings < 0 ){n.seedlings <- 0}  #to avoid negative seedlings
  n.seedlings <- round(n.seedlings)
  
  #create a list of tiny mahoganies. Maybe based on some sory of size probability?
  enrich.table <- data.frame(SPECIES.CODE = unlist(mapply(rep,
                                                          x = 'SM',
                                                          times = n.seedlings)))
  colnames(enrich.table) <- 'SPECIES.CODE'
  
  #adding the columns as in the normal stand table
  enrich.table$DBH <- rnorm(n = nrow(enrich.table), mean = 1, sd = .15)
  enrich.table$DBH[enrich.table$DBH < 0.4] <- 1   #no diameters under .5 cm, changing to 1
  enrich.table$HEIGHT <- get.height(enrich.table$DBH)
  enrich.table$HEIGHT[enrich.table$HEIGHT < 0.5] <- 0.5   #no heights under .5m, changing to 0.5m
  enrich.table$DIAMETER.GROWTH <- rep(x = 0, times = n.seedlings) #no initial growth
  #enrich.table$VOLUME <- get.volume(enrich.table)
  enrich.table$AGB <- get.agb(enrich.table)
  enrich.table$UNDER.BOSQUETE <-  rep(x = TRUE, times = n.seedlings)
  
  #localization of trees
  #It should choose coordinates of one of the harvetsted trees and then plant all of the seedlings
  #inside a radious from the point. For now is random to make simulator work
  enrich.table$COORD.X <-  runif(n = n.seedlings, min = 0, max = 99)
  enrich.table$COORD.Y <-  runif(n = n.seedlings, min = 0, max = 99)
  #retunning a list of enrichment plants
  return(enrich.table)
  }