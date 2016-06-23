#' Main simulator process
#'
#' Lond Description
#'
#' @param scenario: 
#' @param sy
#' @param it
#' @param rotation
#' @param intensity
#' @param bos
#' @param w.dist
#' @param dir.felling
#' @param improved.trail
#' @param lower.impact
#' @param trees.tab
#' @param diameter.eqs
#' @param volume.eqs
#'
#' @references
#' Ninguna por ahora

#' @return a table with yearly results of BA (Basal Area in the stand), AGB (aboveground biomass in stand), INCOME, volume extracted, total sell price of products extraccted 
#'
#' @seealso \code{\link{hd_coef}}. For BA, QD and N see \code{\link{get_stand}}
#'
#' @examples
#' # Example 1: Obtain Dominant Age
#' (AD<-get_site(dom_sp=1, zo
#' 

simulator <- function(scenario, sy, it,    #General simulation parameters
                           rotation, intensity, bos, w.dist, dir.felling, improved.trail, lower.impact,  #Harvesting scenario
                           trees.tab, diameter.eqs, volume.eqs){  #inventory and equations

  ### TABLE TO STORE VALUES AND REPORT -----------
  columns <- c('SCENARIO', 'IT', 'YEAR', 'BA', 'AGB', 'INCOME', 'VOLUME')
  table.results <- data.frame(matrix(ncol = length(columns), nrow = sy*it))
  colnames(table.results) <- columns
  
  table.results$SCENARIO <- scenario  #letter of scenario to the table
  
  #LOOP OF SIMULATION YEARS AND ITERATIONS-----------
  for (i in 1:it){   #For each iteration
    stand <- trees.tab  #return to initial stand
    
    for (y in 1:sy){   #For each simulation year
      #Mortality 
      stand.after.mortality <- mortality.calc(stand)
      stand <- stand.after.mortality[[1]]
      stand.dead <- stand.after.mortality[[2]]
      
      ####### GROWTH FUNCTIONS
      stand <- assigning_diameter_params(stand, diameter.eqs)   #assigning diameter equation
      stand <- assigning_volume_params(stand, volume.eqs)
      stand$DIAMETER.GROWTH <- diameter_growth(stand)   #randomized diameter growth
      stand$DBH <- diameter_growth_assign(stand)  #assign new diameter
      
      ####### Estimating the biomass of each tree
      stand$AGB <- apply(stand[,c('DBH', 'HEIGHT')], 1, agb.calc)
      
      ####### HARVESTED TREES
      harvested <- harvest(stand, intensity, y, rotation) #harvesting the stand and store harvested trees
      harvested$price <- harvested.price(harvested)       #Assigning price to each tree
      
      ####### REMAINING STAND OPERATIONS
      stand <- stand[!rownames(stand) %in% rownames(harvested),]   #Removing harvested trees from the stand
      stand <- regeneration.calc(stand)             #Regeration process
      
      #Storing stand results 
      row.num <- y+(i-1)*sy #row number based on the repetition and simulation year
      table.results[row.num,'IT'] <- i   #Adding iteration to table
      table.results[row.num,'YEAR'] <- y   #Adding year to table
      table.results[row.num,'BA'] <- sum(pi * (stand$DBH/100/2)^2)  #Estimate biomass from the stand (it uses Chave 2014, see helpers.R). Transform to square meters
      table.results[row.num,'AGB'] <- sum(stand$AGB)  #Estimate biomass from the stand
      
      #Store harvested results
      
    }  
  }
  
  
  #Return table of results
  return(table.results)
}