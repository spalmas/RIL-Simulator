#' Main simulator process
#'
#' Lond Description
#'
#' @param scenario: 
#' @param sy
#' @param it
#' @param rotation
#' @param intensity
#' @param enrich.bosquete The ejido performs enrichment bosquete planting (TRUE or FALSE)
#' @param w.dist
#' @param dir.felling
#' @param improved.trail
#' @param trees.tab Should represent the plots that are harvested in the cycle (25 rotation years, then 25 plots)
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
#' source('startup.R')
#' trees.tab <- stand.randomizer()
#' table.results <- simulator(sy = 15, 
#'     it = 2,
#'     rotation = 10, 
#'     intensity = 'BAU',
#'     enrich.bosquete = TRUE, 
#'     w.dist = 10, 
#'     dir.felling  = TRUE, 
#'     improved.trail = TRUE, 
#'     trees.tab  = trees.tab)
#' View(table.results)
#' 

simulator <- function(scenario = 'A',
                      sy, 
                      it, 
                      rotation, 
                      intensity, 
                      enrich.bosquete, 
                      w.dist, 
                      dir.felling, 
                      improved.trail, 
                      trees.tab){ 

  ### TABLE TO STORE VALUES AND REPORT -----------
  columns <- c('SCENARIO', 'IT', 'YEAR', 'BA', 'AGB', 'INCOME')
  table.results <- data.frame(matrix(ncol = length(columns), nrow = sy*it))
  colnames(table.results) <- columns
  #stand <- stand.randomizer()
  table.results$SCENARIO <- scenario  #letter of scenario to the table
  
  #LOOP OF SIMULATION YEARS AND ITERATIONS-----------
  for (i in 1:it){   #For each iteration
    #i=1
    stand <- trees.tab  #return to initial stand
    AGB0 <- stand %>% get.agb() %>% sum(na.rm = TRUE)  #First AGB estimate
    AGB1 <- AGB0  #The first year the sequestration is 0
    
    for (y in 1:sy){   #For each simulation year
      #y=1
      row.num <- y + (i - 1) * sy #row number based on the repetition and simulation year for table of results

      ######## NATURAL MORTALITY AND HURRICANES
      natural.dead <- stand %>% mortality.calc()    #T/F list if they died of natural causes
      stand.dead <- stand[natural.dead,]      #creating a dead list
      stand <- stand[!natural.dead,]          #removing dead from alive stand
      
      hurricane.dead <- stand %>% hurricane.mortality()        #T/F list if they died from a hurricane
      stand.dead <- rbind(stand.dead, stand[hurricane.dead,])     #Adding dead trees to dead  list
      stand <- stand[!hurricane.dead,]          #removing dead from alive stand
      

      ####### GROWTH FUNCTIONS
      stand$DIAMETER.GROWTH <- stand %>% get.diameter.growth()   #randomized diameter growth
      stand$DBH <- stand$DBH + stand$DIAMETER.GROWTH #assign new diameter

      ####### REGENERATION
      #it only occurs every four years
      if (y%%4 == 0){
        regen.table <- get.regeneration (stand, canopy.cover = 999)   #Regeration process
        stand <- rbind(stand, regen.table)  #adding the new trees to the stand
      }      
      
      ####### BIOMASS (IS AFTER NATURAL PROCESSES AND BEFORE HARVEST)
      stand$AGB <- stand %>% get.agb()
      AGB.sequestered <- sum(stand$AGB, na.rm = TRUE) - AGB0  #Sequestred value
      AGB0 <- AGB1  #Updating value      
      
      ####### DEFAULT VALUES FOR SOME VARIABLES. WILL NOT CHANGE IF THIS IS NOT A HARVESTING YEAR IN THE ROTATION.
      table.results[row.num, 'N.HARVESTED'] <- NA
      table.results[row.num, 'VOL.HARVESTED'] <- NA
      table.results[row.num, 'INCOME'] <- NA
      emissions.harvest <- NA
      emissions.skidding <- NA
      emissions.directional <- NA
      
      ####### THINGS HAPPENING IN A HARVESTING YEAR IN THE ROTATION
      if (y%%rotation == 0 | y == 0){   #if it is a harvest year 
        ####### HARVESTING TREES
        harvested.list <- get.harvest(stand = stand, intensity = intensity) #harvesting the stand and store harvested trees
        harvested <- stand[harvested.list,]   #Getting a harvested tree list
        harvested$VOLUME <- harvested %>% get.volume()     #Get total volume harvested
        harvested$PRICE <- harvested %>% get.price()       #Assigning price to each tree
        stand <- stand[!harvested.list,]   #Removing harvested trees from the stand
      
        table.results$N.HARVESTED[row.num] <- nrow(harvested)  #Number of extracted trees
        table.results$VOL.HARVESTED[row.num] <- sum(harvested$VOLUME, na.rm = TRUE)  #All volume extacted in that year
        table.results$INCOME[row.num] <- sum(harvested$PRICE, na.rm = TRUE)  #All income from trees harvested in that year
        emissions.harvest <- sum(harvested$AGB, na.rm = TRUE)  #Harvested Biomass 
        
        ####### DO ENRICHMENT PLANTING
        enrichment.table <- do.enrichment(enrich.bosquete = enrich.bosquete, harvested = harvested)
        stand <- rbind(stand, enrichment.table)  #adding the new trees to the stand
        AGB.sequestered <- sum(enrichment.table$AGB, AGB.sequestered, na.rm = TRUE) #Adding the small sequestered biomass
        
        ####### DO SKIDDING MORTALITY
        skidding.dead.bool <- skidding.mortality(stand = stand, w.dist = w.dist, harvested = harvested)  #skidding mortality
        skidding.dead <- stand[skidding.dead.bool,] #kills inside area and small trees (is 15cm OK?)
        emissions.skidding <- sum(skidding.dead$AGB, na.rm = TRUE) #Winching mortality emissions
        
        ####### DO DIRECTIONAL MORTALITY
        emissions.directional <- 0
        if (dir.felling){
          directional.dead.bool <- directional.mortality(stand = stand, harvested = harvested)    #directinonal felling mortality
          directional.dead <- stand[directional.dead.bool,] #kills from directional felling
          emissions.directional <- sum(directional.dead$AGB, na.rm = TRUE) #Directional Felling emissions
        }
        
      }

      #Storing stand results 
      table.results[row.num,'IT'] <- i   #Adding iteration to table
      table.results[row.num,'YEAR'] <- y   #Adding year to table
      table.results[row.num,'BA'] <- sum(pi * (stand$DBH/100/2)^2, na.rm = TRUE)  #Estimate Basal Area from the stand  Transform to square meters
      table.results[row.num,'AGB'] <- sum(stand$AGB, na.rm = TRUE)  #Estimate biomass from the stand at end of year
      table.results[row.num,'EMISSIONS.HARVEST'] <- emissions.harvest*-1  #Estimate biomass from the stand harvest
      table.results[row.num,'EMISSIONS.SKIDDING'] <- emissions.skidding*-1  #Emissions from winching
      table.results[row.num,'EMISSIONS.DIRECTIONAL'] <- emissions.directional*-1  #Emissions from directional felling
      table.results[row.num,'SEQUESTERED'] <- AGB.sequestered  #Includes growth + recruitment + enrichment - mortality
      #table.results[row.num,'HURRICANE'] <- hurricane.dead  #Add if this was a hurricane year
      
    }  
  }
  
  #Estimate the total Emissions for each year
  table.results['EMISSIONS'] <- table.results$EMISSIONS.HARVEST + table.results$EMISSIONS.SKIDDING  + table.results$EMISSIONS.DIRECTIONAL
  table.results['EMISSIONSperm3'] <- table.results$EMISSIONS / table.results$VOL.HARVESTED
  table.results['NET.SEQUESTERED'] <- rowSums(cbind(table.results$SEQUESTERED, table.results$EMISSIONS), na.rm = TRUE) #Estimate sequestered biomass from the stand
  
  #Return table of results
  return(table.results)
}