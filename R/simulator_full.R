#' Main simulator process
#'
#' Long Description: It is improved from simulator because it takes into consideration a different annual cutting areas.
#' Each annual cutting area, by definition, has their own cycle of cutting. Based on rotation periods.
#' 
#'
#' @param scenario text name of scenario
#' @param sy total simulation years
#' @param it number of iterations of the complete scenarios.
#' @param rotation rotation period of 
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
#' forest.tab <- forest.randomizer(ROTATIONYEARS = 5)
#' table.results <- simulator(sy = 10, 
#'     it = 2,
#'     rotation = 5, 
#'     intensity = 'BAU',
#'     enrich.bosquete = TRUE, 
#'     w.dist = 10, 
#'     dir.felling  = TRUE, 
#'     improved.trail = TRUE, 
#'     forest.tab  = forest.tab)
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
                      forest.tab){ 

  ### TABLE TO STORE VALUES AND REPORT -----------
  columns <- c('SCENARIO', 'IT', 'YEAR', 'BA', 'AGB', 'INCOME')
  table.results <- data.frame(matrix(ncol = length(columns), nrow = sy*it))
  colnames(table.results) <- columns
  #forest <- forest.randomizer()
  table.results$SCENARIO <- scenario  #letter of scenario to the table
  
  
  #LOOP OF SIMULATION YEARS AND ITERATIONS-----------
  for (i in 1:it){   #For each iteration
    #i=1
    forest <- forest.tab  #return to initial forest
    AGB0 <- forest$AGB %>% sum(na.rm = TRUE)  #First AGB estimate
    AGB1 <- AGB0  #The first year the sequestration is 0
    
    for (y in 1:sy){   #For each simulation year
      #y=1
      row.num <- y + (i - 1) * sy #row number based on the repetition and simulation year for table of results

      ######## NATURAL MORTALITY AND HURRICANES
      natural.dead <- forest %>% mortality.calc()    #T/F list if they died of natural causes
      forest.dead <- forest[natural.dead,]      #creating a dead list
      forest <- forest[!natural.dead,]          #removing dead from alive forest
      
      hurricane.dead <- forest %>% hurricane.mortality()        #T/F list if they died from a hurricane
      forest.dead <- bind_rows(forest.dead, forest[hurricane.dead,])     #Adding dead trees to dead  list
      forest <- forest[!hurricane.dead,]          #removing dead from alive forest
      

      ####### GROWTH FUNCTIONS
      forest$DIAMETER.GROWTH <- forest %>% get.diameter.growth()   #randomized diameter growth
      forest$DBH <- forest$DBH + forest$DIAMETER.GROWTH #assign new diameter

      ####### REGENERATION
      #it only occurs every four years
      if (y%%4 == 0){
        regen.table <- get.regeneration (forest, canopy.cover = 999)   #Regeration process
        forest <- bind_rows(forest, regen.table)  #adding the new trees to the forest
      }      
      
      ####### BIOMASS (IS AFTER NATURAL PROCESSES AND BEFORE HARVEST)
      forest$AGB <- get.agb(forest = forest)
      AGB.sequestered <- sum(forest$AGB, na.rm = TRUE) - AGB0  #Sequestred value
      AGB0 <- AGB1  #Updating value      
      
      ####### DEFAULT VALUES FOR SOME VARIABLES. WILL NOT CHANGE IF THIS IS NOT A HARVESTING YEAR IN THE ROTATION.
      table.results[row.num, 'N.HARVESTED'] <- NA
      table.results[row.num, 'VOL.HARVESTED'] <- NA
      table.results[row.num, 'INCOME'] <- NA
      emissions.harvest <- NA
      emissions.skidding <- NA
      emissions.directional <- NA
      
      ####### HARVESTING TREES
      harvested.list <- get.harvest(forest = forest, intensity = intensity, y = y, rotation = rotation) #harvesting the forest and store harvested trees
      harvested <- forest[harvested.list,]   #Getting a harvested tree list
      harvested$VOLUME <- harvested %>% get.volume()     #Get total volume harvested
      harvested$PRICE <- harvested %>% get.price()       #Assigning price to each tree
      forest <- forest[!harvested.list,]   #Removing harvested trees from the stand
      
      table.results$N.HARVESTED[row.num] <- nrow(harvested)  #Number of extracted trees
      table.results$VOL.HARVESTED[row.num] <- sum(harvested$VOLUME, na.rm = TRUE)  #All volume extacted in that year
      table.results$INCOME[row.num] <- sum(harvested$PRICE, na.rm = TRUE)  #All income from trees harvested in that year
      emissions.harvest <- sum(harvested$AGB, na.rm = TRUE)  #Harvested Biomass 
      
      ####### DO ENRICHMENT PLANTING
      if (enrich.bosquete){
        enrichment.table <- do.enrichment(harvested = harvested)
        stand <- bind_rows(forest, enrichment.table)  #adding the new trees to the stand
        AGB.sequestered <- sum(enrichment.table$AGB, AGB.sequestered, na.rm = TRUE) #Adding the small sequestered biomass
      }
      
      ####### DO SKIDDING MORTALITY
      skidding.dead.bool <- skidding.mortality(forest = forest, w.dist = w.dist, harvested = harvested)  #skidding mortality
      skidding.dead <- forest[skidding.dead.bool,] #kills inside area and small trees (is 15cm OK?)
      emissions.skidding <- sum(skidding.dead$AGB, na.rm = TRUE) #Winching mortality emissions
      
      ####### DO DIRECTIONAL MORTALITY
      emissions.directional <- 0
      if (dir.felling){
        directional.dead.bool <- directional.mortality(forest = forest, harvested = harvested)    #directinonal felling mortality
        directional.dead <- forest[directional.dead.bool,] #kills from directional felling
        emissions.directional <- sum(directional.dead$AGB, na.rm = TRUE) #Directional Felling emissions
      }

      #Storing stand results 
      table.results[row.num,'IT'] <- i   #Adding iteration to table
      table.results[row.num,'YEAR'] <- y   #Adding year to table
      table.results[row.num,'BA'] <- sum(pi * (forest$DBH/100/2)^2, na.rm = TRUE)  #Estimate Basal Area from the stand. Transform to square meters and by hectare
      table.results[row.num,'AGB'] <- sum(forest$AGB, na.rm = TRUE)  #Estimate biomass from the stand at end of year
      table.results[row.num,'EMISSIONS.HARVEST'] <- emissions.harvest*-1  #Estimate biomass from the stand harvest
      table.results[row.num,'EMISSIONS.SKIDDING'] <- emissions.skidding*-1  #Emissions from winching
      table.results[row.num,'EMISSIONS.DIRECTIONAL'] <- emissions.directional*-1  #Emissions from directional felling
      table.results[row.num,'SEQUESTERED'] <- AGB.sequestered  #Includes growth + recruitment + enrichment - mortality
      #table.results[row.num,'HURRICANE'] <- hurricane.dead  #Add if this was a hurricane year
      
    }  
  }
  
  #Estimate the total Emissions for each year
  table.results <- table.results %>% mutate(
    EMISSIONS = EMISSIONS.HARVEST + EMISSIONS.SKIDDING  + EMISSIONS.DIRECTIONAL,
    EMISSIONSperm3 = EMISSIONS / VOL.HARVESTED,
    NET.SEQUESTERED = rowSums(cbind(SEQUESTERED, EMISSIONS), na.rm = TRUE) #Estimate sequestered biomass from the stand
    )
  
  #Return table of results
  return(table.results)
}