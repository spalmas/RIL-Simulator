#' Main simulator process
#'
#' Long Description: It is improved from simulator because it takes into consideration a different annual cutting areas.
#' Each annual cutting area, by definition, has their own cycle of cutting. Based on rotation periods.
#' 
#'
#' @param scenario text name of scenario
#' @param sy total simulation years
#' @param it number of iterations of the complete scenarios.
#' @param rotation rotation period of forest. Also the number of ACA areas of one hectare
#' @param intensity
#' @param enrich.bosquete The ejido performs enrichment bosquete planting (TRUE or FALSE)
#' @param w.dist
#' @param dir.felling
#' @param improved.trail
#' @param forest.tab Forest list of trees
#' @param hurr.year year where a hurricane should occur, if desired
#' @param hurr.cat category of the hurricane that occurs at hurr.year
#'
#' @references
#' Ninguna por ahora
#' @return a table with yearly results of BA (Basal Area in the stand), AGB (aboveground biomass in stand), INCOME, volume extracted, total sell price of products extraccted 
#'
#' @examples
#' source('startup.R')
#' rotation <- 25
#' forest.tab <- forest.randomizer(ROTATIONYEARS = rotation)
#' table.results <- simulator(sy = 25, 
#'     it = 3,
#'     rotation = rotation, 
#'     intensity = 'All',
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
                      enrich.bosquete = FALSE, 
                      w.dist = 0, 
                      dir.felling = TRUE, 
                      improved.trail = FALSE, 
                      forest.tab,
                      hurr.year = NA,
                      hurr.cat = NA){ 

  ### TABLE TO STORE VALUES AND REPORT -----------
  columns <- c('SCENARIO', 'IT', 'YEAR', 'BA', 'AGB', 'INCOME', 'N.HARVESTED', 'VOL.HARVESTED')
  table.results <- matrix(ncol = length(columns), nrow = sy*it) %>% as_tibble()
  colnames(table.results) <- columns
  #forest <- forest.randomizer()
  
  table.results <- table.results %>% 
    mutate(SCENARIO = scenario,
           N.HARVESTED = NA,
           VOL.HARVESTED = NA,
           INCOME = NA,
           IT = rep(1:it, times = sy) %>% sort,   #column of iteration
           YEAR = rep(0:(sy-1), times = it)
           )
  
  #hurricane mortalities probabiilites table
  hurr.mortalities <- tibble(small.mortality = c(0, 0.1, 0.2, 0.3), prob = c(0.55, 0.2, 0.15, 0.1))
  
  #LOOP OF SIMULATION YEARS AND ITERATIONS-----------
  for (i in 1:it){   #For each iteration
    #i=1
    forest.tab <- forest.randomizer(ROTATIONYEARS = rotation)
    forest <- forest.tab  #return to initial forest
    
    #AGB0 is the mean of the ACA AGB
    AGB0 <- forest %>% group_by(ACA) %>% summarise(AGB = sum(AGB, na.rm = TRUE)) %>% summarise(AGB = mean(AGB))
    AGB1 <- AGB0  #The first year the sequestration is 0
    
    for (y in 0:(sy-1)){   #For each simulation year
      #row number based on the repetition and simulation year for table of results
      row.num <- (y + 1) + (i - 1) * sy

      #Get ACA from the year and rotation years
      ACA <- y -  rotation*(floor(y/rotation))
  
      ######## NATURAL MORTALITY
      natural.dead <- forest %>% mortality.calc()    #T/F list if they died of natural causes
      #forest.dead <- forest[natural.dead,]      #creating a dead list. For now there is no analysis with the forest.dead
      forest <- forest[!natural.dead,]          #removing dead from alive forest
      
      ######## HURRICANES MORTALITY
      #If there is a valur for hurr.year and if it matches the y
      if (!is.na(hurr.year) &  (hurr.year == y + 1)){
        #Use the hurr.cat value mortalities from table
          small.mortality <- hurr.mortalities$small.mortality[hurr.cat - 3]
        } else {
          #Randomize a hurricane from table
          small.mortality <- sample(x = hurr.mortalities$small.mortality, prob = hurr.mortalities$prob, size = 1)
        }
      hurricane.dead <- forest %>% hurricane.mortality(small.mortality = small.mortality)        #T/F list if they died from a hurricane
      #forest.dead <- forest.dead %>% bind_rows(forest[hurricane.dead,])     #Adding dead trees to dead  list 
      forest <- forest[!hurricane.dead,]          #removing dead from alive forest
      
      ####### GROWTH FUNCTION
      forest <- forest %>% get.diameter.growth()   #randomized diameter growth

      ####### REGENERATION
      #it only occurs every four years
      if ( y %% 4 == 0){
        regen.table <- forest %>% get.regeneration ()   #Regeration process
        forest <- forest %>% bind_rows(regen.table)  #adding the new trees to the forest
      } 
      
      ####### BIOMASS PER HECTARE (IS AFTER NATURAL PROCESSES AND BEFORE HARVEST)
      forest$AGB <- get.agb(forest = forest) #a new estiamate of biomass
      AGB.sequestered <- sum(forest$AGB, na.rm = TRUE)/rotation - AGB0  #mean sequestered value in all forest.
      AGB0 <- AGB1  #Updating value      
      
      ####### DEFAULT VALUES FOR SOME VARIABLES. WILL NOT CHANGE IF THIS IS NOT A HARVESTING YEAR IN THE ROTATION.
      emissions.harvest <- NA; emissions.skidding <- NA; emissions.directional <- NA
      
      ####### HARVESTING TREES. Since harvesting trees already occurs only at one plot,
      #harvested values are already in one hectare scale because it only cuts one ACA
      harvested.list <- get.harvest(forest = forest, intensity = intensity, ACA. = ACA) #harvesting the forest and store harvested trees
      harvested <- forest[harvested.list,]   #Getting a harvested tree list
      harvested$VOLUME <- harvested %>% get.volume()     #Get total volume harvested
      #harvested$PRICE <- harvested %>% get.price()       #Assigning price to each tree
      forest <- forest[!harvested.list,]   #Removing harvested trees from the stand
      
      #Adding result numbers to table of results.
      table.results$N.HARVESTED[row.num] <- nrow(harvested)  #Number of extracted trees
      table.results$VOL.HARVESTED[row.num] <- sum(harvested$VOLUME, na.rm = TRUE)  #All volume extacted in that year
      #table.results$INCOME[row.num] <- sum(harvested$PRICE, na.rm = TRUE)  #All income from trees harvested in that year
      emissions.harvest <- sum(harvested$AGB, na.rm = TRUE)  #Harvested Biomass 
      
      ####### DO ENRICHMENT PLANTING
      if (enrich.bosquete){
        #ACA <- 1
        enrichment.table <- do.enrichment(harvested = harvested, ACA. = ACA) #IT MAY RETURN ACU FROM PREVIOUS CYCLE IF NO WERE CUT
        forest <- forest %>% bind_rows(enrichment.table)  #adding the new trees to the stand
        #Should be in hectare basis because AGB.sequstered was already in hectare
        #and the enrichment table is only using data for one ACA
        AGB.sequestered <- sum(enrichment.table$AGB, na.rm = TRUE) + AGB.sequestered #Adding the small sequestered biomass
      }
      
      ####### DO SKIDDING MORTALITY
      #Should be in hectare basis because harvested only comes from one ACA
      emissions.skidding <- 0  #reset for this yeasr
      skidding.dead.bool <- skidding.mortality(forest = forest, w.dist = w.dist, harvested = harvested)  ##kills inside area and small trees (< 20cm DBH)
      emissions.skidding <- forest[skidding.dead.bool,]$AGB %>% sum(na.rm = TRUE)  #Winching mortality emissions
      forest <- forest[!skidding.dead.bool,]  #Removing them from the forest

      ####### HAVE DIRECTIONAL MORTALITY OF dir.felling is FALSE
      emissions.directional <- 0   #reset for this year
      if (!dir.felling){
        directional.dead.bool <- directional.mortality(forest = forest, harvested = harvested)    #directinonal felling mortality
        emissions.directional <- forest[directional.dead.bool,]$AGB %>% sum(na.rm = TRUE) # directional felling mortality emissions
        forest <- forest[!directional.dead.bool,]  #Removing them from the forest
      }

      ####### computing final forest values
      ACA.parameters <- forest %>% group_by(ACA) %>% 
        summarise(BA = sum(pi * (DBH^2/40000), na.rm = TRUE),
                  AGB = sum(AGB, na.rm = TRUE))
  
      ####### Storing stand results 
      table.results[row.num,'BA'] <- ACA.parameters$BA %>% mean()  #Estimate Basal Area from the stand. Transform to square meters and by hectare
      table.results[row.num,'AGB'] <- ACA.parameters$AGB %>% mean()
      table.results[row.num,'EMISSIONS.HARVEST'] <- emissions.harvest*-1  #Estimate biomass from the stand harvest
      table.results[row.num,'EMISSIONS.SKIDDING'] <- emissions.skidding*-1  #Emissions from winching
      table.results[row.num,'EMISSIONS.DIRECTIONAL'] <- emissions.directional*-1  #Emissions from directional felling
      table.results[row.num,'SEQUESTERED'] <- AGB.sequestered  #Includes growth + recruitment + enrichment - mortality
      #table.results[row.num,'HURRICANE'] <- hurricane.dead  #Add if this was a hurricane year
    }  
  }
  
  #Estimate the total Emissions per hectare for each year
  table.results <- table.results %>% mutate(
    EMISSIONS = EMISSIONS.HARVEST + EMISSIONS.SKIDDING  + EMISSIONS.DIRECTIONAL,
    EMISSIONSperm3 = EMISSIONS / VOL.HARVESTED,
    NET.SEQUESTERED = rowSums(cbind(SEQUESTERED, EMISSIONS), na.rm = TRUE) #Estimate sequestered biomass from the stand
    )
  
  #Return table of results
  return(table.results)
}