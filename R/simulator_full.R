#' Main simulator process
#'
#' Long Description: It is improved from simulator because it takes into consideration a different annual cutting areas.
#' Each annual cutting area, by definition, has their own cycle of cutting. Based on rotation periods.
#' 
#'
#' @param scenario text name of scenario
#' @param sy total simulation years
#' @param it number of iterations of the complete scenarios.
#' @param area Area of plot to be simulated. Normally as one hectare plot.
#' @param rotation rotation period of forest. Also the number of ACA areas of one hectare
#' @param intensity
#' @param enrich.bosquete The ejido performs enrichment bosquete planting (TRUE or FALSE)
#' @param w.dist
#' @param dir.felling
#' @param forest.tab Forest list of trees
#' @param hurr.year year where a hurricane should occur, if desired
#' @param hurr.cat category of the hurricane that occurs at hurr.year
#'
#' @references Ninguna por ahora
#' 
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
#'     forest.tab  = forest.tab)
#' View(table.results)
#'
simulator <- function(scenario = 'A',
                      sy, 
                      it,
                      area = 10000, 
                      rotation, 
                      intensity, 
                      enrich.bosquete = FALSE, 
                      w.dist = 0, 
                      dir.felling = TRUE, 
                      forest.tab,
                      hurr.year = 0,
                      hurr.cat = 0){ 

  #Correction Factor to hectare
  CF = 10000/area 
  
  ### TABLE TO STORE VALUES AND REPORT -----------
  table.results <- matrix(nrow = sy*it) %>% as_tibble() %>%
    mutate(SCENARIO = scenario,
           IT = rep(1:it, times = sy) %>% sort,   #column of iteration
           YEAR = rep(0:(sy-1), times = it),  #column of years
           N.HARVESTED = NA,
           VOL.HARVESTED = NA
           )
  
  #hurricane mortalities probabiilites table
  hurr.mortalities <- tibble(category = c(0,3,4,5), prob = c(0.55, 0.2, 0.1, 0.05), small.mortality = c(0, 0.1, 0.2, 0.3))
  
  #LOOP OF SIMULATION YEARS AND ITERATIONS-----------
  for (i in 1:it){   #For each iteration
    #i=1
    #forest.tab <- forest.randomizer(ROTATIONYEARS = rotation)  #Randomizes every year
    forest <- forest.tab  #return to initial forest
    
    #Random
    #rand.ACA <- tibble(ACA = unique(forest$ACA), ACA2 = sample(unique(forest$ACA)))
    forest$ACA <- sample(forest$ACA)
    
    #AGB0 is the mean of the ACA AGB. Not by hectare
    forest.parameters <- forest %>% 
      group_by(ACA) %>% 
      summarise(BA = sum(pi * (DBH^2/40000), na.rm = TRUE),
                AGB = sum(AGB, na.rm = TRUE)) %>% 
      summarise(BA = mean(BA),
                AGB = mean(AGB))
    
    BA0 <- forest.parameters$BA[1]
    AGB0 <- forest.parameters$AGB[1]
    
    for (y in 0:(sy-1)){   #For each simulation year
      #y <- 0
      #row number based on the repetition and simulation year for table of results
      row.num <- (y + 1) + (i - 1) * sy

      #Get ACA from the year and rotation years
      ACA <- y -  rotation*(floor(y/rotation))
      
      ######## NATURAL MORTALITY
      natural.dead <- forest %>% mortality.calc()    #T/F list if they died of natural causes
      n.trees.dead <- sum(natural.dead)      #creating a dead list. For now there is no analysis with the forest.dead

      ######## HURRICANES MORTALITY
      #If there is a valur for hurr.year and if it matches the y
      #if (!is.na(hurr.year) &  (hurr.year == y + 1)){
        #Use the hurr.cat value mortalities from table
      if (y+1 == hurr.year){ 
        small.mortality <- (hurr.mortalities %>% filter(category == hurr.cat))$prob
      } else {
          #Randomize a hurricane from table
          #small.mortality <- sample(x = hurr.mortalities$small.mortality, prob = hurr.mortalities$prob, size = 1)
          small.mortality <- 0
      }
      hurricane.dead <- forest %>% hurricane.mortality(small.mortality = small.mortality)        #T/F list if they died from a hurricane
      n.trees.dead <- n.trees.dead + sum(hurricane.dead) #total number of deaths
      #forest.dead <- forest.dead %>% bind_rows(forest[hurricane.dead,])     #Adding dead trees to dead  list 
      forest <- forest[!(hurricane.dead | natural.dead),]          #removing dead from alive forest
      
      ####### GROWTH FUNCTION
      forest <- forest %>% get.diameter.growth()   #randomized diameter growth

      ####### REGENERATION
      #it only occurs every four years
      #if (y %% 4 == 0){
        regen.table <- forest %>% get.regeneration (area = area)   #Regeration process
        forest <- forest %>% bind_rows(regen.table)  #adding the new trees to the forest
      #} 
      
      ####### BIOMASS PER HECTARE (IS AFTER NATURAL PROCESSES AND BEFORE HARVEST)
      forest$AGB <- get.agb(forest = forest) #a new estiamate of biomass. Not by hectare
      
      ####### DEFAULT VALUES FOR SOME VARIABLES. WILL NOT CHANGE IF THIS IS NOT A HARVESTING YEAR IN THE ROTATION.
      emissions.harvest <- NA; emissions.skidding <- NA; emissions.directional <- NA
      
      ####### HARVESTING TREES. Since harvesting trees already occurs only at one plot,
      #harvested values are already in one hectare scale because it only cuts one ACA
      harvested.bool <- get.harvest(forest = forest, intensity = intensity, ACA. = ACA) #harvesting the forest and store harvested trees
      harvested <- forest[harvested.bool,]   #Getting a harvested tree list
      harvested$VOLUME <- (harvested %>% get.volume())     #Get total volume harvested. Not by hectare. Transformed below
      #harvested$PRICE <- harvested %>% get.price()       #Assigning price to each tree

      #Adding result numbers to table of results.
      #table.results$INCOME[row.num] <- sum(harvested$PRICE, na.rm = TRUE)  #All income from trees harvested in that year

      ####### DO SKIDDING MORTALITY
      #Should be in hectare basis because harvested only comes from one ACA
      skidding.dead.bool <- skidding.mortality(forest = forest, w.dist = w.dist, harvested = harvested)  ##kills inside area and small trees (< 20cm DBH)

      ####### HAVE DIRECTIONAL MORTALITY OF dir.felling is FALSE
      directional.dead.bool <- directional.mortality(forest = forest, harvested = harvested, dir.felling = dir.felling)    #directinonal felling mortality

      #Removing all killed trees from the forest
      forest <- forest[!(harvested.bool | skidding.dead.bool | directional.dead.bool),]

      # Final emissions
      emissions.operations <- forest[directional.dead.bool | skidding.dead.bool | harvested.bool,]$AGB %>% sum(na.rm = TRUE) # directional felling mortality emissions
      
      ####### DO ENRICHMENT PLANTING
      if (enrich.bosquete){
        #ACA <- 1
        enrichment.table <- do.enrichment(harvested = harvested, ACA. = ACA, area) #IT MAY RETURN ACU FROM PREVIOUS CYCLE IF NO WERE CUT
        forest <- forest %>% bind_rows(enrichment.table)  #adding the new trees to the stand
      }
      
      ####### computing final forest values. Unit: area. These are NOT corrected to HA
      forest.parameters <- forest %>% group_by(ACA) %>% 
        summarise(BA = sum(pi * (DBH^2/40000), na.rm = TRUE),
                  AGB = sum(AGB, na.rm = TRUE)) %>% 
        summarise(BA = mean(BA),
                  AGB = mean(AGB))

      ####### Storing stand results. They should by hectare units
      table.results[row.num, 'N.HARVESTED'] <- nrow(harvested) * CF  #Number of extracted trees
      table.results[row.num, 'VOL.HARVESTED'] <- sum(harvested$VOLUME, na.rm = TRUE) * CF  #All volume extacted in that year
      table.results[row.num, 'BA0'] <- BA0 * CF  # by hectare
      table.results[row.num, 'AGB0'] <- AGB0 * CF #by hectare
      table.results[row.num, 'BA1'] <- forest.parameters$BA[1] * CF  #Mean of ACA values by hectare
      table.results[row.num, 'AGB1'] <- forest.parameters$AGB[1] * CF #by hectare
      table.results[row.num, 'EMISSIONS'] <- emissions.operations * CF  #Estimate biomass from the stand harvest by hectare
      #table.results$N.TREES.DEAD[row.num] <- CF * n.trees.dead/rotation  #Total number of deaths in year by hectare
      #table.results[row.num,'HURRICANE'] <- hurricane.dead  #Add if this was a hurricane year
      
      
      ####### UPDATING VALUES
      BA0 <- forest.parameters$BA[1]
      AGB0 <- forest.parameters$AGB[1]
    }  
      }
  
  #Estimate the total Emissions per hectare for each year
  table.results <- table.results %>% mutate(
    #EMISSIONSperm3 = EMISSIONS / VOL.HARVESTED,
    D.BA = BA1-BA0, #Estimate agb BALANCE
    D.AGB = AGB1 - AGB0 #Estimate agb BALANCE
  )
  
  #Return table of results
  return(table.results)
} 