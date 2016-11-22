source('startup.R')

scenario = 'A'
sy = 25
it = 5    #General simulation parameters
rotation = 20
intensity = 'Normal'
enrich.bosquete = TRUE
w.dist = 5
dir.felling  =TRUE
improved.trail = TRUE
lower.impact = TRUE  #Harvesting scenario
trees.tab <- stand.randomizer()
get.agb(trees.tab)

DBH <- rnorm(n = 1000, mean = 1, sd = .15)
get.height(DBH)

table.results <- simulator(scenario = scenario, 
          sy = sy, 
          it = it,    #General simulation parameters
          rotation = rotation, 
          intensity = intensity,
          enrich.bosquete = enrich.bosquete, 
          w.dist = w.dist, 
          dir.felling  = dir.felling,
          improved.trail = improved.trail,
          lower.impact = lower.impact,  #Harvesting scenario
          trees.tab  = trees.tab)



rowSums(cbind(table.results$EMISSIONS.DIRECTIONAL, table.results$EMISSIONS.HARVEST, table.results$EMISSIONS.SKIDDING), na.rm = TRUE)
#geom_ribbon(aes(ymin = AGB/1000 - 2, ymax = AGB/1000 + 2), fill = "grey70") +


sum(get.volume(NULL), na.rm = TRUE)



harvested$VOLUME <- get.volume(harvested)
get.price(harvested)


head(regen.table)
regen.table = get.regeneration(trees.tab, canopy.cover = 999)
rbind(stand, regen.table)  #adding the new trees to the stand

head(trees.tab)
head(regen.table)

