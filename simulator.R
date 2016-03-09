######################################
#   Main Simulator Process for KUUL 
    #This function dictates the simulation process

#It requires helpers.R 
######################################
YieldSimulator <- function(hs, sy, trees.tab, diameter.eqs, volume.eqs){
    ### YIELD SIMULATOR PROCESS 
  # --------Input
  # hs: Harvesting scenario to run in the simulation.
  # sy: The number of years that the simulation will run.
  
  # TREES: Table with Tree inventory. It has the next columns:
  #     - NAME: Species or group of species name
  #     - DBH: Diameter of the Tree
  #     - HEIGHT: Height of the tree
  
  # SPECIES: Table with Species information. It has the next columns:
  #     - NAME: Species or group of species name
  #     - PRICE: Price per cubic meter of these species group.
  #     - MAI: Mean annual increment of the species. Or parameters of equations.
  #     - VOLUME.EQ: Parameters of allometric equations and form of formula.
  
  # --------Output
  #     - table.results: Table with year, (1) harvested volume, (2) carbon, (3)income results


  
  ### ADDING DBH AND VOLUME PARAMETERS AS COLUMNS TO EASY CALCULATION-----------
  trees.tab['DBH.EQ.NUM'] <- match(trees.tab$SPECIES.CODE, diameter.eqs$SPECIES.CODE ) #getting a list of the order in diameter equation table 
  trees.tab['DBH.EQ.CODE'] <- diameter.eqs$EQ.CODE[trees.tab$DBH.EQ.NUM]  #Equation code for diameter
  trees.tab['DBH.EQ.B0'] <- diameter.eqs$B0[trees.tab$DBH.EQ.NUM]  #Parameter b0 for diameter growth equation
  trees.tab['DBH.EQ.B1'] <- diameter.eqs$B1[trees.tab$DBH.EQ.NUM]  #parameter b1 for diameter growth equation
  
  trees.tab['VOL.EQ.NUM'] <- match(trees.tab$SPECIES.CODE, volume.eqs$SPECIES.CODE ) #getting a list of the order in volume equation table
  trees.tab['VOL.EQ.CODE'] <- volume.eqs$EQ.CODE[trees.tab$VOL.EQ.NUM]  #Equation code for diameter
  trees.tab['VOL.EQ.B0'] <- volume.eqs$B0[trees.tab$VOL.EQ.NUM]  #Parameter b0 for volume growth equation
  trees.tab['VOL.EQ.B1'] <- volume.eqs$B1[trees.tab$VOL.EQ.NUM]  #Parameter b1 for volume growth equation
  
  ### RANDOMIZATION OF INITAL FOREST-----------
  n.trees <- rpois(1, 70)     #Random number of trees
  trees.tab <- trees.tab[sample(x = nrow(trees.tab),  #From the number of rows in the table 
                                size = n.trees,       #choose n.trees
                                replace = TRUE), ]    #With replacement

  
  ### LISTS TO STORE VALUES-----------
  columns <- c('BA', 'AGB', 'INCOME', 'VOLUME')
  table.results <- data.frame(matrix(ncol = length(columns), nrow = sy))
  colnames(table.results) <- columns
  
  
  #LOOP OF SIMULATION YEARS-----------
  for (y in 1:sy){
    trees.tab$DBH <- diameter.calc(trees.tab)   #Diamete growth
    table.results[y,'BA'] <- sum(pi * (trees.tab$DBH/100/2)^2)  #Estimate biomass from the stand (it uses Chave 2014, see helpers.R). Transform to square meters
    table.results[y,'AGB'] <- agb.calc(trees.tab)  #Estimate biomass from the stand (it uses Chave 2014, see helpers.R)
  }
  
  #Return table of results
  return(table.results)
}