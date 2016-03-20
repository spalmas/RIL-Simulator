######################################
#   Main Simulator Process for KUUL 
    #This function dictates the simulation process

#It requires helpers.R 
######################################
YieldSimulator <- function(hs, bos, sy, it, trees.tab, diameter.eqs, volume.eqs){
    ### YIELD SIMULATOR PROCESS 
  # --------Input
  # hs: Harvesting scenario to run in the simulation.
  # sy: The number of years that the simulation will run.
  # bos: Number of bosquetes per hectare 
  # it: Iterations of the simulator
  # trees.tab: Table with Tree inventory. It has the next columns:
  #     - NAME: Species or group of species name
  #     - DBH: Diameter of the Tree
  #     - HEIGHT: Height of the tree
  # diameter.eqs: Table with diameter growth equation
  #     - NAME: Species or group of species name
  #     - PRICE: Price per cubic meter of these species group.
  #     - MAI: Mean annual increment of the species. Or parameters of equations.
  #     - VOLUME.EQ: Parameters of allometric equations and form of formula.
  
  # --------Output
  # table.results: Table with (1) iteration, (2) year, (3) Basal area, (4) carbon stored

  #stand <- stand.randomizer()
  stand <- trees.tab  #initial stand
  
  ### ADDING DBH AND VOLUME PARAMETERS AS COLUMNS TO EASY CALCULATION-----------
  stand['DBH.EQ.NUM'] <- match(stand$SPECIES.CODE, diameter.eqs$SPECIES.CODE ) #getting a list of the order in diameter equation table 
  stand['DBH.EQ.CODE'] <- diameter.eqs$EQ.CODE[stand$DBH.EQ.NUM]  #Equation code for diameter
  stand['DBH.EQ.B0'] <- diameter.eqs$B0[stand$DBH.EQ.NUM]  #Parameter b0 for diameter growth equation
  stand['DBH.EQ.B1'] <- diameter.eqs$B1[stand$DBH.EQ.NUM]  #parameter b1 for diameter growth equation
  
  stand['VOL.EQ.NUM'] <- match(stand$SPECIES.CODE, volume.eqs$SPECIES.CODE ) #getting a list of the order in volume equation table
  stand['VOL.EQ.CODE'] <- volume.eqs$EQ.CODE[stand$VOL.EQ.NUM]  #Equation code for diameter
  stand['VOL.EQ.B0'] <- volume.eqs$B0[stand$VOL.EQ.NUM]  #Parameter b0 for volume growth equation
  stand['VOL.EQ.B1'] <- volume.eqs$B1[stand$VOL.EQ.NUM]  #Parameter b1 for volume growth equation
  
  ### TABLE TO STORE VALUES AND REPORT -----------
  columns <- c('IT', 'YEAR', 'BA', 'AGB', 'INCOME', 'VOLUME')
  table.results <- data.frame(matrix(ncol = length(columns), nrow = sy*it))
  colnames(table.results) <- columns

  
  #LOOP OF SIMULATION YEARS AND ITERATIONS-----------
  for (i in 1:it){   #For each iteration
    for (y in 1:sy){   #For each simulation year
      row.num <- y+(i-1)*sy #row number based on the repetition and simulation year
      table.results[row.num,'IT'] <- i   #Adding iteration to table
      table.results[row.num,'YEAR'] <- y   #Adding year to table
      stand$DBH <- diameter.calc(stand)   #Diamete growth
      table.results[row.num,'BA'] <- sum(pi * (stand$DBH/100/2)^2)  #Estimate biomass from the stand (it uses Chave 2014, see helpers.R). Transform to square meters
      table.results[row.num,'AGB'] <- agb.calc(stand)  #Estimate biomass from the stand (it uses Chave 2014, see helpers.R)
    }  
  }
  
  
  #Return table of results
  return(table.results)
}