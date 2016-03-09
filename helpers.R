######################################
#   helper funcitons for KUUL
######################################

#Function to estimate Above Ground Biomass of the trees
agb.calc <- function(trees){
  AGB = sum(0.0673 * (0.7 * trees$DBH^2 * trees$HEIGHT)^0.976)  #Chave 2014 equation
  return (AGB)
}

#Function to grow diameter of trees
diameter.calc <- function(trees){
  return (trees$DBH + trees$DBH.EQ.B0 + trees$DBH.EQ.B1 * trees$DBH)
}

plot_nav <- function(nav) {
  
  layout(matrix(c(1,2,1,3),2,2))
  
  palette(c("black", "grey50", "grey30", "grey70", "#d9230f"))
  
  # plot all scenarios
  matplot(nav,
          type = 'l', lwd = 0.5, lty = 1, col = 1:5,
          xlab = 'Years', ylab = 'Dollars',
          main = 'Projected Income from wood selling')
  
  
  ########## Second Plot: % of scenarios that are still paying
  p.alive = 1 - rowSums(is.na(nav)) / ncol(nav)
  plot(100 * p.alive, las = 1, xlab = 'Months', ylab = 'Percentage Paying',
       main = 'Percentage of Paying Scenarios', ylim=c(0,100))
  grid()
  
  
  ########## Third Plot: plot distribution of final wealth
  last.period = nrow(nav)
  
  final.nav = nav[last.period, ]
  final.nav = final.nav[!is.na(final.nav)]
  
  if(length(final.nav) ==  0) return()
  
  plot(density(final.nav, from=0, to=max(final.nav)), las = 1, xlab = 'Final Capital',
       main = paste0('Distribution of Final Capital\n', 100 * p.alive[last.period], '% are still paying'))
  grid()
}


