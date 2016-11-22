#' Estimate mean and standard deviation from a list correcting for iterations
#'
#' It esimtaes mean and sd correcting for iterations
#'
#' @param X X column of localization of plots
#' @param Y Y column of localization of plots
#'
#' @references
#' Ninguna por ahora

#' @return A formated mean and sd from the given column
#'
#' @examples
#' source('startup.R')
#' trees.tab <- stand.randomizer()
#' table.results <- simulator(sy = 15, 
#'     it = 2,
#'     rotation = 10, 
#'     intensity = 'Normal',
#'     enrich.bosquete = TRUE, 
#'     w.dist = 10, 
#'     dir.felling  = TRUE, 
#'     improved.trail = TRUE, 
#'     lower.impact = TRUE, 
#'     trees.tab  = trees.tab)
#'     
#' mean.sd(results.column = 'EMISSIONS', data = table.results, scenario = 'RIL')
#'
mean.sd <- function(results.column, data, scenario, na.rm = TRUE, zeros = TRUE){
  
  tabla <- aggregate(get(results.column) ~ IT , data=data, sum, na.action = na.omit)
  
  string <- paste0(round(mean(tabla[,2], na.rm = TRUE), digits = 1),' (', round(sd(tabla[,2], na.rm = TRUE), digits = 1), ')')
  
  return(string)
}
