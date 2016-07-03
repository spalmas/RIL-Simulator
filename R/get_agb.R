#' Function to estimate Above Ground Biomass of the trees from Cairns Paper
#'
#' Lond Description
#'
#' @param 
#' @param 
#'
#' @references
#' 
#' @return AGB in kilograms of Carbon of dried biomass
#'
#' @seealso \code{\link{hd_coef}}. For BA, QD and N see \code{\link{get_stand}}
#'
#' @examples

get.agb <- function(stand){
  return(0.47*(exp(-2.173+0.868 * log(stand$DBH^2 * stand$HEIGHT)+0.0939 / 2))) # Cairns paper
}

