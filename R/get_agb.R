#' Function to estimate Above Ground Biomass of the trees from Cairns Paper
#'
#' Lond Description
#'
#' @param 
#' @param 
#'
#' @references
#' Cairns 
#' 
#' @return AGB in kilograms of Carbon of dried biomass
#'
#' @seealso \code{\link{hd_coef}}. For BA, QD and N see \code{\link{get_stand}}
#'
#' @examples
#' # Example 1: Obtain Dominant Age

get.agb <- function(x){
  return(0.47*sum(exp(-2.173+0.868 * log(x[1]^2 * x[2])+0.0939 / 2))) # Cairns paper
}

