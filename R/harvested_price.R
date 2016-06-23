#' Function to estimate Above Ground Biomass of the trees from Cairns Paper
#'
#' Lond Description
#'
#' @param 
#' @param 
#'
#' @references
#' Gezan, S.A. and Ortega, A. (2001). Desarrollo de un Simulador de Rendimiento para
#' Renovales de Roble, Rauli y Coigue. Reporte Interno. Projecto FONDEF D97I1065, Chile

#' @return AGB
#'
#' @seealso \code{\link{hd_coef}}. For BA, QD and N see \code{\link{get_stand}}
#'
#' @examples
#' # Example 1: Obtain Dominant Age
#' (AD<-get_site(dom_sp=1, zone=2, HD=14, SI=10))
#' round(AD,0)  # Rounded

harvested.price <- function(harvested){
  return(0) # Cairns paper
}

