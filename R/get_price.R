#' Function that receives a table of trees with species and sizes and returns the price for each one
#'
#' @param stand trees (or basically any table) with the columns: SPECIES.CODE, VOLUME
#' 
#' @return A list of prices for each tree. Prices in Thousands of Pesos. This can be added as another column in the table of stand trees.
#'
#' @examples
#' source('startup.R')
#' stand <- stand.randomizer()
#' stand$VOLUME <- get.volume(stand)
#' get.price(stand = stand)

get.price <- function(stand){
  #dataframe of species code names and its prices. In a good simulation model,
  #this should be allowed to be input by the user. Maybe later
  prices <- data.frame(SPECIES.CODE = c('SM', 'LL', 'MB', 'MZ', 'PU', 'XX'),
                       PRICE = c(3.5, 1.8, 1.5, 1.8, 2.0, 1.0))
  
  #Assigning price of prices per cubic meter
  price.list <- prices$PRICE[match(stand$SPECIES.CODE, table = prices$SPECIES.CODE)]
  
  #return price times volume
  return(price.list * stand$VOLUME)
}

