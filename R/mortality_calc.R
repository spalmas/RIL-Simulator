#function to simulate natural mortality of the stand
#returns a table of the stand and another of the dead trees
mortality.calc <- function(stand){
  
  #There is 1% probability of dying in a year
  random.trees <- sample(x = c(TRUE, FALSE), 
                         size = nrow(stand),
                         replace = TRUE, 
                         prob = c(0.99, 0.01))
  
  return(list(stand[random.trees,],
              stand[!random.trees,]))
}
