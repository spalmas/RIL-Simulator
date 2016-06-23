# source('helpers.R')
# 
# stand <- stand.randomizer()
# diameter.eqs <- read.csv('Data/diameter_growth.csv')
# stand <- assigning_diameter_params(stand, diameter.eqs)
# stand$DIAMETER.GROWTH <- diameter.growth(stand)
# 
# 
# 
# rnorm(1, stand$DBH.GROWTH.MEAN[1], stand$DBH.GROWTH.SD[1])

# 
# dbinom(x =nrow(harvested), size = 1, prob = .50)
# intensity <- 'High'
# 
# euc.dist <- function(x1, y1, x2, y2){
#   return(sqrt((x2-x1)^2 + (y2-y1)^2))
# }
# 
# distance  <- function(harvested){
#   pairs <- combn(1:nrow(harvested), m= 2)
#   distance.list <- rep(NA, ncol(pairs))
#   for (i in 1:ncol(pairs)){
#     #calculating euclidean distnace
#     distance <- euc.dist(x1 = harvested$COORD.X[pairs[1,i]],
#                          y1 = harvested$COORD.Y[pairs[1,i]],
#                          x2 = harvested$COORD.X[pairs[2,i]],
#                          y2 = harvested$COORD.Y[pairs[2,i]])
#     
#     distance.list[i] <- distance  #adding distance to list
#   }
# 
#   distance.list <- cbind(t(pairs), distance.list)[order(distance.list),]
#   return(distance.list)
# }
# 
# rotation = 5   #rotation period
# y = 0  #current year
# sy%%rotation == 0
# 
# distance (harvested)
