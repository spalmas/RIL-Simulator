# Function to assign the new diameter based on the current DBH and
# diameter_growth randomizer. It is a sum
diameter_growth_assign <- function(stand){
  DBH <- stand$DBH + stand$DIAMETER.GROWTH
}