#Function to assign volume equation parameters for each tree.
assigning_volume_params <- function(stand, volume.eqs){
  stand['VOL.EQ.NUM'] <- match(stand$SPECIES.CODE, volume.eqs$SPECIES.CODE ) #getting a list of the order in volume equation table
  stand['VOL.EQ.B0'] <- volume.eqs$B0[stand$VOL.EQ.NUM]  #Parameter b0 for volume growth equation
  stand['VOL.EQ.B1'] <- volume.eqs$B1[stand$VOL.EQ.NUM]  #Parameter b1 for volume growth equation
  return (stand)
}