# Image Erosion function
####################################################
# Team member:
# Ang Liu
# Linda Zhou
# Jake Zhao
# Dingquan Li
# Flora Pan
# Jin Liu
# Li Shen
# Qiaorou Yan
# Jack Zhou
# Minshu Cheng
# 
#
# Image Erosion is like a virus spreading around in a city, which is
# enclosed by a closed wall barrier closely. Everything inside the wall
# of the city will be infected, meanwhile the courterparts outside the wall
# will be safe.

Erosion = function( y ) {
  ### This function is used for filling up closed edges, which can be convex or non-convex.
  # obtain the red edges of land
  #edge <- (y[,,1] == 1) * (y[,,2] == 0) * (y[,,3] == 0)
  edge <- ( y[,,1]>0.7) * (y[,,2]<0.5) * (y[,,3] < 0.5)
  #image(edge)
  edge = 3*(edge>0.5)
  
  # Create a seed to initialize Erosion.
  seed = apply( which( edge>1 , arr.ind = TRUE),2,mean)
  edge[seed[[1]],seed[[2]]] <- 1;
  
  # Generally 2 times for-loop is abundant, but more loops is also recommended.
  for (i in 1:2 ){ 
    # Erose from left to right
    for ( j in 2:nrow(edge)-1 ){
      for ( k in 2:ncol(edge)-1) {
        if ( edge[j,k] == 1 ){
          if ( sum( as.numeric( edge[(j-1):(j+1),(k-1):(k+1)]==3 )) < 2) {
            # Stop once the virus encounters the wall
            # If infected, the pixel will be 1, else 0.
            edge[(j-1):(j+1),(k-1):(k+1)] <- 
              ( edge[(j-1):(j+1),(k-1):(k+1)]==0 ) + edge[(j-1):(j+1),(k-1):(k+1)]
          }
        }
      }
    }
    # Erose from right to left
    for ( j in seq(nrow(edge)-1,2,by=-1) ){
      for ( k in seq(ncol(edge)-1,2,by=-1)) {
        if ( edge[j,k] == 1 ){
          if ( sum( as.numeric( edge[(j-1):(j+1),(k-1):(k+1)]==3 )) < 2 ) {
            edge[(j-1):(j+1),(k-1):(k+1)] <- 
              ( edge[(j-1):(j+1),(k-1):(k+1)]==0 ) + edge[(j-1):(j+1),(k-1):(k+1)]
          }
        }
      }
    }
    # image(edge)
  }
  # The area inside the red edges
  land <- ( edge >= 1 )
  
  return( land )
}
