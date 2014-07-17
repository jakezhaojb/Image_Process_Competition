load("4images.RData")
# all the 1~4 work fine
y <- x[[4]] 

# get the red line of the land
edge <- ( y[,,1]>0.7) * (y[,,2]<0.5) * (y[,,3] < 0.5)
image(edge)
edge = 3*(edge>0.5)

# get a seed to spread
# like a virus to spread and the red edge is a closed wall
# of a city. virus starting from the seed point will infect
# all the people in the wall, but do not spread across the wall 
seed = apply( which( edge>1 , arr.ind = TRUE),2,mean)
edge[seed[[1]],seed[[2]]] <- 1;

# 2 times is enough
for (i in 1:1 ){ 
  # spread from left to right
  for ( j in 2:nrow(edge)-1 ){
    for ( k in 2:ncol(edge)-1) {
      if ( edge[j,k] == 1 ){
        if ( sum( as.numeric( edge[(j-1):(j+1),(k-1):(k+1)]==3 )) < 2) {
          # not go across the wall, 
          # and then 0 points into 1
          edge[(j-1):(j+1),(k-1):(k+1)] <- 
            ( edge[(j-1):(j+1),(k-1):(k+1)]==0 ) + edge[(j-1):(j+1),(k-1):(k+1)]
        }
      }
    }
  }
  # spread from right to left
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
  image(edge)
}
land <- ( edge == 1 )
image(land)
areaLand <- sum(land )

# color of hourse is different
# count the hourses in the land
image( (y[,,2]>0.7) * (y[,,2] < 0.9) * land )
areaHouse <- sum( (y[,,2]>0.7) * (y[,,2] < 0.9) * land )

result <- areaHouse/areaLand
