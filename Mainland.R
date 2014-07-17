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


# Drawing plots
myPlot <- function(im, ratio) {
  xvals <- rep(1:(dim(im)[1]), dim(im)[2])
  yvals <- rep(1:(dim(im)[2]), each=dim(im)[1])
  dim(im) <- c(dim(im)[1]*dim(im)[2], 3)
  plot(xvals, yvals, pch=18, cex=0.4,
       col=apply(im, 1, function(a) {
         return(rgb(a[1], a[2], a[3]))}))
  text(90, 400, round(ratio, 5))
}


###########################################
# main function
load("4images.RData")
# all the 1~4 work fine
par(mfrow = c(2, 2)) 
# Start!
for (i in 1:4) {
  y <- x[[i]]
  land <- Erosion(y)
  areaLand <- sum( land )
  
  #image( (y[,,2]>0.7) * (y[,,2] < 0.9) * land )
  
  # find houses inside the land
  house <- (y[,,2]>0.7) * (y[,,2] < 0.9) * land
  
  # move the houses according to 4 directions to cover 
  # the green lines and the black edge 
  house2 <- house[ 2:(nrow(house)-1) , 2:(ncol(house)-1) ] + 
    house[ 1:(nrow(house)-2) , 2:(ncol(house)-1) ] + # up
    house[ 2:(nrow(house)-1) , 1:(ncol(house)-2) ] + # left
    house[ 3:(nrow(house)) , 2:(ncol(house)-1) ] +   # down
    house[ 2:(nrow(house)-1) , 3:(ncol(house)) ]     # right
#  #image( house2)
  house2 <- ( house2>=1 )*1  # turn all non-zero number into 1
  areaHouse <- sum( house2 )  # count the area of house
  
  result <- areaHouse/areaLand # count the ratio
  
  im <- array(1, dim = c( (nrow(land) - 2), (ncol(land) - 2), 3 ))
  im[,,1] <- land[ 2: (nrow(land) - 1), 2: (ncol(land) - 1) ]
  im[,,3] <- house2
  myPlot(im, result) 
}

