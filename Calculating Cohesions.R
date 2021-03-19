library(tidyverse)
#loading in the file
player_distance <- read.csv("C:/Users/ryans/OneDrive/Desktop/Spring 2021/Sports Analytics/Basketball/NBA-player-and-lineup-clustering/player_distance.csv") %>%
  select(-X)
player_distance[1:5,1:5]

#####function definition#####
#distance_matrix is a matrix of distances between the data points of players
getCohesionMatrix <- function(distance_matrix){

  n = dim(distance_matrix)[1] #number of players
  A3=matrix(0,n,n)
  
  for(x in 1:(n-1)){
    for(y in (x+1):n){
      
      #Distances or x and all other points, and y and all other points
      dists_x=distance_matrix[x,] #Row x
      dists_y=distance_matrix[y,] #Row y
      
      #Distance between x and y
      dist_xy <- distance_matrix[x,y]
      
      #Conflict focus: any points that are closer to x than y is (and vice versa)
      conflict_points = which( (dists_x <= dist_xy) | (dists_y <= dist_xy) )
      
      #Distances between x and conflict points
      dists_x_points <- dists_x[conflict_points]
      
      #Distances between y and conflict points
      dists_y_points <- dists_y[conflict_points]
      
      #Local depth: The probability that a conflict-point is closer to x than it is to y (ties are determined by coin-flip)
      local_depth_x <- (dists_x_points < dists_y_points) + .5*(dists_x_points == dists_y_points)
      local_depth_y <- (dists_y_points < dists_x_points) + .5*(dists_x_points == dists_y_points)
      
      #The number of conflict points
      num_conflict_points <- length(conflict_points)
      
      #Cohesion: The contribution of the conflict point to the the depth of x
      A3[x,conflict_points] = A3[x,conflict_points] + local_depth_x/num_conflict_points
      A3[y,conflict_points] = A3[y,conflict_points] + local_depth_y/num_conflict_points
    }
  }
  rownames(A3)=colnames(distance_matrix)
  colnames(A3)=colnames(distance_matrix)
  
  #Take calculate the average cohesions
  cohesion_matrix <- A3/(n-1) #The matrix of partitioned local depths (cohesions)
  return(cohesion_matrix)
}

cohesion_matrix <- getCohesionMatrix(player_distance)
write.csv(cohesion_matrix, "C:/Users/ryans/OneDrive/Desktop/Spring 2021/Sports Analytics/Basketball/NBA-player-and-lineup-clustering/player_cohesions.csv")
