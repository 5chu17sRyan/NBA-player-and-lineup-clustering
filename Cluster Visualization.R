library(tidyverse)
library(igraph)

##### PaLD Clustering Attempt #####

calcCohesionThreshold <- function(cohesion_matrix){
  diagonal_entries <- diag(as.matrix(cohesion_matrix))
  avg_diag_entry <- mean(diagonal_entries) #The algorithm normally has mean
  
  #Threshold for determining if two points are particularly cohesive
  threshold <- avg_diag_entry/2
  return(threshold)
}

# Import player cohesion data
player_cohesions <- read.csv("C:/Users/ryans/OneDrive/Desktop/Spring 2021/Sports Analytics/Basketball/NBA-player-and-lineup-clustering/player_cohesions.csv") %>%
  select(-X) %>%
  as.matrix()

# Calculate cohesion threshold
threshold <- calcCohesionThreshold(player_cohesions)

# Creates a graph from the cohesion threshold
graph <- graph_from_adjacency_matrix(player_cohesions, mode = "min", weighted = TRUE)

# Removes edges with weak cohesion from the graph
cohesions <- E(graph)
weak_cohesions <- cohesions[cohesions$weight < threshold]
graph <- graph - weak_cohesions
graph <- simplify(graph)

# Determines connected components 
components(graph, mode = "strong")
#63 components but 464 out of 530 players are in one component next biggest component is 4

# Plots vertices using Fruchterman-Reingold layout
plot(layout_with_fr(graph))
#Produces one big cloud of vetrices, no clear clusters :(