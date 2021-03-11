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

PCA_mean_imputted <- read.csv("C:/Users/ryans/OneDrive/Desktop/Spring 2021/Sports Analytics/Basketball/NBA-player-and-lineup-clustering/PCA_mean_imputted.csv")

##### K-MEANS
library(mclust)
set.seed(NULL)

PCA_mean_imputted <- read.csv("C:/Users/ryans/OneDrive/Desktop/Spring 2021/Sports Analytics/Basketball/NBA-player-and-lineup-clustering/PCA_mean_imputted.csv")

PCAs <- PCA_mean_imputted %>%
  select(-X)

library(bios2mds)

sil_score <- sil.score(PCAs, nb.clus = c(2:20))

sil_score_improvement <- NULL
for(i in 2:20){
  improvement <- 1 - ((1 - sil_score[i]) / (1 - sil_score[i-1]))
  sil_score_improvement <- c(sil_score_improvement, improvement)
}
which.max(sil_score_improvement)

kmeans_clust <- PCAs %>%
  kmeans(15)

Players <- PCA_mean_imputted %>%
  mutate(kmclust = as.factor(kmeans_clust$cluster))

ggplot(Players, aes(x=PC1, y=PC2, color = kmclust)) +
  geom_point()

##### Hierarchical Clustering #####
player_distance <- read.csv("C:/Users/ryans/OneDrive/Desktop/Spring 2021/Sports Analytics/Basketball/NBA-player-and-lineup-clustering/player_distance.csv") %>%
  select(-X) %>%
  as.dist()

view(player_distance)

hclust <- hclust(player_distance)
hclust15 <- cutree(hclust, k=15)

Players <- Players %>%
  mutate(hclust = as.factor(hclust15))

ggplot(Players, aes(x=PC1, y=PC2, color = hclust)) +
  geom_point()

##### Minimax Linkage #####
library(protoclust)

minimax_clust <- protoclust(player_distance)
minimax_clust_cut <- protocut(minimax_clust, k = 15)

head(Players)

Players <- Players %>%
  mutate(mmclust = as.factor(minimax_clust_cut$cl))

ggplot(Players, aes(x=PC1, y=PC2, color = mmclust)) +
  geom_point()
