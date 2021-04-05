library(tidyverse)

#Import Clustering Data
clustered_players <- read.csv("C:/Users/ryans/OneDrive/Desktop/Spring 2021/Sports Analytics/Basketball/NBA-player-and-lineup-clustering/clustered_players.csv", row.names=1)

#Sample players who are similar to Cody Zeller to test the accuracy of the clusterings

##### Greedy #####
zeller <- clustered_players %>%
  filter(X == "Cody Zeller")

zeller_community <- zeller$mmclust #Change clustering variable

members <- clustered_players %>%
  filter(mmclust == zeller_community) #Change clustering variable

num_members <- nrow(members)
  
sample <- members[sample(num_members, 30),]$X
