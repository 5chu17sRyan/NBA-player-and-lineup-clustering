#load packages
require(tidyverse)

#load in the dataset
PCA_mean_imputted <- read.csv("C:/Users/18083/Desktop/202110/Sports Analytics/Basketball/NBA-player-and-lineup-clustering/PCA_mean_imputted.csv", row.names=1)

#create a Lineup Column to store if players are in lineup 1 or 2
PCA_mean_imputted$lineup <- NA

#do just a test lineup
PCA_mean_imputted$lineup[1:5] <- "Lineup 1"
PCA_mean_imputted$lineup[6:10] <- "Lineup 2"

#Filter for the Lineup Players
lineups <- PCA_mean_imputted$lineup
PCA_mean_imputted <- PCA_mean_imputted %>%
  filter(!is.na(lineup)) %>%
  select(-c(lineup))

#calculate distances between players
player_distance <- dist(PCA_mean_imputted, diag=T, upper=T)
player_distance <- as.matrix(player_distance, nrows=10, ncol=10)
player_distance <- cbind(player_distance, lineups)
player_distance <- as.data.frame(player_distance)

#Remove Distances for Players On the Same Lineup
for (i in 1:nrow(player_distance)){
  for (j in 1:nrow(player_distance)) {
    if (player_distance$lineups[i]==player_distance$lineups[j]) {
      player_distance[i, j] <- NA
  }
  }
}

#calculate lineup distance
player_distance <- player_distance %>%
  select(-c(lineups))

player_distance <- as.matrix(player_distance)
player_distance <- as.vector(distance)
player_distance <- as.numeric(player_distance)

#lineup distance
mean(player_distance, na.rm=TRUE)
