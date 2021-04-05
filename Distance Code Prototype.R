#load packages
require(tidyverse)

#load in the dataset
PCA_mean_imputted <- read.csv("C:/Users/ryans/OneDrive/Desktop/Spring 2021/Sports Analytics/Basketball/NBA-player-and-lineup-clustering/PCA_mean_imputted.csv")


#Charlotte Hornets Starting Lineup
charlotte_5 <- PCA_mean_imputted %>%
  filter(X == "Stephen Curry" |
         X == "Victor Oladipo" |
         X == "LeBron James" |
         X == "Nikola Jokic" |
         X == "Rudy Gobert") %>%
  mutate(lineup = 1)

without_kemba <- PCA_mean_imputted %>%
  filter(X == "Dennis Schroder" |
         X == "Jerami Grant" |
         X == "Seth Curry" |
         X == "Aaron Gordon" |
         X == "Nikola Vucevic") %>%
  mutate(lineup = 2)

#Filter for the Lineup Players
lineups <- rbind(charlotte_5, without_kemba)

players <- lineups[,c(1,7)]
PCAs <- lineups[,2:6]

#calculate distances between players
player_distance <- dist(PCAs, diag=T, upper=T)
player_distance <- as.matrix(player_distance, nrows=10, ncol=10)
player_distance <- cbind(player_distance, players)
player_distance <- as.data.frame(player_distance)



#Remove Distances for Players On the Same Lineup
for (i in 1:nrow(player_distance)){
  for (j in 1:nrow(player_distance)) {
    if (player_distance$lineup[i]==player_distance$lineup[j]) {
      player_distance[i, j] <- NA
  }
  }
}


#calculate lineup distance
player_distance <- player_distance %>%
  select(-c(lineup,X))

player_distance <- as.matrix(player_distance)
player_distance <- as.vector(player_distance)
player_distance <- as.numeric(player_distance)

#lineup distance
mean(player_distance, na.rm=TRUE)
