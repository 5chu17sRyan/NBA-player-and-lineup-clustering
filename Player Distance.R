#read in the dataset
PCA_mean_imputted <- read.csv("C:/Users/ryans/OneDrive/Desktop/Spring 2021/Sports Analytics/Basketball/NBA-player-and-lineup-clustering/PCA_mean_imputted.csv")

num_players <- nrow(PCA_mean_imputted)

# distance_data <- read.csv("C:/Users/ryans/OneDrive/Desktop/Spring 2021/Sports Analytics/Basketball/NBA-player-and-lineup-clustering/player_distance.csv") %>%
#   as.matrix()
# distance_data <- distance_data[1:num_players,1:num_players]

#set player names as columns
rownames(PCA_mean_imputted) <- PCA_mean_imputted$X
PCA_mean_imputted <- PCA_mean_imputted %>%
  select(-c(X))

#calculate distance matrix using dist()
player_distance <- dist(PCA_mean_imputted, diag=T, upper=T)
player_distance <- as.matrix(player_distance, nrow=num_players, ncol=num_players)

#output the distance file
write.csv(player_distance, "C:/Users/ryans/OneDrive/Desktop/Spring 2021/Sports Analytics/Basketball/NBA-player-and-lineup-clustering/player_distance.csv")
