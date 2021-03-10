#read in the dataset
PCA_mean_imputted <- read.csv("C:/Users/18083/Desktop/202110/Sports Analytics/Basketball/NBA-player-and-lineup-clustering/PCA_mean_imputted.csv")

#set player names as columns
rownames(PCA_mean_imputted) <- PCA_mean_imputted$X
PCA_mean_imputted <- PCA_mean_imputted %>%
  select(-c(X))

#calculate distance matrix using dist()
player_distance <- dist(PCA_mean_imputted, diag=T, upper=T)
player_distance <- as.matrix(player_distance, nrow=530, ncol=530)

#output the distance file
write.csv(player_distance, "C:/Users/18083/Desktop/202110/Sports Analytics/Basketball/NBA-player-and-lineup-clustering/player_distance.csv")
