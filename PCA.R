
#https://www.statology.org/principal-components-analysis-in-r/
library(tidyverse)
library(dplyr)
library(ggplot2)

#1. Remerge the data with additional information
box_advanced <- read.csv("C:/Users/ryans/OneDrive/Desktop/box_advanced.csv")
all_data <- box_advanced %>%
  select(PersonName, Height, Weight)
names(all_data)[1] <- "player"

cleaned_data_2019 <- read.csv("C:/Users/ryans/OneDrive/Desktop/Spring 2021/Sports Analytics/Basketball/NBA-player-and-lineup-clustering/Data/cleaned_data_2019.csv")
basketball_data <- cleaned_data_2019 %>%
  select(-c(X))

basketball_data <- merge(basketball_data, all_data, by="player", all.x = T)
names(basketball_data)[23] <- "height"
names(basketball_data)[24] <- "weight"

#2. Make sure all the variables are numeric

basketball_data$age <- as.numeric(basketball_data$age)
basketball_data$height <- as.numeric(basketball_data$height)
basketball_data$weight <- as.numeric(basketball_data$weight)

#3. Convert everything to a z-score
basketball_data[,2:24] <- scale(basketball_data[,2:24])

# #4. Look at PCA unimputed (we are not using this)
# basketball_noNA <- basketball_data[complete.cases(basketball_data),]
# 
# basketball_noNA <- basketball_noNA %>%
#   select(-c(player))
# 
# results <- prcomp(basketball_noNA, scale = TRUE)
# results$rotation <- -1*results$rotation
# percentage_of_variance <- results$sdev^2 / sum(results$sdev^2)
# plot(percentage_of_variance, xlim=c(3,23), ylim=c(0,0.1)) #five factors
# 
# PCA_omitted_Nas <- as.data.frame(results$x)
# PCA_omitted_Nas <- cbind.data.frame(basketball_noNA$player, PCA_omitted_Nas)
# names(PCA_omitted_Nas)[1] <- "player"

#5. Look at PCA imputed with the mean
#imputed PCA
basketball_data_mean_imp <- basketball_data %>%
  select(-c(player))
rownames(basketball_data_mean_imp)<- basketball_data$player

for(i in 1:ncol(basketball_data_mean_imp)) {
  basketball_data_mean_imp[ , i][is.na(basketball_data_mean_imp[ , i])] <- mean(basketball_data_mean_imp[ , i], na.rm = TRUE)
}

#looking at the results
results_mean_imp <- prcomp(basketball_data_mean_imp, scale=TRUE)
results_mean_imp$rotation <- -1*results_mean_imp$rotation
percentage_of_variance_2 <- results_mean_imp$sdev^2 / sum(results_mean_imp$sdev^2)
plot.new() 
plot(percentage_of_variance_2, xlim=c(3,23), ylim=c(0,0.1), xlab="Component", ylab="Percentage of Variance", main="Percentage of Variance Explained by Each Component") #five factors


write.csv(results_mean_imp$rotation, "C:/Users/ryans/OneDrive/Desktop/Spring 2021/Sports Analytics/Basketball/NBA-player-and-lineup-clustering/PCA_Components_mean_imputted.csv")


#saving the PCA. We think it makes most sense to use the mean imputted because same # of components and seems to make sense given that NAs are players who made no attempts
PCA_mean_imputted <- as.data.frame(results_mean_imp$x)
PCA_mean_imputted <- PCA_mean_imputted[,1:6]

write.csv(PCA_mean_imputted, "C:/Users/ryans/OneDrive/Desktop/Spring 2021/Sports Analytics/Basketball/NBA-player-and-lineup-clustering/PCA_mean_imputted.csv")

head(PCA_mean_imputted)
