require(tidyverse)
require(dplyr)

Height.and.Weight.Data <- read.csv("C:/Users/Shanti/Desktop/Height and Weight Data.csv")
names(Height.and.Weight.Data)[1] <- "PLAYER"

HeightWeightData <- Height.and.Weight.Data %>%
  filter(TEAM!="") %>%
  select(c(PLAYER, HEIGHT, WEIGHT))

HeightWeightData$WEIGHT <- substr(HeightWeightData$WEIGHT, 1, 3)
table(HeightWeightData$HEIGHT)
HeightWeightData$FEET <- as.numeric(substr(HeightWeightData$HEIGHT, 1, 1))
HeightWeightData$INCHES <- as.numeric(substr(HeightWeightData$HEIGHT, 3, length(HeightWeightData$HEIGHT)))
HeightWeightData$HEIGHT.in <- HeightWeightData$FEET*12 + HeightWeightData$INCHES
HeightWeightData <-  HeightWeightData %>%
  select(-c(HEIGHT))

#write.csv(HeightWeightData, "C:/Users/Shanti/Desktop/HeightWeightData.csv")
HeightWeightData <- read.csv("C:/Users/Shanti/Desktop/202110/Sports Analytics/NBA-player-and-lineup-clustering/HeightWeightData.csv")
cleaned_data_2019 <- read.csv("C:/Users/Shanti/Desktop/202110/Sports Analytics/NBA-player-and-lineup-clustering/cleaned_data_2019.csv")

HeightWeightData <- HeightWeightData %>% 
  select(-c(X))
cleaned_data_2019 <- cleaned_data_2019 %>%
  select(-c(X))

length_name <- NA
names(HeightWeightData)[1] <- "player"
for (i in 1:nrow(HeightWeightData)){
  length_name <- nchar(HeightWeightData$player[i])-1
  HeightWeightData$player[i] <- substr(HeightWeightData$player[i], 1, length_name)
}

cleaned_data_2019_w_HW <- merge(cleaned_data_2019, HeightWeightData, by="player", all.x=TRUE)
write.csv(cleaned_data_2019_w_HW, "C:/Users/Shanti/Desktop/cleaned_data_2019_w_HW.csv")
cleaned_data_2019_w_HW <- read.csv("C:/Users/Shanti/Desktop/cleaned_data_2019_w_HW.csv")