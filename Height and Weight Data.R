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

write.csv(HeightWeightData, "C:/Users/Shanti/Desktop/HeightWeightData1.csv")
