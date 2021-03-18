library(rvest)
library(purrr)
library(tidyverse)

##### Per 100 #####

url_2019_per_100 <- "https://www.basketball-reference.com/leagues/NBA_2019_per_poss.html"

data_per_100 <- url_2019_per_100 %>% 
  read_html() %>% 
  html_nodes("table") %>%
  purrr::pluck(1) %>%
  html_table()
  
data_per_100 <- data_per_100[,-30] %>% #Column 30 was empty
  filter(Rk != "Rk") #Some rows just re list the variable names

write.csv(data_per_100, "C:/Users/ryans/OneDrive/Desktop/Spring 2021/Sports Analytics/Basketball/NBA-player-and-lineup-clustering/2019_per_100.csv")


##### Advanced #####

url_2019_advanced <- "https://www.basketball-reference.com/leagues/NBA_2019_advanced.html"

data_advanced <- url_2019_advanced %>% 
  read_html() %>% 
  html_nodes("table") %>%
  purrr::pluck(1) %>%
  html_table()

data_advanced <- data_advanced[,-c(20,25)] %>% #Column 20 and 25 was empty
  filter(Rk != "Rk") #Some rows just re list the variable names

write.csv(data_advanced, "C:/Users/ryans/OneDrive/Desktop/Spring 2021/Sports Analytics/Basketball/NBA-player-and-lineup-clustering/2019_advanced.csv")


##### Shooting #####

#Formatting was very strange decided to download and clean data manually

# url_2019_shooting <- "https://www.basketball-reference.com/leagues/NBA_2019_shooting.html"
# 
# data_shooting <- url_2019_shooting %>% 
#   read_html() %>% 
#   html_nodes("table") %>%
#   purrr::pluck(1) %>%
#   html_table()
# 
# head(data_shooting)
# 
# data_shooting <- data_shooting[,-c(1,10,23,26,29,32)] %>% #Column 30 was empty
#   filter(Rk != "Rk") #Some rows just re list the variable names
# 
# write.csv(data_shooting, "C:/Users/ryans/OneDrive/Desktop/Spring 2021/Sports Analytics/Basketball/NBA-player-and-lineup-clustering/2019_shooting")

#trying to find height and weight

player_data_url <- "https://www.nba.com/players"

height_weight_data <- player_data_url %>% 
     read_html() %>% 
     html_nodes("table") %>%
     purrr::pluck() %>%
     html_table()
hw2 <- height_weight_data[[1]]
