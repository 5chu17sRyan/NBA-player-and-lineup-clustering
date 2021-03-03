library(tidyverse)
library(janitor)

# Reading datasets and cleaning names
per_100 <- read.csv("C:/Users/ryans/OneDrive/Desktop/Spring 2021/Sports Analytics/Basketball/NBA-player-and-lineup-clustering/2019_per_100.csv") %>%
  clean_names() %>%
  rename(made_fg = fg, attempted_fg = fga, percentage_fg = fg_2, 
         made_3p = x3p, attempted_3p = x3pa, percentage_3p = x3p_2,
         made_2p = x2p, attempted_2p = x2pa, percentage_2p = x2p_2,
         made_ft = ft, attempted_ft = fta, percentage_ft = ft_2) %>%
  select(-c(x))
advanced <- read.csv("C:/Users/ryans/OneDrive/Desktop/Spring 2021/Sports Analytics/Basketball/NBA-player-and-lineup-clustering/2019_advanced.csv") %>%
  clean_names() %>%
  rename(attempt_rate_3p = x3p_ar, ft_rate = f_tr, orb_percentage = orb,
         drb_percentage = drb, trb_percentage = trb, ast_percentage = ast,
         stl_percentage = stl, blk_percentage = blk, tov_percentage = tov) %>%
  select(-c(x, rk, pos, age, g, mp))
shooting <- read.csv("C:/Users/ryans/OneDrive/Desktop/Spring 2021/Sports Analytics/Basketball/NBA-player-and-lineup-clustering/2019_shooting.csv") %>%
  clean_names() %>%
  select(-c(rk,pos,age,g,mp,fg))

# Joining per_100, advanced, and shooting data for the 201 season
data_2019 <- per_100 %>%
  inner_join(advanced, group_by = c("player", "tm")) %>%
  inner_join(shooting, group_by = c("player", "tm"))

# Removing non-unique player entries - Basketball-reference will give multiple player entries for players who were traded
multiple_team_player_TOT <- data_2019[data_2019$tm == "TOT",]
multiple_team_player_names <- multiple_team_player_TOT$player

unique_player_indices <- NULL

num_NBA_players <- nrow(data_2019)

#For each player
for(i in 1:nrow(data_2019)){
  #Check if they are a unique player
  is_non_unique_player <- 0
  for(j in 1:length(multiple_team_player_names)){
    if(data_2019[i,]$player == multiple_team_player_names[j]){
      is_non_unique_player <- 1
    }
  }
  #Record if they are unique or not
  if(is_non_unique_player && data_2019[i,]$tm != "TOT"){
    unique_player_indices[i] <- FALSE
  } else {
    unique_player_indices[i] <- TRUE
  }
}

# Select unique players and their full (TOT) season data
data_2019 <- data_2019[unique_player_indices,]

# Select variables of interest
cleaned_data_2019 <- data_2019 %>%
  mutate(attempts_0_3 = attempted_fg * x_of_fga_by_distance_0_3) %>%
  mutate(attempts_3_10 = attempted_fg* x_of_fga_by_distance_3_10) %>%
  mutate(attempts_0_10 = attempts_0_3 + attempts_0_10) %>% #Scoring attempts within 10ft per 100 possessions
  mutate(made_0_3 = fg_by_distance_0_3*attempts_0_3) %>%
  mutate(made_3_10 = fg_by_distance_3_10*attempts_3_10) %>%
  mutate(made_0_10 = made_0_3 + made_0_10) %>%
  mutate(percent_0_10 = made_0_10 / attempts_0_10) %>% #FG% within 10ft
  mutate(attempts_dunk = attempted_fg * dunks_fga) %>% #Dunk attempts per 100 possessions
  mutate(attempts_10_16 = attempted_fg * x_of_fga_by_distance_10_16) %>%
  mutate(attempts_16_3p = attempted_fg* x_of_fga_by_distance_16_3p) %>%
  mutate(attempts_10_3p = attempts_0_3 + attempts_0_10) %>% #Scoring attempts between 10ft and 3P line per 100 possessions
  mutate(made_10_16 = fg_by_distance_10_16*attempts_10_16) %>%
  mutate(made_16_3p = fg_by_distance_16_3p*attempts_16_3p) %>%
  mutate(made_10_3p = made_10_16 + made_16_3p) %>%
  mutate(percent_10_3p = made_10_3p / attempts_10_3p) %>% #FG% between 10ft and 3P line
  select(player, age, attempts_0_10, percent_0_10, attempts_dunk, x_of_fg_ast_d_2p)
  
  