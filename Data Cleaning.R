library(tidyverse)
library(janitor)

per_100 <- read.csv("C:/Users/ryans/OneDrive/Desktop/Spring 2021/Sports Analytics/Basketball/NBA-player-and-lineup-clustering/2019_per_100.csv") %>%
  clean_names() %>%
  rename(made_fg = fg, attempted_fg = fga, percentage_fg = fg_2, 
         made_3p = x3p, attempted_3p = x3pa, percentage_3p = x3p_2,
         made_2p = x2p, attempted_2p = x2pa, percentage_2p = x2p_2,
         made_ft = ft, attempted_ft = fta, percentage_ft = ft_2)
advanced <- read.csv("C:/Users/ryans/OneDrive/Desktop/Spring 2021/Sports Analytics/Basketball/NBA-player-and-lineup-clustering/2019_advanced.csv") %>%
  clean_names() %>%
  rename(attempt_rate_3p = x3p_ar, ft_rate = f_tr)
shooting <- read.csv("C:/Users/ryans/OneDrive/Desktop/Spring 2021/Sports Analytics/Basketball/NBA-player-and-lineup-clustering/2019_shooting.csv") %>%
  clean_names() 


