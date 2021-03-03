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

data_2019 <- per_100 %>%
  inner_join(advanced, group_by = c("player", "tm")) %>%
  inner_join(shooting, group_by = c("player", "tm"))
