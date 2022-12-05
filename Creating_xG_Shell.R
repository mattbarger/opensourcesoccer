library(tidyverse)
library(ggplot2)
library(worldfootballR)


### GET ALL WORLD CUP URLs
wc_2022_urls <- fb_match_urls(country = "", gender = "M", season_end_year = 2022, tier = "", non_dom_league_url = "https://fbref.com/en/comps/1/history/World-Cup-Seasons")
rm(wc_2018_urls)

wc_shots_22 <- fb_match_shooting(wc_2022_urls)

wc_results_22 <- fb_match_results(country = "", gender = "M", season_end_year = 2022, tier = "", non_dom_league_url = "https://fbref.com/en/comps/1/history/World-Cup-Seasons")

wc_resclean <- wc_results_22 %>%
  mutate(
    Home = str_sub(Home, end = -4) %>% trimws(),
    Away = str_sub(Away, start = 4) %>% trimws()
  ) %>%
  select(Date, Home, Away, HomeGoals, AwayGoals, MatchURL)

away_res <- wc_resclean %>%
  select(Date, Squad = Away, Opponent = Home, GF = AwayGoals, GA = HomeGoals, MatchURL)

wc_res_by_team <- wc_resclean %>%
  select(Date, Squad = Home, Opponent = Away, GF = HomeGoals, GA = AwayGoals,MatchURL) %>%
  bind_rows(away_res) 
  


wc_shots_clean <- wc_shots_22 %>%
  mutate(xG = as.numeric(xG),
         Date = as.Date.character(Date),
         Squad = str_sub(Squad, start = 4) %>% trimws()) %>%
  left_join(wc_res_by_team, by = c('Date','Squad')) %>%
  select(Date, Squad, Opponent, everything()) %>%
  select(-Home_Away) %>%
  separate(Player, into = c("Player","Penalty"), sep = " \\(") %>%
  mutate(Penalty = ifelse(is.na(Penalty),0,1))

wc_shots_clean %>%
  filter(Penalty == 0) %>%
  arrange(-xG) %>%
  mutate(top20percent = ifelse(row_number() <= 220, 1, 0)) %>%
  group_by(Squad, Opponent) %>%
  summarize(shots = n(),
            bigchances = sum(top20percent)) %>%
  arrange(-bigchances) %>% View()
