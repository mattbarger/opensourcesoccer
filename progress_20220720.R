rm(list = ls())

library(tidyverse)
library(worldfootballR)
library(ggplot2)

all_leagues <- read_csv('https://raw.githubusercontent.com/mattbarger/opensourcesoccer/main/all_matchstats.csv')
all_passing <- read_csv('https://raw.githubusercontent.com/mattbarger/opensourcesoccer/main/202122_passing.csv')
all_possession <- read_csv('https://raw.githubusercontent.com/mattbarger/opensourcesoccer/main/202122_possession.csv')
all_defensive <- read_csv('https://raw.githubusercontent.com/mattbarger/opensourcesoccer/main/202122_defensive.csv')

possession_added <- all_leagues %>% left_join(all_possession)


possession_added <- all_possession %>% select(Game_URL, Team, Player, 30:53) %>% right_join(all_leagues %>% select(1:51, -Press, -Touches, -Int))
all_pos_def <- all_defensive %>% select(Game_URL, Team, Player, 30:52) %>% right_join(possession_added)

team_stats <- all_pos_def %>%
  filter(Country == "ENG") %>%
  group_by(Game_URL, Match_Date, Team, Home_Away, Home_Score, Away_Score) %>%
  summarize(PassCmp   = sum(Cmp_Passes, na.rm = T),
            PassAtt   = sum(Att_Passes, na.rm = T),
            Shots     = sum(Sh, na.rm = T),
            SoT       = sum(SoT, na.rm = T),
            xG        = sum(xG_Expected),
            press_suc = sum(Succ_Pressures, na.rm = T),
            press_hi  = sum(`Att 3rd_Pressures`, na.rm = T),
            press_md  = sum(`Mid 3rd_Pressures`, na.rm = T),
            press_lo  = sum(`Def 3rd_Pressures`, na.rm = T),
            touch_hi  = sum(`Att 3rd_Touches`, na.rm = T),
            touch_md  = sum(`Mid 3rd_Touches`, na.rm = T),
            touch_lo  = sum(`Def 3rd_Touches`, na.rm = T)
            ) %>%
  mutate(press_tot = press_hi + press_md + press_lo,
         touch_tot = touch_hi + touch_md + touch_lo)

team_for <- team_stats %>%
  pivot_wider(
    names_from = Home_Away,
    names_glue = "{Home_Away}_{.value}",
    values_from = c(Team, PassCmp, PassAtt, Shots, SoT, xG, press_suc, press_hi, press_md, press_lo, press_tot, touch_hi, touch_md, touch_lo, touch_tot)
  ) %>%
  select(Game_URL, Match_Date, Home_Team, Away_Team, everything())

team_with_allowed <- team_stats %>%
  mutate(Away_Home = ifelse(Home_Away == "Home","Away","Home")) %>%
  ungroup() %>%
  select(-Home_Away) %>%
  pivot_wider(
    names_from = Away_Home,
    names_glue = "{Away_Home}_{.value}_a",
    values_from = c(Team, PassCmp, PassAtt, Shots, SoT, xG, press_suc, press_hi, press_md, press_lo, press_tot, touch_hi, touch_md, touch_lo, touch_tot)
  ) %>%
  select(Game_URL, 7:34) %>%
  right_join(team_for) %>%
  select(Game_URL, 30:62, everything())

expanded_team_stats <- team_with_allowed %>%
  mutate(Home_Score_a = Away_Score, Away_Score_a = Home_Score) %>%
  pivot_longer(-(1:4), names_to = 'indicator', values_to = 'value') %>%
  separate(indicator, into = c('Home_Away','indicator'), sep = "_", extra = 'merge') %>%
  mutate(Team = ifelse(Home_Away == "Home", Home_Team, Away_Team), 
         indicator = str_replace(indicator, 'Score', 'Goals')) %>%
  pivot_wider(values_from = value, names_from = indicator) %>%
  mutate(result = ifelse(Goals == Goals_a, "Draw", ifelse(Goals < Goals_a, "Loss","Win")),
         opponent = ifelse(Home_Team == Team, Away_Team, Home_Team))

tot <- expanded_team_stats %>%
  filter(Team == "Tottenham Hotspur") %>%
  select( Match_Date, opponent, Home_Away, result, xG, xG_a, Goals, Goals_a, Shots, Shots_a, press_suc, press_tot, touch_tot_a, press_hi, press_md, press_lo, touch_hi_a, touch_md_a, touch_lo_a) %>%
  arrange(Match_Date) %>% print(n = 34)

  
expanded_team_stats %>%
  group_by(Team) %>%
  summarize(avg_press = mean(press_tot),
            avg_press_succ = mean(press_suc)) %>% arrange(-avg_press) %>% print(n = 27)

expanded_team_stats %>% summarize(mean(touch_hi_a), mean(Shots_a))
