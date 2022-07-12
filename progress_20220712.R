library(worldfootballR)
library(tidyverse)


mls22 <- get_match_urls('USA','M',2022,'1st')

mls22 <- get_advanced_match_stats(mls22, 'summary','player')


mls22 <- bind_rows(mls22, 
                   'https://fbref.com/en/matches/42767543/CF-Montreal-Vancouver-Whitecaps-FC-April-16-2022-Major-League-Soccer' %>% 
                     get_advanced_match_stats("summary", 'player')) 

mls22 <- mls22 %>% mutate(Match_Date = as.Date.character(Match_Date), 
                          Home_Yellow_Cards = as.integer(Home_Yellow_Cards),
                          Home_Red_Cards = as.integer(Home_Red_Cards),
                          Away_Yellow_Cards = as.integer(Away_Yellow_Cards),
                          Away_Red_Cards = as.integer(Away_Red_Cards))

mls21 <- read_csv('https://raw.githubusercontent.com/mattbarger/opensourcesoccer/main/all_matchstats.csv') %>% filter(Country == "USA")
  
mls <- mls21 %>% 
  filter(str_detect(Matchweek, 'Regular')) %>%
  bind_rows(mls22)

mls_per_match <- mls %>%
  group_by(Game_URL, Season, Match_Date, League, Home_Away, Team, Home_Score, Away_Score) %>%
  summarize(PassCmp = sum(Cmp_Passes, na.rm = T),
            PassAtt = sum(Att_Passes, na.rm = T),
            shots = sum(Sh, na.rm = T),
            SoT = sum(SoT, na.rm = T),
            xG = sum(xG_Expected, na.rm = T)) %>%
  mutate(PassPct = PassCmp/PassAtt) %>%
  ungroup() %>%
  pivot_wider(
    names_from = Home_Away,
    names_glue = "{Home_Away}_{.value}",
    values_from = c(Team, PassPct, PassCmp, shots, SoT, xG, PassAtt)
  ) %>%
  mutate(Home_Possession = 100*Home_PassCmp/(Home_PassCmp + Away_PassCmp),
         Away_Possession = 100-Home_Possession)

mls_per_team <- mls_per_match %>%
  mutate(Home_PassCmp_a = Away_PassCmp, Away_PassCmp_a = Home_PassCmp,
         Home_PassAtt_a = Away_PassAtt, Away_PassAtt_a = Home_PassAtt,
         Home_shots_a = Away_shots, Away_shots_a = Home_shots,
         Home_SoT_a = Away_SoT, Away_SoT_a = Home_SoT,
         Home_xG_a = Away_xG, Away_xG_a = Home_xG,
         Home_Score_a = Away_Score, Away_Score_a = Home_Score,
         Home_result = ifelse(Home_Score > Away_Score, 1, ifelse(Home_Score == Away_Score, 0.5, 0)),
         Away_result = ifelse(Home_Score < Away_Score, 1, ifelse(Home_Score == Away_Score, 0.5, 0))) %>%
  select(Game_URL, Season, Home_Team, Away_Team, everything()) %>%
  pivot_longer(-(1:6), names_to = 'indicator', values_to = 'value') %>%
  separate(indicator, into = c("Home_Away","indicator"), sep = "_", extra = "merge") %>%
  mutate(Team = ifelse(Home_Away == "Home", Home_Team, Away_Team),
         indicator = ifelse(str_detect(indicator, "Score") == T, str_replace(indicator, "Score", "goals"), indicator)) %>%
  pivot_wider(values_from = value, names_from = indicator) %>%
  mutate(result = ifelse(result == 1, "Win", ifelse(result == 0, "Loss", "Draw")) %>% factor(., levels = c("Loss","Draw","Win")),
         all_PassCmp = PassCmp + PassCmp_a, 
         xG_diff = xG - xG_a, 
         shot_diff = shots - shots_a, 
         SoT_diff = SoT - SoT_a,
         goal_diff = goals - goals_a, 
         pass_diff = PassCmp - PassCmp_a)

mean_passes <- mls_per_team %>%
  ungroup() %>%
  summarize(mean = mean(pass_diff), sd = sd(pass_diff)) %>%
  mutate(lower_limit = mean - 2*sd, lower_mid = mean - sd, upper_mid = mean + sd, upper_limit = mean + 2*sd) %>%
  select(-sd) %>%
  unlist() %>% as.vector()

mean_xG <- mls_per_team %>%
  ungroup() %>%
  summarize(mean = mean(xG_diff), sd = sd(xG_diff)) %>%
  mutate(lower_limit = mean - 2*sd, lower_mid = mean - sd, upper_mid = mean + sd, upper_limit = mean + 2*sd) %>%
  select(-sd) %>%
  unlist() %>% as.vector()


mls_per_team %>%
ggplot() +
  geom_point(aes(x = pass_diff, y = xG_diff), color = '#C2FFCF') +
  geom_point(data = . %>% filter(Team == "D.C. United"),
             aes(x = pass_diff, y = xG_diff, color = result)) +
  geom_hline(yintercept = 0, color = '#EDFFF1', size = 2, alpha = 0.7) +
  geom_vline(xintercept = 0, color = '#EDFFF1', size = 2, alpha = 0.7) +
  facet_wrap(vars(Season)) +
  theme(plot.background = element_rect('#EDFFF1'),
        panel.background = element_rect('#D6FFDF'),
        legend.position = 'none',
        panel.grid = element_blank()) +
  scale_color_manual(values = c('#BA3F3F','#BAB03F','#3FBA5A'))

mls_per_team %>%
  filter(Home_Away == "Home") %>%
  mutate(xG_desc = ifelse(xG_diff > 0, 'Positive', 'Negative'),
         pass_desc = ifelse(pass_diff > 0, 'Positive','Negative')) %>%
  group_by(Season, xG_desc, pass_desc) %>%
  summarize(GP = n(),
            W = sum(result == 'Win', na.rm = T),
            D = sum(result == 'Draw', na.rm = T),
            L = sum(result == 'Loss', na.rm = T)) %>%
  mutate(Pts = W*3 + D,
         PPG = Pts/GP) %>%
  select(pass_desc, GP, Pts, PPG) %>% group_by(Season) %>% mutate(total_gp = sum(GP), GP_pct = GP/sum(GP))
shots_ci %>% pivot_longer(-Season, names_to = 'indicator', values_to = 'value')

mls_per_team %>% filter(Team == "D.C. United") %>% 
  select(Season, Match_Date,shots, shots_a) %>%
  group_by(Season) %>%
  arrange(Match_Date) %>%
  mutate(match_number = row_number()) %>%
  pivot_longer(c(shots, shots_a), names_to = 'indicator', values_to = 'value') %>%
  ggplot() +
  geom_rect(data = shots_ci, xmin = 0, xmax = 35, aes(ymin = min_75ci, ymax = max_75ci), fill = '#D6FFDF') +
  geom_hline(data = shots_ci %>% pivot_longer(-Season, names_to = 'indicator', values_to = 'value'), 
             aes(yintercept = value), color = '#D8D8D8', linetype = "dotted", size = 1) + 
  geom_hline(data = shots_ci %>% pivot_longer(-Season, names_to = 'indicator', values_to = 'value') %>% filter(indicator == 'mean'),
             aes(yintercept = value), color = '#D8D8D8', size = 1.5) + 
  geom_linerange(data = . %>% 
                   pivot_wider(names_from = indicator, values_from = value) %>%
                   mutate(shot_diff = ifelse(shots - shots_a > 0, 'shots','shots_a')),
                 aes(x = match_number, ymin = shots, ymax = shots_a, color = shot_diff), size = 1) +
  geom_point(aes(x = match_number, y = value, fill = indicator), size = 2, shape = 21, color = '#D8D8D8') +
  scale_y_continuous(limits = c(0, 30)) +
  scale_fill_manual(values = c('#231F20','#DF4545')) +
  scale_color_manual(values = c('#231F20','#DF4545')) +
  facet_grid(. ~ Season, scales = "free_x", space = "free") +
  labs(title = 'D.C. United: Where Did All The Shots Go?',
       subtitles = 'Shots Created (black) vs Shots Allowed (red) per Game, 2021 & 2022 Seasons',
       x = element_blank(),
       y = element_blank(),
       caption = 'Gray line represents league average shots for each season. Shaded area represents 75% confidence interval for each season.\ngithub: mattbarger/opensourcesoccer. Data: FBREF') +
  theme(plot.background = element_rect('#EDFFF1'),
        panel.background = element_rect('#EDFFF1'),
        plot.title.position = 'plot',
        panel.grid = element_blank(),
        panel.border = element_rect(color = '#D8D8D8', fill = NA, size = 1),
        strip.background = element_rect(fill = '#D6FFDF'),
        legend.position = 'none')


mls_per_team %>% filter(Team == "D.C. United") %>% 
  select(Season, Match_Date,xG, xG_a) %>%
  group_by(Season) %>%
  arrange(Match_Date) %>%
  mutate(match_number = row_number()) %>%
  pivot_longer(c(xG, xG_a), names_to = 'indicator', values_to = 'value') %>%
  ggplot() +
  geom_rect(data = xG_ci, xmin = 0, xmax = 35, aes(ymin = min_75ci, ymax = max_75ci), fill = '#D6FFDF') +
  geom_hline(data = xG_ci %>% pivot_longer(-Season, names_to = 'indicator', values_to = 'value'), 
             aes(yintercept = value), color = '#D8D8D8', linetype = "dotted", size = 1) + 
  geom_hline(data = xG_ci %>% pivot_longer(-Season, names_to = 'indicator', values_to = 'value') %>% filter(indicator == 'mean'),
             aes(yintercept = value), color = '#D8D8D8', size = 1.5) + 
  geom_linerange(data = . %>% 
                   pivot_wider(names_from = indicator, values_from = value) %>%
                   mutate(shot_diff = ifelse(xG - xG_a > 0, 'xG','xG_a')),
                 aes(x = match_number, ymin = xG, ymax = xG_a, color = shot_diff), size = 1) +
  geom_point(aes(x = match_number, y = value, fill = indicator), size = 2, shape = 21, color = '#D8D8D8') +
  scale_y_continuous(limits = c(0, 5)) +
  scale_fill_manual(values = c('#231F20','#DF4545')) +
  scale_color_manual(values = c('#231F20','#DF4545')) +
  facet_grid(. ~ Season, scales = "free_x", space = "free") +
  labs(title = 'D.C. United: What Happened to the Chance Quality?',
       subtitles = 'xG Created (black) vs xG Allowed (red) per Game, 2021 & 2022 Seasons',
       x = element_blank(),
       y = element_blank(),
       caption = 'Gray line represents league average xG for each season. Shaded area represents 75% confidence interval for each season.\ngithub: mattbarger/opensourcesoccer. Data: FBREF') +
  theme(plot.background = element_rect('#EDFFF1'),
        panel.background = element_rect('#EDFFF1'),
        plot.title.position = 'plot',
        panel.grid = element_blank(),
        panel.border = element_rect(color = '#D8D8D8', fill = NA, size = 1),
        strip.background = element_rect(fill = '#D6FFDF'),
        legend.position = 'none')
library(extrafont)


shots_ci  <- mls_per_team %>% 
  group_by(Season) %>%
  summarize(mean = mean(shots), sd = sd(shots)) %>% 
  mutate(min_95ci = mean - 1.96 * sd, 
         min_75ci = mean - 1.15 * sd, 
         max_75ci = mean + 1.15 * sd, 
         max_95ci = mean + 1.96 * sd) %>% select(-sd)

xG_ci  <- mls_per_team %>% 
  group_by(Season) %>%
  summarize(mean = mean(xG), sd = sd(xG)) %>% 
  mutate(min_95ci = mean - 1.96 * sd, 
         min_75ci = mean - 1.15 * sd, 
         max_75ci = mean + 1.15 * sd, 
         max_95ci = mean + 1.96 * sd) %>% select(-sd)
