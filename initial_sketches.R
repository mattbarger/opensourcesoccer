library(readr)
all_matchstats <- read_csv("https://raw.githubusercontent.com/mattbarger/opensourcesoccer/main/all_matchstats.csv")

ams_1 <- all_matchstats %>% 
  replace_na(list(Away_Score = 2))

gamestats <- ams_1 %>%
  filter(!(str_detect(League, "Major") & !str_detect(Matchweek, "Regular"))) %>%
  group_by(Game_URL, Match_Date, League, Home_Away, Team, Home_Score, Away_Score) %>%
  select(Sh, Cmp_Passes, Att_Passes, xG_Expected) %>%
  summarize(PassCmp = sum(Cmp_Passes, na.rm = T),
            PassAtt = sum(Att_Passes, na.rm = T),
            shots = sum(Sh, na.rm = T),
            xG = sum(xG_Expected, na.rm = T)) %>%
  mutate(PassPct = PassCmp/PassAtt) %>%
  ungroup() %>%
  pivot_wider(
    names_from = Home_Away,
    names_glue = "{Home_Away}_{.value}",
    values_from = c(Team, PassPct, PassCmp, shots, xG, PassAtt)
  ) %>%
  mutate(Home_Possession = 100*Home_PassCmp/(Home_PassCmp + Away_PassCmp),
         Away_Possession = 100-Home_Possession)

gamestats_by_team <- gamestats %>%
  mutate(Home_PassCmp_a = Away_PassCmp, Away_PassCmp_a = Home_PassCmp,
         Home_PassAtt_a = Away_PassAtt, Away_PassAtt_a = Home_PassAtt,
         Home_shots_a = Away_shots, Away_shots_a = Home_shots,
         Home_xG_a = Away_xG, Away_xG_a = Home_xG,
         Home_Score_a = Away_Score, Away_Score_a = Home_Score,
         Home_result = ifelse(Home_Score > Away_Score, 1, ifelse(Home_Score == Away_Score, 0.5, 0)),
         Away_result = ifelse(Home_Score < Away_Score, 1, ifelse(Home_Score == Away_Score, 0.5, 0))) %>%
  select(Game_URL, Home_Team, Away_Team, everything()) %>%
  pivot_longer(-(1:5), names_to = 'indicator', values_to = 'value') %>%
  separate(indicator, into = c("Home_Away","indicator"), sep = "_", extra = "merge") %>%
  mutate(Team = ifelse(Home_Away == "Home", Home_Team, Away_Team),
         indicator = ifelse(str_detect(indicator, "Score") == T, str_replace(indicator, "Score", "goals"), indicator)) %>%
  pivot_wider(values_from = value, names_from = indicator) %>%
  mutate(result = ifelse(result == 1, "Win", ifelse(result == 0, "Loss", "Draw")) %>% factor(., levels = c("Loss","Draw","Win")),
         all_PassCmp = PassCmp + PassCmp_a, xG_diff = xG - xG_a, shot_diff = shots - shots_a, goal_diff = goals - goals_a)

mean_passes <- gamestats_by_team %>% 
  filter(League == "Premier League") %>%
  summarize(mean = mean(PassCmp)) %>%
  unlist() %>% as.vector()

sd_lines <- gamestats_by_team %>% 
  filter(League == "Premier League") %>%
  summarize(mean = mean(PassCmp),
            sd = sd(PassCmp)) %>%
  mutate(sd_very_low = mean - 1.96*sd,
         sd_low = mean - sd,
         sd_high = mean + sd,
         sd_very_high = mean + 1.96*sd) %>% select(-(1:2)) %>% unlist() %>% as.vector()

library(ggplot2)

league_table <- gamestats_by_team %>%
  group_by(League, Team) %>%
  summarize(GP = n(),
            W = sum(result == "Win"),
            D = sum(result == "Draw"),
            L = sum(result == "Loss"),
            GF = sum(goals),
            GA = sum(goals_a),
            GD = sum(goal_diff)) %>%
  mutate(Pts = W*3 + D) %>%
  arrange(-Pts, -GD, -GF) %>%
  mutate(rank = row_number())

ams_1 %>% filter(!(str_detect(League, "Major") & str_detect(Matchweek, "Regular"))) %>% View()

gamestats_by_team %>%
  filter(League == "Premier League") %>%
  left_join(league_table) %>%
  ggplot() +
  geom_dotplot(aes(y = PassCmp, x = reorder(factor(Team), -rank)), fill = '#85CB7C', color = '#85CB7C', alpha = 0.5, binaxis = "y", binwidth = 5, stackdir = 'center', dotsize = 1.2) +
  geom_hline(yintercept = mean_passes, color = "white", size = 1.5) +
  geom_hline(yintercept = sd_lines, color = "white", linetype = "longdash", size = 1) +
  geom_point(data = . %>% group_by(Team) %>% summarize(med = median(PassCmp)), aes(y = med, x = factor(Team)), color = "#962222", size = 6) +
  geom_point(data = . %>% group_by(Team) %>% summarize(med = median(PassCmp)), aes(y = med, x = factor(Team)), color = '#2F9EB6', size = 4) +
  coord_flip() +
  labs(title = "A League Above: Manchester City Completed Passes at 95th Percentile for Half the Season",
       subtitle = "Passes Completed per Match, 2021-2022 Season",
       y = element_blank(),
       x = element_blank(),
       caption = "Blue dot represents median. Dashed lines represent standard deviations away from the PL average 387 passes per match.\n Analysis and Graphics by Matt Barger. github:mattbarger/opensourcesoccer. Data: FBREF.") +
  theme(plot.background = element_rect("#E7F5E5"),
        panel.background = element_rect("#D6EED3"),
        panel.grid = element_blank(),
        strip.background = element_rect("#37752F"),
        strip.text = element_text(color = "#E7F5E5"),
        legend.position = c(0.9, 0.1),
        legend.direction = 'vertical',
        legend.background = element_blank(),
        legend.title = element_blank(),
        legend.key = element_blank(),
        plot.title.position = 'plot',
        plot.caption.position = 'plot') +
  scale_fill_manual(values = c('#962222', '#ACAC2D', '#2F9EB6'))

gamestats_by_team %>%
  left_join(league_table) %>%
  group_by(League) %>%
  filter(Team == "Leeds United") %>%
  filter(str_detect(League, "Premier")) %>%
  mutate(passdiff = PassCmp - PassCmp_a,
         shotdiff = shots - shots_a,
         manager = factor(ifelse(Match_Date <= as.Date.character("2022-02-27"), "Marcelo Bielsa","Jesse Marsch"))) %>%
  ggplot() +
  geom_point(data = gamestats_by_team %>%
               filter(str_detect(League, "Premier")) %>%
               select(-Team) %>%
               mutate(passdiff = PassCmp - PassCmp_a,
                      shotdiff = shots - shots_a),
             aes(x = passdiff, y = shotdiff), color = "light gray") +
  geom_point(aes(x = passdiff, y = shotdiff, color = result)) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  facet_wrap(vars(manager))

leeds_manager_join <- gamestats_by_team %>%
  left_join(league_table) %>%
  group_by(League) %>%
  filter(Team == "Leeds United") %>%
  filter(str_detect(League, "Premier")) %>%
  mutate(passdiff = PassCmp - PassCmp_a,
         shotdiff = shots - shots_a,
         manager = factor(ifelse(Match_Date <= as.Date.character("2022-02-27"), "Marcelo Bielsa","Jesse Marsch"))) %>%
  select(Game_URL, manager)

ams_1 %>%
  left_join(leeds_manager_join) %>%
  filter(Team == "Leeds United") %>%
  filter(!is.na(manager)) %>%
  group_by(manager, Player) %>%
  summarize(Min = sum(Min, na.rm = T)) %>% 
  mutate(Min = ifelse(manager == "Jesse Marsch", Min/12, Min/26)) %>%
  group_by(manager) %>%
  pivot_wider(names_from = manager, values_from = Min, values_fill = 0) %>% 
  arrange(-`Marcelo Bielsa`) %>%
  mutate(diff = `Jesse Marsch` - `Marcelo Bielsa`) %>% arrange(diff) %>%
  adorn_totals('row')

library(janitor)
sd_shots <- gamestats_by_team %>%
  filter(str_detect(League, "Premier")) %>%
  mutate(shotdiff = shots - shots_a) %>%
  summarize(mean = mean(shotdiff),
            sd = sd(shotdiff))
  summarize()
