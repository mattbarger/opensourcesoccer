usmnt <- norm_fitp90 %>%
  mutate(player_clean = stri_trans_general(player, 'Latin-ASCII') %>% tolower()) %>%
  filter(player_clean %in% c('walker zimmerman','erik palmer-brown', 'chris richards', 'miles robinson', 'aaron long','james sands',
                             'sergino dest', 'john brooks','joe scally','sam vines',
                             'kellyn acosta','tyler adams','weston mckennie','yunus musah',
                             'paul arriola', 'timothy weah','christian pulisic',
                             'daryl dike','ricardo pepi','jesus ferreira','josh sargent', 'gianluca busio'))

usmnt %>% select(player_clean, nation, team) %>% print(n = 24)

usmnt.pivot <- usmnt %>%
  select(player_clean, 15:90) %>%
  pivot_longer(-player_clean, names_to = 'indicator', values_to = 'value_target') %>%
  rename(closest_to = player_clean)

usmnt_c100 <- tibble()

norm_fitp90 <- norm_fitp90 %>%
  mutate(player_clean = stri_trans_general(player, 'Latin-ASCII') %>% tolower())


for(i in 1:24) {
  player_tmp <- usmnt$player_clean[i]
  player_tmp.x <- usmnt$UMAP1[i]
  player_tmp.y <- usmnt$UMAP2[i]
  player_closest100 <- norm_fitp90 %>%
    mutate(player_side1 = UMAP1 - player_tmp.x,
           player_side2 = UMAP2 - player_tmp.y,
           player_distance = sqrt(player_side1^2 + player_side2^2)) %>%
    arrange(player_distance) %>%
    mutate(closest_to = player_tmp,
           closest_rank = row_number()) %>%
    filter(closest_rank <= 101)

  usmnt_c100 <- bind_rows(usmnt_c100, player_closest100)

  rm(player_tmp, player_tmp.x, player_tmp.y, player_closest100)

}

usmnt_c100 %>%
  filter(closest_to == "weston mckennie") %>%
  select(player, team, player_distance) %>% print(n = 50)

ggplot(norm_fitp90, aes(x = UMAP1, y = -UMAP2, color = .class)) +
  geom_point(alpha = 0.2) +
  geom_point(data = usmnt,
             aes(x = UMAP1, y = -UMAP2), size = 4) +
  ggrepel::geom_text_repel(data = usmnt,
                            aes(x = UMAP1, y = -UMAP2, label = player),
                            size = 3, fontface = 2, color = 'navyblue',
                            max.overlaps = Inf) +
  scale_color_manual(values = c("#D55E00", "#E69F00", "#009E73",
                                "#56B4E9", "#CC79A7")) +
  theme_void() +
  theme(legend.position = 'none',
        plot.caption = element_text(hjust = c(0, 1)))+
  labs(title = "Five Stripes Served Two Ways: Atlanta United and NY Red Bulls",
       subtitle = "Players Grouped by Proximity in Playstyles, Season '21/'22",
       caption = c('N = 2,394 outfield players across top 5 European Leagues and MLS (minimum 450 minutes played).\nPlayer points placed on aggregate per-90 attempts (not completions) of game event actions, projected across two-component UMAP.', 'Viz: Matt Barger (@MBarger13). Data: FBREF.'),
       color = 'Expected Assists per 90 (Percentile)')

all_stats <- usmnt_c100 %>%
  filter(closest_to == 'sergino dest') %>%
  pivot_longer(15:90,
               names_to = 'indicator',
               values_to = 'sd_above_mean') %>%
  left_join(good_stat_names)

usmnt_vs_c100 <- usmnt_c100 %>%
  group_by(closest_to) %>%
  summarize(across(15:90, ~mean(.x))) %>%
  pivot_longer(-closest_to, names_to = 'indicator', values_to = 'sd_above_mean') %>%
  left_join(usmnt.pivot) %>%
  mutate(percentile_closest = 100 * pnorm(sd_above_mean),
         percentile_target  = 100 * pnorm(value_target)) %>%
  arrange(closest_to, -sd_above_mean) %>%
  left_join(good_stat_names) %>%
  mutate(category_pretty = cat_pretty[category])
  group_by(closest_to)

cat_pretty = c('Defensive' = 'DEFENDING',
               'Attacking' = 'ATTACKING',
               'Possessive' = 'HOLDING',
               'Passing'  = 'ADVANCING')

ggplot(usmnt_vs_c100 %>% filter(closest_to == 'weston mckennie', percentile_target >= 54)) +
  geom_col(aes(y = fct_reorder(ind_name, percentile_target), x = percentile_closest, fill = category)) +
  geom_text(aes(y = ind_name, x = percentile_closest -0.5, label = round(percentile_closest, 0)),
            color = 'white', hjust = 1, fontface = 2) +
  geom_text(aes(y = ind_name, x = 2, label = category_pretty), hjust = 0, color = 'white',  fontface = 2, size = 3) +
  geom_point(aes(y = ind_name, x = percentile_target),
             color = 'white', fill = 'navyblue', shape = 23, size = 4) +
  labs(title = 'Weston McKennie and his Closest 100 Friends',
       subtitle = 'Percentile Rank across Role-Specific Statistical Categories: McKennie (point) vs. Role Average (bar)',
       caption = c('N = 2,394 outfield players across top 5 European Leagues and MLS (minimum 450 minutes played).','Viz: Matt Barger (@MBarger13). Data: FBREF'),
       y = NULL, x = NULL) +
  scale_fill_manual(values = c('Possessive' = "#B99C33",'Attacking' = "#02401B", 'Passing' = "#81A665", 'Defensive' = "#972D15"))+
  scale_y_discrete(position = 'right') +
  scale_x_continuous(position = 'top', limits = c(0, 100), breaks = c(0, 25, seq(50, 100, 10)), minor_breaks = seq(0,100,10)) +
  theme(legend.position = 'none',
        plot.caption.position = 'plot',
        plot.caption = element_text(hjust = c(0,1)),
        panel.background  = element_rect(color = 'black'),
        panel.grid.major = element_line(color = 'grey50'),
        panel.spacing = unit(c(1,1,1,1), 'cm'))


ggplot(norm_fitp90) +
  geom_point(aes(x = UMAP1, y = -UMAP2, color = .class), alpha = 0.4) +
  geom_point(data = norm_fitp90 %>% filter(player_clean == 'weston mckennie'),
               aes(x = UMAP1, y = -UMAP2, color = .class), size = 6) +
  ggrepel::geom_text_repel(data = mck_c100 %>% filter(player_clean == 'weston mckennie'),
             aes(x = UMAP1, y = -UMAP2, label = player), size = 5, fontface = 2) +

  geom_point(data = mck_c100,
             aes(x = UMAP1, y = -UMAP2, color = .class), size = 4, alpha = 0.4) +

  ggrepel::geom_text_repel(data = mck_c100 %>% filter(player_clean %in% c('joelinton',
                                                                          'enock mwepu',
                                                                          'paxton pomykal',
                                                                          'alex iwobi',
                                                                          'eryk williamson',
                                                                          'tsubasa endoh',
                                                                          'dele alli',
                                                                          'georginio wijnaldum',
                                                                          'jeffrey schlupp',
                                                                          'caden clark',
                                                                          'helder costa',
                                                                          'henrikh mkhitaryan',
                                                                          'mateusz klich')),
                           aes(x = UMAP1, y = -UMAP2, label = player), size = 4) +
  ggrepel::geom_text_repel(data = mck_c100 %>% filter(player_clean == 'yunus musah'),
                           aes(x = UMAP1, y = -UMAP2, label = player), size = 5, fontface = 2) +
  scale_x_continuous(limits = c(2,4)) +
  scale_y_continuous(limits = c(0.5,2.5)) +
  theme_void() +
  theme(legend.position = 'none',
        plot.caption = element_text(hjust = c(0, 1)))+
  labs(title = "Weston McKennie and his 100 Closest Friends",
       subtitle = "Players Grouped by Proximity in Playstyles, Season '21/'22",
       caption = c('N = 2,394 outfield players across top 5 European Leagues and MLS (minimum 450 minutes played).\nPlayer points placed on aggregate per-90 attempts (not completions) of game event actions, projected across two-component UMAP.', 'Viz: Matt Barger (@MBarger13). Data: FBREF.'),
       )

ggplot(norm_fitp90) +
  geom_point(aes(x = UMAP1, y = -UMAP2, color = .class), alpha = 0.4) +
  geom_point(data = mus_c100,
             aes(x = UMAP1, y = -UMAP2, color = .class), size = 4, alpha = 0.3) +

  ggrepel::geom_text_repel(data = mus_c100 %>% filter(player_clean %in% c('dejan kulusevski',
                                                                          'paxton pomykal',
                                                                          'alex iwobi',
                                                                          'nikola vlasic',
                                                                          'andros townsend',
                                                                          'aaron lennon',
                                                                          'fredy montero',
                                                                          'anthony elanga',
                                                                          'caden clark',
                                                                          'helder costa',
                                                                          'daniel gazdag',
                                                                          'jordan ayew',
                                                                          'antoine griezmann')),
                           aes(x = UMAP1, y = -UMAP2, label = player), size = 4, box.padding = 0.2) +
  ggrepel::geom_text_repel(data = mus_c100 %>% filter(player_clean == 'weston mckennie'),
                           aes(x = UMAP1, y = -UMAP2, label = player), size = 5, fontface = 2) +
  geom_point(data = norm_fitp90 %>% filter(player_clean == 'yunus musah'),
             aes(x = UMAP1, y = -UMAP2, color = .class), size = 6) +
  ggrepel::geom_text_repel(data = mck_c100 %>% filter(player_clean == 'yunus musah'),
                           aes(x = UMAP1, y = -UMAP2, label = player), size = 5, fontface = 2) +

  scale_x_continuous(limits = c(2.5, 4.5)) +
  scale_y_continuous(limits = c(0,2.5)) +
  theme_void() +
  theme(legend.position = 'none',
        plot.caption = element_text(hjust = c(0, 1)))+
  labs(title = "Yunus Musah and his 100 Closest Friends",
       subtitle = "Players Grouped by Proximity in Playstyles, Season '21/'22",
       caption = c('N = 2,394 outfield players across top 5 European Leagues and MLS (minimum 450 minutes played).\nPlayer points placed on aggregate per-90 attempts (not completions) of game event actions, projected across two-component UMAP.', 'Viz: Matt Barger (@MBarger13). Data: FBREF.'),
  )

install.packages('extrafont')

extrafont::choose_font()
library(extrafont)
font_import(pattern = 'Karla')
ggplot(norm_fitp90) +
  geom_point(aes(x = UMAP1, y = -UMAP2, color = .class), alpha = 0.2) +
  ggrepel::geom_label_repel(data = usmnt %>% filter(!player_clean %in% c('ricardo pepi','gianluca busio')|
                                                      player_clean =='ricardo pepi' & team == 'Augsburg'|
                                                      player_clean =='gianluca busio' & team == 'Venezia'),
                  aes(x = UMAP1, y = -UMAP2, label = player),
                  size = 3, fontface = 2, color = 'navyblue', fill = 'white',
                  box.padding = 0.07, max.overlaps =  Inf) +
  theme_void() +
  scale_color_manual(values = c("#D55E00", "#E69F00", "#009E73",
                                "#56B4E9", "#CC79A7")) +
  theme(legend.position = 'none',
        plot.caption = element_text(hjust = c(0, 1)))+
  labs(title = "Selected USMNT Players, SPRUCED by Role",
       subtitle = "Players Grouped by Proximity in Playstyles, Season '21/'22 (Europe), '21 (MLS)",
       caption = c('SPRUCED: Soccer Player-Role UMAP Cluster by Event Data.\nN = 2,394 outfield players across top 5 European Leagues and MLS (minimum 450 minutes played).\nPlayer points placed on aggregate per-90 attempts (not completions) of game event actions, projected across two-component UMAP.', 'Viz: Matt Barger (@MBarger13). Data: FBREF.'),
  )


norm_fitp90 %>%
  mutate(across(15:90, ~pnorm(.x) * 100)) %>%
ggplot() +
  geom_point(aes(x = UMAP1, y = -UMAP2, color = total_pressures)) +
  ggrepel::geom_label_repel(data = usmnt %>% filter(!player_clean %in% c('tyler adams','weston mckennie','yunus musah',
                                                                         'ricardo pepi','gianluca busio')|
                                                      player_clean =='ricardo pepi' & team == 'Augsburg'|
                                                      player_clean =='gianluca busio' & team == 'Venezia'),
                            aes(x = UMAP1, y = -UMAP2, label = player), alpha = 0.4,
                            size = 3, fontface = 2, color = 'navyblue', fill = 'white',
                            box.padding = 0.07, max.overlaps =  Inf) +
  ggrepel::geom_label_repel(data = usmnt %>% filter(player_clean %in% c('tyler adams','weston mckennie','yunus musah')),
                            aes(x = UMAP1, y = -UMAP2, label = player),
                            size = 4, fontface = 2, color = 'navyblue', fill = 'white',
                            box.padding = 0.7, max.overlaps =  Inf) +

  scale_color_viridis() +
  theme_void() +
  theme(legend.position = c(0.7, 0.3),
        legend.direction = 'horizontal',
        plot.caption = element_text(hjust = c(0, 1)))+
  labs(title = "Selected USMNT Players, SPRUCED by Role",
       subtitle = "Players Grouped by Proximity in Playstyles, Season '21/'22 (Europe), '21 (MLS)",
       caption = c('SPRUCED: Soccer Player-Role UMAP Cluster by Event Data.\nN = 2,394 outfield players across top 5 European Leagues and MLS (minimum 450 minutes played).\nPlayer points placed on aggregate per-90 attempts (not completions) of game event actions, projected across two-component UMAP.', 'Viz: Matt Barger (@MBarger13). Data: FBREF.')
       , color = 'Total Pressures (percentile)'
  )





ggplot(norm_fitp90) +
  geom_point(aes(x = UMAP1, y = -UMAP2, color = .class), alpha = 0.3) +
  geom_point(data = norm_fitp90 %>% filter(player_clean == 'tyler adams'),
             aes(x = UMAP1, y = -UMAP2, color = .class), size = 6) +

  geom_point(data = ada_c100,
             aes(x = UMAP1, y = -UMAP2, color = .class), size = 4, alpha = 0.4) +
  ggrepel::geom_text_repel(data = ada_c100 %>% filter(player_clean == 'tyler adams'),
                           aes(x = UMAP1, y = -UMAP2, label = player), size = 5, fontface = 2) +

  ggrepel::geom_text_repel(data = ada_c100 %>% filter(player_clean %in% c('casemiro',
                                                                          'rodrigo bentancur',
                                                                          'pape gueye',
                                                                          'lucas torreira',
                                                                          'manuel locatelli',
                                                                          'fabinho',
                                                                          'franck kessie',
                                                                          'yves bissouma',
                                                                          'thomas partey',
                                                                          'koke',
                                                                          'axel witsel',
                                                                          'fred',
                                                                          'declan rice',
                                                                          'naby keita',
                                                                          'jorginho',
                                                                          'youri tielemans')),
                           aes(x = UMAP1, y = -UMAP2, label = player), size = 4, box.padding = 0.2) +
  scale_x_continuous(limits = c(-1, 1)) +
  scale_y_continuous(limits = c(0.75, 2.25)) +
  theme_void() +
  theme(legend.position = 'none',
        plot.caption = element_text(hjust = c(0, 1)))+
  labs(title = "Tyler Adams and his Closest 100 Friends",
       subtitle = "Players Grouped by Proximity in Playstyles, Season '21/'22",
       caption = c('N = 2,394 outfield players across top 5 European Leagues and MLS (minimum 450 minutes played).\nPlayer points placed on aggregate per-90 attempts (not completions) of game event actions, projected across two-component UMAP.', 'Viz: Matt Barger (@MBarger13). Data: FBREF.'),
  )


ada_c100 <- usmnt_c100 %>%
  filter(closest_to == "tyler adams")
ada_c100 %>% select(player, team, player_distance) %>% print(n = 101)


mck_c100 <- usmnt_c100 %>%
  filter(closest_to == "weston mckennie")

mus_c100 <- usmnt_c100 %>%
  filter(closest_to == "yunus musah")
mus_c100 %>% select(player, team, player_distance) %>% print(n = 1010)
closest.100s %>% filter(dest.closest100 == 1) %>% arrange(dest.distance) %>% select(player, team) %>% print(n = 101)
