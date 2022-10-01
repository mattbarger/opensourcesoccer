norm_fitp90 %>%
  mutate(across(15:90, ~pnorm(.x) * 100)) %>%
  select(player, player_clean, team, league, UMAP1, UMAP2, .class, total_pressures, def_3rd_pressures, mid_3rd_pressures, att_3rd_pressures) %>%
  pivot_longer(total_pressures:att_3rd_pressures, names_to = 'indicator', values_to = 'value') %>%
  left_join(good_stat_names) %>%
  ggplot() +
  geom_point(aes(x = UMAP1, y = -UMAP2, color = value, group = ind_name)) +
  ggrepel::geom_label_repel(data = . %>% filter(player_clean %in% c('ben mee',
                                                    "n'golo kante",
                                                    'nathaniel clyne',
                                                    'aymeric laporte',
                                                    'thomas muller',
                                                    'trent alexander-arnold',
                                                    'moussa sissoko',
                                                    'ciro immobile',
                                                    'salomon rondon')),
            aes(x = UMAP1, y = -UMAP2, label = player), size = 2.5, fontface = 2, alpha = 0.75,
            box.padding = 1,
            label.padding = unit(0.1,'lines'), seed = 5212) +
  scale_color_gradientn(colors = c('#01044F','#254869','#318774','#5A944A', '#EEFC51'),
                        guide = guide_colorbar(title = 'Percentile Rank',
                                               direction = 'horizontal',
                                               title.position = 'top',
                                               barheight = unit(0.2, 'cm'))) +
  facet_wrap(~factor(ind_name, levels = c('Pressures in Defensive Third','Pressures in Middle Third','Pressures in Attack Third','Total Pressures'))) +
  theme_void() +
  theme(plot.caption = element_text(hjust = c(0, 1)),
        legend.position = c(0.9, 0.05),
        legend.title = element_text(size = 9, face = 'bold'),
        legend.text = element_text(size = 7),
        strip.background = element_rect(fill = 'grey90', color = 'grey90'),
        strip.placement = 'inside',
        strip.text = element_text(face = 'bold', size = 10, margin = margin(b = 2, t = 2))) +
  labs(title = "SPRUCED: Mapped Pressure Indicators",
       subtitle = "Players Grouped by Proximity in Playstyles, Season '21/'22 (Europe), '21 (MLS)",
       caption = c('SPRUCED: Soccer Player-Role UMAP Cluster by Event Data.\nN = 2,394 outfield players across top 5 European Leagues and MLS (minimum 450 minutes played).\nPlayer points placed on aggregate per-90 attempts (not completions) of game event actions, projected across two-component UMAP.', 'Viz: Matt Barger (@MBarger13). Data: FBREF.'))


norm_fitp90 %>%
  filter(UMAP1 > 6) %>%
  arrange(-UMAP1) %>%
  View()
