mbappe_haaland <- norm_fitp90 %>%
  mutate(player_clean = stri_trans_general(player, 'Latin-ASCII') %>% tolower()) %>%
  filter(player_clean %in% c('kylian mbappe', 'erling haaland'))
mbappe_haaland %>% select(player)
mbappe_haaland.pivot <-mbappe_haaland %>%
  select(player_clean, 15:90) %>%
  pivot_longer(-player_clean, names_to = 'indicator', values_to = 'value_target') %>%
  separate(player_clean, into = c(NA,'closestto'), sep = " ")

mbappe.x <- mbappe_haaland$UMAP1[2]
mbappe.y <- mbappe_haaland$UMAP2[2]
haaland.x <- mbappe_haaland$UMAP1[1]
haaland.y <- mbappe_haaland$UMAP2[1]

closest.100s <- norm_fitp90 %>%
  mutate(player_clean = stri_trans_general(player, 'Latin-ASCII') %>% tolower()) %>%
  mutate(mbappe.side1 = UMAP1 - mbappe.x,
         mbappe.side2 = UMAP2 - mbappe.y,
         mbappe.distance = sqrt(mbappe.side1^2  + mbappe.side2^2)) %>%
  arrange(mbappe.distance) %>%
  mutate(mbappe.closest100 = ifelse(row_number() <= 101, 1, 0)) %>%
  mutate(haaland.side1 = UMAP1 - haaland.x,
         haaland.side2 = UMAP2 - haaland.y,
         haaland.distance = sqrt(haaland.side1^2  + haaland.side2^2)) %>%
  arrange(haaland.distance) %>%
  mutate(haaland.closest100 = ifelse(row_number() <= 101, 1, 0)) %>%
  filter(haaland.closest100 == 1|mbappe.closest100 == 1)

mh_vs_c100<- closest.100s %>%
  mutate(closestto = ifelse(haaland.closest100 == 1, 'haaland','mbappe')) %>%
  group_by(closestto) %>%
  summarize(across(15:90, ~mean(.x))) %>%
  pivot_longer(-closestto, names_to = 'indicator', values_to = 'sd_above_mean') %>%
  left_join(mbappe_haaland.pivot) %>%
  mutate(percentile_closest = 100 * pnorm(sd_above_mean),
         percentile_target  = 100 * pnorm(value_target)) %>%
  arrange(closestto, -sd_above_mean) %>%
  left_join(good_stat_names) %>%
  group_by(closestto)

ggplot(mh_vs_c100 %>% filter(closestto == 'mbappe', percentile_closest > 50)) +
  geom_col(aes(y = fct_reorder(ind_name, percentile_closest), x = percentile_closest, fill = category)) +
  geom_point(aes(y = ind_name, x = percentile_target),
             color = 'white', fill = 'darkgreen', shape = 21, size = 4) +
  geom_text(aes(y = ind_name, x = percentile_closest -0.5, label = round(percentile_closest, 0)),
            color = 'white', hjust = 1, fontface = 2) +
  labs(title = 'Kylian Mbappé and his Closest 100 Friends',
       subtitle = 'Percentile Rank across Role-Specific Statistical Categories: Mbappé (point) vs. Role Average (bar)',
       caption = 'Viz: Matt Barger (@MBarger13). Data: FBREF',
       y = NULL, x = NULL) +
  scale_y_discrete(position = 'right') +
  scale_x_continuous(position = 'top') +
  theme(legend.position = 'none',
        plot.caption = element_text(hjust = 0))
