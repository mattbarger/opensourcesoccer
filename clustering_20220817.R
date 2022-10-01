
install.packages('embed')
install.packages('tidytext')

library(readr)
library(tidyverse)
library(tidytext)


sum <- read_csv("data/all_matchstats.csv")
def <- read_csv("data/202122_defensive.csv")
msc <- read_csv("data/202122_misc.csv")
pas <- read_csv("data/202122_passing.csv")
pos <- read_csv("data/202122_possession.csv")
pst <- read_csv("data/202122_passtypes.csv")

msc %>% names()

sum1 <- sum %>% select(League, Country, Season, Game_URL, Match_Date, Team, Player, Nation, Pos, Age, Min, 30:54)
def1 <- def %>% select(League, Country, Season, Game_URL, Match_Date, Team, Player, Nation, Pos, Age, Min, 30:52)
msc1 <- msc %>% select(League, Country, Season, Game_URL, Match_Date, Team, Player, Nation, Pos, Age, Min, 30:45)
pas1 <- pas %>% select(League, Country, Season, Game_URL, Match_Date, Team, Player, Nation, Pos, Age, Min, 30:50)
pos1 <- pos %>% select(League, Country, Season, Game_URL, Match_Date, Team, Player, Nation, Pos, Age, Min, 30:53)
pst1 <- pst %>% select(League, Country, Season, Game_URL, Match_Date, Team, Player, Nation, Pos, Age, Min, 30:54)


full_stat_readout <- sum1 %>% left_join(def1) %>% left_join(msc1) %>% left_join(pas1) %>% left_join(pos1) %>% left_join(pst1)


all_stats_per90 <- full_stat_readout %>%
  janitor::clean_names() %>%
  select(-contains("percent")) %>%
  filter(pos != "GK") %>%
  separate(age, into = c('age_yrs','age_days'), sep = "-", convert = T) %>%
  mutate(age_days = 365*age_yrs + age_days,
         birth_date = match_date - age_days) %>%
  select(1:9, birth_date, age_yrs, age_days, everything()) %>%
  group_by(player, team, country, season, nation, birth_date) %>%
  summarize_at(vars(min:blocks_outcomes), ~sum(., na.rm = T)) %>%
  mutate(m_90s = min/90) %>%
  select(1:7, m_90s, everything()) %>%
  mutate_at(vars(9:124), ~.x/m_90s)

asp90 <- all_stats_per_90 %>% filter(m_90s >= 6)


do_dimr_clust <- function(
    n,
    k,
    f_dimr = c('pca', 'umap'),
    f_clust = c('kmeans', 'mclust'),
    ...) {
  f_dimr <- match.arg(f_dimr)
  f_clust <- match.arg(f_clust)
  f_step <- ifelse(f_dimr == 'pca', recipes::step_pca, embed::step_umap)
  f_fit <- ifelse(f_clust == 'mclust', stats::kmeans, mclust::Mclust)

  data <-
    recipes::recipe(formula( ~ .), data = asp90) %>%
    recipes::update_role(player, team, country, season, nation, birth_date, new_role = "ID") %>%
    recipes::step_normalize(recipes::all_numeric_predictors()) %>%
    f_step(recipes::all_numeric_predictors(), num_comp = n) %>%
    recipes::prep() %>%
    recipes::juice() %>%
    select(where(is.numeric))
  fit <- f_fit(data, ...)
  broom::glance(fit)
}

metrics <- crossing(
  n = seq.int(2, 8),
  k = seq.int(2, 8),
  f_dimr = c('pca', 'umap'),
  f_clust = c('kmeans', 'mclust'))


metrics %>%
  mutate(metrics = pmap(
    list(n, k, f_dimr, f_clust),
    ~ do_dimr_clust(
      n = ..1,
      k = ..2,
      f_dimr = ..3,
      f_clust = ..4
    )
  ))
metrics

library(recipes)

asp90 <- all_stats_per90 %>% filter(m_90s >= 10)


p90_rec <- recipe(formula( ~ .), data = asp90) %>%
  update_role(player, team, country, season, nation, birth_date, new_role = "ID") %>%
  step_normalize(all_numeric_predictors()) %>%
  step_umap(all_numeric_predictors()) %>%
  prep()

p90set <- p90_rec %>%  juice()

p90_sd <- p90_rec$steps[[2]]$res$sdev
p90_explvar <- p90_sd^2/sum(p90_sd^2)
tidiedp90 <- tidy(p90_rec, 2)

tidiedp90 %>%
  filter(component %in% paste0("PC", 1:4)) %>%
  mutate(component = fct_inorder(component)) %>%
  ggplot(aes(value, terms, fill = terms)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~component, nrow = 1) +
  labs(y = NULL)

tidiedp90 %>%
  filter(component %in% paste0("PC", 1:4)) %>%
  group_by(component) %>%
  top_n(8, abs(value)) %>%
  ungroup() %>%
  mutate(terms = reorder_within(terms, abs(value), component)) %>%
  ggplot(aes(abs(value), terms, fill = value > 0)) +
  geom_col() +
  facet_wrap(~component, scales = "free_y") +
  scale_y_reordered() +
  labs(
    x = "Absolute value of contribution",
    y = NULL, fill = "Positive?"
  )

p90set %>% mutate(MLS_d = ifelse(country == "USA", 1, 0)) %>% ggplot(aes(x = PC1, y = PC2)) + geom_point(aes(color = factor(MLS_d))) + scale_color_manual(values = c('grey70', 'red'))

mclust::Mclust(p90set %>% select(UMAP1, UMAP2))

fit<- mclust::Mclust(p90set %>% select(UMAP1, UMAP2), G = 4)
augment(fit) %>% ggplot(aes(x = UMAP1, y = UMAP2, color = .class)) + geom_point()

fit <- augment(fit)
p90set %>% left_join(fit) %>% ggplot(aes(x = UMAP1, y = UMAP2, color = .class, size = .uncertainty)) + geom_point()


