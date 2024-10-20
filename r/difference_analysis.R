source('existing_upzoned_stations.R')

transformed_scores <- readRDS('r_objects/transformed_scores.Rdata')

transformed_scores %>%
  as.data.frame() %>%
  arrange(desc(score)) %>%
  slice_head(n = 50) %>%
  mutate(rank = row_number()) %>%
  filter(!(station %in% existing_upzoned_stations)) %>%
  filter(!(station %in% other_existing_precincts)) %>%
  select(station, rank)

transformed_scores %>%
  as.data.frame() %>%
  arrange(desc(score)) %>%
  slice_head(n = 50) %>%
  mutate(rank = row_number() ) %>%
  filter((station %in% c(existing_upzoned_stations, other_existing_precincts))) %>%
  select(station, rank)
