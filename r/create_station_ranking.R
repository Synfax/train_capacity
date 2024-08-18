source('r/station_functions.R')

stations = c('Brunswick', 'Carnegie', 'Merri')

if(!dir.exists('station_zoning_info')) {
  dir.create('station_zoning_info')
}

if(!dir.exists('station_walkability_info')) {
  dir.create('station_walkability_info')
}

stations = patronage_data %>%
  filter(Mode == 'Metro') %>%
  select(Station_Name) %>%
  distinct() %>%
  slice_sample(prop = 1) %>%
  unlist() %>% 
  as.vector()

#or

stations = list.files('station_zoning_info/') %>%
  str_replace(., ".Rdata", "")

##

station_rankings <- stations %>%
  map(~ as_tibble(as.list(return_information(.x))), .progress = T) %>%
  list_rbind()

saveRDS(station_rankings, 'r_objects/station_rankings.Rdata')

transformed_scores = transform_scores(station_rankings)

saveRDS(transformed_scores, 'r_objects/transformed_scores.Rdata')

