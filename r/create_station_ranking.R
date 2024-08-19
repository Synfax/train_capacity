source('r/station_functions.R')

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

#set globals

walkability = read_parquet('data/walkability_by_node.parquet')%>%
  janitor::clean_names() %>%
  st_set_geometry('geometry') %>%
  st_set_crs('wgs84')

dwelling_data = readRDS(paste0(prefix_dir, 'data/final_dwelling_data.Rdata')) %>%
  st_transform( 'wgs84')

station_rankings <- stations %>%
  map(~ as_tibble(as.list(return_information(.x))), .progress = T) %>%
  list_rbind() %>%
  mutate(across(-station, as.numeric)) %>%
  filter(!is.nan(walkability_score))

#saveRDS(station_rankings, 'r_objects/station_rankings.Rdata')

station_rankings = readRDS('r_objects/station_rankings.Rdata') 

transformed_scores = transform_scores(station_rankings)

saveRDS(transformed_scores, 'r_objects/transformed_scores.Rdata')

