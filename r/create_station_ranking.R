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


#set globals

walkability = read_parquet('data/walkability_by_node.parquet')%>%
  janitor::clean_names() %>%
  st_set_geometry('geometry') %>%
  st_set_crs('wgs84')

dwelling_data = readRDS(paste0('data/final_dwelling_data.Rdata')) %>%
  st_transform( 'wgs84')

station_rankings <- stations %>%
  map(~ as_tibble(as.list(return_information(.x))), .progress = T) %>%
  list_rbind() %>%
  mutate(across(-station, as.numeric)) %>%
  filter(!is.nan(walkability_score))

write_csv(station_rankings %>% filter(distance > 3000, distance < 25000), 'data/csv_output.csv')

saveRDS(station_rankings, 'r_objects/station_rankings.Rdata')

#station_rankings = readRDS('r_objects/station_rankings.Rdata') 

transformed_scores = transform_scores_xminxmax(station_rankings)

saveRDS(transformed_scores, 'r_objects/transformed_scores.Rdata')


#~~~~~~~~~~~~~~~~~~~~~~~


for_the_age <- stations %>%
  map(~ as_tibble(as.list(prepare_data_for_the_age(.x))), .progress = T) %>%
  list_rbind() %>%
  mutate(across(-station, as.numeric)) %>%
  rename(Station_Name = station) %>%
  left_join(locations, by = 'Station_Name') %>%
  st_as_sf(coords = c('lng', 'lat')) %>%
  st_set_crs('wgs84')

for_the_age = st_transform(for_the_age, 7844)

lgas = read_sf('shapefiles/lga_boundaries/LGA_2023_AUST_GDA2020.shp')

for_the_age = st_join(for_the_age, lgas)

for_the_age = for_the_age %>% select(c(Station_Name, total_heritage, residential, suitable, LGA_NAME23)) 
