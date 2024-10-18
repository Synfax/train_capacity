source('r/station_functions.R')
source('r/transform.R')

if(!dir.exists('station_zoning_info')) {
  dir.create('station_zoning_info')
}

if(!dir.exists('station_walkability_info')) {
  dir.create('station_walkability_info')
}


#or

walk(stations, .f = function(x) { map_amenities( get_near_osm(x, fromQuarto = F), x, fromQuarto = F) })

map_amenities(get_near_osm('Hawthorn', fromQuarto = F), 'Hawthorn', fromQuarto = F)

#set globals

# walkability = read_parquet('data/walkability_by_node.parquet')%>%
#   janitor::clean_names() %>%
#   st_set_geometry('geometry') %>%
#   st_set_crs('wgs84')

dwelling_data = readRDS(paste0('data/final_dwelling_data.Rdata')) %>%
  st_transform( 'wgs84')

station_rankings <- stations %>%
  map(~ as_tibble(as.list(return_information(.x, debug = T))), .progress = T) %>%
  list_rbind() %>%
  mutate(across(-station, as.numeric)) 

walkability_df = station_rankings %>% 
  select(c('station', 'distance', critical_variables$val))

saveRDS(walkability_df %>% select(-distance), 'r_objects/station_walkability.Rdata')

walkability_df = transform_walkability_scores(walkability_df)

station_rankings_ = station_rankings %>% 
  select(-c(critical_variables$val)) %>%
  left_join(walkability_df, by = 'station') %>%
  filter(!is.na(walkability_score))

  
  
#write_csv(station_rankings, 'data/csv_output.csv')

#split intwo two dfs

saveRDS(station_rankings_, 'r_objects/station_rankings.Rdata')

#station_rankings_ = readRDS('r_objects/station_rankings.Rdata') 

transformed_scores = transform_scores_xminxmax(station_rankings_)

saveRDS(transformed_scores, 'r_objects/transformed_scores.Rdata')


randomise_weights <- function() {
  
  n = 1000
  
  res_df <- data.frame()
  
  for(i in 1:n) {
    weights_random = c(
      "grz_nrz_pc" = runif(1,0,1),
      "capacity_delta" = 0,
      'average_peak_service_freq' = runif(1,0,1),
      'average_peak_service_cap' = runif(1,0,1),
      'walkability_score' = runif(1,0,1),
      'distance' = runif(1,0,1), 
      'n_bus_tram' = runif(1,0,1)
    )
    
    max_score <- sum(weights_random)
    
    wide_results <- transform_scores_xminxmax(station_rankings_, weights_random) %>%
      as.data.frame() %>%
      mutate(pc_score = score/max_score) %>%
      select(station, pc_score) %>%
      pivot_wider(values_from = 'pc_score', names_from = 'station')
    
    res_df = bind_rows(res_df, wide_results)
  }
  
  
}

#~~~~~~~~~~~~~~~~~~~~~~~
# 
# 
# for_the_age <- stations %>%
#   map(~ as_tibble(as.list(prepare_data_for_the_age(.x))), .progress = T) %>%
#   list_rbind() %>%
#   mutate(across(-station, as.numeric)) %>%
#   rename(Station_Name = station) %>%
#   left_join(locations, by = 'Station_Name') %>%
#   st_as_sf(coords = c('lng', 'lat')) %>%
#   st_set_crs('wgs84')
# 
# for_the_age = st_transform(for_the_age, 7844)
# 
# lgas = read_sf('shapefiles/lga_boundaries/LGA_2023_AUST_GDA2020.shp')
# 
# for_the_age = st_join(for_the_age, lgas)
# 
# for_the_age = for_the_age %>% select(c(Station_Name, total_heritage, residential, suitable, LGA_NAME23)) 
