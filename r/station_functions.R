
return_information <- function(station) {
  
  return(c(
    "station" = station,
    "grz_nrz_percentage" = find_zoning_suitability(station),
    "capacity_delta" = find_zoned_capacity(station),
    "heritage_pc" = find_heritage_pc(station),
    "average_peak_service_freq" = get_peak_service_frequency(station),
    "average_peak_service_cap" = get_peak_service_capacity(station),
    "walkability_score" = get_walkability_score(station)
  ))
  
}

radius = 1000
peak_morning = 7:10
peak_evening = 14:19



find_zoning_suitability <- function(station) {
 
  near_properties = get_near_properties(station)
  
  split_by_type = split_by_zone_type(near_properties)
  
  nrz_grz_area = split_by_type %>%
    filter(type_short %in% c('Neighbourhood residential', 'General residential')) %>%
    select(ttl_area) %>%
    sum()
  
  total_area = split_by_type %>% select(ttl_area) %>% sum()
  
  return(nrz_grz_area/total_area) 

}

get_near_properties <- function(station, fromQuarto = F) {
  
  prefix_dir = ifelse(fromQuarto, '../', '')
  
  #check if near properties are saved
  
  dir = paste0(prefix_dir, 'station_zoning_info/', station, '.Rdata')
  
  if(file.exists(dir) ) {
    
    near_properties = readRDS(dir) 
    
  } else {
    
    locations = readRDS(paste0(prefix_dir, 'r_objects/locations.Rdata'))
    
    station_location = locations %>%
      filter(Station_Name == station) %>%
      st_as_sf(coords = c('lng','lat'), crs = 'wgs84')
    
    buffer = st_buffer(station_location, dist = radius)
    
    dwelling_data = readRDS(paste0(prefix_dir, 'data/data.Rdata'))
    
    dwelling_data = st_transform(dwelling_data, 'wgs84')
    
    near_properties = st_within(dwelling_data, buffer, sparse = F)
    near_properties = dwelling_data %>%
      filter(near_properties)
    
    saveRDS(near_properties, dir)
    
  }
  
  return(near_properties)
  
}

split_by_zone_type <- function(near_properties) {
  
  near_properties %>%
    mutate(area = st_area(geom)) %>%
    st_drop_geometry() %>%
    group_by(type_short) %>%
    summarise(ttl_area = sum(area)) %>%
    return(.)
  
}

find_zoned_capacity <- function(station) {

  near_properties = get_near_properties(station)
  
  #TODO: FIXXXXXXXXXXXXXXXX, either both net or neither net.
  
  current_capacity = near_properties %>%
    as.data.frame() %>%
    select(dwellings_est) %>%
    sum()
  
  current_zoned_capacity = near_properties %>%
    as.data.frame() %>%
    select(buxton_yeilds_corrected_net) %>%
    sum()

  missing_middle_zoned_capacity = near_properties %>%
    as.data.frame() %>%
    select(mm_yield_net) %>%
    sum()
  
  delta <- missing_middle_zoned_capacity - current_zoned_capacity
  
  return(delta)
}

find_heritage_pc <- function(station) {
  
  near_properties = get_near_properties(station) %>%
    as.data.frame()
  
  percent_heritage <- near_properties %>%
    st_drop_geometry() %>%
    filter(zoning_permits_housing == 'Housing permitted', !feature_preventing_development) %>%
    count(heritage) %>%
    filter(heritage == TRUE) %>% select(n) /
    ( near_properties %>%
    st_drop_geometry() %>%
    filter(zoning_permits_housing == 'Housing permitted', !feature_preventing_development) %>% nrow() ) %>%
    as.numeric(.)
  
  percent_heritage = ifelse(is.na(percent_heritage$n[1]), 0, percent_heritage$n[1])
  
  return(percent_heritage)
  
}

get_peak_service_frequency <- function(station, fromQuarto = F) {
  
  prefix_dir = ifelse(fromQuarto, '../', '')
  
  service_frequencies = readRDS(paste0(prefix_dir, 'r_objects/service_frequencies.Rdata')) %>%
    filter(Station_Name == station)
  
  morning_to_flinders = service_frequencies %>%
    filter(Direction == 'Towards Flinders',
           as.numeric(hour_of_day) %in% peak_morning,
           Day_Type == 'Normal Weekday') %>%
    select(sph) %>% 
    unlist() %>%
    unname() %>% mean()
  
  evening_away_from_flinders = service_frequencies %>%
    filter(Direction == 'Away from Flinders',
           as.numeric(hour_of_day) %in% peak_evening,
           Day_Type == 'Normal Weekday') %>%
    select(sph) %>% 
    unlist() %>%
    unname() %>% mean()
  
  return(mean(morning_to_flinders, evening_away_from_flinders))
}

get_peak_service_capacity <- function(station, fromQuarto = F) {
  
  prefix_dir = ifelse(fromQuarto, '../', '')
  
  hourly_factors = readRDS(paste0(prefix_dir, 'r_objects/hourly_factors.Rdata'))
  
  avg_peak_morning_load = hourly_factors %>%
    filter(Station_Name == station, Direction == 'Towards Flinders') %>%
    filter(as.numeric(hour_of_day) %in% peak_morning) %>%
    select(capacity_factor) %>%
    unlist() %>%
    unname() %>%
    mean()
  
  avg_peak_evening_load = hourly_factors %>%
    filter(Station_Name == station, Direction == 'Away from Flinders') %>%
    filter(as.numeric(hour_of_day) %in% peak_evening) %>%
    select(capacity_factor) %>%
    unlist() %>%
    unname() %>%
    mean()
  
  return(mean(avg_peak_evening_load, avg_peak_morning_load))
}

get_walkability_score <- function(station) {
  
  #todo: fix
  critical_variables = c('restaurant', 'grocery', 'cafe', 'bar', 'school', 'child_care', 'park')
  
  walkability = get_near_osm(station)
  
  walkability = walkability %>%
    st_drop_geometry() %>%
    select(ends_with('500m')) %>%
    select(starts_with(critical_variables))
  
  average_walkability_score = walkability %>%
    summarise(across(dplyr::everything(), mean)) %>%
    mutate(avg = mean( c_across(dplyr::everything()))) %>%
    select(avg) %>%
    unlist() %>%
    as.vector()
  
  return(average_walkability_score)

}

get_near_osm <- function(station, fromQuarto = F) {
  
  prefix_dir = ifelse(fromQuarto, '../', '')
  
  file_dir = paste0(prefix_dir, 'station_walkability_info/', station, ".Rdata")
  
  if(file.exists(file_dir)) {
    
    readRDS(file_dir) %>% return()
    
  } else {
    
    locations = readRDS(paste0(prefix_dir, 'r_objects/locations.Rdata'))
    
    station_location = locations %>%
      filter(Station_Name == station) %>%
      st_as_sf(coords = c('lng','lat'), crs = 'wgs84')
    
    buffer = st_buffer(station_location, dist = radius)
    walkability = read_parquet('data/walkability_by_node.parquet')%>%
      janitor::clean_names() %>%
      st_set_geometry('geometry') %>%
      st_set_crs('wgs84')
    
    within_nodes <- st_within(walkability, buffer, sparse = F)
    
    walkability_near_station = walkability %>%
      filter(within_nodes)
    
    saveRDS(walkability_near_station, file_dir)
    
    return(walkability_near_station)
    
  }
  
}

get_distance_to_flinders <- function(station) {
  
  locations = readRDS(paste0(prefix_dir, 'r_objects/locations.Rdata'))
  
  station_location = locations %>%
    filter(Station_Name == station) %>%
    st_as_sf(coords = c('lng','lat'), crs = 'wgs84')
  
}

get_bus_and_tram_stops <- function(station) {
  
}

transform_scores <- function(station_rankings) {
  
  bad_columns = c('heritage_pc', 'average_peak_service_cap')
  
  transformed_station_rankings = station_rankings %>%
    mutate( across(-station, ~ as.numeric(.) )) %>%
    mutate( across(-station, .fns = function(x) { x / max(x) } )) %>%
    mutate(across(all_of(bad_columns), .fns = function(x) {-x} )) %>%
    rowwise() %>%
    mutate(score =sum(across(-station)),
           n_metrics = ncol( station_rankings %>% select(-station )),
           percent_score = score/n_metrics) 
  
  # print(transformed_station_rankings %>%
  #         select(station, score) %>%
  #         tibble())
  
  return(transformed_station_rankings)
}

