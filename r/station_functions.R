

return_information <- function(station) {
  
  return(c(
    "station" = station,
    "grz_nrz_pc" = find_zoning_suitability(station),
    "capacity_delta" = find_zoned_capacity(station),
    #"heritage_pc" = find_heritage_pc(station),
    "average_peak_service_freq" = get_peak_service_frequency(station),
    "average_peak_service_cap" = get_line_peak_capacity_at_closest_station(station),
    "walkability_score" = get_walkability_score(station),
    "distance" = get_distance_to_flinders(station),
    "n_bus_tram" = get_bus_and_tram_stops(station)
  ))
  
}


find_zoning_suitability <- function(station) {
 
  near_properties = get_near_properties(station) %>%
    mutate(area = st_area(geom))
  
  # split_by_type = split_by_zone_type(near_properties)
  # 
  # nrz_grz_area_no_heritage = split_by_type %>%
  #   filter(type_short %in% c('Neighbourhood residential', 'General residential')) %>%
  #   filter(heritage == F) %>%
  #   select(ttl_area) %>%
  #   sum()
  
  nrz_grz_area_no_heritage = near_properties %>%
    filter(type_short %in% c('Neighbourhood residential', 'General residential')) %>%
    filter(heritage == F) %>%
    st_drop_geometry() %>%
    select(area) %>%
    unlist() %>%
    sum()

  total_area = near_properties %>%
    st_drop_geometry() %>%
    select(area) %>%
    unlist() %>%
    sum()
  
  return(nrz_grz_area_no_heritage/total_area) 

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
    sum(., na.rm = T)
  
  current_zoned_capacity = near_properties %>%
    as.data.frame() %>%
    select(buxton_yeilds_corrected_net) %>%
    sum(., na.rm = T)

  missing_middle_zoned_capacity = near_properties %>%
    as.data.frame() %>%
    select(mm_yield_net) %>%
    sum(.,na.rm = T)
  
  delta <- missing_middle_zoned_capacity - current_zoned_capacity
  
  return(delta)
}

find_heritage_pc <- function(station) {
  
  near_properties = get_near_properties(station) %>%
    as.data.frame()
  
  percent_heritage <- near_properties %>%
    st_drop_geometry() %>%
    filter(zone_short %in% c('Neighbourhood residential', 'General residential', 'Residential growth'), !feature_preventing_development, dwellings_est <= 1) %>%
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
  
  return(mean( c(morning_to_flinders, evening_away_from_flinders) ))
}

get_peak_service_capacity <- function(station, fromQuarto = F) {
  
  
  prefix_dir = ifelse(fromQuarto, '../', '')
  
  hourly_factors = readRDS(paste0(prefix_dir, 'r_objects/hourly_factors.Rdata'))
  
  avg_peak_morning_load = hourly_factors %>%
    filter(Station_Name == station, Direction == 'Towards Flinders') %>%
    filter(as.numeric(hour_of_day) %in% peak_morning) %>%
    select(avg_patronage) %>%
    unlist() %>%
    unname() %>%
    mean()

  avg_peak_evening_load = hourly_factors %>%
    filter(Station_Name == station, Direction == 'Away from Flinders') %>%
    filter(as.numeric(hour_of_day) %in% peak_evening) %>%
    select(avg_patronage) %>%
    unlist() %>%
    unname() %>%
    mean()
  
  # hourly_factors %>%
  #   filter(Station_Name == station, Day_Type == 'Normal Weekday') %>%
  #   mutate(peak_type = ifelse(hour_of_day %in% peak_morning, 'm', NA),
  #          peak_type = ifelse(hour_of_day %in% peak_evening, 'e', peak_type)) %>%
  #   group_by(Line_Name, Direction, peak_type) %>%
  #   summarise(avg_capacity = mean(capacity_factor)) %>%
  #   filter(!is.na(peak_type)) %>%
  #   filter( !(peak_type == "e" & Direction == 'Towards Flinders') ) %>%
  #   filter( !(peak_type == "m" & Direction == 'Away from Flinders') ) %>%
  #   pull(avg_capacity) %>%
  #   mean() %>%
  #   return()
  
  return(mean( c(avg_peak_evening_load, avg_peak_morning_load)))
  
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
    
    within_nodes <- st_within(walkability, buffer, sparse = F)
    
    walkability_near_station = walkability %>%
      filter(within_nodes)
    
    saveRDS(walkability_near_station, file_dir)
    
    return(walkability_near_station)
    
  }
  
}

get_distance_to_flinders <- function(station, fromQuarto = F) {
  
  prefix_dir = ifelse(fromQuarto, '../', '')
  
  #use station chainage
  
  locations = readRDS(paste0(prefix_dir, 'r_objects/locations.Rdata'))

  distance = locations %>%
    filter(Station_Name == station) %>%
    mutate(cbd_lon = 144.9671,
           cbd_lat = -37.8183) %>%
    rowwise() %>%
    mutate(flinders_dist = distHaversine(c(lng, lat),
                                    c(cbd_lon, cbd_lat))) %>%
    select(flinders_dist) %>%
    unlist() %>%
    as.vector()

  return(distance)
  
  # chainage_info = readRDS('r_objects/station_chainages.Rdata') 
  # 
  # city_loop_stations = c('Flagstaff', 'Parliament', 'Melbourne Central', 'Flinders Street', 'Southern Cross')
  # 
  # lines = patronage_data %>%
  #   filter(Station_Name == station) %>%
  #   select(Line_Name) %>%
  #   distinct() %>%
  #   pull()
  # 
  # #what?
  #   
  # city_start_chainage = chainage_info %>%
  #   filter(Line_Name %in% lines) %>%
  #   filter(Station_Name %in% city_loop_stations) %>%
  #   group_by(Line_Name) %>%
  #   summarise(city_start_chainage = max(Station_Chainage))
  #   
  # avg_distance = chainage_info %>%
  #   filter(Line_Name %in% lines) %>%
  #   filter(Station_Name == station) %>%
  #   left_join(city_start_chainage, by = 'Line_Name') %>%
  #   mutate(distance = Station_Chainage - city_start_chainage) %>%
  #   pull(distance) %>%
  #   mean()
  
  return(avg_distance)
  
}

get_bus_and_tram_stops <- function(station, fromQuarto = F) {
  
  
  prefix_dir = ifelse(fromQuarto, '../', '')
 
  bus_stops = read_sf('shapefiles/ptv/PTV_METRO_BUS_STOP.shp')
  tram_stops = read_sf('shapefiles/ptv/PTV_METRO_TRAM_STOP.shp')
  
  bus_frequencies = readRDS(paste0(prefix_dir, 'r_objects/bus_stop_frequencies.Rdata')) %>%
    rename(STOP_ID = "stop_id") %>%
    mutate(STOP_ID = as.character(STOP_ID)) %>%
    select(peak_type, n, STOP_ID) %>%
    group_by(STOP_ID) %>%
    summarise(avg_sph = mean(n))
  
  tram_frequencies = readRDS(paste0(prefix_dir, 'r_objects/tram_stop_frequencies.Rdata')) %>%
    rename(STOP_ID = "stop_id") %>%
    mutate(STOP_ID = as.character(STOP_ID)) %>%
    select(peak_type, n, STOP_ID) %>%
    group_by(STOP_ID) %>%
    summarise(avg_sph = mean(n))
  
  locations = readRDS(paste0(prefix_dir, 'r_objects/locations.Rdata'))
  
  station_location = locations %>%
    filter(Station_Name == station) %>%
    st_as_sf(coords = c('lng','lat'), crs = 'wgs84')
  
  buffer = st_buffer(station_location, dist = radius) %>%
    st_transform(crs = 7844)
  
  near_bus_vector <- st_within(bus_stops, buffer, sparse = F)
  
  # old summation method
  # bus_stops_near_station = bus_stops %>%
  #   filter(near_bus_vector) %>%
  #   left_join(bus_frequencies, by = 'STOP_ID') %>%
  #   pull(avg_sph) %>%
  #   sum(., na.rm = T)
  
  bus_stops_near_station = bus_stops %>%
    filter(near_bus_vector) %>%
    left_join(bus_frequencies, by = 'STOP_ID') %>%
    group_by(ROUTEUSSP) %>%
    summarise(avg_sph = mean(avg_sph, na.rm=T)) %>%
    filter(!str_detect(ROUTEUSSP, ',')) %>%
    st_drop_geometry() %>%
    pull(avg_sph) %>%
    sum(., na.rm = T)
  
  near_tram_vector <- st_within(tram_stops, buffer, sparse = F)
  
  tram_stops_near_station = tram_stops %>%
    filter(near_tram_vector) %>%
    left_join(tram_frequencies, by = 'STOP_ID')  %>%
    group_by(ROUTEUSSP) %>%
    summarise(avg_sph = mean(avg_sph, na.rm=T)) %>%
    filter(!str_detect(ROUTEUSSP, ',')) %>%
    st_drop_geometry() %>%
    pull(avg_sph) %>%
    sum(., na.rm = T)
  
  #one method is we could sum them.

  tram_weight = 2
  bus_weight = 1
  
  #its much more important to be near trams than busses. given their infrequency
  
  total_number = (tram_stops_near_station*tram_weight) + (bus_stops_near_station * bus_weight)

  return(total_number)     
}


get_number_of_stops_to_flinders <- function(station) {
  
  # doesn't work for Ltd express.
  
  avg_number_of_stops = readRDS('r_objects/station_chainages.Rdata') %>%
    filter(Station_Name == station) %>%
    pull(rn) %>% mean()
  
  return(avg_number_of_stops)
      
}

get_line_peak_capacity_at_closest_station <- function(station, fromQuarto = F) {
  
  
  prefix_dir = ifelse(fromQuarto, '../', '')
  
  
  hourly_factors = readRDS(paste0(prefix_dir, 'r_objects/hourly_factors.Rdata'))
  
  lines = get_lines_serving_station(station)
  
  #check if target_station can ever return multiple results without taking the first
  
  target_station = get_target_stations(station)
  
  hourly_factors %>%
    ungroup() %>%
    as.data.frame() %>%
    mutate(hour_of_day = as.numeric(hour_of_day)) %>%
    filter(Station_Name == target_station, Day_Type == 'Normal Weekday') %>%
    filter(Line_Name %in% lines) %>%
    mutate(peak_type = ifelse(hour_of_day %in% peak_morning, 'm', NA),
           peak_type = ifelse(hour_of_day %in% peak_evening, 'e', peak_type)) %>%
    group_by(Line_Name, Direction, peak_type) %>%
    summarise(avg_patronage = mean(avg_patronage)) %>%
    filter(!is.na(peak_type)) %>%
    filter( !(peak_type == "e" & Direction == 'Towards Flinders') ) %>%
    filter( !(peak_type == "m" & Direction == 'Away from Flinders') ) %>%
    pull(avg_patronage) %>%
    mean() %>%
    return()
}

get_lines_serving_station <- function(station) {
  
  lines = patronage_data %>%
    filter(Station_Name == station) %>%
    select(Line_Name) %>%
    distinct() %>%
    pull() 
  
  return(lines)
  
}

get_target_stations <- function(station, returnFirst = T) {
  
  chainage_info = readRDS('r_objects/station_chainages.Rdata') 
  
  city_loop_stations = c('Flagstaff', 'Parliament', 'Melbourne Central', 'Flinders Street', 'Southern Cross')
  
  lines = get_lines_serving_station(station)
  
  targets <- chainage_info %>%
    filter(Line_Name %in% lines) %>%
    filter(!(Station_Name %in% city_loop_stations)) %>%
    group_by(Line_Name) %>%
    arrange(Station_Chainage) %>%
    slice_head(n=1) %>%
    ungroup() %>%
    pull(Station_Name) 
  
  if(returnFirst) {
    return(targets %>% first())
  } else {
    return(targets)
  }
}

prepare_data_for_the_age = function(station) {
  near_properties = get_near_properties(station) 
  
  heritage_pc = (near_properties %>%
    st_drop_geometry() %>%
    count(heritage) %>%
    filter(heritage) %>%
    pull(n)) / nrow(near_properties)
  
  heritage_pc = ifelse(is_empty(heritage_pc), 0, heritage_pc)
  
  resi = near_properties %>%
    st_drop_geometry() %>%
    filter(zone_short %in% c('General residential', 'Residential growth ', 'Neighbourhood residential'))  
  
  residential_pc = (resi %>% count(heritage) %>% filter(heritage) %>% pull(n)) / nrow(resi)
  residential_pc = ifelse(is_empty(residential_pc), 0, residential_pc)
  
  suitable = resi %>%
    filter(dwellings_est <= 1) %>%
    filter(!feature_preventing_development)
  
  suitable_pc = (suitable %>% count(heritage) %>% filter(heritage) %>% pull(n)) / nrow(suitable)
  suitable_pc = ifelse(is_empty(suitable_pc), 0, suitable_pc)
  
  return(c( station = station, total_heritage = heritage_pc, residential = residential_pc, suitable = suitable_pc ))
}
