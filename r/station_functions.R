
return_information <- function(station) {
  
  return(c(
    "grz_nrz_percentage" = find_zoning_suitability(station)
  ))
  
}

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
  radius = 1000
  
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

find_zoned_capacity <- function() {
  
  
  
}
