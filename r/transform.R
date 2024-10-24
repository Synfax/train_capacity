

# transform_scores <- function(station_rankings) {
#   
#   bad_columns = c('heritage_pc', 'average_peak_service_cap', 'distance')
#   
#   basic_transform = station_rankings %>%
#     mutate( across(-station, ~ as.numeric(.) )) %>%
#     filter(distance < 25000, distance > 3000) %>%
#     mutate( across(-station, .fns = function(x) { x / max(x) } )) %>%
#     mutate(across(any_of(bad_columns), .fns = function(x) {-x} )) %>%
#     rowwise() %>%
#     mutate(score =sum(across(-station)))
#   
#   # print(transformed_station_rankings %>%
#   #         select(station, score) %>%
#   #         tibble())
#   
#   return(basic_transform)
# }
# 
# transform_scores_normal <- function(station_rankings) {
#   
#   bad_columns = c('heritage_pc', 'average_peak_service_cap', 'distance')
#   
#   transformed_station_rankings = station_rankings %>%
#     mutate( across(-station, ~ as.numeric(.) )) %>%
#     filter(distance < 25000, distance > 3000) %>%
#     mutate( across(-station, .fns = function(x) { (x - mean(x, na.rm= T)) / var(x) } )) %>%
#     mutate(across(any_of(bad_columns), .fns = function(x) {-x} )) %>%
#     rowwise() %>%
#     mutate(score =sum(across(-station)))
#   
#   # print(transformed_station_rankings %>%
#   #         select(station, score) %>%
#   #         tibble())
#   
#   return(transformed_station_rankings)
# }
# 

filter_stations <- function(dataframe, name_var = 'station') {
  dataframe %>%
    filter(!(get(name_var) %in% city_loop_stations)) %>%
    filter(!(get(name_var) %in% other_important_stations)) %>%
    filter(!(get(name_var) %in% event_stations)) %>%
    filter(distance < 25000) %>%
    return()
}

remove_existing_stations <- function(dataframe, name_var = 'station') {
  dataframe %>%
    filter( !(get(name_var) %in% c(existing_upzoned_stations, other_existing_precincts)) ) %>%
    return()
}

remove_null_weighted_rows <- function(dataframe, row_col_name = "metric", w_ = weights) {
  
  null_weights <- w_[w_ == 0] %>%
    names()
  
  dataframe %>%
    filter(!(get(row_col_name) %in% null_weights)) %>%
    return()
}

remove_null_weighted_cols <- function(dataframe, w_ = weights) {
  
  null_weights <- w_[w_ == 0] %>%
    names()
  
  dataframe %>%
    select(!any_of(null_weights) ) %>%
    return()
}

remove_null_weighted_translations <- function(ts, inv = T, w_ = weights) {
  
  null_weights <- w_[w_ == 0] %>%
    names()
  
  if(inv) {
    ts[ !(ts %in% null_weights)] %>%
      return()
  } else {
    ts[ !(names(ts) %in% null_weights) ] %>%
      return()
  }
  
  
}


transform_scores_xminxmax <- function(station_rankings, weights_ = weights, print_ = F) {
  
  
  xminmaxtransform = station_rankings %>%
    mutate( across(-station, ~ as.numeric(.) )) %>%
    filter_stations(.) %>%
    remove_existing_stations() %>%
    mutate( across(-station, .fns = function(x) { ( x - min(x, na.rm= T)) / ( max(x, na.rm= T) - min(x, na.rm= T)  ) } )) %>%
    mutate(across(any_of(columns_to_negate), .fns = function(x) {1-x} )) %>%
    mutate(across(-station, .fns = function(x) { x*(weights_[cur_column()] %>% as.vector())  }  )) %>%
    rowwise() %>%
    mutate(score = sum(across(-station))) %>%
    arrange(desc(score))
  
  if(print_) {
    print(xminmaxtransform %>%
            select(station, score) %>%
            tibble())
  }
  
  return(xminmaxtransform)
}


ranked_by_lga = function() {
  
  lgas <- read_sf('shapefiles/lga_boundaries/LGA_2023_AUST_GDA2020.shp') %>%
    select(LGA_NAME23) %>%
    rename(lga = 'LGA_NAME23') %>%
    st_transform('wgs84')
  
  by_lga = xminmaxtransform %>%
    rename(Station_Name = 'station') %>%
    dplyr::left_join(locations, by = 'Station_Name') %>%
    st_as_sf(coords = c('lng','lat'), crs = 'wgs84') %>%
    st_transform('wgs84') %>%
    sf::st_join(lgas) %>%
    st_drop_geometry()
  
  slice_n = 25
  
  by_lga %>%
    slice_head(n = slice_n) %>%
    count(lga) %>%
    arrange(desc(n)) %>%
    mutate(pc_of_total = (n / slice_n)*100)
  
  
  
}

transform_walkability_scores <- function(walk_df) {
  
  xminmaxtransform = walk_df %>%
    mutate( across(-station, ~ as.numeric(.) )) %>%
    filter(!(station %in% city_loop_stations)) %>%
    filter(distance < 25000) %>%
    select(-distance) %>%
    mutate( across(-station, .fns = function(x) { ( x - min(x, na.rm= T)) / ( max(x, na.rm= T) - min(x, na.rm= T)  ) } )) %>%
    rowwise() %>%
    mutate(score = sum(across(-station))) %>%
    arrange(desc(score)) %>%
    rename(walkability_score = score) %>%
    select(station, walkability_score)
  
  return(xminmaxtransform)
  
}

make_map <- function() {
  
  
  # data_to_map_bbox = data_to_map %>%
  #   st_bbox() %>%
  #   st_as_sfc(., crs = st_crs(7844)) %>%
  #   st_centroid(.) %>%
  #   st_coordinates() %>%
  #   as.data.frame()
  # 
  # center = c(data_to_map_bbox$X,data_to_map_bbox$Y)
  
  #write_sf(data_to_map, 'shapefiles/qgis/data_to_map.shp')
  # 
  # data_to_map_bbox = data_to_map %>% st_bbox() %>%
  #   st_as_sfc(., crs = st_crs(7844)) %>%
  #   st_as_sf(.) %>%
  #   st_buffer(., dist = 500)
  
  # mapgl::maplibre(style = carto_style('positron'), bounds = data_to_map_bbox) %>%
  #   mapgl::add_circle_layer(id = "stations", source = data_to_map, min_zoom = 9, circle_color = "#000") %>%
  #   mapgl::add_layer(id = "station_labels", source = data_to_map, type = 'symbol', min_zoom = 11,
  #                    layout = list( 'text-field' = c('get', 'label'),
  #                                   'text-offset' = c(-1,0),
  #                                   'text-size' = 12,
  #                                   'text-justify' = 'auto',
  #                                   'text-anchor' = c('right') ),
  #                    paint = list( 'text-color' = "#000" )) %>%
  #   mapgl::set_layout_property(., layer = 'station_labels', name = 'text-field', value = list(
  #     
  #   ) )
  
  # mapgl::maplibre(style = carto_style('positron'), bounds = data_to_map_bbox) %>%
  #   mapgl::add_circle_layer(
  #     id = "stations", 
  #     source = data_to_map, 
  #     min_zoom = 9, 
  #     circle_color = "#000"
  #   ) %>%
  #   mapgl::add_layer(
  #     id = "station_labels", 
  #     source = data_to_map, 
  #     
  #     type = 'symbol', 
  #     min_zoom = 11,
  #     layout = list(
  #       "text-field" = list(
  #         "format",
  #         c("get", "Station_Name"),
  #         list("font-scale" = 1),
  #         "\n",
  #         c('get', 'rank'),
  #         list("font-scale" = 0.7)
  #       ),
  #       'text-offset' = c(-1,0),
  #       'text-size' = 16,
  #      'text-justify' = 'center',
  #      'text-variable-anchor' = c('right','left'),
  #       'symbol-sort-key' = c('get', 'rank')
  #     ),
  #     paint = list( 'text-color' = "#000" ))
  
  
}



