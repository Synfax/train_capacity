
transform_scores <- function(station_rankings) {
  
  bad_columns = c('heritage_pc', 'average_peak_service_cap', 'distance')
  
  transformed_station_rankings = station_rankings %>%
    mutate( across(-station, ~ as.numeric(.) )) %>%
    mutate( across(-station, .fns = function(x) { x / max(x) } )) %>%
    mutate(across(any_of(bad_columns), .fns = function(x) {-x} )) %>%
    rowwise() %>%
    mutate(score =sum(across(-station)),
           n_metrics = ncol( station_rankings %>% select(-station )),
           percent_score = score/n_metrics) 
  
  # print(transformed_station_rankings %>%
  #         select(station, score) %>%
  #         tibble())
  
  return(transformed_station_rankings)
}

transform_scores_normal <- function(station_rankings) {
  
  bad_columns = c('heritage_pc', 'average_peak_service_cap', 'distance')
  
  transformed_station_rankings = station_rankings %>%
    mutate( across(-station, ~ as.numeric(.) )) %>%
    filter(distance < 25000, distance > 3000) %>%
    mutate( across(-station, .fns = function(x) { (x - mean(x, na.rm= T)) / var(x) } )) %>%
    mutate(across(any_of(bad_columns), .fns = function(x) {-x} )) %>%
    rowwise() %>%
    mutate(score =sum(across(-station)))
  
  # print(transformed_station_rankings %>%
  #         select(station, score) %>%
  #         tibble())
  
  return(transformed_station_rankings)
}



