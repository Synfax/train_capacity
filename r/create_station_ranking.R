source('r/station_functions.R')

stations = c('Brunswick', 'Carnegie', 'Macaulay', 'Merri', 'Northcote' )

station_rankings <- stations %>%
  map(~ as_tibble(as.list(return_information(.x))), .progress = T) %>%
  list_rbind()



transform_scores <- function(station_rankings) {
  
  bad_columns = c('heritage_pc')
  
  transformed_station_rankings = station_rankings %>%
    mutate( across(-station, ~ as.numeric(.) )) %>%
    mutate( across(-station, .fns = function(x) { x / max(x) } )) %>%
    mutate(across(all_of(bad_columns), .fns = function(x) {-x} )) %>%
    rowwise() %>%
    mutate(score =sum(across(-station)),
           n_metrics = ncol( transformed_station_rankings %>% select(-station )),
           percent_score = score/n_metrics) %>%
    arrange(desc(score))
  
  transformed_station_rankings %>%
    tibble()
}
