source('r/station_functions.R')

stations = patronage_data %>%
  filter(Mode == 'Metro') %>%
  select(Station_Name) %>%
  distinct() %>%
  slice_sample(prop = 0.05) %>%
  unlist() %>% 
  as.vector()

#or

stations = list.files('station_zoning_info/') %>%
  str_replace(., ".Rdata", "")

station_rankings <- stations %>%
  map(~ as_tibble(as.list(return_information(.x))), .progress = T) %>%
  list_rbind()

saveRDS(station_rankings, 'r_objects/station_rankings.Rdata')

transformed_scores = transform_scores(station_rankings)

saveRDS(transformed_scores, 'r_objects/transformed_scores.Rdata')

transform_scores <- function(station_rankings) {
  
  bad_columns = c('heritage_pc', 'average_peak_service_cap')
  
  transformed_station_rankings = station_rankings %>%
    mutate( across(-station, ~ as.numeric(.) )) %>%
    mutate( across(-station, .fns = function(x) { x / max(x) } )) %>%
    mutate(across(all_of(bad_columns), .fns = function(x) {-x} )) %>%
    rowwise() %>%
    mutate(score =sum(across(-station)),
           n_metrics = ncol( station_rankings %>% select(-station )),
           percent_score = score/n_metrics) %>%
    arrange(desc(score))
  
  print(transformed_station_rankings %>%
          select(station, score) %>%
        tibble())
    
  return(transformed_station_rankings)
}
