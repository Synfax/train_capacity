source('r/station_functions.R')

stations = c('Brunswick', 'Carnegie')

df <- stations %>%
  map(~ as_tibble(as.list(return_information(.x)))) %>%
  list_rbind()



