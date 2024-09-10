tram_stop_times <- read_csv('data/gtfs/3/google_transit/stop_times.txt')
tram_routes <- read_csv('data/gtfs/3/google_transit/routes.txt')
tram_trips <- read_csv('data/gtfs/3/google_transit/trips.txt')
tram_stops <- read_csv('data/gtfs/3/google_transit/stops.txt')
tram_calendar_dates <- read_csv('data/gtfs/3/google_transit/calendar_dates.txt')

tram_calendar <- read_csv('data/gtfs/3/google_transit/calendar.txt') %>%
  as.data.frame() %>%
  mutate(across(c(start_date, end_date), ~ as.Date( as.character(.x) , format = "%Y%m%d") ))

current_date <- as.Date(Sys.Date(), format = "%Y%m%d")

ok_services = tram_calendar %>%
  filter(current_date >= start_date) %>%
  filter(tuesday == 1) %>%
  pull(service_id)

gtfs <- tram_stop_times %>%
  left_join(tram_trips, by = 'trip_id') %>%
  left_join(tram_routes, by = 'route_id') 

nt <- gtfs %>%
  filter(service_id == temp) %>%
  rowwise() %>%
  mutate(hour_of_day = hour(departure_time) ) %>%
  mutate(peak_type = ifelse(hour_of_day %in% peak_morning, 'm', NA),
         peak_type = ifelse(hour_of_day %in% peak_evening, 'e', peak_type)) %>%
  filter(!is.na(peak_type)) %>%
  group_by(stop_id, peak_type) %>%
  summarise(n = n()) %>%
  left_join(tram_stops, by = 'stop_id') %>%
  mutate(n = case_when( peak_type == 'e' ~ n / length(peak_evening) , peak_type == 'm' ~ n / length(peak_morning) ) )

saveRDS(nt, 'r_objects/tram_stop_frequencies.Rdata')  

# nt = nt %>%
#   filter(service_id == temp) %>%
#   group_by(trip_id, shape_id) %>%
#   summarise(fir = first(arrival_time), headsign = first(trip_headsign)) %>%
#   arrange(fir) %>%
#   filter(headsign == "Flinders Street Station, City to North Coburg") %>%
#   as.data.frame()
# 
# nt = nt %>%
#   left_join(shapes_and_lengths, by = 'shape_id')
# 

#~~~~~~~~~~~~~~ SHAPES ~~~~~~~~~~~~~~~~~~~~~~~~~~ #

tram_shapes <- read_csv('data/gtfs/3/google_transit/shapes.txt')

tram_shapes = tram_shapes %>%
  filter( str_detect(shape_id, '3-19')) %>%
  as.data.frame()

shapes_and_lengths <- tram_shapes %>%
  group_by(shape_id) %>%
  summarise(max_dist = max(shape_dist_traveled), n_stops = max(shape_pt_sequence)) %>%
  as.data.frame()

uniques <- tram_shapes %>%
  pull(shape_id) %>%
  unique()

walk(uniques, drawShape)

drawShape <- function(shape) {
  
  sf <- tram_shapes %>%
    filter(shape_id == shape) %>%
    st_as_sf(coords = c('shape_pt_lon', 'shape_pt_lat')) %>%
    group_by(shape_id) %>%
    dplyr::summarise(do_union = F) %>%
    st_cast('LINESTRING')
  
  shape_map <- leaflet(sf) %>%
    addProviderTiles('CartoDB.Positron') %>%
    addPolylines() %>%
    addLegend(position = 'topright', values = NA, colors = NA, labels = NA, title = shape)
    
  print(shape_map)
}
