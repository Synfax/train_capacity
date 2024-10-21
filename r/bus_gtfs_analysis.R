bus_stop_times <- read_csv('data/gtfs/4/google_transit/stop_times.txt')
bus_routes <- read_csv('data/gtfs/4/google_transit/routes.txt')
bus_trips <- read_csv('data/gtfs/4/google_transit/trips.txt')
bus_stops <- read_csv('data/gtfs/4/google_transit/stops.txt')
bus_calendar_dates <- read_csv('data/gtfs/4/google_transit/calendar_dates.txt')

#current_date <- as.Date(Sys.Date(), format = "%Y%m%d")

current_date <- '2024-09-10'

#find type 2 exemptions in service_id from calendar_dates
type_2_exemptions = bus_calendar_dates %>%
  mutate(date = as.Date(as.character(date), format = "%Y%m%d")) %>%
  filter(date == current_date) %>%
  pull(service_id)

bus_calendar <- read_csv('data/gtfs/4/google_transit/calendar.txt') %>%
  as.data.frame() %>%
  mutate(across(c(start_date, end_date), ~ as.Date( as.character(.x) , format = "%Y%m%d") ))

#find stopping patterns for today.
ok_services = bus_calendar %>%
  filter(current_date >= start_date) %>%
  filter(tuesday == 1) %>%
  pull(service_id) %>%
  unique()

#join all into one for ez manipulation
  
bus_gtfs <- bus_stop_times %>%
  left_join(bus_trips, by = 'trip_id') %>%
  left_join(bus_routes, by = 'route_id') 

#restrict big set into service_ids running today and remove type2 exemptions

bus_nt <- bus_gtfs %>%
  filter(service_id %in% ok_services) %>%
  filter(!service_id %in% type_2_exemptions) %>%
  rowwise() %>%
  mutate(hour_of_day = hour(departure_time) ) %>%
  mutate(peak_type = ifelse(hour_of_day %in% peak_morning, 'm', NA),
         peak_type = ifelse(hour_of_day %in% peak_evening, 'e', peak_type)) %>%
  filter(!is.na(peak_type)) %>%
  group_by(stop_id, peak_type, route_short_name) %>%
  summarise(n = n()) %>%
  mutate(sph = case_when( peak_type == 'e' ~ n / length(peak_evening) , peak_type == 'm' ~ n / length(peak_morning) ) )

# ttx <- bus_nt %>% 
#   rename(Num_Departures = 'n') %>%
#   mutate(sph = case_when( peak_type == 'e' ~ Num_Departures / length(peak_evening) , peak_type == 'm' ~ Num_Departures / length(peak_morning) ) )
# 
# txx <- bus_gtfs %>%
#   filter(service_id %in% ok_services) %>%
#   filter(!service_id %in% type_2_exemptions) %>%
#   rowwise() %>%
#   mutate(hour_of_day = hour(departure_time) ) %>%
#   mutate(peak_type = ifelse(hour_of_day %in% peak_morning, 'm', NA),
#          peak_type = ifelse(hour_of_day %in% peak_evening, 'e', peak_type)) %>%
#   filter(!is.na(peak_type)) %>%
#   filter(stop_id == 1406)
# 
# txx  %>% filter(peak_type == 'e', route_short_name == 603) %>% v



saveRDS(bus_nt, 'r_objects/bus_stop_frequencies.Rdata')

#test to make sure it aligns with the timetable
ntt = bus_gtfs %>%
    filter(route_short_name == '508') %>%
    filter(service_id %in% ok_services) %>%
    group_by(trip_id, shape_id, service_id) %>%
    summarise(fir = first(arrival_time), headsign = first(trip_headsign)) %>%
    arrange(fir) %>%
    filter(headsign == 'Alphington') %>%
    filter(!(service_id %in% type_2_exemptions)) %>%
    as.data.frame()
