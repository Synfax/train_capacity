radius <- 1000

peak_morning = 7:10
peak_evening = 16:19

weights = c(
  "grz_nrz_pc" = 1,
  "capacity_delta" = 1,
  'average_peak_service_freq' = 1,
  'average_peak_service_cap' = -1,
  'walkability_score' = 1,
  'distance' = -1, 
  'n_bus_tram' = 1
)

critical_variables = data.frame(val = c('restaurant', 'supermarket', 'cafe', 'bar', 'school', 'childcare', 'park'),
                                key =  c('amenity', 'shop', 'amenity', 'amenity', 'amenity', 'amenity', 'leisure'))

city_loop_stations = c('Flagstaff', 'Flinders Street', 'Melbourne Central', 'Southern Cross', 'Parliament')
other_important_stations = c('North Melbourne', 'South Yarra', 'Richmond')
event_stations = c('Showgrounds', 'Flemington Racecourse')

translations = c(
  'grz_nrz_pc' = 'Zoning suitability (% land area)',
  'capacity_delta' = 'Potential new homes in broad 1000m upzoning',
  'average_peak_service_freq' = 'Train frequency in peak (trains per hour)',
  'average_peak_service_cap' = 'Average patronage of train line(s) in peak (people)',
  'walkability_score' = 'Existing local amenities (composite index)',
  'distance' = 'Distance to CBD (m)',
  'n_bus_tram' = 'Mode weighted bus and tram frequencies'
)

translations_simple = c(
  'grz_nrz_pc' = 'Zoning suitability',
  'capacity_delta' = 'Potential new homes',
  'average_peak_service_freq' = 'Train frequency',
  'average_peak_service_cap' = 'Line Capacity',
  'walkability_score' = 'Local Amenities',
  'distance' = 'Distance to CBD',
  'n_bus_tram' = 'Other transport connections'
)

translations_simple_inv <- setNames(names(translations_simple), translations_simple)


translations_inverse = c(
  
)

colours = tribble(
  ~group, ~ colour,
  'Sandringham' , "#F178AF",
  'CrossCity' , "#028430",
  'Cran/Pak' , "#279FD5",
  "Burnley" , "#152C6B",
  "Northern" , "#FFBE00",
  "Eastern" , "#BE1014",
  'event' , "#95979A"
)

line_groups = tribble(
  ~line, ~ group,
  "Sandringham", "Sandringham",
  "Upfield", "Northern",
  "Craigieburn", "Northern",
  "Sunbury", "Northern",
  "Mernda", "Eastern",
  "Hurstbridge", "Eastern",
  "Glen Waverley", "Burnley",
  "Lilydale", "Burnley", 
  "Alamein", "Burnley",
  "Belgrave", "Burnley",
  "Cranbourne", "Cran/Pak",
  "Pakenham", "Cran/Pak",
  "Frankston", "CrossCity",
  "Werribee", "CrossCity"
)

bus_weight = 1
tram_weight = 2

inner_lgas <- c("Melbourne",
                "Yarra",
                "Port Phillip",
                "Stonnington",
                "Maribyrnong")

middle_lgas <- c("Boroondara",
                 "Darebin",
                 "Glen Eira",
                 "Merri-bek",
                 "Banyule",
                 "Bayside",
                 "Hobsons Bay",
                 "Kingston",
                 "Manningham",
                 "Monash",
                 "Moonee Valley",
                 "Whitehorse",
                 "Maroondah",
                 "Brimbank"
)

outer_lgas <- c("Knox",
                "Mornington Peninsula",
                "Nillumbik",
                "Yarra Ranges",
                "Greater Dandenong",
                "Frankston")

greenfield <- c("Cardinia",
                "Casey",
                "Hume",
                "Melton",
                "Whittlesea",
                "Wyndham")