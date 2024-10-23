
#set weights manually, default is 1.

weights = c(
  "grz_nrz_pc" = 2,
  "capacity_delta" = 0,
  'average_peak_service_freq' = 1,
  'average_peak_service_cap' = 1,
  'walkability_score' = 1,
  'distance' = 2, 
  'n_bus_tram' = 1
)

#variables used in station function creation

radius <- 1000

peak_morning = 7:10
peak_evening = 16:19

bus_weight = 1
tram_weight = 2

#palettes


yimby_colours <- list(
  hero = "#10461B",
  body = "#000000",
  background  = "#FDFFEE",
  green_base  = "#10461B",
  blue_base   = "#283696",
  red_base    = "#BA1B21",
  yellow_base = "#F6AE00",
  green_palette  = c("#10461B", "#2C6F3A", "#579A64", "#8FC49A", "#D6EFDB"),
  blue_palette   = c("#1A235F", "#283696", "#9EA7E2", "#D0D6FF", "#E8ECFF"),
  red_palette    = c("#D92127", "#BA1B21", "#8E3437", "#C6A2A3", "#E3BABB"),
  yellow_palette = c("#F6AE00", "#DE9B00", "#C79E3E", "#F2DCA5", "#FFF7E3")
)


create_green_palette <- function(n) {
  colorRampPalette(c("#579A64", "#10461B"))(n)
}

#variables where a higher value is worse
columns_to_negate <- c('average_peak_service_cap', 'distance')

#amenities to pull from OpenStreetMap
critical_variables = data.frame(val = c('restaurant', 'supermarket', 'cafe', 'bar', 'school', 'childcare', 'park'),
                                key =  c('amenity', 'shop', 'amenity', 'amenity', 'amenity', 'amenity', 'leisure'))

#Station definitions
city_loop_stations = c('Flagstaff', 'Flinders Street', 'Melbourne Central', 'Southern Cross', 'Parliament')
other_important_stations = c('North Melbourne', 'South Yarra', 'Richmond')
event_stations = c('Showgrounds', 'Flemington Racecourse')


#Fixed arrays to translate variable names to normal english
translations = c(
  'grz_nrz_pc' = 'Zoning suitability (% land area)',
  'capacity_delta' = 'Potential new homes in broad 1000m upzoning',
  'average_peak_service_freq' = 'Train frequency in peak (trains per hour)',
  'average_peak_service_cap' = 'Spare capacity on train line in peak (# people)',
  'walkability_score' = 'Existing local amenities (composite index)',
  'distance' = 'Proximity to the CBD (m)',
  'n_bus_tram' = 'Mode weighted bus and tram frequencies'
)

# Simple translations
translations_simple = c(
  'grz_nrz_pc' = 'Zoning suitability',
  'capacity_delta' = 'Potential new homes',
  'average_peak_service_freq' = 'Train frequency',
  'average_peak_service_cap' = 'Spare line Capacity',
  'walkability_score' = 'Local Amenities',
  'distance' = 'Proximity to CBD',
  'n_bus_tram' = 'Other transport connections'
)

translations_simple_inv <- setNames(names(translations_simple), translations_simple)


translations_inverse = c(
  
)

# Line colours
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

#Line groups
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
  "Werribee", "CrossCity",
  "Williamstown", "CrossCity"
)




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