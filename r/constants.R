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