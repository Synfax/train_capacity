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