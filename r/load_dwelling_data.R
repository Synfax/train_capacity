dwelling_data = st_read('data/Melbourne dwelling data.gpkg')

com_fixed = dwelling_data %>%
  fix_com_commercial_zoning() 

final = com_fixed %>%
  add_missing_middle_zoning_info()

# 
# 
# com_fixed = readRDS('com_fixed.Rdata')
# #saveRDS(com_fixed, 'com_fixed.Rdata')
# 
# cd = dwelling_data %>%
#       st_drop_geometry() %>%
#   left_join(com_fixed %>%
#               st_drop_geometry() %>%
#               select(c(lat,lon,com_tall_building)), by = c('lat', 'lon'))
# 
# # saveRDS(final, 'data/final_dwelling_data.Rdata')
# 
# #final = readRDS('data/final_dwelling_data.Rdata')
# 
# lat_lon_com = com_fixed %>% select(c('lat', 'lon', 'com_tall_building', 'geom')) %>%
#   as.data.frame()
# 
# testx <- dwelling_data %>% st_drop_geometry() %>%
#   left_join(lat_lon_com, by = c('lat', 'lon'))
# 
# testx = add_missing_middle_zoning_info(testx)
# 
# saveRDS(testx, 'data/final_dwelling_data.Rdata')
