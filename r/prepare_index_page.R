prepare_index <- function(returnTopTen = F, fromQuarto = F, font_size_q = '16pt', w = 2560, h = 1440) {
  
  prefix_dir <- ifelse(fromQuarto, '../', '')
  
  locations = readRDS(paste0(prefix_dir,'r_objects/locations.Rdata'))
  
  
  
  data_to_map <- readRDS(paste0(prefix_dir,'r_objects/transformed_scores.Rdata')) %>%
    as.data.frame() %>%
    slice_head(n = 10) %>%
    arrange(desc(score)) %>%
    mutate(rank = row_number(), label = paste0(station, ": ", rank )) %>%
    select(station, score, rank, label) %>%
    rename(Station_Name = 'station') %>%
    dplyr::left_join(locations, by = 'Station_Name') %>%
    mutate(message = 
             paste0(
               '<div style = \" background-color: black; \">',
               "<p style = \" color: white; text-align:center ; padding: 5px; \" >", rank, '. ' , Station_Name, '</p>',
               '</div>'
               
             )
    ) 
  
  #saveRDS(data_to_map, 'r_objects/index_map/data_to_map.Rdata')
  
  
  lines_dissolved <- read_sf(paste0(prefix_dir, 'shapefiles/qgis/final_map.shp')) %>%
    left_join(colours, by = 'group') %>%
    st_transform('wgs84')
  
  #saveRDS(lines_dissolved, 'r_objects/index_map/lines_dissolved.Rdata')
  
  
  # coords_vec <-  data_to_map %>%
  #   st_as_sf(coords = c('lng','lat'), crs = 'wgs84') %>%
  #   st_bbox() %>%
  #   st_as_sfc() %>%
  #   st_buffer(dist = 800) %>%
  #   st_bbox() %>%
  #   as.vector()
  
  test <- data_to_map %>%
    st_as_sf(coords = c('lng','lat'), crs = 'wgs84') %>%
    st_bbox() %>%
    st_as_sfc() %>%
    st_buffer(dist = 800) %>%
    st_as_sf() %>%
    st_set_geometry('x')
  
  bounds <- c(min(data_to_map$lng), min(data_to_map$lat),
              max(data_to_map$lng), max(data_to_map$lat))
  
  #saveRDS(coords_vec, 'r_objects/index_map/coords_vec.Rdata')
  
  #  addTiles('https://tiles.stadiamaps.com/tiles/stamen_toner_lite/{z}/{x}/{y}{r}.png') %>%
  
  # had to remove the {r} for some reason?
  
  map <- leaflet(width = w , height = h) %>%
    addPolylines(data = lines_dissolved, color = ~colour, opacity = 1) %>%
    addTiles('https://tiles.stadiamaps.com/tiles/stamen_toner_background/{z}/{x}/{y}.png?api_key=090a847c-32a2-4e35-99a9-543ad8f4ecc8', options = tileOptions(opacity = 0.5)) %>%
    addCircleMarkers(lng = data_to_map$lng, lat=data_to_map$lat, radius = 5, color = 'black', fillColor = 'black', fillOpacity = 1, opacity = 0.7) %>%
    addLabelOnlyMarkers(lng = data_to_map$lng,
                        lat = data_to_map$lat,
                        label = lapply(data_to_map$message, htmltools::HTML),
                        labelOptions = labelOptions(noHide = T, direction = 'auto',  textOnly = T, textsize = font_size_q,
                                                    style = list(
                                                      'font-weight' = '800',
                                                      'text-decoration' = 'underline',
                                                      'text-decoration-color' = 'white',
                                                      'margin' = '5px',
                                                      'letter-spacing' = '0.5px'
                                                    ))) %>%
    fitBounds(lng1 = bounds[1], lat1 = bounds[2], lng2 = bounds[3], lat2 = bounds[4])
  
  if(returnTopTen) {
    return(map)
  }
  
  mapshot(map, file = 'quarto/index_images/main.png')
  
  
  writeStationMaps <- function(rank, zm = 17) {
    
    station_map = map %>%
      setView(data_to_map$lng[rank], data_to_map$lat[rank], zoom = zm)
    
    mapshot(station_map, file = paste0('quarto/index_images/',as.numeric(rank),'.png'))
  }   
  
  walk(1:10, .f = writeStationMaps)
  
}
