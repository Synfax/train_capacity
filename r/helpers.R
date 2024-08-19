v = function(x) {
  View(x)
}

adf = function(x) {
  return(as.data.frame(x))
}

mp = function(x) {
  leaflet(x) %>% addProviderTiles('CartoDB.Positron') %>% addPolygons()
}

mc = function(x) {
  leaflet(x) %>% addProviderTiles('CartoDB.Positron') %>% addCircleMarkers()
}

sdg = function(x) {
  return(st_drop_geometry(x))
}
