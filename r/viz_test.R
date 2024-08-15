library(qs)
library(sf)
library(tidyverse)
devtools::install_github("walkerke/mapgl")
library(mapgl)

#download.file("https://yimby-mel.s3.ap-southeast-2.amazonaws.com/rmd_data.qs",destfile = 'data/rmd_data.qs')

#data <- qs::qread_url("https://yimby-mel.s3.ap-southeast-2.amazonaws.com/rmd_data.qs")
#saveRDS(data, '../data/data.Rdata')
library(data.table)

#st_write(data, 'data/rmd_data.shp')

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

specified_colors <- c("Already developed"                       = "#989898",
                      "Housing not permitted"                   = "#b1b1b1",
                      "Civic use makes development less likely" = "#cacaca", 
                      "Low density residential"                 = yimby_colours$blue_palette[5],
                      "2 storeys (NRZ)"                         = yimby_colours$blue_palette[4],
                      "3 storeys (GRZ)"                         = yimby_colours$blue_palette[3],
                      "4 storeys (RGZ)"                         = yimby_colours$blue_palette[2],
                      "4+ storeys (Mixed use zones)"            = yimby_colours$blue_palette[1],
                      "6 storeys (Missing middle)"              = yimby_colours$green_palette[3]
)


data = data %>% st_set_geometry('geom')

#brunswick = data %>% filter(lga_name_2022 == "Merri-bek") %>% filter(category != 'Already developed')
brunswick = data %>% filter(sa2_code_2021 == "206011106") %>% filter(category != 'Already developed')

percentage_to_build = 0.15

profitable_apartments = brunswick %>%
  st_drop_geometry() %>%
  arrange(desc(profit)) %>%
  slice_head(prop=percentage_to_build ) %>%
  mutate(willBeBuilt = T) %>%
  select(c(lat,lon,willBeBuilt))

brunswick = brunswick %>%
  left_join(profitable_apartments, by = c('lat', 'lon'))

height_of_storey = 6
map_mult = 1
multiplier = height_of_storey * map_mult

brunswick = brunswick %>%
  mutate(old_height = as.numeric(regmatches(category, gregexpr("[0-9]+", category))) * multiplier ,
         new_height = ifelse( !is.na(willBeBuilt) , storey * multiplier , old_height )  )


values = c(brunswick$category, brunswick$category_new) %>% unique()

brunswick = brunswick %>% mutate( old_color = specified_colors[category], new_color = specified_colors[category_new] )


m1 <- mapgl::maplibre(style = mapgl::carto_style('positron')) %>%
  mapgl::add_fill_extrusion_layer(id = 'brunswick', source = brunswick,
                           fill_extrusion_opacity = 0.85,
                           fill_extrusion_color = c('get', 'old_color'),
                           fill_extrusion_height = c("get", "old_height") ) %>%
    mapgl::fly_to(
      center = c(145.063115,-37.814175),
      zoom = 9,
      pitch = 40
    )

m2 <- mapgl::maplibre(style = mapgl::carto_style('positron')) %>%
  mapgl::add_fill_extrusion_layer(id = 'brunswick', source = brunswick,
                                  fill_extrusion_opacity = 0.85,
                                  fill_extrusion_color = c('get', 'new_color'),
                                  fill_extrusion_height = c("get", "new_height") ) %>%
  mapgl::fly_to(
    center = c(145.063115,-37.814175),
    zoom = 9,
    pitch = 40
  )

mapgl::compare(m1,m2)

# %>%
#   mapgl::add_legend(
#     "Tree Coverage by SA1 (%)",
#     values = c(0,91.6),
#     colors = c(colCols[1], colCols[9])
#   ) %>%  mapgl::add_fullscreen_control(position = "bottom-left") %>%
#   mapgl::add_navigation_control() 