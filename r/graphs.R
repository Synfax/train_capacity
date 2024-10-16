library(ggiraph)
library(patchwork)



top_stations_by_lga <- function(fromQuarto = T, n_slice = 25) {
  
  prefix = ifelse(fromQuarto, '../', '')
  
  transformed_scores = readRDS(paste0(prefix, 'r_objects/transformed_scores.Rdata')) %>%
    as.data.frame() %>%
    rename(Station_Name = "station")
  
  locations = readRDS(paste0(prefix, 'r_objects/locations.Rdata')) 
  
  scores_for_lga = transformed_scores %>%
    left_join(locations, by = 'Station_Name') %>%
    st_as_sf(coords = c('lng','lat')) %>%
    arrange(desc(score)) %>%
    mutate(rank = row_number())
  
  scores_for_lga = slice_head(scores_for_lga, n = n_slice) %>%
    select(Station_Name) 
  
  scores_for_lga = st_set_crs(scores_for_lga, 'wgs84') %>%
    st_transform(crs = 7844)
  
  
  
  create_green_palette <- function(n) {
    colorRampPalette(c("#E8F5E9", "#1B5E20"))(n)
  }
  
  #load geometries 
  
  lga <- read_sf(paste0(prefix, 'shapefiles/lga_boundaries/LGA_2023_AUST_GDA2020.shp'))
  
  lga_with_n_sf <- st_join(lga, scores_for_lga) %>%
    filter(!is.na(Station_Name)) %>% 
    group_by(LGA_NAME23) %>%
    summarise(n = n())
  
  lga_chart <- ggplot(as.data.frame(lga_with_n_sf), mapping = aes(x = reorder(LGA_NAME23, n), y = n, fill = n, data_id = LGA_NAME23)) +
    geom_col_interactive() +
    scale_fill_gradientn(colors = create_green_palette(11)) +
    ylab('Number of stations in top 25') +
    theme_gray(base_size = 8) +
    theme_report() +
    theme(legend.position = 'none') +
    coord_flip() 
  
  lga_chart
  
  lga_map <-(  ggplot() + 
    geom_sf(data = lga_with_n_sf, fill = 'lightgrey', color = 'lightgrey') +
    geom_sf_interactive(data = lga_with_n_sf, aes(fill = n, data_id = LGA_NAME23)) +
      scale_fill_gradientn(colors = create_green_palette(11)) +
      theme_void()  +
      theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_rect(fill = "#fdffee", color = "#fdffee"),
        plot.background =  element_rect(fill = "#fdffee", color = "#fdffee"),
        legend.position = "none"
      )
    
    
    )

  lga_map  
  
  combined_plot <-   lga_chart + (lga_map) + plot_layout(widths = c(2.5, 1.5))
  
  # Create the interactive plot
  interactive_plot <- girafe(ggobj = combined_plot)
  interactive_plot <- girafe_options(
    interactive_plot,
    opts_hover(css = "fill:red;stroke:black;")
  )
  
  interactive_plot
  
  return(interactive_plot)
  
}

top_10_chart <- function(fromQuarto = T) {
  prefix = ifelse(fromQuarto, '../', '')
  
  transformed_scores = readRDS(paste0(prefix, 'r_objects/transformed_scores.Rdata')) %>%
    as.data.frame() %>%
    rename(Station_Name = "station") %>%
    slice_head(n = 10)
  
  ggplot(transformed_scores, mapping = aes(x = score, y = Station_Name) ) + geom_col()
  
  transformed_scores %>% 
    select(Station_Name, score) %>%
    mutate(score = round(score, 2)) %>%
  reactable(
    class = "pageinforeactable",
    columns = list(
      'Station_Name' = colDef(name = 'Station'),
      'score' = colDef(name = "Total Score",
                       cell = data_bars(.,
                                        text_position = 'inside-end',
                                        fill_color = '#1B5E20',
                                        text_color = 'white',
                                        background = 'opaque',
                                        fill_opacity = 1,
                                        bar_height = 24,
                                        bold_text = T))
    )
    
  ) %>%
    return()
}