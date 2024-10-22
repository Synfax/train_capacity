library(ggiraph)
library(patchwork)



top_stations_by_lga <- function(fromQuarto = T, n_slice = 25, include_existing_25 = F, debug = F) {
  
  prefix = ifelse(fromQuarto, '../', '')
  
  title_text <- "Number of stations in Top 25"
  
  stations_to_include = readRDS(paste0(prefix, 'r_objects/transformed_scores.Rdata')) %>%
    as.data.frame() %>%
    rename(Station_Name = "station") %>%
    slice_head(n = n_slice) %>%
    select(Station_Name)
  
  if(debug) { print ('fnd transf')}
  
  if(include_existing_25) {
    
    title_text <- "Number of stations in top 25 + Announced stations"
    
    existing <- data.frame(Station_Name = existing_upzoned_stations) %>%
      filter(Station_Name != "Toorak Village")
    
    stations_to_include = bind_rows(stations_to_include, existing )
  }
  
  #locations = readRDS(paste0(prefix, 'r_objects/locations.Rdata')) 
  
  # scores_for_lga = stations_to_include %>%
  #   left_join(locations, by = 'Station_Name') %>%
  #   st_as_sf(coords = c('lng','lat')) 
  # 
  # scores_for_lga = scores_for_lga  %>%
  #   select(Station_Name) 
  # 
  # scores_for_lga = st_set_crs(scores_for_lga, 'wgs84') %>%
  #   st_transform(crs = 7844)
  
  create_green_palette <- function(n) {
    colorRampPalette(c("#E8F5E9", "#1B5E20"))(n)
  }
  
  create_greengray_palette <- function(n) {
    if (n <= 1) return("lightgray")  # Return gray if only one color requested
    
    # Generate the green gradient for remaining colors
    green_colors <- colorRampPalette(c("#E8F5E9", "#1B5E20"))(n - 1)
    
    # Combine gray with the green gradient
    c("lightgray", green_colors)
  }
  
  #load geometries 
  
  stations_in_lga <- readRDS(paste0(prefix, 'r_objects/stations_in_lga.Rdata')) %>%
    mutate( LGA_NAME23 = gsub("\\s*\\(Vic\\.\\)\\s*$", "", LGA_NAME23) )
  
  if(debug) { print ('fnd stn in lga')}
  
  lga <- read_sf(paste0(prefix, 'shapefiles/lga_boundaries/LGA_2023_AUST_GDA2020.shp')) %>%
    mutate( LGA_NAME23 = gsub("\\s*\\(Vic\\.\\)\\s*$", "", LGA_NAME23) )
  
  if(debug) { print ('fnd sf')}
  
  # lga_with_n_sf <- st_join(lga, scores_for_lga) %>%
  #   filter(!is.na(Station_Name)) %>%
  #   group_by(LGA_NAME23) %>%
  #   summarise(n = n())
  
  stations_to_include <- stations_to_include %>% as.data.frame() %>% pull(Station_Name)
  
  lga_with_n_sf <- stations_in_lga %>%
    filter(Station_Name %in% stations_to_include ) %>%
    group_by(LGA_NAME23) %>%
    summarise(n = n())
  
  list_not_yet_mapped <- setdiff(c(inner_lgas, middle_lgas), lga_with_n_sf$LGA_NAME23)
  
  unmapped_lgas <- lga %>%
    filter(LGA_NAME23 %in% list_not_yet_mapped) %>%
    select(LGA_NAME23) %>%
    mutate(n = 0)
   
  lga_with_n_sf = bind_rows(lga_with_n_sf, unmapped_lgas) 
  
  lga_chart <- ggplot(as.data.frame(lga_with_n_sf) %>% filter(n > 0), mapping = aes(x = reorder(LGA_NAME23, n), y = n, fill = n, data_id = LGA_NAME23)) +
    geom_col_interactive() +
    scale_fill_gradientn(colors = create_green_palette(11)) +
    ylab(title_text) +
    theme_gray(base_size = 8) +
    theme_report() +
    theme(legend.position = 'none') +
    theme(axis.title.y = element_blank()) +
    coord_flip() 
  
  lga_chart
  
  lga_map <-(  ggplot() + 
    geom_sf(data = lga_with_n_sf, fill = 'lightgrey', color = 'lightgrey') +
    geom_sf_interactive(data = lga_with_n_sf, aes(fill = n, data_id = LGA_NAME23)) +
      scale_fill_gradientn(colors = create_greengray_palette(11)) +
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
  
  combined_plot <-   lga_chart + (lga_map) + plot_layout(widths = c(2.5, 1.5)) &
    theme(plot.margin = unit(c(0,0,0,0), "cm"),
          plot.background = element_rect(fill = "#fdffee", color = "#fdffee"),
          panel.spacing = unit(0, "cm"))
  
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