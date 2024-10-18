library(quarto)
library(fs)
library(tidyverse)

try_render_stupid_quarto_files <- function() {
  already_rendered <- list.files('../rankings_website/stations/', pattern = ".html", recursive = F, full.names = F) %>%
    str_replace_all(., '_', ' ') %>%
    str_replace_all(., ".html", "") %>%
    dplyr::intersect(., stations)
  
  valid_stations <- readRDS('r_objects/transformed_scores.Rdata') %>% pull(station)
  
  edited_station_list = setdiff(valid_stations, already_rendered)
  
  
  # Render a report for each station
  for (station in edited_station_list) {
    print(station)
    # Create a safe filename
    safe_name <- gsub("[^a-zA-Z0-9]", "_", station)
    
    # # Set the output directory for this station
    # output_dir <- file.path("stations", safe_name)
    # dir.create(output_dir, recursive = T, showWarnings = FALSE)
    # 
    # Sys.sleep(2)
    
    # Render the report
    quarto_render(
      input = "quarto/run_for_station.qmd",
      output_file = paste0(safe_name, ".html"),
      execute_params = list(station = station),
      output_format = "html", quarto_args = c('--output-dir', '../rankings_website/stations/')
      
    )
    
    # Sys.sleep(2)
    # 
    # fs::file_move(
    #   path = file.path(output_dir, paste0(safe_name, ".html")),
    #   new_path = file.path(output_dir, "quarto/", paste0(safe_name, ".html"))
    # )
    # 
    # Sys.sleep(2)
    
  }
  
}
