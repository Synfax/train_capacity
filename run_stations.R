library(quarto)
library(fs)
library(tidyverse)

already_rendered <- list.dirs('stations/', recursive = F, full.names = F) %>%
  str_replace_all(., '_', ' ')

edited_station_list = setdiff(stations, already_rendered)

edited_station_list = 'Brunswick'

# Render a report for each station
for (station in edited_station_list) {
  print(station)
  # Create a safe filename
  safe_name <- gsub("[^a-zA-Z0-9]", "_", station)

  # Set the output directory for this station
  output_dir <- file.path("stations", safe_name)
  dir.create(output_dir, recursive = T, showWarnings = FALSE)
  
  Sys.sleep(1)

  # Render the report
  quarto_render(
    input = "quarto/run_for_station.qmd",
    output_file = paste0(safe_name, ".html"),
    execute_params = list(station = station),
    output_format = "html",
    quarto_args = c("--output-dir", output_dir)
  )
  
  Sys.sleep(1)

   fs::file_move(
    path = file.path(output_dir, paste0(safe_name, ".html")),
    new_path = file.path(output_dir, "quarto/", paste0(safe_name, ".html"))
  )

  Sys.sleep(1)

}
