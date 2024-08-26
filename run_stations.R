library(quarto)

# List of stations
stations <- c("South Yarra")  # Replace with your actual list


# Render a report for each station
for (station in stations) {
  # Create a safe filename
  safe_name <- gsub("[^a-zA-Z0-9]", "_", station)
  
  # Set the output directory for this station
  output_dir <- file.path("stations", safe_name)
  dir.create(output_dir, recursive = T, showWarnings = FALSE)
  
  # Render the report
  quarto_render(
    input = "quarto/run_for_station.qmd",
    output_file = paste0(safe_name, ".html"),
    execute_params = list(station = station),
    output_format = "html",
    quarto_args = c("--output-dir", output_dir)
  )
  
  fs::file_move(
    path = file.path(output_dir, paste0(safe_name, ".html")),
    new_path = file.path(output_dir, "quarto/", paste0(safe_name, ".html"))
  )
  
}
