library(quarto)
library(tidyverse)

stations = c('Brunswick', 'South Yarra')

reports <-
  tibble(
    input = "run_for_station.qmd",
    output_file = str_glue("{stations}.html"),
    execute_params = map(stations, ~ list(station = .))
  )

pwalk(reports, quarto_render)


# 
# run_a_station <- function(stn) {
#   
#   quarto::quarto_render('stations/run_for_station.qmd',
#                         output_file = paste0(stn, '.html'),
#                         output_format = 'html',
#                         execute_params = list('station' = stn))
#   
#   my.file.rename(from = paste0(stn,".html"), to = paste0("stations/",stn,'.html'))
#   
# }
# 
# 
# my.file.rename <- function(from, to) {
#   todir <- dirname(to)
#   if (!isTRUE(file.info(todir)$isdir)) dir.create(todir, recursive=TRUE)
#   file.rename(from = from,  to = to)
# }
