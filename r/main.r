library(devtools)
#devtools::install_github("walkerke/mapgl")
library(mapgl)
library(sf)
library(tidyverse)
library(data.table)
library(leaflet)
library(htmltools)
library(quarto)
library(dtplyr)
library(janitor)
library(arrow)
library(stringr)
library(DescTools)
library(mapview)
library(geosphere)
library(chron)

source('r/helpers.r')
source('r/constants.r')
source('r/main_functions.r')



# check directories

if(!dir.exists('r_objects') ) {
  dir.create('r_objects')
}

if(!dir.exists('data') ) {
  dir.create('data')
}


# run things for the quarto pages

patronage_data = load_and_clean_patronage_data()

hourly_factors = generate_hourly_information()

service_frequences = generate_service_frequency_information()

generate_station_location_info()

stations = patronage_data %>%
  filter(Mode == 'Metro') %>%
  select(Station_Name) %>%
  pull() %>%
  unique()


#

inner_lgas <- c("Melbourne",
                "Yarra",
                "Port Phillip",
                "Stonnington",
                "Maribyrnong")

middle_lgas <- c("Boroondara",
                 "Darebin",
                 "Glen Eira",
                 "Merri-bek",
                 "Banyule",
                 "Bayside",
                 "Hobsons Bay",
                 "Kingston",
                 "Manningham",
                 "Monash",
                 "Moonee Valley",
                 "Whitehorse",
                 "Maroondah",
                 "Brimbank"
)

outer_lgas <- c("Knox",
                "Mornington Peninsula",
                "Nillumbik",
                "Yarra Ranges",
                "Greater Dandenong",
                "Frankston")

greenfield <- c("Cardinia",
                "Casey",
                "Hume",
                "Melton",
                "Whittlesea",
                "Wyndham")