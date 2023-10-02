library(tidyverse)
library(tidygeocoder)
library(leaflet)

# This is power plant data from the WRI that is last updated in 2018

data <- readr::read_csv(file = "usa_power_plant_data_world_resources_institure.csv")

plot <- leaflet::leaflet() %>%
  leaflet::addProviderTiles("Stamen.Toner") %>%
  leaflet::addCircleMarkers(
    data = data,
    lat = ~ latitude,
    lng = ~longitude,
    label = lapply(data$name, htmltools::HTML),
    labelOptions = leaflet::labelOptions(noHide = F, direction = 'auto'),
    stroke = F,
    fillColor = "navy",
    radius = 5
  )
plot


# Takes in the shape file and returns a dataframe

## This is the Iowa CAFO Data that was last updated in

my_spdf <- sf::st_read(
  dsn = "~/Downloads/animal_feeding_operations/"
)

# readr::write_csv(test, file = "iowa_cafo_data.csv")

plot_1 <- leaflet::leaflet() %>%
  leaflet::addProviderTiles("Stamen.Toner") %>%
  leaflet::addCircleMarkers(
    data = my_spdf,
    lat = ~ Latitude,
    lng = ~Longitude,
    label = lapply(my_spdf$FacilityNa, htmltools::HTML),
    labelOptions = leaflet::labelOptions(noHide = F, direction = 'auto'),
    stroke = F,
    fillColor = "navy",
    radius = 5
  )
plot_1
