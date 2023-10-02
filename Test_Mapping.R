library(tidyverse)
library(tidygeocoder)



data <- readr::read_csv(file = "global_power_plant_database.csv")

data <- data %>%
  dplyr::filter(country_long == "United States of America")

data_1 <- data %>%
  tidygeocoder::geocode(full_address, method = 'arcgis', lat = latitude, long = longitude)

plot <- leaflet::leaflet() %>%
  leaflet::addProviderTiles("Stamen.Toner") %>%
  leaflet::addCircleMarkers(
    data = data,
    lat = ~ latitude,
    lng = ~longitude,
    label = lapply(data$name, htmltools::HTML),
    labelOptions = leaflet::labelOptions(noHide = F, direction = 'auto'),
    stroke = F,
    fillColor = "navy"
  )
plot
