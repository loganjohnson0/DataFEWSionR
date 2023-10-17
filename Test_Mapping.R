library(tidyverse)
library(tidygeocoder)
library(leaflet)
library(leaflet.extras)
library(leaflet.extras2)

# This is power plant data from the WRI that is last updated in 2018

data <- readr::read_csv(file = "usa_power_plant_data_world_resources_institure.csv",
                        col_types = "ccccnnnffffnccccnnnnnnnnncnnnnnccccc")

data <- data %>%
  dplyr::filter(latitude > 40.3754,
                latitude < 43.5000,
                longitude > -96.6395,
                longitude < -90.1401)

counties <- tigris::counties(state = "IA", cb = TRUE, class = "sf") %>%
  sf::st_transform("+proj=longlat +datum=WGS84") #Reproject to WFS84

counties$NAME <- stringr::str_replace_all(string = counties$NAME, pattern = "'", replacement = " ")
counties$NAMELSAD <- stringr::str_replace_all(string = counties$NAMELSAD, pattern = "'", replacement = " ")

my_spdf <- readr::read_csv(file = "iowa_cafo_data.csv")

my_spdf <- my_spdf %>%
  dplyr::filter(Swine_Grow > 1)

stuff <- readr::read_csv(file = "usda.csv",
                         col_select = !c(1)) %>%
  dplyr::mutate(CountyName = stringr::str_to_title(CountyName))

counties_1 <- as.data.frame(counties)

counties_1 <- counties_1 %>%
  dplyr::select(1:7) %>%
  dplyr::mutate(CountyName = NAME) %>%
  dplyr::left_join(x = ., y = stuff, by = "CountyName")


pal_num <- leaflet::colorNumeric(
  palette = "viridis", NULL)

pal_fct <- leaflet::colorFactor(
  palette = "viridis", NULL)

leaflet::leaflet(data = data) %>%
  leaflet::addProviderTiles("Stamen.Toner") %>%
  leaflet::addCircleMarkers(
    fillColor = ~ pal_fct(data$primary_fuel),
    stroke = FALSE,
    lat = ~ latitude,
    lng = ~ longitude,
    label = lapply(data$name, htmltools::HTML),
    labelOptions = leaflet::labelOptions(noHide = F, direction = 'auto'),
    radius = 5,
    fillOpacity = 0.8)


leaflet::leaflet() %>%
  leaflet::addProviderTiles("Stamen.Toner") %>%
  leaflet::addPolygons(data = counties,
                       weight = 1,
                       smoothFactor = 0.3,
                       color = "black",
                       fillOpacity = 0) %>%
  leaflet.extras::addHeatmap(
    data = my_spdf,
    lat = ~ Latitude,
    lng = ~ Longitude,
    intensity = ~ Swine_Grow,
    radius = 10) %>%
  leaflet::addCircleMarkers(
    data = my_spdf,
    lat = ~ Latitude,
    lng = ~ Longitude,
    label = lapply(my_spdf$FacilityNa, htmltools::HTML),
    labelOptions = leaflet::labelOptions(noHide = F, direction = 'auto'),
    stroke = F,
    fillColor = pal_fct(my_spdf$OpeartionT),
    radius = 2) %>%
  leaflet::addScaleBar(position = "bottomleft")



leaflet::leaflet() %>%
  leaflet::addProviderTiles("Stamen.Toner") %>%
  leaflet::addPolygons(data = counties,
                       fillColor = ~ pal_num(counties_1$CGY),
                       weight = 1,
                       smoothFactor = 0.3,
                       color = "black",
                       fillOpacity = 0.7,
                       label = ~ as.character(counties_1$CGY)) %>%
  leaflet::addScaleBar(position = "bottomleft") %>%
  leaflet::addLegend(position = "bottomright",
                     pal = pal_num,
                     values = counties_1$CGY)

