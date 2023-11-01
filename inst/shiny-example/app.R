
library(shiny)
library(tidyverse)
library(leaflet)
library(tidycensus)
library(sf)

iowa_counties <- sf::read_sf("input_data/iowa_county_boarder/") %>%
    dplyr::rename(County_name = Cnty_nm)
census_county <- sf::read_sf("input_data/iowa_county_census_data/")
census_county_subdivision <- sf::read_sf("input_data/iowa_county_subdivision_census_data/")
# real_GDP <- readr::read_csv(file = "input_data/Real_GDP_Iowa_Released_December_2022.csv")
# iowa_cafos <- readr::read_csv(file = "input_data/iowa_cafo_data.csv")


usda_crops <- sf::read_sf("input_data/usda_crop_data/") %>%
    dplyr::rename(County_name = Cnty_nm,
                  Attribute = Attribt)

food_access <- sf::read_sf("input_data/food_access_data/") %>%
    dplyr::rename(Percent_low_access_to_store_households = pct_lc_,
                  Grocery_stores_per_thousand = grocpth,
                  Supercenters_club_stores_per_thousand = sprcpth,
                  Convenience_stores_per_thousand = cnvspth,
                  Specialized_food_stores_per_thousand = spcspth,
                  Fast_food_restaurants_per_thousand = ffrpth,
                  Farmers_markets_per_thousand = fmrktpt,
                  Food_insecurity_rate = fd_nsc_,
                  County_name = cnty_nm)  %>%
    tidyr::pivot_longer(cols = c(2:9),
                        names_to = "Attribute",
                        values_to = "Value")


base_map <- leaflet::leaflet() %>%
    leaflet::addProviderTiles(providers$OpenStreetMap)  %>%
    leaflet::fitBounds(-100,-60,60,70) %>%
    leaflet::setView(lng = -93.5, lat = 41, zoom = 6.5)

pal_num <- leaflet::colorNumeric(
    palette = colorRamp(c("lightgray", "blue", "red"), interpolate = "spline"), NULL)

pal_fct <- leaflet::colorFactor(
    palette = "viridis", NULL)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("DataFEWSion Project"),

    br(),

    navbarPage(
        title = "Home",

        tabPanel("HomePage",
                 div(class="outer",

                     tags$head(includeCSS("styles.css")),

                     h3("Welcome to our Shiny Application! Here is an overview on Iowa Population Statistics."),

                     leafletOutput("iowa.census", height = "125%", width = "100%"))),

        tabPanel("Crop and Livestock Statistics",

                 div(class="outer",

                     tags$head(includeCSS("styles.css")),

                     leafletOutput("usda.crops", height = "125%", width = "100%"),

                     absolutePanel(id = "controls", class = "panel panel-default",
                                   fixed = TRUE, draggable = TRUE, top = 150, right = 55,
                                   width = 300, height = "auto",

                                   selectInput(inputId = "crop.stat", choices = unique(usda_crops$Attribute),
                                               label = "Choose the Data to Visualize."),

                                   sliderInput(inputId = "crop.year",
                                               min = min(unique(usda_crops$Year)),
                                               max = max(unique(usda_crops$Year)),
                                               label = "Choose the Year to Visualize",
                                               value = min(unique(usda_crops$Year)))
                     ))),
        tabPanel("Food Access Statistics",
                 div(class="outer",

                     tags$head(includeCSS("styles.css")),

                     leafletOutput("food.access", height = "125%", width = "100%"),

                     absolutePanel(id = "controls", class = "panel panel-default",
                                   fixed = TRUE, draggable = TRUE, top = 150, right = 55,
                                   width = 300, height = "auto",

                                   selectInput(inputId = "food.stat", choices = unique(food_access$Attribute),
                                               label = "Choose the Data to Visualize.")

                     )))
    ),
    fillPage = TRUE
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$iowa.census <- renderLeaflet({
        base_map %>%
            leaflet::addPolygons(data = census_county %>%
                                     dplyr::filter(Year == "2000"),
                                 weight = 1,
                                 smoothFactor = 0.3,
                                 color = "black",
                                 fillOpacity = 0.5,
                                 group = "County 2000",
                                 fillColor = ~ pal_num(Population),
                                 label = ~ as.character(Population)) %>%
            leaflet::addPolygons(data = census_county %>%
                                     dplyr::filter(Year == "2010"),
                                 weight = 1,
                                 smoothFactor = 0.3,
                                 color = "black",
                                 fillOpacity = 0.5,
                                 group = "County 2010",
                                 fillColor = ~ pal_num(Population),
                                 label = ~ as.character(Population)) %>%
            leaflet::addPolygons(data = census_county %>%
                                     dplyr::filter(Year == "2020"),
                                 weight = 1,
                                 smoothFactor = 0.3,
                                 color = "black",
                                 fillOpacity = 0.5,
                                 group = "County 2020",
                                 fillColor = ~ pal_num(Population),
                                 label = ~ as.character(Population)) %>%
            leaflet::addPolygons(data = census_county_subdivision %>%
                                     dplyr::filter(Year == "2010"),
                                 weight = 1,
                                 smoothFactor = 0.3,
                                 color = "black",
                                 fillOpacity = 0.5,
                                 group = "Census Subdivision 2010",
                                 fillColor = ~ pal_num(Population),
                                 label = ~ as.character(Population)) %>%
            leaflet::addPolygons(data = census_county_subdivision %>%
                                     dplyr::filter(Year == "2020"),
                                 weight = 1,
                                 smoothFactor = 0.3,
                                 color = "black",
                                 fillOpacity = 0.5,
                                 group = "Census Subdivision 2020",
                                 fillColor = ~ pal_num(Population),
                                 label = ~ as.character(Population)) %>%
            leaflet::addLayersControl(baseGroups = c("County 2000",
                                                     "County 2010",
                                                     "County 2020",
                                                     "Census Subdivision 2010",
                                                     "Census Subdivision 2020"),
                                      options = leaflet::layersControlOptions(collapsed = FALSE))
    })

    output$usda.crops <- renderLeaflet({
        base_map %>%
            leaflet::addPolygons(data = usda_crops %>%
                                     dplyr::filter(Attribute %in% input$crop.stat) %>%
                                     dplyr::filter(Year %in% input$crop.year),
                                 weight = 1,
                                 smoothFactor = 0.3,
                                 color = "black",
                                 fillOpacity = 0.5,
                                 fillColor = ~ pal_num(usda_crops %>%
                                                           dplyr::filter(Attribute %in% input$crop.stat) %>%
                                                           dplyr::filter(Year %in% input$crop.year) %>%
                                                           dplyr::pull(Value)),
                                 label = ~ as.character(usda_crops %>%
                                                            dplyr::filter(Attribute %in% input$crop.stat) %>%
                                                            dplyr::filter(Year %in% input$crop.year) %>%
                                                            dplyr::pull(Value)))
    })

    output$food.access <- renderLeaflet({
        base_map %>%
            leaflet::addPolygons(data = food_access%>%
                                     dplyr::filter(Attribute %in% input$food.stat),
                                 weight = 1,
                                 smoothFactor = 0.3,
                                 color = "black",
                                 fillOpacity = 0.5,
                                 fillColor = ~ pal_num(food_access %>%
                                                           dplyr::filter(Attribute %in% input$food.stat) %>%
                                                           dplyr::pull(Value)),
                                 label = ~ as.character(food_access %>%
                                                            dplyr::filter(Attribute %in% input$food.stat) %>%
                                                            dplyr::pull(Value)))
    })
}


# Run the application
shinyApp(ui = ui, server = server)
