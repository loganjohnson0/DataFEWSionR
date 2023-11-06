
library(shiny)
library(tidyverse)
library(leaflet)
library(tidycensus)
library(sf)

iowa_counties <- sf::read_sf("input_data/iowa_county_boarder/") %>%
    dplyr::rename(County_name = Cnty_nm) %>%
    dplyr::mutate(GEOID = as.numeric(GEOID))
census_county <- sf::read_sf("input_data/iowa_county_census_data/")
census_county_subdivision <- sf::read_sf("input_data/iowa_county_subdivision_census_data/")

###

usda_crops <- readr::read_csv(file = "input_data/usda.csv")

usda_crops <- usda_crops %>%
    dplyr::full_join(x = usda_crops, y = iowa_counties, by = "County_name") %>%
    dplyr::select(1:4,9, 16)

usda_crops <- sf::st_as_sf(usda_crops)

###

food_access <- readr::read_csv(file = "input_data/food_access.csv")

food_access <- food_access %>%
    dplyr::full_join(x = food_access, y = iowa_counties, by = "GEOID") %>%
    dplyr::select(1:4, 16)

food_access <- sf::st_as_sf(food_access)

####

climate_opinion <- readr::read_csv(file = "input_data/yale_climate_opinion.csv") %>%
    dplyr::mutate(Year = as.numeric(Year))

climate_opinion <- climate_opinion %>%
    dplyr::full_join(x = climate_opinion, y = iowa_counties, by = "GEOID") %>%
    dplyr::select(1:6, 18)

climate_opinion <- sf::st_as_sf(climate_opinion)

###

real_GDP <- readr::read_csv(file = "input_data/Real_GDP_Iowa_Released_December_2022.csv")


# iowa_cafos <- readr::read_csv(file = "input_data/iowa_cafo_data.csv")



base_map <- leaflet::leaflet() %>%
    leaflet::addProviderTiles(providers$OpenStreetMap)  %>%
    leaflet::fitBounds(-100,-60,60,70) %>%
    leaflet::setView(lng = -93.5, lat = 42, zoom = 6.5)

# pal_num <- leaflet::colorNumeric(
#     palette = colorRamp(c("lightgray", "blue", "red"), interpolate = "spline"), NULL)

pal_num <- leaflet::colorNumeric(
    palette = "magma", NULL)

pal_fct <- leaflet::colorFactor(
    palette = "viridis", NULL)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("DataFEWSion Food Group Project"),

    br(),

    navbarPage(
        title = "Home",

        tabPanel("HomePage",
                 div(class="outer",

                     tags$head(includeCSS("styles.css")),

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

                                   selectInput(inputId = "crop.year", choices = unique(usda_crops$Year),
                                               label = "Choose the Data to Visualize.")



                                   # sliderInput(inputId = "crop.year",
                                   #             min = min(unique(usda_crops$Year)),
                                   #             max = max(unique(usda_crops$Year)),
                                   #             label = "Choose the Year to Visualize",
                                   #             value = min(unique(usda_crops$Year)))
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

                     ))),
        tabPanel("Climate Opinion",
                 div(class="outer",

                     tags$head(includeCSS("styles.css")),

                     leafletOutput("climate.opinion", height = "125%", width = "100%"),

                     absolutePanel(id = "controls", class = "panel panel-default",
                                   fixed = TRUE, draggable = TRUE, top = 150, right = 55,
                                   width = 300, height = "auto",

                                   selectInput(inputId = "climate",
                                               choices = unique(climate_opinion$Attribute),
                                               label = "Choose the Data to Visualize."),

                                   selectInput(inputId = "climate.year",
                                               label = "Choose the Year to Visualize",
                                               choices = unique(climate_opinion$Year))

                     )))
    ),
    fillPage = TRUE
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {

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

    observe({
        climate_filtered <- climate_opinion %>%
            dplyr::filter(Attribute %in% input$climate)

        climate_year <- unique(climate_filtered$Year)

        updateSelectInput(session,
                          "climate.year",
                          choices = climate_year)
    })

    output$climate.opinion <- renderLeaflet({
        base_map %>%
            leaflet::addPolygons(data = climate_opinion %>%
                                     dplyr::filter(Attribute %in% input$climate) %>%
                                     dplyr::filter(Year %in% input$climate.year),
                                 weight = 1,
                                 smoothFactor = 0.3,
                                 color = "black",
                                 fillOpacity = 0.5,
                                 fillColor = ~ pal_num(climate_opinion %>%
                                                           dplyr::filter(Attribute %in% input$climate) %>%
                                                           dplyr::filter(Year %in% input$climate.year) %>%
                                                           dplyr::pull(Value)),
                                 label = ~ as.character(climate_opinion %>%
                                                            dplyr::filter(Attribute %in% input$climate) %>%
                                                            dplyr::filter(Year %in% input$climate.year) %>%
                                                            dplyr::pull(Value)))
    })


}


# Run the application
shinyApp(ui = ui, server = server)
