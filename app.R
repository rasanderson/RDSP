# Libraries needed for app
library(leaflet)
library(mapview)
library(shiny)
library(shinydashboard)
library(sf)

rm(list=ls())


r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

source("hydrology.R")

ui <- dashboardPage(title = "Newcastle University Rural Observatory" ,
  dashboardHeader(title="Rural Observatory"),
  dashboardSidebar(),
  dashboardBody(
    navbarPage("",
               
    tabPanel("Home",
             h1("Newcastle University Rural Observatory"),
             h2("Introduction"),
             p("Welcome to the Rural Observatory (beta). This website give access
               to physical, environmental, socio-economic and medical data for
               North East England. Navigate through the website using the tab
               buttons across the top, and the menu bars on the left.")
             ),
    
    tabPanel("Physical",
             h1("Physical landscape"),
             p("Here you can access elevation, meteorology etc.")),
    
    tabPanel(title="Hydrology",
             h1("Hydrology and river networks"),
             p("Information on sub-catchments for the River Tyne"),
             fluidPage(
               leafletOutput("mymap"),
               p(),
               actionButton("recalc", "New points")
             )
    )
)))

server <- function(input, output, session) {
  
  output$mymap <- renderLeaflet({

    leaflet() %>%
      addTiles(group = "OSM (default)") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
      addFeatures(topmod_30catch_sf_ll,
                  highlightOptions = highlightOptions(color = "red", weight = 2,
                                                      bringToFront = TRUE),
                  group="basins") %>%
      addLayersControl(
        baseGroups = c("OSM (default)", "Satellite"),
        overlayGroups = c("basins"),
        options = layersControlOptions(collapsed = FALSE))  %>%
      addMarkers(lng=-1.6178, lat=54.9783, popup="Newcastle upon Tyne")
  })
}

shinyApp(ui, server)

