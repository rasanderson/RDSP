# Libraries needed for app
library(leaflet)
library(mapview)
library(shiny)
library(shinydashboard)
library(sf)
library(rgdal)
library(raster)

rm(list=ls())


r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

source("hydrology.R")
source("AgCensus.R")

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
    tabPanel(title = "Physical",
             h1("Physical landscape"),
             p("Here you can access elevation, meteorology etc."),
             fluidPage(
               leafletOutput("physical_map")
             )),
    tabPanel(title = "Livestock",
             h1("Agricultural census data"),
             p("Summary data from the Agricultural census, 2km resolution"),
             fluidPage(
               leafletOutput("census_map")
             )),
    tabPanel(title = "Hydrology",
             h1("Hydrology and river networks"),
             p("Information on sub-catchments for the River Tyne"),
             fluidPage(
               leafletOutput("hydrology_map")
             ))
)))

server <- function(input, output, session) {
  
  output$physical_map  <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 8, zoomDelta=0.05, zoomSnap=0.05)) %>%
      addTiles(group = "OSM (default)") %>%
      addProviderTiles(providers$Esri.WorldTerrain, group = "Satellite") %>% 
      setView( lng = -2
               , lat = 55
               , zoom = 8.6 )  %>%
      setMaxBounds( lng1 = -1.5
                    , lat1 = 54.5
                    , lng2 = -2.5
                    , lat2 = 55.5 )
  })
  
  output$census_map <- renderLeaflet({
    # Colours: http://colorbrewer2.org/#type=sequential&scheme=YlGn&n=3 
    palcow <- colorBin(c("#FFFFCC", "#c2e699", "#78c679", "#31a354", "#006837"),
                       values(AgCensus_cows_ll), na.color = "transparent")
    palsheep <- colorBin(c("#FFFFCC", "#c2e699", "#78c679", "#31a354", "#006837"),
                         values(AgCensus_sheep_ll), na.color = "transparent")
    
    leaflet(options = leafletOptions(minZoom = 8, zoomDelta=0.05, zoomSnap=0.05)) %>%
      addTiles(group = "OSM (default)") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
      addRasterImage(AgCensus_cows_ll,  colors=palcow,   group="Cattle", opacity=0.6) %>% 
      addRasterImage(AgCensus_sheep_ll, colors=palsheep, group="Sheep", opacity=0.6) %>%
      addLayersControl(
        baseGroups = c("OSM (default)", "Satellite"),
        overlayGroups = c("Cattle", "Sheep"),
        options = layersControlOptions(collapsed = FALSE))  %>%
      addLegend(pal=palcow,   values=values(AgCensus_cows_ll), title="Cattle/km", group="Cattle") %>%
      addLegend(pal=palsheep, values=values(AgCensus_sheep_ll), title="Sheep/km", group="Sheep") %>%
      setView( lng = -2
               , lat = 55
               , zoom = 8.6 )  %>%
      setMaxBounds( lng1 = -1.5
                    , lat1 = 54.5
                    , lng2 = -2.5
                    , lat2 = 55.5 ) %>% 
      hideGroup("Sheep")
  })

  output$hydrology_map <- renderLeaflet({
    leaflet(options = leafletOptions(minZoom = 8, zoomDelta=0.05, zoomSnap=0.05)) %>%
      addTiles(group = "OSM (default)") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
      addFeatures(topmod_30catch_sf_ll,
                  highlightOptions = highlightOptions(color = "red", weight = 2,
                                                      bringToFront = TRUE),
                  group="Tyne subcatchments") %>%
      addFeatures(tyne_rivers_ll, group="River network") %>% 
      addLayersControl(
        baseGroups = c("OSM (default)", "Satellite"),
        overlayGroups = c("Tyne subcatchments", "River network"),
        options = layersControlOptions(collapsed = FALSE))  %>%
      addMarkers(lng=-1.6178, lat=54.9783, popup="Newcastle upon Tyne") %>% 
      setView( lng = -2
               , lat = 55
               , zoom = 8.6 )  %>%
      setMaxBounds( lng1 = -1.5
                    , lat1 = 54.5
                    , lng2 = -2.5
                    , lat2 = 55.5 ) %>% 
    hideGroup("Tyne subcatchments")
  })
}

shinyApp(ui, server)

