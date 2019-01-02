# Libraries needed for app
library(leaflet)
library(mapview)
library(shiny)
library(shinydashboard)
library(sf)
library(rgdal)
library(raster)
library(dplyr)

rm(list=ls())


r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

source("hydrology.R")
source("AgCensus.R")
source("National_Biodiversity_Network.R")

ui <- dashboardPage(title = "Newcastle University Rural Observatory" ,
  dashboardHeader(title="Rural Observatory"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName="home", icon=icon("home")),
      menuItem("Biodiversity", tabName = "nbn", icon = icon("bug")),
      menuItem("Farming", tabName = "AgCensus", icon = icon("grain", lib="glyphicon")),
      menuItem("Hydrology", tabName = "Hydrology", icon = icon("tint"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "home", # UI Home ####
              h1("Newcastle University Rural Observatory"),
              h2("Introduction"),
              h3("Welcome to the Rural Observatory (beta). This website give access
                to physical, environmental, socio-economic and medical data for
                North East England. Navigate through the website using the menu 
                bars on the left.")
      ),
      tabItem(tabName = "nbn", # UI Biodiversity ####
              h1("National Biodiversity Atlas"),
              h4("Here you can access selected species distribution data from the
                 National Biodiversit Atlas (previously the National Biodiversity
                 Network)."),
              fluidPage(
                sidebarLayout(
                  sidebarPanel(
                    selectInput(inputId = "nbn_select", h2("Family"), 
                                choices = nbn_family_lst) #,
                    #verbatimTextOutput("out1")
                  ),
                  mainPanel(
                    leafletOutput("nbn_map")
                  )
                )
              )
      ),
      tabItem(tabName = "AgCensus", # UI AgCensus ####
              h1("Agricultural census data"),
              h4("Summary data from the Agricultural census, 2km resolution"),
              fluidPage(
                leafletOutput("census_map")
              )
      ),
      tabItem(tabName = "Hydrology", # UI Hydrology ####
              h1("Hydrology and river networks"),
              h4("Information on sub-catchments for the River Tyne"),
              fluidPage(
                leafletOutput("hydrology_map")
              )
      )
    )
  )
)


server <- function(input, output, session) {
  
  output$nbn_map  <- renderLeaflet({ # Server Biodiversity ####
    nbn_subset_plot <- reactive(
      if(input$nbn_select=="All records"){
        nbn_subset_ll
      } else {
        dplyr::filter(nbn_subset_ll, family==input$nbn_select)
      }
    )

    leaflet(options = leafletOptions(minZoom = 8, zoomDelta=0.05, zoomSnap=0.05)) %>%
      addTiles(group = "OSM (default)") %>%
      addCircleMarkers(lng = nbn_subset_plot()$lng, lat = nbn_subset_plot()$lat,
                       popup = paste("<b>Taxonomy:</b> ", nbn_subset_plot()$spp,"<br/>",
                                    "<b>Year:</b> ",  nbn_subset_plot()$year)) %>% 
      setView(lng = -2, lat = 55, zoom = 8.6)  %>%
      setMaxBounds(lng1 = -1.5,
                   lat1 = 54.5,
                   lng2 = -2.5,
                   lat2 = 55.5)
  })
  
  output$out1 <- renderPrint(input$nbn_select)
  
  output$census_map <- renderLeaflet({ # Server AgCensus ####
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
      setView( lng = -2, lat = 55, zoom = 8.6)  %>%
      setMaxBounds(lng1 = -1.5,
                   lat1 = 54.5,
                   lng2 = -2.5,
                   lat2 = 55.5) %>% 
      hideGroup("Sheep")
  })

  output$hydrology_map <- renderLeaflet({ # Server hydrology ####
    leaflet(options = leafletOptions(minZoom = 8, zoomDelta=0.05, zoomSnap=0.05)) %>%
      addTiles(group = "OSM (default)") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
      addProviderTiles(providers$Esri.WorldShadedRelief, group = "Relief") %>% 
      addFeatures(topmod_30catch_sf_ll,
                  highlightOptions = highlightOptions(color = "red", weight = 2,
                                                      bringToFront = TRUE),
                  group="Tyne subcatchments") %>%
      addFeatures(tyne_rivers_ll, group="River network") %>% 
      addLayersControl(
        baseGroups = c("OSM (default)", "Satellite", "Relief"),
        overlayGroups = c("Tyne subcatchments", "River network"),
        options = layersControlOptions(collapsed = FALSE))  %>%
      addMarkers(lng=-1.6178, lat=54.9783, popup="Newcastle upon Tyne") %>% 
      setView(lng = -2, lat = 55, zoom = 8.6)  %>%
      setMaxBounds(lng1 = -1.5,
                   lat1 = 54.5,
                   lng2 = -2.5,
                   lat2 = 55.5 ) %>% 
    hideGroup("Tyne subcatchments")
  })
}

shinyApp(ui, server)

