#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)

r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

ui <- fluidPage(
  titlePanel("Rural Observatory"),
  leafletOutput("mymap"),
  p(),
  actionButton("recalc", "New points")
)

server <- function(input, output, session) {
  
  #points <- eventReactive(input$recalc, {
  #  cbind(rnorm(40) * 2 + 13, rnorm(40) + 48)
  #}, ignoreNULL = FALSE)
   
  output$mymap <- renderLeaflet({
      # #setView(lng = -1.6178, lat = 54.9783, zoom = 9) %>% 
      # addTiles(m@map, group="Basins") %>% 
      # addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>% 
      # addProviderTiles(providers$OpenStreetMap, group = "Street map") %>% 
      # addLayersControl(
      #    baseGroups = c("Basins","Satellite", "Street map"),
      #    options = layersControlOptions(collapsed = FALSE)
      # ) 
      # 
    
    leaflet() %>% 
      addTiles(group = "OSM (default)") %>% 
      #  addMapPane("backdrop", zindex=10) %>% 
      #  addMapPane("frontpane", zindex=20) %>% 
      #addProviderTiles(providers$OpenStreetMap, group = "Street map") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>% 
      #                   options=leafletOptions(pane="backdrop")) %>% 
      addFeatures(topmod_30catch_sf_ll,
                  highlightOptions = highlightOptions(color = "red", weight = 2,
                                                      bringToFront = TRUE),
                  #options=leafletOptions(pane="frontpane"),
                  group="basins") %>%  
      addLayersControl(
        baseGroups = c("OSM (default)", "Satellite"),
        overlayGroups = c("basins"),
        options = layersControlOptions(collapsed = FALSE))  %>% 
      addMarkers(lng=-1.6178, lat=54.9783, popup="Newcastle upon Tyne")
  })
}

shinyApp(ui, server)
