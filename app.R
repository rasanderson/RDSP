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
