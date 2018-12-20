library(leaflet)

#m <- leaflet() %>% setView(lng = -1.6178, lat = 54.9783, zoom = 10)
#m %>% addTiles()
#m %>%
#leaflet() %>%
  # setView(m@map, lng = -1.6178, lat = 54.9783, zoom = 10) %>% 
  #  addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>% 
  # addTiles(topmod_30catch_sf_ll, group="basins") %>% 
  # addTiles(m@map) %>% 
  # addProviderTiles(providers$OpenStreetMap, group = "Street map") %>% 
  # addLayersControl(
  #    baseGroups = c("Satellite", "Street map"),
  #                   options = layersControlOptions(collapsed = FALSE)
  # )

  
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

# Check whether ESRI Online available for other maps
library(leaflet.esri)
