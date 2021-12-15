library(mapview)
library(leaflet)
library(leafem)
library(sf)

# topmod_30catch_sp_os <- st_read("data/topmod_30.shp")
# topmod_30catch_sf_os <- st_as_sf(topmod_30catch_sp_os)
# topmod_30catch_sf_os <- st_set_crs(topmod_30catch_sf_os, 27700)
# topmod_30catch_sf_ll <- st_transform(topmod_30catch_sf_os, 4326)

topmod_30catch_sf_ll <- readRDS("data/topmod_30catch.RDS")
topmod_30catch_sf_ll <- sf::st_set_crs(topmod_30catch_sf_ll, "EPSG:4386")


m <- mapview(topmod_30catch_sf_ll)
leaflet() %>%
 addFeatures(topmod_30catch_sf_ll, group="basins") %>%  # Add default OpenStreetMap map tiles
 addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
 addProviderTiles(providers$OpenStreetMap, group = "Street map") %>%
 addLayersControl(
    baseGroups = c("basins", "Satellite", "Street map"),
    options = layersControlOptions(collapsed = FALSE))  %>%
 addMarkers(lng=-1.6178, lat=54.9783, popup="Newcastle upon Tyne")
