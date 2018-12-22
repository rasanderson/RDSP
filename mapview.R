library(mapview)
library(leaflet)
# library(rgrass7)
library(sf)

# initGRASS(gisBase="/usr/lib/grass74", location="tyne", mapset="nras",
#           gisDbase="/data/grass", override=TRUE, remove_GISRC=TRUE)
# execGRASS("g.region", res="25")
# execGRASS("g.list", parameters=list(type="vect"))
# topmod_30catch_sp_os <- readVECT("topmod_30catch")
topmod_30catch_sp_os <- st_read("data/topmod_30.shp")
topmod_30catch_sf_os <- st_as_sf(topmod_30catch_sp_os)
topmod_30catch_sf_os <- st_set_crs(topmod_30catch_sf_os, 27700)
topmod_30catch_sf_ll <- st_transform(topmod_30catch_sf_os, 4326)

# osCRS <- CRS("+init=epsg:27700")
# llCRS <- CRS("+proj=longlat +ellps=WGS84")
# proj4string(topmod_30catch_sp_os) <- osCRS
# topmod_30catch_sp_ll <- spTransform(topmod_30catch_sp_os, llCRS)


m <- mapview(topmod_30catch_sf_ll)
leaflet() %>% 
#m@map %>%
  #leaflet() %>%
  #setView(lng = -1.6178, lat = 54.9783, zoom = 10) # %>%
  addFeatures(topmod_30catch_sf_ll, group="basins") %>%  # Add default OpenStreetMap map tiles
  addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>% 
  addProviderTiles(providers$OpenStreetMap, group = "Street map") %>% 
  addLayersControl(
     baseGroups = c("basins", "Satellite", "Street map"),
     options = layersControlOptions(collapsed = FALSE))  %>% 
  addMarkers(lng=-1.6178, lat=54.9783, popup="Newcastle upon Tyne")

  #addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") 
