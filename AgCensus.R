# Agricultural Census

# AgCensus_sheep_os <- raster("data/AgCensus_sheep_2km.tif")
# AgCensus_cows_os  <- raster("data/AgCensus_cows_2km.tif")
# AgCensus_sheep_os <- projectRaster(AgCensus_sheep_os, crs = "+init=epsg:27700")
# AgCensus_cows_os  <- projectRaster(AgCensus_cows_os, crs = "+init=epsg:27700")
# AgCensus_sheep_ll <- projectRaster(AgCensus_sheep_os, crs = "+init=epsg:4326")
# AgCensus_cows_ll  <- projectRaster(AgCensus_cows_os, crs = "+init=epsg:4326")
# 
# saveRDS(AgCensus_sheep_ll, "data/AgCensus_sheep_ll.RDS")
# saveRDS(AgCensus_cows_ll, "data/AgCensus_cows_ll.RDS")

# Revised AgCensus data for new RO area
agcensus <- readRDS("data/agcensus.RDS")
crs(agcensus) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

library(leaflet)


paltcow <- colorBin(c("#FFFFCC", "#c2e699", "#78c679", "#31a354", "#006837"),
                   values(agcensus$tot_cattle), na.color = "transparent")
palbeef <- colorBin(c("#FFFFCC", "#c2e699", "#78c679", "#31a354", "#006837"),
                    values(agcensus$beef), na.color = "transparent")
paldairy <- colorBin(c("#FFFFCC", "#c2e699", "#78c679", "#31a354", "#006837"),
                    values(agcensus$dairy), na.color = "transparent")
palpigs <- colorBin(c("#FFFFCC", "#c2e699", "#78c679", "#31a354", "#006837"),
                    values(agcensus$pigs), na.color = "transparent")
palpoultry <- colorBin(c("#FFFFCC", "#c2e699", "#78c679", "#31a354", "#006837"),
                    values(agcensus$poultry), na.color = "transparent")
palsheep <- colorBin(c("#FFFFCC", "#c2e699", "#78c679", "#31a354", "#006837"),
                     values(agcensus$sheep), na.color = "transparent")

leaflet(options = leafletOptions(minZoom = 7.25, zoomDelta=0.1, zoomSnap=0.05)) %>%
  addTiles(group = "OSM (default)") %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
  addRasterImage(agcensus$tot_cattle,  colors=paltcow,   group="Cattle", opacity=0.6) #%>% 
  addRasterImage(agcensus$beef, colors=palbeef, group="Beef", opacity=0.6) %>%
  addRasterImage(agcensus$dairy, colors=paldairy, group="Dairy", opacity=0.6) %>%
  addRasterImage(agcensus$pigs, colors=palpigs, group="Pigs", opacity=0.6) %>%
  addRasterImage(agcensus$poultry, colors=palpoultry, group="Poultry", opacity=0.6) %>%
  addRasterImage(agcensus$sheep, colors=palsheep, group="Sheep", opacity=0.6) %>%
  addLayersControl(
    baseGroups = c("OSM (default)", "Satellite"),
    overlayGroups = c("Cattle", "Beef", "Dairy", "Pigs", "Poultry", "Sheep"),
    options = layersControlOptions(collapsed = FALSE))  %>%
  addLegend(pal=paltcow,  values=values(agcensus$tot_cattle), title="Tot Cattle/km", group="Cattle") %>%
  addLegend(pal=palbeef, values=values(agcensus$beef), title="Beef cattle/km", group="Beef") %>%
  addLegend(pal=paldairy, values=values(agcensus$dairy), title="Dairy/km", group="Dairy") %>%
  addLegend(pal=palpigs, values=values(agcensus$pigs), title="Pigs/km", group="Pigs") %>%
  addLegend(pal=palpoultry, values=values(agcensus$poultry), title="Poultry/km", group="Poultry") %>%
  addLegend(pal=palsheep, values=values(agcensus$sheep), title="Sheep/km", group="Sheep") %>%
  setView(lng = -2, lat = 54, zoom=4) %>% 
  setMaxBounds(lng1 = as.numeric(st_bbox(ro_region)[1]),
               lat1 = as.numeric(st_bbox(ro_region)[2]),
               lng2 = as.numeric(st_bbox(ro_region)[3]),
               lat2 = as.numeric(st_bbox(ro_region)[4])) %>%
  hideGroup(c("Beef","Dairy", "Pigs", "Poultry", "Sheep"))

