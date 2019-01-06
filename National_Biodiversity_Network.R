records_nbn <- readRDS("data/Diptera.RDS")
# class(records_nbn)
# library(sf)
# library(dplyr)
records_ll <- st_as_sf(records_nbn$data, coords=c("longitudeWGS84", "latitudeWGS84"))
records_ll <- st_set_crs(records_ll, 4326)
nbn_subset_ll <- records_ll %>% 
  dplyr::select(rank, order, family, genus, spp=scientificNameOriginal, year=endYear, recorder) %>% 
  mutate(lng=st_coordinates(records_ll)[,1], lat=st_coordinates(records_ll)[,2])

nbn_family_lst <- as.list(c("All records", sort(unique(nbn_subset_ll$family))))
names(nbn_family_lst) <- c("All records", sort(unique(nbn_subset_ll$family)))
nbn_family_lst <- nbn_family_lst[nbn_family_lst[] != ""] # Remove un-named records                        

# library(leaflet)
# library(mapview)
# nbn_subset_ll <- dplyr::filter(subset_ll, year==1978, genus=="Machimus")

# nbn_subset_ll %>% 
#   leaflet() %>% 
#   addTiles() %>% 
#   addCircleMarkers(lng= ~lng, lat= ~lat,
#                    popup= ~spp)
# leaflet() %>%
#    addTiles() %>%
#    addCircleMarkers(lng= nbn_subset_ll$lng, lat= nbn_subset_ll$lat,
#                     popup= nbn_subset_ll$spp)
# 
# leaflet(options = leafletOptions(minZoom = 8, zoomDelta=0.05, zoomSnap=0.05)) %>%
#   addTiles(group = "OSM (default)") %>%
#   addCircleMarkers(lng = nbn_subset_ll$lng, lat = nbn_subset_ll$lat,
#                    popup = nbn_subset_ll$spp) %>% 
# setView( lng = -2
#          , lat = 55
#          , zoom = 8.6 )  %>%
#   setMaxBounds( lng1 = -1.5
#                 , lat1 = 54.5
#                 , lng2 = -2.5
#                 , lat2 = 55.5 )
# 
