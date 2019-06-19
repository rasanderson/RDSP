# Agricultural Census

AgCensus_sheep_os <- raster("data/AgCensus_sheep_2km.tif")
AgCensus_cows_os  <- raster("data/AgCensus_cows_2km.tif")
AgCensus_sheep_os <- projectRaster(AgCensus_sheep_os, crs = "+init=epsg:27700")
AgCensus_cows_os  <- projectRaster(AgCensus_cows_os, crs = "+init=epsg:27700")
AgCensus_sheep_ll <- projectRaster(AgCensus_sheep_os, crs = "+init=epsg:4326")
AgCensus_cows_ll  <- projectRaster(AgCensus_cows_os, crs = "+init=epsg:4326")

saveRDS(AgCensus_sheep_ll, "data/AgCensus_sheep_ll.RDS")
saveRDS(AgCensus_cows_ll, "data/AgCensus_cows_ll.RDS")
