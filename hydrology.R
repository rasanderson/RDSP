# Import hydrology data and makes it available to main app.R

topmod_30catch_sp_os <- st_read("data/topmod_30.shp")
topmod_30catch_sf_os <- st_as_sf(topmod_30catch_sp_os)
topmod_30catch_sf_os <- st_set_crs(topmod_30catch_sf_os, 27700)
topmod_30catch_sf_ll <- st_transform(topmod_30catch_sf_os, 4326)

