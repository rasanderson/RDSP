# Import hydrology data and makes it available to main app.R
# Data from GRASS in OS projection (EPSG 27700); transform to lat-lon (4326)
# which is required for leaflet.

# 30 subcatchments
topmod_30catch_sp_os <- st_read("data/topmod_30.shp")
topmod_30catch_sf_os <- st_as_sf(topmod_30catch_sp_os)
topmod_30catch_sf_os <- st_set_crs(topmod_30catch_sf_os, 27700)
topmod_30catch_sf_ll <- st_transform(topmod_30catch_sf_os, 4326)

# River network; use st_geometry as GRASS original does not have attributes
tyne_rivers_os <- st_geometry(st_read("data/tyne_rivers.shp"))
tyne_rivers_os <- st_set_crs(tyne_rivers_os, 27700)
tyne_rivers_ll <- st_transform(tyne_rivers_os, 4326)
