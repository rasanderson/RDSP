# Read in RDS and other data files

rm(list=ls())

rm(list=ls())


r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

topmod_30catch_sf_ll <- readRDS("data/topmod_30catch.RDS")
topmod_30catch_sf_ll <- sf::st_set_crs(topmod_30catch_sf_ll, "EPSG:4386")
tyne_rivers_ll <- readRDS("data/tyne_rivers.RDS")
tyne_rivers_ll <- sf::st_set_crs(tyne_rivers_ll, "EPSG:4386")
AgCensus_sheep_ll <- readRDS("data/AgCensus_sheep_ll.RDS")
AgCensus_cows_ll  <- readRDS("data/AgCensus_cows_ll.RDS")
agcensus <- readRDS("data/agcensus.RDS")
crs(agcensus) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

RTA_ll <- readRDS("data/RTA_ll.RDS")
RTA_ll <- sf::st_set_crs(RTA_ll, "EPSG:4386")
RTA_daily_counts <- readRDS("data/RTA_daily_counts.RDS")
RTA_daily_counts <- ungroup(RTA_daily_counts)
RTA_daily_counts <- sf::st_set_crs(RTA_daily_counts, "EPSG:4386")
ro_region <- readRDS("data/rural_observatory.RDS")
ro_region <- sf::st_set_crs(ro_region, "EPSG:4386")

nbn_araneae <- readRDS("data/nbn_araneae.RDS")
nbn_araneae <- sf::st_set_crs(nbn_araneae, "EPSG:4386")
nbn_araneae <- dplyr::mutate(nbn_araneae, lat=st_coordinates(nbn_araneae)[,2],
                             lng = st_coordinates(nbn_araneae)[,1])
nbn_araneae_family_lst <- as.list(c("All records", sort(as.character(unique(nbn_araneae$family)))))
names(nbn_araneae_family_lst) <- c("All records", sort(as.character(unique(nbn_araneae$family))))
nbn_mammals <- readRDS("data/nbn_mammals17.RDS")
nbn_mammals <- sf::st_set_crs(nbn_mammals, "EPSG:4386")
nbn_mammals <- dplyr::mutate(nbn_mammals, lat=st_coordinates(nbn_mammals)[,2],
                             lng = st_coordinates(nbn_mammals)[,1])
nbn_mammals_family_lst <- as.list(c("All records", sort(as.character(unique(nbn_mammals$family)))))
names(nbn_mammals_family_lst) <- c("All records", sort(as.character(unique(nbn_mammals$family))))
nbn_insecta <- readRDS("data/nbn_insecta17.RDS")
nbn_insecta <- sf::st_set_crs(nbn_insecta, "EPSG:4386")
nbn_insecta <- dplyr::mutate(nbn_insecta, lat=st_coordinates(nbn_insecta)[,2],
                             lng = st_coordinates(nbn_insecta)[,1])
nbn_insecta_family_lst <- as.list(c("All records", sort(as.character(unique(nbn_insecta$family)))))
names(nbn_insecta_family_lst) <- c("All records", sort(as.character(unique(nbn_insecta$family))))
RTA_ll <- readRDS("data/stats19.RDS")
RTA_ll <- sf::st_set_crs(RTA_ll, "EPSG:4386")
transport_a_road <- readRDS("data/transport_a_road.RDS")
transport_a_road <- sf::st_set_crs(transport_a_road, "EPSGL4386")
transport_b_road <- readRDS("data/transport_b_road.RDS")
transport_b_road <- sf::st_set_crs(transport_b_road, "EPSGL4386")
transport_primary <- readRDS("data/transport_primary.RDS")
transport_primary <- sf::st_set_crs(transport_primary, "EPSGL4386")
transport_railway <- readRDS("data/transport_railway.RDS")
transport_railway <- sf::st_set_crs(transport_railway, "EPSGL4386")
