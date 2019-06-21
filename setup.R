# Read in RDS and other data files

rm(list=ls())

rm(list=ls())


r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

topmod_30catch_sf_ll <- readRDS("data/topmod_30catch.RDS")
tyne_rivers_ll <- readRDS("data/tyne_rivers.RDS")
AgCensus_sheep_ll <- readRDS("data/AgCensus_sheep_ll.RDS")
AgCensus_cows_ll  <- readRDS("data/AgCensus_cows_ll.RDS")
nbn_subset_ll <- readRDS("data/nbn_subset_ll.RDS")
nbn_family_lst <- readRDS("data/nbn_family_lst.RDS")
RTA_ll <- readRDS("data/RTA_ll.RDS")
RTA_daily_counts <- readRDS("data/RTA_daily_counts.RDS")
ro_region <- readRDS("data/rural_observatory.RDS")
agcensus <- readRDS("data/agcensus.RDS")
nbn_araneae <- readRDS("data/nbn_araneae.RDS")
nbn_araneae <- dplyr::mutate(nbn_araneae, lat=st_coordinates(nbn_araneae)[,2],
                             lng = st_coordinates(nbn_araneae)[,1])
nbn_araneae_family_lst <- as.list(c("All records", sort(as.character(unique(nbn_araneae$family)))))
names(nbn_araneae_family_lst) <- c("All records", sort(as.character(unique(nbn_araneae$family))))

  