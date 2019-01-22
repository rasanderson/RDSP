# Read road accident records

library(sf)
library(readr)
library(lubridate)
library(ggplot2)
RTA_os <- read_csv("data/road_casualties.csv")
RTA_os$Date <- dmy(RTA_os$Date)
RTA_os <- st_as_sf(RTA_os, coords=c('Easting', 'Northing'))
RTA_os <- st_set_crs(RTA_os, "+init=epsg:27700") # OS GB

# Needs to be in LL to display in leaflet on shiny
RTA_ll <- st_transform(RTA_os, crs=4326) # or 4326
RTA_severity_lst <- list(all = "All records", Slight = "Slight", Serious = "Serious",
                         Fatal = "Fatal")
names(RTA_severity_lst)[1] <- "All records"
RTA_ll <- RTA_ll %>% 
  mutate(lng=st_coordinates(RTA_ll)[,1], lat=st_coordinates(RTA_ll)[,2])
RTA_ll <- 
  RTA_ll %>% 
  mutate(symbol_color = ifelse(`Casualty Severity` == "Slight", "yellow",
                               ifelse(`Casualty Severity` == "Serious", "red",
                                      ifelse(`Casualty Severity` == "Fatal", "black", NA))))


RTA_daily_counts <- RTA_ll %>% group_by(month = floor_date(Date), `Casualty Severity`) %>% 
  summarise(monthly_RTA = n())
ggplot(RTA_daily_counts, aes(x=month, y=monthly_RTA, color = `Casualty Severity`)) +
         geom_smooth()
