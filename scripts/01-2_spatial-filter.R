##
## Spatial filter (occurrence dataset)
## Remove records outside the Brazilian Blue Amazon
##

### ----- [NO NEED TO RUN THIS SCRIPT AGAIN]

## Libraries ####
library(dplyr)
library(sf)
library(mapview)

## Read data ####

df_turtles_spp <- read.csv("./data-processed/spp_sea-turtles.csv")
df_birds_spp <- read.csv("./data-processed/spp_seabirds.csv")
df_mammals_spp <- read.csv("./data-processed/spp_marine-mammals.csv")

## Read spatial data ####

sf_brazil <-
  sf::read_sf("./data-spatial/Brazil_Federal-states.gpkg") %>%
  sf::st_transform(4326)

sf_blue_amazon <-
  sf::read_sf("./data-spatial/Brazil_Blue-Amazon-extended.gpkg") %>%
  sf::st_transform(4326)
# plot(sf_blue_amazon)

## 'df' to 'sf' ####

## Turtles
NA_turtles_spp <-
  df_turtles_spp %>%
  dplyr::filter(is.na(Latitude) & is.na(Longitude))

sf_turtles_spp <-
  df_turtles_spp %>%
  dplyr::mutate(lat = as.numeric(Latitude),
                lon = as.numeric(Longitude)) %>%
  dplyr::filter(! is.na(lat),
                ! is.na(lon)) %>%
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)

## Birds
NA_birds_spp <-
  df_birds_spp %>%
  dplyr::filter(is.na(Latitude) & is.na(Longitude))

sf_birds_spp <-
  df_birds_spp %>%
  dplyr::mutate(lat = as.numeric(Latitude), # lost 9 records here - why?
                lon = as.numeric(Longitude)) %>%
  dplyr::filter(! is.na(lat),
                ! is.na(lon)) %>%
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)

## Mammals
NA_mammals_spp <-
  df_mammals_spp %>%
  dplyr::filter(is.na(Latitude) & is.na(Longitude))

sf_mammals_spp <-
  df_mammals_spp %>%
  dplyr::mutate(lat = as.numeric(Latitude),
                lon = as.numeric(Longitude)) %>%
  dplyr::filter(! is.na(lat),
                ! is.na(lon)) %>%
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)

## Spatial filter (only data inside Brazil+Blue Amazon), and save them ####

## Create a polygon to identify records outside Brazil+Blue Amazon
sf::sf_use_s2(FALSE)
sf_BRA_plus_BA <-
  sf::st_union(sf::st_make_valid(sf::st_union(sf_brazil)),
               sf::st_make_valid(sf_blue_amazon)) %>%
  sf::st_buffer(dist = 0.05)
sf::sf_use_s2(TRUE)

## Check
# plot(sf_BRA_plus_BA)

## Save the polygon, just in case
# sf::st_write(sf_BRA_plus_BA,
#              "./data-spatial/Brazil-plus-Blue-Amazon.gpkg")

## Find records that do not overlap with BRA_plus_BA polygon
not_inside_turtles <- sf::st_difference(sf_turtles_spp, sf_BRA_plus_BA)
not_inside_birds <- sf::st_difference(sf_birds_spp, sf_BRA_plus_BA)
not_inside_mammals <- sf::st_difference(sf_mammals_spp, sf_BRA_plus_BA)

## Check - good
# mapview(sf_BRA_plus_BA) + not_inside_turtles
# mapview(sf_BRA_plus_BA) + not_inside_birds
# mapview(sf_BRA_plus_BA) + not_inside_mammals

## Remove these records from main dataset
sf_turtles_spp <-
  sf_turtles_spp[! c(sf_turtles_spp$Unique_spp_code %in% not_inside_turtles$Unique_spp_code), ]
sf_birds_spp <-
  sf_birds_spp[! c(sf_birds_spp$Unique_spp_code %in% not_inside_birds$Unique_spp_code), ]
sf_mammals_spp <-
  sf_mammals_spp[! c(sf_mammals_spp$Unique_spp_code %in% not_inside_mammals$Unique_spp_code), ]

## Save, but remember to merge back the rows with Lat/Lon as "NA"
write.csv(rbind((sf_turtles_spp %>% sf::st_drop_geometry()),
                 NA_turtles_spp),
           file = "./data-processed/spp_sea-turtles.csv",
           row.names = F)

write.csv(rbind((sf_birds_spp %>% sf::st_drop_geometry()),
                 NA_birds_spp),
           file = "./data-processed/spp_seabirds.csv",
           row.names = F)

write.csv(rbind((sf_mammals_spp %>% sf::st_drop_geometry()),
                 NA_mammals_spp),
           file = "./data-processed/spp_marine-mammals.csv",
           row.names = F)

rm(not_inside_birds, not_inside_mammals, not_inside_turtles,
   NA_birds_spp, NA_mammals_spp, NA_turtles_spp,
   sf_BRA_plus_BA)

## [EXTRA] Create polygon BR to flag records inland ####

library(rnaturalearth)

# Because 'sf_brazil' has each state border delimitation and I need only the
# country delimitation, it is easier to retrieve the polygon from {rnaturalearth}

sf_brazil_union <- rnaturalearth::ne_countries(country = "brazil",
                                               scale = 10,
                                               returnclass = "sf")
# mapview::mapview(sf_brazil_union)

## Break it down and simplify it a little bit
sf_brazil_union_line <-
  sf_brazil_union %>%
  sf::st_cast('MULTILINESTRING') %>%
  sf::st_cast('LINESTRING', do_split = TRUE) %>%
  dplyr::mutate(npts = mapview::npts(geometry, by_feature = TRUE)) %>% # sf::st_cast('POLYGON') %>%
  dplyr::filter(npts > 40) %>%
  sf::st_union()
# mapview::mapview(sf_brazil_union_line)

## Create a 15km buffer
sf_brazil_buff <- sf::st_buffer(sf_brazil_union_line, dist = 15000)
# mapview::mapview(sf_brazil_buff)

sf_brazil_to_crop_terrestrial <-
  sf::st_difference(sf_brazil_union, sf_brazil_buff) %>%
  dplyr::select(name_en, geometry)
# mapview::mapview(sf_brazil_to_crop_terrestrial)

## Backup this polygon
# sf::st_write(sf_brazil_to_crop_terrestrial,
#              "./data-spatial/polygon-to-crop-terrestrial-records.gpkg")

rm(sf_brazil_union, sf_brazil_union_line, sf_brazil_buff)
