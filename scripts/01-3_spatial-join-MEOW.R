##
## Spatial join with MEOWs + Blue Amazon (Ecoregions and Provinces)
## 
##

## Libraries ####
library(dplyr)
library(sf)
# library(mapview) # For visualization only

## Read data ####

df_turtles_spp <- read.csv("./data-processed/spp_sea-turtles.csv")
df_birds_spp <- read.csv("./data-processed/spp_seabirds.csv")
df_mammals_spp <- read.csv("./data-processed/spp_marine-mammals.csv")

## 'df' to 'sf' ####

# Note some records didn't have lat/lon.
# These are stored in the 'NA_*' objects below and will 
# be rbind()'ed back to the dataset at the final stage of the spatial wrangling.

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

## Read spatial data ####

sf_meow <-
  sf::read_sf("./data-spatial/MEOW-plus-Blue-Amazon.gpkg") %>%
  sf::st_transform(4326)

sf_brazil_inland <-
  sf::read_sf("./data-spatial/polygon-to-crop-terrestrial-records.gpkg") %>%
  sf::st_transform(4326)
# mapview::mapview(sf_brazil_inland)

## Remove oceanic and Marajó Islands
sf_brazil_inland <- 
  sf::st_cast(sf_brazil_inland, 'MULTILINESTRING') %>% 
  sf::st_cast('LINESTRING', do_split = TRUE) %>%
  dplyr::mutate(npts = mapview::npts(geom, by_feature = TRUE)) %>%
  sf::st_cast('POLYGON')

sf_brazil_inland <- 
  sf_brazil_inland %>% 
  dplyr::filter(npts == 667) %>%  ## Select only 'mainland'
  sf::st_union() %>%
  sf::st_sf()
# mapview::mapview(sf_brazil_inland)

## Spatial join with MEOWs+BA ####

sf::sf_use_s2(FALSE) # Turn off to allow spatial operations

sf_turtles_spp <- 
  sf::st_join((df_turtles_spp %>% 
                 dplyr::mutate(lat = as.numeric(Latitude),
                               lon = as.numeric(Longitude)) %>%
                 dplyr::filter(! is.na(lat),
                               ! is.na(lon)) %>% 
                 sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)),
              sf_meow,
              join = st_nearest_feature)

sf_birds_spp <- 
  sf::st_join((df_birds_spp %>% 
                 dplyr::mutate(lat = as.numeric(Latitude),
                               lon = as.numeric(Longitude)) %>%
                 dplyr::filter(! is.na(lat),
                               ! is.na(lon)) %>% 
                 sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)),
              sf_meow,
              join = st_nearest_feature)

sf_mammals_spp <- 
  sf::st_join((df_mammals_spp %>% 
                 dplyr::mutate(lat = as.numeric(Latitude),
                               lon = as.numeric(Longitude)) %>%
                 dplyr::filter(! is.na(lat),
                               ! is.na(lon)) %>% 
                 sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)),
              sf_meow,
              join = st_nearest_feature)

## Check join - - - - - -  - - - - - - 
mapview::mapview(sf_turtles_spp, zcol = "ECOREGION") + sf_meow
mapview::mapview(sf_birds_spp, zcol = "ECOREGION") + sf_meow
mapview::mapview(sf_mammals_spp, zcol = "ECOREGION") + sf_meow
## - - - - - - - - - - -  - - - - - - - - -

## Specify records 'inland' as such ####

sf_turtles_spp$inland <- sf::st_intersects(sf_turtles_spp, 
                                           sf_brazil_inland, sparse = FALSE)

sf_birds_spp$inland <- sf::st_intersects(sf_birds_spp, 
                                         sf_brazil_inland, sparse = FALSE)

sf_mammals_spp$inland <- sf::st_intersects(sf_mammals_spp, 
                                           sf_brazil_inland, sparse = FALSE)

## Check these records
# plyr::count(sf_turtles_spp$inland) # zero records
# plyr::count(sf_birds_spp$inland)   # 199 records
# plyr::count(sf_mammals_spp$inland) # 3 records

# mapview::mapview(sf_birds_spp[sf_birds_spp$inland == TRUE,])
# mapview::mapview(sf_mammals_spp[sf_mammals_spp$inland == TRUE,])

sf::sf_use_s2(TRUE) # Turn it back on, as default

## Transform in 'df' again, rbind 'NA' records, and save it ####

sf_turtles_spp <-
  # Remove geometry
  sf_turtles_spp %>% 
  sf::st_drop_geometry() %>% 
  rbind(., 
        # Add the three new cols to 'NA_*'
        (NA_turtles_spp %>% 
           dplyr::mutate(ECOREGION = rep("NA", times = nrow(NA_turtles_spp)),
                         PROVINCE = rep("NA", times = nrow(NA_turtles_spp)),
                         inland = rep("NA", times = nrow(NA_turtles_spp)))))

sf_birds_spp <-
  # Remove geometry
  sf_birds_spp %>% 
  sf::st_drop_geometry() %>% 
  rbind(., 
        # Add the three new cols to 'NA_*'
        (NA_birds_spp %>% 
           dplyr::mutate(ECOREGION = rep("NA", times = nrow(NA_birds_spp)),
                         PROVINCE = rep("NA", times = nrow(NA_birds_spp)),
                         inland = rep("NA", times = nrow(NA_birds_spp)))))

sf_mammals_spp <-
  # Remove geometry
  sf_mammals_spp %>% 
  sf::st_drop_geometry() %>% 
  rbind(., 
        # Add the three new cols to 'NA_*'
        (NA_mammals_spp %>% 
           dplyr::mutate(ECOREGION = rep("NA", times = nrow(NA_mammals_spp)),
                         PROVINCE = rep("NA", times = nrow(NA_mammals_spp)),
                         inland = rep("NA", times = nrow(NA_mammals_spp)))))

write.csv(sf_turtles_spp,
          file = "./data-processed/spp_sea-turtles_meow.csv",
          row.names = FALSE)

write.csv((sf_birds_spp %>% sf::st_drop_geometry()),
          file = "./data-processed/spp_seabirds_meow.csv",
          row.names = FALSE)

write.csv((sf_mammals_spp %>% sf::st_drop_geometry()),
          file = "./data-processed/spp_marine-mammals_meow.csv",
          row.names = FALSE)
