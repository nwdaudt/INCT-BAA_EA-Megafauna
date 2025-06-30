##
## Spatial join with MEOWs (Ecoregions and Provinces)
## 
##

## Libraries ####
library(plyr)
library(dplyr)
library(sf)
library(mapview)
library(rnaturalearth)

## Read data ####

df_turtles_spp <- read.csv2("./data-processed/spp_sea-turtles.csv")
df_birds_spp <- read.csv2("./data-processed/spp_seabirds.csv")
df_mammals_spp <- read.csv2("./data-processed/spp_marine-mammals.csv")

## Read spatial data ####

sf_meow <- 
  sf::read_sf("./data-spatial/MEOW_East-South-America.gpkg") %>% 
  dplyr::select(ECOREGION, PROVINCE)

sf_brazil <- 
  sf::read_sf("./data-spatial/Brazil_Federal-states.gpkg") %>% 
  sf::st_transform(4326)

sf_blue_amazon <- 
  sf::read_sf("./data-spatial/Brazil_Blue-Amazon-extended.gpkg") %>% 
  sf::st_transform(4326)

# plot(sf_blue_amazon)

## Spatial join ####

sf_turtles_spp <- 
  sf::st_join((df_turtles_spp %>% 
                 dplyr::mutate(lat = as.numeric(Latitude),
                               lon = as.numeric(Longitude)) %>%
                 dplyr::filter(! is.na(lat),
                               ! is.na(lon)) %>% 
                 sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)),
              sf_meow)

sf_birds_spp <- 
  sf::st_join((df_birds_spp %>% 
                 dplyr::mutate(lat = as.numeric(Latitude),
                               lon = as.numeric(Longitude)) %>%
                 dplyr::filter(! is.na(lat),
                               ! is.na(lon)) %>% 
                 sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)),
              sf_meow)

sf_mammals_spp <- 
  sf::st_join((df_mammals_spp %>% 
                 dplyr::mutate(lat = as.numeric(Latitude),
                               lon = as.numeric(Longitude)) %>%
                 dplyr::filter(! is.na(lat),
                               ! is.na(lon)) %>% 
                 sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)),
              sf_meow)

## Check join - - - - - -  - - - - - - # zcol="ECOREGION"
# mapview::mapview(sf_turtles_spp, zcol = "PROVINCE") + sf_blue_amazon
# mapview::mapview(sf_turtles_spp, zcol = "PROVINCE") + sf_brazil
## - - - - - - - - - - -  - - - - - - - - -

## Polygon BR to crop records on land ####

# Because 'sf_brazil' has each state border delimitation and I need only the
# country delimitation, it is easier to retrieve the polygon from {rnaturalearth}

sf_brazil_union <- rnaturalearth::ne_countries(country = "brazil",
                                               scale = 10,
                                               returnclass = "sf")
# mapview::mapview(sf_brazil_union)

## Break it down and simplify a little bit
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

## Crop records within (terrestrial) continental areas ####

sfs_list <- list(
  spp_turtles = sf_turtles_spp,
  spp_seabirds = sf_birds_spp,
  spp_mammals = sf_mammals_spp
)

sfs_cropped_list <- list()

for (sf in 1:length(sfs_list)) {
  
  ## Get 'sf' name
  sf_name <- names(sfs_list)[sf]
  
  ## Get data according to 'sf' name
  tmp <- sfs_list[[sf_name]]
  
  ## Crop to 
  sf_cropped <-
    sf::st_difference(tmp, sf_brazil_to_crop_terrestrial) 
  
  ## Did not work, not sure why, but thats OK... 
  # sf_use_s2(FALSE) 
  # sf_cropped <- sf_cropped[sf::st_contains(sf_cropped, sf::st_make_valid(sf_blue_amazon)),]
  
  ## Rename objs
  sf_name_cropped <- paste0(sf_name, "_cropped")
  
  ## Store them in the list
  sfs_cropped_list[[sf_name_cropped]] <- sf_cropped
  
  rm(sf, sf_name, tmp, sf_name_cropped, sf_cropped)
}

## Transform in 'df' again and save it ####

# names(sfs_cropped_list)

write.csv((sfs_cropped_list[["spp_turtles_cropped"]] %>% sf::st_drop_geometry()),
          file = "./data-processed/spp_sea-turtles_meow.csv")

write.csv((sfs_cropped_list[["spp_seabirds_cropped"]] %>% sf::st_drop_geometry()),
          file = "./data-processed/spp_seabirds_meow.csv")

write.csv((sfs_cropped_list[["spp_mammals_cropped"]] %>% sf::st_drop_geometry()),
          file = "./data-processed/spp_marine-mammals_meow.csv")
