##
## Data grooming
## Checking format, typos, etc.
##

## Libraries ####
library(plyr)
library(dplyr)
library(readxl)
# library(sf)
# library(mapview)

## Sea turtles ####

## -------------------------------------------------------------------------- ##
## ------------------------------ Read data --------------------------------- ##
## -------------------------------------------------------------------------- ##

df_turtles_bib <- readxl::read_excel("./data-raw/tartarugas_UNIFICADO.xlsx", sheet = 1)
df_turtles_spp <- readxl::read_excel("./data-raw/tartarugas_UNIFICADO.xlsx", sheet = 2)

## -------------------------------------------------------------------------- ##
## --------------------------- df_turtles_bib ------------------------------- ##
## -------------------------------------------------------------------------- ##

# colnames(df_turtles_bib)

length(unique(df_turtles_bib$Number)) ## All good

length(unique(df_turtles_bib$Citation)) ## Some citations are repeated -- check

unique(df_turtles_bib$Source)
View(filter(df_turtles_bib, Source == "Snowballing")) ## 7 cases without specific ref from 'Snowballing' -- do we need to worry about this?

length(unique(df_turtles_bib$Title)) ## All good

plyr::count(df_turtles_bib$Publication_type) ## "Master Thesis" and "Thesis" -- check

plyr::count(is.na(df_turtles_bib$Journal_Institution)) ## All good
# head(dplyr::arrange(plyr::count(df_turtles_bib$Journal_Institution), dplyr::desc(freq)), n = 15)

plyr::count(df_turtles_bib$Journal_scope) ## All good

plyr::count(df_turtles_bib$Language) ## "English | Spanish" -- check

plyr::count(df_turtles_bib$Realm) ## All good

plyr::count(df_turtles_bib$Federal_states) ## 6 "NA" -- check 
plyr::count(is.na(df_turtles_bib$Federal_states)) ## >>>> not showing up as 'NA', prob because it's as.character

plyr::count(df_turtles_bib$Ecological_scale) ## All good

plyr::count(df_turtles_bib$Time_scale) ## 49 "NA" -- check

plyr::count(df_turtles_bib$Primary_theme)
dplyr::arrange(plyr::count(df_turtles_bib$Primary_theme), dplyr::desc(freq))  ## OK -- but... 
                      ## ...check classifications e.g.: 
                          ## "Animal health" vs "Anthropogenic impact" (marine debris)
                          ## "Behavior and physiology" vs "Feeding" (diet)
                          ## "Feeding" vs "Trophic ecology" (diet)
                          ## "Behavior and physiology" vs "Socioecology" (what's the difference?)
                          ## "Ecological modeling" (what's this? could be under any class?)
                          ## "Climate change" (could be under other themes?)

plyr::count(df_turtles_bib$Secondary_theme)
dplyr::arrange(plyr::count(df_turtles_bib$Secondary_theme), dplyr::desc(freq))  ## OK -- but... same as above

plyr::count(df_turtles_bib$Anthropogenic_threats) ## Seems OK but have to understand what this column means

unique(df_turtles_bib$Conservation_Unity) ## Check "Y" -- but also need to understand what this column means

plyr::count(df_turtles_bib$Georeferencing_data) ## All good

plyr::count(df_turtles_bib$Data_availability) ## All good

plyr::count(df_turtles_bib$Database) ## All good

length(unique(df_turtles_bib$Reference)) ## All good

# -------------- Check what "GT*_" cols mean

## -------------------------------------------------------------------------- ##
## --------------------------- df_turtles_spp ------------------------------- ##
## -------------------------------------------------------------------------- ##

# colnames(df_turtles_spp)

plyr::count(df_turtles_spp$Kingdom_Domain) ## All good
plyr::count(df_turtles_spp$Phylum) ## All good
plyr::count(df_turtles_spp$Class) ## All good
plyr::count(df_turtles_spp$Order) ## All good

plyr::count(df_turtles_spp$Family) ## --- Need to remove 'Podocnemididae'
plyr::count(df_turtles_spp$Species) ## --- Check/Remove: "Chelidae sp.", "Hydromedusa sp.", "Podocnemis unifilis"
                                         # Check "Hybrid (Eretmochelys imbricata x Caretta caretta x Chelonia mydas)" -- Hybrid of THREE sp?
                                         # Check "Hybrid (Eretmochelys imbricata x Eretmochelys imbricata)" -- Hybrid of the SAME sp?
                                    ## --- Standardise: 
                                         # Hybrid (Eretmochelys imbricata x Lepidochelysolivacea) <-> Hybrid (Lepidochelys olivacea x Eretmochelys imbricata)
                                         # Hydrid (Caretta caretta x Eretmochelys imbricata) --> Hybrid (Eretmochelys imbricata x Caretta caretta)
                                    ## --- Check: "NA"s

plyr::count(df_turtles_spp$Species_author) ## All good

plyr::count(df_turtles_spp$Ecological_group) ## Check 'Plankton'?

plyr::count(df_turtles_spp$Habitat) ## --- Fix typos:
                                        # "Seamounts | Oceani island" -- fix typo in *oceaniC*
                                        # "Sandy beach | Rochy shore" -- fix typo in *RocKy*

plyr::count(df_turtles_spp$Data_type) ## --- Check: 'Landing'

plyr::count(df_turtles_spp$Date_year) ## All good
plyr::count(df_turtles_spp$Date_month) ## --- Fix: numbers showing up as decimals (i.e. 10.0 instead of 10)
plyr::count(df_turtles_spp$Date_day) ## All good

plyr::count(df_turtles_spp$Federal_states) ## --- Check: What about the 'estimated' ones -- are their classified under a single 'state'?

plyr::count(grepl(pattern = ",", x = df_turtles_spp$Latitude)) ## --- Fix: Lat with comma as decimal
plyr::count(grepl(pattern = ",", x = df_turtles_spp$Longitude)) ## --- Fix: Lon with comma as decimal

plyr::count(df_turtles_spp$Coord_source) ## All good


 

## --- Fix Lat/Lon columns
df_turtles_spp$Longitude <- gsub(pattern = ",", 
                                 replacement = ".",
                                 x = df_turtles_spp$Longitude)

df_turtles_spp$Latitude <- gsub(pattern = ",", 
                                replacement = ".",
                                x = df_turtles_spp$Latitude)


## Seabirds ####

## -------------------------------------------------------------------------- ##
## ------------------------------ Read data --------------------------------- ##
## -------------------------------------------------------------------------- ##

df_birds_bib <- readxl::read_excel("./data-raw/aves_UNIFICADO.xlsx", sheet = 1)
df_birds_spp <- readxl::read_excel("./data-raw/aves_UNIFICADO.xlsx", sheet = 2)

## -------------------------------------------------------------------------- ##
## ----------------------------- df_birds_bib ------------------------------- ##
## -------------------------------------------------------------------------- ##


## -------------------------------------------------------------------------- ##
## ----------------------------- df_birds_spp ------------------------------- ##
## -------------------------------------------------------------------------- ##

sf_birds_spp <- 
  df_birds_spp %>% 
  dplyr::mutate(lat = as.numeric(Latitude),
                lon = as.numeric(Longitude)) %>% 
  dplyr::filter(! is.na(lat)) %>% 
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)

mapview::mapview(sf_birds_spp)

## Marine mammals ####

## -------------------------------------------------------------------------- ##
## ------------------------------ Read data --------------------------------- ##
## -------------------------------------------------------------------------- ##

df_mammals_bib <- readxl::read_excel("./data-raw/mamiferos_UNIFICADO.xlsx", sheet = 1)
df_mammals_spp <- readxl::read_excel("./data-raw/mamiferos_UNIFICADO.xlsx", sheet = 2)

## -------------------------------------------------------------------------- ##
## --------------------------- df_mammals_bib ------------------------------- ##
## -------------------------------------------------------------------------- ##


## -------------------------------------------------------------------------- ##
## --------------------------- df_mammals_bib ------------------------------- ##
## -------------------------------------------------------------------------- ##


## --- Fix typo on Lat/Lon columns
df_mammals_spp$Longitude <- gsub(pattern = ",", 
                                 replacement = ".",
                                 x = df_mammals_spp$Longitude)


df_mammals_spp$Latitude <- gsub(pattern = ",", 
                                replacement = ".",
                                x = df_mammals_spp$Latitude)

sf_mammals_spp <- 
  df_mammals_spp %>% 
  dplyr::mutate(lat = as.numeric(Latitude),
                lon = as.numeric(Longitude)) %>%
  dplyr::filter(! is.na(lon)) %>%
  dplyr::filter(! is.na(lat)) %>%
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)

mapview::mapview(sf_mammals_spp)
