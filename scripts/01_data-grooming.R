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

### ---
length(unique(df_turtles_bib$Number)) ## -- All good

### ---
length(unique(df_turtles_bib$Citation)) 
## -- Some citations are repeated, but it's all good

# multiple_citations <- 
#   df_turtles_bib %>% 
#   dplyr::group_by(Citation) %>% 
#   dplyr::summarise(n = n()) %>% 
#   dplyr::filter(n > 1) %>% 
#   dplyr::pull(Citation)
# 
# tmp <- 
#   df_turtles_bib %>% 
#   dplyr::filter(Citation %in% multiple_citations) %>% 
#   dplyr::select(Number, Citation, Title)
# 
# length(unique(tmp$Number)); length(unique(tmp$Title))
# rm(tmp, multiple_citations)

### --- 
unique(df_turtles_bib$Source)
# View(dplyr::filter(df_turtles_bib, Source == "Snowballing")) 
## -- 7 cases without specific ref from 'Snowballing' 
## -- Do we need to worry about this? Probably not

### ---
length(unique(df_turtles_bib$Title)) ## All good

### ---
plyr::count(df_turtles_bib$Publication_type) 
## --"Master Thesis" (n = 1) and "Thesis" (n = 1)
## -- Remove those 

IDs_to_rm <- ## Create this vector now and add 'Number' (ID) as needed to be removed later on
  df_turtles_bib %>% 
  dplyr::filter(Publication_type %in% c("Master Thesis", "Thesis")) %>% 
  dplyr::pull(Number)

### ---
plyr::count(is.na(df_turtles_bib$Journal_Institution)) ## -- All good
# head(dplyr::arrange(plyr::count(df_turtles_bib$Journal_Institution), dplyr::desc(freq)), n = 15)

### ---
plyr::count(df_turtles_bib$Journal_scope) ## -- All good

### ---
plyr::count(df_turtles_bib$Language) 
## "English | Spanish" -- All good, it is actually written in two languages

# tmp <- 
#   df_turtles_bib %>% 
#   dplyr::filter(Language == "English | Spanish")
# 
# rm(tmp)

### ---
plyr::count(df_turtles_bib$Realm) ## -- All good

### ---
plyr::count(df_turtles_bib$Federal_states) ## 6 "NA" -- check...

# tmp <-
#   df_turtles_bib %>%
#   dplyr::filter(Federal_states == "NA")
# 
# rm(tmp)

### ---
plyr::count(df_turtles_bib$Ecological_scale) ## -- All good

### ---
plyr::count(df_turtles_bib$Time_scale) ## 49 "NA" -- check...

### ---
plyr::count(df_turtles_bib$Primary_theme)
dplyr::arrange(plyr::count(df_turtles_bib$Primary_theme), dplyr::desc(freq))  ## OK -- but... 
                      ## ...check classifications e.g.: 
                          ## "Animal health" vs "Anthropogenic impact" (marine debris)
                          ## "Behavior and physiology" vs "Feeding" (diet)
                          ## "Feeding" vs "Trophic ecology" (diet)
                          ## "Behavior and physiology" vs "Socioecology" (what's the difference?)
                          ## "Ecological modeling" (what's this? could be under any class?)
                          ## "Climate change" (could be under other themes?)

### ---
plyr::count(df_turtles_bib$Secondary_theme)
dplyr::arrange(plyr::count(df_turtles_bib$Secondary_theme), dplyr::desc(freq))  ## OK -- but... same as above

### ---
plyr::count(df_turtles_bib$Anthropogenic_threats) ## Seems OK but have to understand what this column means

### ---
unique(df_turtles_bib$Conservation_Unity) ## Check "Y" -- but also need to understand what this column means

### ---
plyr::count(df_turtles_bib$Georeferencing_data) ## -- All good

### ---
plyr::count(df_turtles_bib$Data_availability) ## -- All good

### ---
plyr::count(df_turtles_bib$Database) ## -- All good

length(unique(df_turtles_bib$Reference)) ## -- All good

# -------------- Check what "GT*_" cols mean

### --- Remove IDs identified to be removed
df_turtles_bib <-
  df_turtles_bib %>% 
  dplyr::filter(! Number %in% IDs_to_rm)

write.csv2(df_turtles_bib, "./data-processed/bib_sea-turtles.csv", row.names = FALSE)

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

# colnames(df_turtles_bib)

### ---
length(unique(df_birds_bib$Number)) ## -- All good

### ---
length(unique(df_birds_bib$Citation)) 
## -- Some citations are repeated, but it's all good

# multiple_citations <-
#   df_birds_bib %>%
#   dplyr::group_by(Citation) %>%
#   dplyr::summarise(n = n()) %>%
#   dplyr::filter(n > 1) %>%
#   dplyr::pull(Citation)
# 
# tmp <-
#   df_birds_bib %>%
#   dplyr::filter(Citation %in% multiple_citations) %>%
#   dplyr::select(Number, Citation, Title)
# 
# length(unique(tmp$Number)); length(unique(tmp$Title))
# rm(tmp, multiple_citations)

### --- 
unique(df_birds_bib$Source)
# View(dplyr::filter(df_birds_bib, Source == "Snowballing")) 
## -- 1 case without specific ref from 'Snowballing' 
## -- Do we need to worry about this? Probably not

# View(dplyr::filter(df_birds_bib, Source == "NA")) 
## -- 1 case without source specified as "NA"
## -- Do we need to worry about this? Probably not

### ---
length(unique(df_birds_bib$Title)) ## -- All good

### ---
plyr::count(df_birds_bib$Publication_type) 
## --"Master Thesis" (n = 1) and "Thesis" (n = 1)
## -- Remove those 

# View(dplyr::filter(df_birds_bib, Publication_type == "Master Thesis"))

# View(dplyr::filter(df_birds_bib, Publication_type == "Report")) ## -- These are 'Papers', so fix them:
df_birds_bib[!is.na(df_birds_bib$Publication_type) & df_birds_bib$Publication_type == "Report", ]$Publication_type <- "Paper"

IDs_to_rm <- ## Create this vector now and add 'Number' (ID) as needed to be removed later on
  df_birds_bib %>% 
  dplyr::filter(Publication_type %in% c("Master Thesis")) %>% 
  dplyr::pull(Number)

### ---
plyr::count(is.na(df_birds_bib$Journal_Institution)) ## -- All good
# head(dplyr::arrange(plyr::count(df_turtles_bib$Journal_Institution), dplyr::desc(freq)), n = 15)

### ---
plyr::count(df_birds_bib$Journal_scope) ## -- All good

### ---
plyr::count(df_birds_bib$Language) 
## "English | Spanish" -- All good, it is actually written in two languages

### ---
plyr::count(df_birds_bib$Realm) ## -- All good

### ---
plyr::count(df_birds_bib$Federal_states) ## 6 "NA" -- check...

# tmp <-
#   df_birds_bib %>%
#   dplyr::filter(Federal_states == "NA")
# 
# rm(tmp)

### ---
plyr::count(df_birds_bib$Ecological_scale) ## -- All good

### ---
plyr::count(df_birds_bib$Time_scale) ## -- 49 "NA" -- check...

### ---
plyr::count(df_birds_bib$Primary_theme)
dplyr::arrange(plyr::count(df_birds_bib$Primary_theme), dplyr::desc(freq))  ## -- All good

### ---
plyr::count(df_birds_bib$Secondary_theme)
dplyr::arrange(plyr::count(df_birds_bib$Secondary_theme), dplyr::desc(freq))  ## -- All good

### ---
plyr::count(df_birds_bib$Anthropogenic_threats) ## Seems OK but have to understand what this column means (see "NA"s)

### ---
unique(df_birds_bib$Conservation_Unity) ## Check "N" -- but also need to understand what this column means

### ---
plyr::count(df_birds_bib$Georeferencing_data) ## -- All good

### ---
plyr::count(df_birds_bib$Data_availability) ## -- All good

### ---
plyr::count(df_birds_bib$Database) ## -- All good

### ---
length(unique(df_birds_bib$Reference)) ## -- All good

# -------------- Check what "GT*_" cols mean

### --- Remove IDs identified to be removed
df_birds_bib <-
  df_birds_bib %>% 
  dplyr::filter(! Number %in% IDs_to_rm)

write.csv2(df_birds_bib, "./data-processed/bib_seabirds.csv", row.names = FALSE)

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

# colnames(df_mammals_bib)

### ---
length(unique(df_mammals_bib$Number)) ## -- All good

### ---
length(unique(df_mammals_bib$Citation)) 
## -- Some citations are repeated, but it's all good

# multiple_citations <-
#   df_mammals_bib %>%
#   dplyr::group_by(Citation) %>%
#   dplyr::summarise(n = n()) %>%
#   dplyr::filter(n > 1) %>%
#   dplyr::pull(Citation)
# 
# tmp <-
#   df_mammals_bib %>%
#   dplyr::filter(Citation %in% multiple_citations) %>%
#   dplyr::select(Number, Citation, Title)
# 
# length(unique(tmp$Number)); length(unique(tmp$Title))
# rm(tmp, multiple_citations)

### --- 
unique(df_mammals_bib$Source)
# View(dplyr::filter(df_mammals_bib, Source == "Snowballing")) 
## -- 3 case without specific ref from 'Snowballing' 
## -- Do we need to worry about this? Probably not

### ---
length(unique(df_mammals_bib$Title)) ## -- All good

### ---
plyr::count(df_mammals_bib$Publication_type) 
## --"Master Thesis" (n = 1) and "Thesis" (n = 1)
## -- Remove those 

# View(dplyr::filter(df_mammals_bib, Publication_type == "Master Thesis"))
# View(dplyr::filter(df_mammals_bib, Publication_type == "Thesis"))

# View(dplyr::filter(df_mammals_bib, Publication_type == "Report")) 
## -- These are all from 'Reports of the International Whaling Commission'; shall we consider them as 'Paper'?
# df_mammals_bib[!is.na(df_mammals_bib$Publication_type) & df_mammals_bib$Publication_type == "Report", ]$Publication_type <- "Paper"

IDs_to_rm <- ## Create this vector now and add 'Number' (ID) as needed to be removed later on
  df_mammals_bib %>% 
  dplyr::filter(Publication_type %in% c("Master Thesis", "Thesis")) %>% 
  dplyr::pull(Number)

### ---
plyr::count(is.na(df_mammals_bib$Journal_Institution)) ## -- All good
# head(dplyr::arrange(plyr::count(df_mammals_bib$Journal_Institution), dplyr::desc(freq)), n = 15)

### ---
plyr::count(df_mammals_bib$Journal_scope) ## -- All good

### ---
plyr::count(df_mammals_bib$Language) 
## "English | Spanish" -- All good, it is actually written in two languages

### ---
plyr::count(df_mammals_bib$Realm) ## -- All good

### ---
plyr::count(df_mammals_bib$Federal_states) ## 31 "NA" -- check...

# tmp <-
#   df_mammals_bib %>%
#   dplyr::filter(Federal_states == "NA")
# 
# rm(tmp)

### ---
plyr::count(df_mammals_bib$Ecological_scale) ## -- All good

### ---
plyr::count(df_mammals_bib$Time_scale) ## -- 148 "NA" -- check...

### ---
plyr::count(df_mammals_bib$Primary_theme)
dplyr::arrange(plyr::count(df_mammals_bib$Primary_theme), dplyr::desc(freq))  ## -- All good

### ---
plyr::count(df_mammals_bib$Secondary_theme)
dplyr::arrange(plyr::count(df_mammals_bib$Secondary_theme), dplyr::desc(freq))  ## -- All good

### ---
plyr::count(df_mammals_bib$Anthropogenic_threats) ## Check "N"; Seems OK but have to understand what this column means (see "NA"s)

### ---
unique(df_mammals_bib$Conservation_Unity) ## Check "N" -- but also need to understand what this column means

### ---
plyr::count(df_mammals_bib$Georeferencing_data) ## -- All good

### ---
plyr::count(df_mammals_bib$Data_availability) ## -- All good

### ---
plyr::count(df_mammals_bib$Database) ## -- All good

### ---
length(unique(df_mammals_bib$Reference)) ## -- 3 refs that are wrong: check all info

# tmp <-
#   df_mammals_bib %>% 
#   dplyr::group_by(Reference) %>% 
#   dplyr::summarise(n = n()) %>% 
#   dplyr::filter(n > 1)
# 
# tmp2 <-
#   df_mammals_bib %>% 
#   dplyr::group_by(Number) %>% 
#   dplyr::summarise(n = n()) %>% 
#   dplyr::filter(n > 1)

# -------------- Check what "GT*_" cols mean

### --- Remove IDs identified to be removed
df_mammals_bib <-
  df_mammals_bib %>% 
  dplyr::filter(! Number %in% IDs_to_rm)

write.csv2(df_mammals_bib, "./data-processed/bib_marine-mammals.csv", row.names = FALSE)

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
  dplyr::filter(Coord_source == "Author") %>% 
  dplyr::mutate(lat = as.numeric(Latitude),
                lon = as.numeric(Longitude)) %>%
  dplyr::filter(! is.na(lon)) %>%
  dplyr::filter(! is.na(lat)) %>%
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)

mapview::mapview(sf_mammals_spp, zcol = "Family")
