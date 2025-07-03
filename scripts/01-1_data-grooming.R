##
## Data grooming
## Checking format, typos, etc.
##

## Libraries ####
library(plyr)
library(dplyr)
library(readxl)

## Sea turtles ####

## -------------------------------------------------------------------------- ##
## ------------------------------ Read data --------------------------------- ##
## -------------------------------------------------------------------------- ##

df_turtles_bib <- readxl::read_excel("./data-raw/INCT_BAA_FINAL_08_Tartarugas_ok.xlsx", sheet = 1)
df_turtles_spp <- readxl::read_excel("./data-raw/INCT_BAA_FINAL_08_Tartarugas_ok.xlsx", sheet = 2)

## -------------------------------------------------------------------------- ##
## --------------------------- df_turtles_bib ------------------------------- ##
## -------------------------------------------------------------------------- ##

# colnames(df_turtles_bib)

### ---
length(unique(df_turtles_bib$Number)) ## -- All good

### ---
length(unique(df_turtles_bib$Citation)) 
## -- Some citations are repeated, but it's all good

multiple_citations <-
  df_turtles_bib %>%
  dplyr::group_by(Citation) %>%
  dplyr::summarise(n = n()) %>%
  dplyr::filter(n > 1) %>%
  dplyr::pull(Citation)

tmp <-
  df_turtles_bib %>%
  dplyr::filter(Citation %in% multiple_citations) %>%
  dplyr::select(Number, Citation, Title)

length(unique(tmp$Number)); length(unique(tmp$Title))

# Find duplicates
tmp2 <- tmp[duplicated(tmp$Title), ] ## Duarte et al 2018 - "Quantifying the morphology of key..."

# Pull 'Numbers' to remove
IDs_to_rm <-
  tmp2 %>%
  dplyr::pull(Number)

# Clean env
rm(tmp, multiple_citations, tmp2)

### --- 
unique(df_turtles_bib$Source)
# View(dplyr::filter(df_turtles_bib, Source == "Snowballing"))  # -- All good

### ---
length(unique(df_turtles_bib$Title)) ## All good, I'll clean later using "IDs_to_rm"

### ---
plyr::count(df_turtles_bib$Publication_type)
## --"Master Thesis" (n = 1) and "Thesis" (n = 1)
## -- Remove those 

IDs_to_rm <- ## Create this vector now and add 'Number' (ID) as needed to be removed later on
  df_turtles_bib %>% 
  dplyr::filter(Publication_type %in% c("Master Thesis", "Thesis")) %>% 
  dplyr::pull(Number) %>% 
  append(IDs_to_rm, .)

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
df_turtles_bib %>% 
  tidyr::separate_rows(Realm, sep = " \\| ") %>%
  dplyr::group_by(Realm) %>% 
  dplyr::summarise(n = n()) ## -- All good


### ---
df_turtles_bib %>% 
  tidyr::separate_rows(Federal_states, sep = " \\| ") %>%
  dplyr::group_by(Federal_states) %>% 
  dplyr::summarise(n = n()) ## -- All good

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
View(
df_turtles_bib %>% 
  tidyr::separate_rows(Anthropogenic_threats, sep = " \\| ") %>%
  dplyr::group_by(Anthropogenic_threats) %>% 
  dplyr::summarise(n = n())
) ## All good

### ---
View(
  df_turtles_bib %>% 
    tidyr::separate_rows(Conservation_Unity, sep = " \\| ") %>%
    dplyr::group_by(Conservation_Unity) %>% 
    dplyr::summarise(n = n())
) ## All good

### ---
plyr::count(df_turtles_bib$Georeferencing_data) ## -- All good

### ---
plyr::count(df_turtles_bib$Data_availability) ## -- All good

### ---
View(
  df_turtles_bib %>% 
    tidyr::separate_rows(Database, sep = " \\| ") %>%
    dplyr::group_by(Database) %>% 
    dplyr::summarise(n = n())
) ## ----------------------------------- Check nomenclature Supp 'material' / 'information' & 'Data on the article itself'

### ---
length(unique(df_turtles_bib$Reference)) ## -- 501... But note there is a Title doubled

# tmp <-
#   df_turtles_bib %>%
#   dplyr::filter(grepl(pattern = "Quantifying the morphology of key", x = df_turtles_bib$Title))
# rm(tmp)
### Refs are "different" (pages). This will be fixed once the duplicate ID gets removed


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
plyr::count(df_turtles_spp$Family) ## All good

plyr::count(df_turtles_spp$Species) ## --- Standardise any "Hybrid (sp1 x sp2)" as "Hybrid" later in the code
plyr::count(df_turtles_spp$Species_author) ## --- See comments below

## Fix typos, and standardise order of spp in 'hybrid (sp1 x sp2)' (in "Species", too)
    # Hydrid (Caretta caretta x Eretmochelys imbricata) --> Hybrid (Eretmochelys imbricata x Caretta caretta)
    # Hybrid (Eretmochelys imbricata x Lepidochelysolivacea) --> Hybrid (Eretmochelys imbricata x Lepidochelys olivacea)
    # Hybrid (Lepidochelys olivacea x Eretmochelys imbricata) --> Hybrid (Eretmochelys imbricata x Lepidochelys olivacea)

# Typo 1 = 
df_turtles_spp[df_turtles_spp$Species == 
                 "Hydrid (Caretta caretta x Eretmochelys imbricata)", ]$Species <- 
  "Hybrid (Eretmochelys imbricata x Caretta caretta)"

df_turtles_spp[df_turtles_spp$Species_author == 
                 "Hydrid (Caretta caretta x Eretmochelys imbricata)", ]$Species_author <- 
  "Hybrid (Eretmochelys imbricata x Caretta caretta)"

# Typo 2 = 
df_turtles_spp[df_turtles_spp$Species == 
                 "Hybrid (Eretmochelys imbricata x Lepidochelysolivacea)", ]$Species <- 
  "Hybrid (Eretmochelys imbricata x Lepidochelys olivacea)"

df_turtles_spp[df_turtles_spp$Species_author == 
                 "Hybrid (Eretmochelys imbricata x Lepidochelysolivacea)", ]$Species_author <- 
  "Hybrid (Eretmochelys imbricata x Lepidochelys olivacea)"

# Standardise spp's in hybrid = 
df_turtles_spp[df_turtles_spp$Species == 
                 "Hybrid (Lepidochelys olivacea x Eretmochelys imbricata)", ]$Species <- 
  "Hybrid (Eretmochelys imbricata x Lepidochelys olivacea)"

df_turtles_spp[df_turtles_spp$Species_author == 
                 "Hybrid (Lepidochelys olivacea x Eretmochelys imbricata)", ]$Species_author <- 
  "Hybrid (Eretmochelys imbricata x Lepidochelys olivacea)"

## Fix in 'Species_author' only
    # Hybrid ( Eretmochelys imbricata X Caretta caretta) --> Hybrid (Eretmochelys imbricata x Caretta caretta)
    # Hybrid (Lepidochelys olivacea x Caretta caretta) --> Hybrid (Caretta caretta x Lepidochelys olivacea)

df_turtles_spp[df_turtles_spp$Species_author == 
                 "Hybrid ( Eretmochelys imbricata X Caretta caretta)", ]$Species_author <- 
  "Hybrid (Eretmochelys imbricata x Caretta caretta)"

df_turtles_spp[df_turtles_spp$Species_author == 
                 "Hybrid (Lepidochelys olivacea x Caretta caretta)", ]$Species_author <- 
  "Hybrid (Caretta caretta x Lepidochelys olivacea)"

# plyr::count(df_turtles_spp$Species); plyr::count(df_turtles_spp$Species_author) ### -- Fixed ;)

### ---
plyr::count(df_turtles_spp$Ecological_group) ## -- All good

### ---
View(
  df_turtles_bib %>% 
    tidyr::separate_rows(Habitat, sep = " \\| ") %>%
    dplyr::group_by(Habitat) %>% 
    dplyr::summarise(n = n())
) ## Typo -- "Sandybeach" should be "Sandy beach"" --- Fixed by hand ;)

# df_turtles_bib <- 
#   df_turtles_bib %>%
#   dplyr::mutate(Habitat = gsub(pattern = "Sandybeach", 
#                                replacement = "Sandy beach", 
#                                x = Habitat))


### ---
View(
  df_turtles_spp %>% ### --- "Data_type" column name was "-ok"..... Fixed below
    tidyr::separate_rows(`-ok`, sep = " \\| ") %>%
    dplyr::group_by(`-ok`) %>% 
    dplyr::summarise(n = n())
)

df_turtles_spp <-
  df_turtles_spp %>% 
  dplyr::rename(Data_type = `-ok`)
# colnames(df_turtles_spp)

### ---
View(
  df_turtles_spp %>% 
    tidyr::separate_rows(Date_year, sep = " \\| ") %>%
    dplyr::group_by(Date_year) %>% 
    dplyr::summarise(n = n())
) ## All good

### ---
View(
  df_turtles_spp %>% 
    tidyr::separate_rows(Date_month, sep = " \\| ") %>%
    dplyr::group_by(Date_month) %>% 
    dplyr::summarise(n = n())
)  ## All good

### ---
View(
  df_turtles_spp %>% 
    tidyr::separate_rows(Date_day, sep = " \\| ") %>%
    dplyr::group_by(Date_day) %>% 
    dplyr::summarise(n = n())
)  ## All good

### --- 
View(
  df_turtles_spp %>% 
    tidyr::separate_rows(Federal_states, sep = " \\| ") %>%
    dplyr::group_by(Federal_states) %>% 
    dplyr::summarise(n = n())
)  ## --- Check: What about the 'estimated' ones -- are their classified under a single 'state'?

plyr::count(grepl(pattern = ",", x = df_turtles_spp$Latitude)) ## All good
plyr::count(grepl(pattern = ",", x = df_turtles_spp$Longitude)) ## All good

plyr::count(df_turtles_spp$Coord_source) ## All good

## --- Clean taxonomy
df_turtles_spp <-
  df_turtles_spp %>% 
  dplyr::mutate(Species = gsub(x = Species, 
                               pattern = "Hybrid .*",
                               replacement = "Hybrid"))

## --- Fix Lat/Lon columns (not needed anymore)
# df_turtles_spp$Longitude <- gsub(pattern = ",", 
#                                  replacement = ".",
#                                  x = df_turtles_spp$Longitude)
# 
# df_turtles_spp$Latitude <- gsub(pattern = ",", 
#                                 replacement = ".",
#                                 x = df_turtles_spp$Latitude)

### --- Remove IDs identified to be removed
df_turtles_spp <-
  df_turtles_spp %>% 
  dplyr::filter(! Number %in% IDs_to_rm)

write.csv2(df_turtles_spp, "./data-processed/spp_sea-turtles.csv", row.names = FALSE)

## Seabirds ####

## -------------------------------------------------------------------------- ##
## ------------------------------ Read data --------------------------------- ##
## -------------------------------------------------------------------------- ##

df_birds_bib <- readxl::read_excel("./data-raw/INCT-BAA_FINAL_11_AvesMarinhas_ok.xlsx", sheet = 1)
df_birds_spp <- readxl::read_excel("./data-raw/INCT-BAA_FINAL_11_AvesMarinhas_ok.xlsx", sheet = 2)

## -------------------------------------------------------------------------- ##
## ----------------------------- df_birds_bib ------------------------------- ##
## -------------------------------------------------------------------------- ##

# colnames(df_birds_bib)

### ---
length(unique(df_birds_bib$Number)) ## -- All good

### ---
length(unique(df_birds_bib$Citation)) 
## -- Some citations are repeated, but it's all good

multiple_citations <-
  df_birds_bib %>%
  dplyr::group_by(Citation) %>%
  dplyr::summarise(n = n()) %>%
  dplyr::filter(n > 1) %>%
  dplyr::pull(Citation)

tmp <-
  df_birds_bib %>%
  dplyr::filter(Citation %in% multiple_citations) %>%
  dplyr::select(Number, Citation, Title)

length(unique(tmp$Number)); length(unique(tmp$Title))
rm(tmp, multiple_citations)

### --- 
unique(df_birds_bib$Source)
# View(dplyr::filter(df_birds_bib, Source == "Snowballing")) 

# View(dplyr::filter(df_birds_bib, Source == "NA")) 
## -- 1 case without source specified as "NA"

### ---
length(unique(df_birds_bib$Title)) ## All good

### ---
plyr::count(df_birds_bib$Publication_type) 
## -- "Master Thesis" (n = 1) and "Book chapter" (n = 1) -- Remove those 

# View(dplyr::filter(df_birds_bib, Publication_type == "Brief report"))
# View(dplyr::filter(df_birds_bib, Publication_type == "Case report"))
# View(dplyr::filter(df_birds_bib, Publication_type == "Clinical report"))
## -- These are 'Papers', so fix them:
df_birds_bib[!is.na(df_birds_bib$Publication_type) & df_birds_bib$Publication_type == "Brief report", ]$Publication_type <- "Paper"
df_birds_bib[!is.na(df_birds_bib$Publication_type) & df_birds_bib$Publication_type == "Case report", ]$Publication_type <- "Paper"
df_birds_bib[!is.na(df_birds_bib$Publication_type) & df_birds_bib$Publication_type == "Clinical report", ]$Publication_type <- "Paper"

IDs_to_rm <- ## Create this vector now and add 'Number' (ID) as needed to be removed later on
  df_birds_bib %>% 
  dplyr::filter(Publication_type %in% c("Master Thesis", "Book chapter")) %>% 
  dplyr::pull(Number)

### ---
plyr::count(is.na(df_birds_bib$Journal_Institution)) ## -- All good
# sort(unique(df_birds_bib$Journal_Institution))
# head(dplyr::arrange(plyr::count(df_birds_bib$Journal_Institution), dplyr::desc(freq)), n = 15)

## Fix typos
df_birds_bib[!is.na(df_birds_bib$Journal_Institution) & df_birds_bib$Journal_Institution == "PlOs Computational Biology", ]$Journal_Institution <- "PLoS Computational Biology"
# df_birds_bib[!is.na(df_birds_bib$Journal_Institution) & df_birds_bib$Journal_Institution == "scientific reports", ]$Journal_Institution <- "Scientific Reports"
# df_birds_bib[!is.na(df_birds_bib$Journal_Institution) & df_birds_bib$Journal_Institution == "Environmental pollution", ]$Journal_Institution <- "Environmental Pollution"
# df_birds_bib[!is.na(df_birds_bib$Journal_Institution) & df_birds_bib$Journal_Institution == "Bulletin – British Ornithologists' Club", ]$Journal_Institution <- "Bulletin of the British Ornithologists' Club"
# df_birds_bib[!is.na(df_birds_bib$Journal_Institution) & df_birds_bib$Journal_Institution == "Ocean and Coastal Management", ]$Journal_Institution <- "Ocean & Coastal Management"
# df_birds_bib[!is.na(df_birds_bib$Journal_Institution) & df_birds_bib$Journal_Institution == "Frontier Marine Science", ]$Journal_Institution <- "Frontier in Marine Science"
# df_birds_bib[!is.na(df_birds_bib$Journal_Institution) & df_birds_bib$Journal_Institution == "Estuarine Coastal and Shelf Science", ]$Journal_Institution <- "Estuarine, Coastal and Shelf Science"
# "Elsevier" -- will be removed

### ---
plyr::count(df_birds_bib$Journal_scope) ## -- All good

### ---
plyr::count(df_birds_bib$Language) ## -- All good

### ---
df_birds_bib %>% 
  tidyr::separate_rows(Realm, sep = " \\| ") %>%
  dplyr::group_by(Realm) %>% 
  dplyr::summarise(n = n()) ## -- All good

### ---
View(
  df_birds_bib %>% 
  tidyr::separate_rows(Federal_states, sep = " \\| ") %>%
  dplyr::group_by(Federal_states) %>% 
  dplyr::summarise(n = n())
) ## All good

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
View(
  df_birds_bib %>% 
       tidyr::separate_rows(Anthropogenic_threats, sep = " \\| ") %>%
       dplyr::group_by(Anthropogenic_threats) %>% 
       dplyr::summarise(n = n())
) ## All good

### ---
View(
  df_birds_bib %>% 
    tidyr::separate_rows(Conservation_Unity, sep = " \\| ") %>%
    dplyr::group_by(Conservation_Unity) %>% 
    dplyr::summarise(n = n())
) ## All good 

## -------------------------------------------- UCs check
# MONA ASPSP 'vs' APA de Fernando de Noronha-Rocas-São Pedro e São Paulo 'vs' APA Arquipélago de São Pedro e São Paulo
# PES da Ilha do Mel 'vs' ESEC Ilha do Mel

### ---
plyr::count(df_birds_bib$Georeferencing_data) ## -- All good

### ---
plyr::count(df_birds_bib$Data_availability) ## -- All good

### ---
View(
  df_birds_bib %>% 
    tidyr::separate_rows(Database, sep = " \\| ") %>%
    dplyr::group_by(Database) %>% 
    dplyr::summarise(n = n())
)  ## -- All good

df_birds_bib[df_birds_bib$Database == "Genbank", ]$Database <- "GenBank"

### ---
length(unique(df_birds_bib$Reference)) ## -- All good

### --- Remove IDs identified to be removed
df_birds_bib <-
  df_birds_bib %>% 
  dplyr::filter(! Number %in% IDs_to_rm)

write.csv2(df_birds_bib, "./data-processed/bib_seabirds.csv", row.names = FALSE)

## -------------------------------------------------------------------------- ##
## ----------------------------- df_birds_spp ------------------------------- ##
## -------------------------------------------------------------------------- ##

# colnames(df_birds_spp)

plyr::count(df_birds_spp$Kingdom_Domain) ## All good
plyr::count(df_birds_spp$Phylum) ## All good
plyr::count(df_birds_spp$Class) ## All good
plyr::count(df_birds_spp$Order) ## --- For analysis, need to remove several orders
plyr::count(df_birds_spp$Family) ## --- For analysis, need to remove several families
plyr::count(df_birds_spp$Species) ## --- For analysis, need to remove several species, including "sp."
plyr::count(df_birds_spp$Species_author) ## There are several typos, but, all 'good'...
plyr::count(df_birds_spp$Ecological_group) ## Check 'Terrestrial' and 'NA'?

### ---
View(
  df_birds_spp %>% 
    tidyr::separate_rows(Habitat, sep = " \\| ") %>%
    dplyr::group_by(Habitat) %>% 
    dplyr::summarise(n = n())
) ## All good

View(
  df_birds_spp %>% 
    tidyr::separate_rows(Data_type, sep = " \\| ") %>%
    dplyr::group_by(Data_type) %>% 
    dplyr::summarise(n = n())
) ## --- Check: 'Tracking' vs 'Satellite'

### ---
View(
  df_birds_spp %>% 
    tidyr::separate_rows(Date_year, sep = " \\| ") %>%
    dplyr::group_by(Date_year) %>% 
    dplyr::summarise(n = n())
) ## All good

### ---
View(
  df_birds_spp %>% 
    tidyr::separate_rows(Date_month, sep = " \\| ") %>%
    dplyr::group_by(Date_month) %>% 
    dplyr::summarise(n = n())
) ## ---All good

### ---
View(
  df_birds_spp %>% 
    tidyr::separate_rows(Date_day, sep = " \\| ") %>%
    dplyr::group_by(Date_day) %>% 
    dplyr::summarise(n = n())
) ## ----------- Fix by hand -- not sure why the code below is not working...

# df_birds_spp[df_birds_spp$Date_day == "6 |7", ]$Date_day <- "6 | 7"

### ---
View(
  df_birds_spp %>% 
    tidyr::separate_rows(Federal_states, sep = " \\| ") %>%
    dplyr::group_by(Federal_states) %>% 
    dplyr::summarise(n = n())
)  ## --- Check: What about the 'estimated' ones -- are their classified under a single 'state'?

plyr::count(grepl(pattern = ",", x = df_birds_spp$Latitude)) ## --- All good
plyr::count(grepl(pattern = ",", x = df_birds_spp$Longitude)) ## --- All good

plyr::count(df_birds_spp$Coord_source) ## What's 'NA'?

## --- Fix Lat/Lon columns -- no longer needed
# df_birds_spp$Longitude <- gsub(pattern = ",", 
#                                  replacement = ".",
#                                  x = df_birds_spp$Longitude)
# 
# df_birds_spp$Latitude <- gsub(pattern = ",", 
#                                 replacement = ".",
#                                 x = df_birds_spp$Latitude)

### --- Remove IDs identified to be removed
df_birds_spp <-
  df_birds_spp %>% 
  dplyr::filter(! Number %in% IDs_to_rm)

write.csv2(df_birds_spp, "./data-processed/spp_seabirds.csv", row.names = FALSE)

## Marine mammals ####

## -------------------------------------------------------------------------- ##
## ------------------------------ Read data --------------------------------- ##
## -------------------------------------------------------------------------- ##

df_mammals_bib <- readxl::read_excel("./data-raw/INCT_BAA_FINAL_09_10_Mamiferos_ok.xlsx", sheet = 1)
df_mammals_spp <- readxl::read_excel("./data-raw/INCT_BAA_FINAL_09_10_Mamiferos_ok.xlsx", sheet = 2)

## -------------------------------------------------------------------------- ##
## --------------------------- df_mammals_bib ------------------------------- ##
## -------------------------------------------------------------------------- ##

# colnames(df_mammals_bib)

### ---
length(unique(df_mammals_bib$Number)) ## -- All good

### ---
length(unique(df_mammals_bib$Citation)) 
## -- Some citations are repeated, but it's all good

multiple_citations <-
  df_mammals_bib %>%
  dplyr::group_by(Citation) %>%
  dplyr::summarise(n = n()) %>%
  dplyr::filter(n > 1) %>%
  dplyr::pull(Citation)

tmp <-
  df_mammals_bib %>%
  dplyr::filter(Citation %in% multiple_citations) %>%
  dplyr::select(Number, Citation, Title)

length(unique(tmp$Number)); length(unique(tmp$Title))
rm(tmp, multiple_citations)

### --- 
unique(df_mammals_bib$Source)
# View(dplyr::filter(df_mammals_bib, Source == "Snowballing")) 

df_mammals_bib[df_mammals_bib$Source == "Sowballing: de Moura et al., 2010 (9_137)", ]$Source <- "Snowballing: de Moura et al., 2010 (9_137)"

### ---
length(unique(df_mammals_bib$Title)) ## -- All good

### ---
plyr::count(df_mammals_bib$Publication_type) 
## --"Master Thesis" (n = 3) and "Thesis" (n = 3) and "Book chapter" (n = 3)
## -- Remove those 

# View(dplyr::filter(df_mammals_bib, Publication_type == "Report")) 
## -- These are all from 'Reports of the International Whaling Commission' and were peer-reviewed; so, we shall consider them as 'Paper'
df_mammals_bib[!is.na(df_mammals_bib$Publication_type) & df_mammals_bib$Publication_type == "Report", ]$Publication_type <- "Paper"

IDs_to_rm <- ## Create this vector now and add 'Number' (ID) as needed to be removed later on
  df_mammals_bib %>% 
  dplyr::filter(Publication_type %in% c("Master Thesis", "Thesis", "Book chapter")) %>% 
  dplyr::pull(Number)

### ---
plyr::count(is.na(df_mammals_bib$Journal_Institution)) ## -- All good
# sort(unique(df_mammals_bib$Journal_Institution))
# head(dplyr::arrange(plyr::count(df_mammals_bib$Journal_Institution), dplyr::desc(freq)), n = 15)

df_mammals_bib[df_mammals_bib$Journal_Institution == "Aqua : journal of ichthyology and aquatic biology", ]$Journal_Institution <- "Aqua: Journal of Ichthyology and Aquatic Biology"
df_mammals_bib[df_mammals_bib$Journal_Institution == "Environmental pollution", ]$Journal_Institution <- "Environmental Pollution"
df_mammals_bib[df_mammals_bib$Journal_Institution == "holos", ]$Journal_Institution <- "Holos"
df_mammals_bib[df_mammals_bib$Journal_Institution == "Marine BioDiversity Records", ]$Journal_Institution <- "Marine Biodiversity Records"
df_mammals_bib[df_mammals_bib$Journal_Institution == "Marine BioDiversity", ]$Journal_Institution <- "Marine Biodiversity"
df_mammals_bib[df_mammals_bib$Journal_Institution == "Scientific reports", ]$Journal_Institution <- "Scientific Reports"
df_mammals_bib[df_mammals_bib$Journal_Institution == "The Anatomical Record", ]$Journal_Institution <- "Anatomical Record"
df_mammals_bib[df_mammals_bib$Journal_Institution == "The Journal of Cetacean Research and Management", ]$Journal_Institution <- "Journal of Cetacean Research and Management"
df_mammals_bib[df_mammals_bib$Journal_Institution == "The Journal of Parasitology", ]$Journal_Institution <- "Journal of Parasitology"
df_mammals_bib[df_mammals_bib$Journal_Institution == "The Journal of the Acoustical Society of America", ]$Journal_Institution <- "Journal of the Acoustical Society of America"
df_mammals_bib[df_mammals_bib$Journal_Institution == "Ocean and Coastal Management", ]$Journal_Institution <- "Ocean & Coastal Management"
## "Cadernos em Biodiversidade" --- check, isn't it "Cadernos da Biodiversidade"
## "Deep Sea Research"          --- All good

### ---
plyr::count(df_mammals_bib$Journal_scope) ## -- All good

### ---
plyr::count(df_mammals_bib$Language) 
## "English | Spanish" -- All good, it is actually written in two languages

### ---
df_mammals_bib %>% 
  tidyr::separate_rows(Realm, sep = " \\| ") %>%
  dplyr::group_by(Realm) %>% 
  dplyr::summarise(n = n()) ## -- All good

### ---
View(
  df_mammals_bib %>% 
    tidyr::separate_rows(Federal_states, sep = " \\| ") %>%
    dplyr::group_by(Federal_states) %>% 
    dplyr::summarise(n = n())
) ## -- All good

### ---
plyr::count(df_mammals_bib$Ecological_scale) ## -- All good

### ---
plyr::count(df_mammals_bib$Time_scale) ## -- 147 "NA" -- check...

### ---
plyr::count(df_mammals_bib$Primary_theme)
dplyr::arrange(plyr::count(df_mammals_bib$Primary_theme), dplyr::desc(freq))  ## -- All good

### ---
plyr::count(df_mammals_bib$Secondary_theme)
dplyr::arrange(plyr::count(df_mammals_bib$Secondary_theme), dplyr::desc(freq))  ## -- All good

### ---
View(
  df_mammals_bib %>% 
    tidyr::separate_rows(Anthropogenic_threats, sep = " \\| ") %>%
    dplyr::group_by(Anthropogenic_threats) %>% 
    dplyr::summarise(n = n())
) ## Typo

# "Bycacth" 
# Fix it
df_mammals_bib <- 
  df_mammals_bib %>% 
  dplyr::mutate(Anthropogenic_threats = gsub(pattern = "Bycacth",
                                             replacement = "Bycatch",
                                             x = Anthropogenic_threats))

### ---
View(
  df_mammals_bib %>% 
    tidyr::separate_rows(Conservation_Unity, sep = " \\| ") %>%
    dplyr::group_by(Conservation_Unity) %>% 
    dplyr::summarise(n = n())
)  ## All good

### ---
plyr::count(df_mammals_bib$Georeferencing_data) ## -- All good

### ---
plyr::count(df_mammals_bib$Data_availability) ## -- All good

### ---
View(
  df_mammals_bib %>% 
    tidyr::separate_rows(Database, sep = " \\| ") %>%
    dplyr::group_by(Database) %>% 
    dplyr::summarise(n = n())
) ## -- Few typos, fix:

df_mammals_bib[df_mammals_bib$Database == "DRYAD", ]$Database <- "Dryad"
df_mammals_bib[df_mammals_bib$Database == "Genbank", ]$Database <- "GenBank"
df_mammals_bib[df_mammals_bib$Database == "NCI SRA", ]$Database <- "SRA"
df_mammals_bib[df_mammals_bib$Database == "NIH SRA", ]$Database <- "SRA"

### ---
length(unique(df_mammals_bib$Reference)) ## -- 3 refs that are wrong: check all info

tmp <-
  df_mammals_bib %>%
  dplyr::group_by(Reference) %>%
  dplyr::summarise(n = n()) %>%
  dplyr::filter(n > 1)
#### --------------------------------- Need to check these in 'tmp'

# Simões-Lopes, 1998 ("10_677") has the wrong Reference and DOI
df_mammals_bib[df_mammals_bib$Number == "10_677", ]$DOI_ISBN <- "NA"
df_mammals_bib[df_mammals_bib$Number == "10_677", ]$Reference <- 
  "Simões-Lopes, P. C. (1998). Intraspecific agonistic behavior of Tursiops truncatus (Cetacea, Delphinidae) during dolphin-human cooperative fishing in southern Brazil. Biotemas, 11(2), 165–171."

# Moura et al., 2016 ("10_148") has the wrong Reference and DOI
df_mammals_bib[df_mammals_bib$Number == "10_148", ]$DOI_ISBN <- "https://doi.org/10.1371/journal.pone.0146108"
df_mammals_bib[df_mammals_bib$Number == "10_148", ]$Reference <- 
  "Moura, J. F., Acevedo-Trejos, E., Tavares, D. C., Meirelles, A. C., Silva, C. P., Oliveira, L. R., Santos, R. A., Wickert, J. C., Machado, R., Siciliano, S. and Merico, A. (2016). Stranding events of Kogia whales along the Brazilian coast. PLoS One, 11(1), e0146108."

# --------------- "Bisi et al., 2012" [wrong] (Number "7_214") has the bib info quite wrong ?

### --- Remove IDs identified to be removed
df_mammals_bib <-
  df_mammals_bib %>% 
  dplyr::filter(! Number %in% IDs_to_rm)

write.csv2(df_mammals_bib, "./data-processed/bib_marine-mammals.csv", row.names = FALSE)

## -------------------------------------------------------------------------- ##
## --------------------------- df_mammals_spp ------------------------------- ##
## -------------------------------------------------------------------------- ##

# colnames(df_mammals_spp)

plyr::count(df_mammals_spp$Kingdom_Domain) ## All good
plyr::count(df_mammals_spp$Phylum) ## All good
plyr::count(df_mammals_spp$Class) ## All good

plyr::count(df_mammals_spp$Order) ## --- For analysis, need to remove several orders
plyr::count(df_mammals_spp$Family) ## --- For analysis, need to remove several families

df_mammals_spp[df_mammals_spp$Family == "Didelhidae", ]$Family <- "Didelphidae"

plyr::count(df_mammals_spp$Species) ## --- For analysis, need to remove several species, including "sp." etc.

plyr::count(df_mammals_spp$Species_author) ## All good

plyr::count(df_mammals_spp$Ecological_group) ## Check 'Terrestrial', 'Benthos' and 'NA'?

### ---
View(
  df_mammals_spp %>% 
    tidyr::separate_rows(Habitat, sep = " \\| ") %>%
    dplyr::group_by(Habitat) %>% 
    dplyr::summarise(n = n())
) ## Typo -- "|Shallow reefs"
# --- Code below is not working... need to figure it out... ----- Fixed by hand ;)
# df_mammals_spp <- 
#   df_mammals_spp %>% 
#   dplyr::mutate(Habitat = gsub(pattern = "|Shallow",
#                                replacement = "| Shallow",
#                                x = Habitat))

### ---
View(
  df_mammals_spp %>% 
    tidyr::separate_rows(Data_type, sep = " \\| ") %>%
    dplyr::group_by(Data_type) %>% 
    dplyr::summarise(n = n())
) ## --- Fix?? 'Fishery' -> 'Fishing'?

### ---
View(
  df_mammals_spp %>% 
    tidyr::separate_rows(Date_year, sep = " \\| ") %>%
    dplyr::group_by(Date_year) %>% 
    dplyr::summarise(n = n())
) ## All good

### ---
View(
  df_mammals_spp %>% 
    tidyr::separate_rows(Date_month, sep = " \\| ") %>%
    dplyr::group_by(Date_month) %>% 
    dplyr::summarise(n = n())
)  ## --- All good

### ---
View(
  df_mammals_spp %>% 
    tidyr::separate_rows(Date_day, sep = " \\| ") %>%
    dplyr::group_by(Date_day) %>% 
    dplyr::summarise(n = n())
)  ## --- All good

### ---
View(
  df_mammals_spp %>% 
    tidyr::separate_rows(Federal_states, sep = " \\| ") %>%
    dplyr::group_by(Federal_states) %>% 
    dplyr::summarise(n = n())
)  ## --- Check: What about the 'estimated' ones -- are their classified under a single 'state'?

plyr::count(grepl(pattern = ",", x = df_mammals_spp$Latitude)) ## --- All good
plyr::count(grepl(pattern = ",", x = df_mammals_spp$Longitude)) ## --- All good

plyr::count(df_mammals_spp$Coord_source) ## What's 'NA'?

## --- Fix Lat/Lon columns -- no longer needed
# df_mammals_spp$Longitude <- gsub(pattern = ",", 
#                                  replacement = ".",
#                                  x = df_mammals_spp$Longitude)
# 
# df_mammals_spp$Latitude <- gsub(pattern = ",", 
#                                 replacement = ".",
#                                 x = df_mammals_spp$Latitude)
# df_mammals_spp$Latitude <- gsub(pattern = "_", 
#                                 replacement = "",
#                                 x = df_mammals_spp$Latitude)

### --- Remove IDs identified to be removed
df_mammals_spp <-
  df_mammals_spp %>% 
  dplyr::filter(! Number %in% IDs_to_rm)

write.csv2(df_mammals_spp, "./data-processed/spp_marine-mammals.csv", row.names = FALSE)

