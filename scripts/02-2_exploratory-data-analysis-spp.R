##
## Exploratory Data Analysis: Species records info
## Plots and summaries to explore the datasets
##

## Misc [not used] ####

# Colour palette
# manual_cols <- 
#   palette.colors(palette = "Polychrome 36")[1:length(unique(tmp$Data_type))] %>% 
#   as.vector()

## Libraries ####
library(plyr)
library(dplyr)
library(tidyr)
library(tibble)
library(forcats)
library(ggplot2)
library(ggrepel)
library(ggspatial)
library(patchwork)
library(sf)
library(mapview)
library(betapart)

## Read data ####

df_turtles_spp <- read.csv("./data-processed/spp_sea-turtles_meow.csv")
df_birds_spp <- read.csv("./data-processed/spp_seabirds_meow.csv")
df_mammals_spp <- read.csv("./data-processed/spp_marine-mammals_meow.csv")

#### ----------- Clean 'turtles' taxonomy
df_turtles_spp <-
  df_turtles_spp %>% 
  dplyr::filter(! is.na(Species)) %>% 
  dplyr::filter(! Species == "Hybrid")

# unique(df_turtles_spp$Species)

#### ----------- Clean 'birds' taxonomy
## Filter families
df_birds_spp <-
  dplyr::filter(df_birds_spp,
                Family %in% c("Ardeidae", "Charadriidae", "Chionidae", "Ciconiidae",
                              "Diomedeidae", "Fregatidae", "Haematopodidae", "Hydrobatidae",
                              "Laridae", "Oceanitidae", "Pelecanidae", "Phaethontidae",
                              "Phalacrocoracidae", "Procellariidae", "Scolopacidae", "Spheniscidae",
                              "Stercorariidae", "Sulidae")) 

## Filter freshwater-associated species and " sp"
df_birds_spp <-
  df_birds_spp %>% 
  dplyr::filter(! Species %in% c("NA", "Ciconia maguari", "Gallinago paraguaiae", "Syrigma sibilatrix",
                                 "Ardeola ralloides", "Ixobrychus involucris", "Hybrid", 
                                 "Cochlearius cochlearius", "Jabiru mycteria", "Ixobrychus exilis",
                                 "Agamia agami", "Mycteria americana")) %>% 
  dplyr::filter(! stringr::str_detect(string = Species, pattern = " sp"))

# Check 'Species' again
# sort(unique(df_birds_spp$Species))

## Still some errors... fix them
df_birds_spp[df_birds_spp$Species == "Diomedea melanophris", ]$Species <- "Thalassarche melanophris"
df_birds_spp[df_birds_spp$Species == "Larus atricilla", ]$Species <- "Leucophaeus atricilla"
df_birds_spp[df_birds_spp$Species == "Larus maculipennis", ]$Species <- "Chroicocephalus maculipennis"
df_birds_spp[df_birds_spp$Species == "Larus delawerensis", ]$Species <- "Larus delawarensis"


## Check:
# "Ardeidae" & "Ciconiidae"... 
# "Egretta" spp., "Botaurus pinnatus", "Butorides striata", ...


#### ----------- Clean 'mammals' taxonomy

# sort(unique(df_mammals_spp$Order))
## Filter 'Orders' away
df_mammals_spp <-
  df_mammals_spp %>% 
  dplyr::filter(! Order %in% c("Chiroptera", "Cingulata", "Didelphimorphia", 
                               "Lagomorpha", "Pilosa", "Rodentia")) 

# sort(unique(df_mammals_spp$Family))
## Filter 'Families' away
df_mammals_spp <-
  df_mammals_spp %>% 
  dplyr::filter(! Family %in% c("Bovidae", "Canidae", "Cervidae", "Felidae", 
                                "Procyonidae", "Suidae"))

# sort(unique(df_mammals_spp$Species))

## Fix typos: 
# an undefined species to "sp."
df_mammals_spp[df_mammals_spp$Species == "Balaenoptera acutorostrata/bonaerensis", ]$Species <- "Balaenoptera sp."
# a subspecies to species
df_mammals_spp[df_mammals_spp$Species == "Balaenoptera musculus intermedia", ]$Species <- "Balaenoptera musculus"

# Filter
df_mammals_spp <-
  df_mammals_spp %>%
  dplyr::filter(! stringr::str_detect(string = Species, pattern = " sp")) %>% 
  dplyr::filter(! stringr::str_detect(string = Species, pattern = "Hybrid")) %>%
  dplyr::filter(! is.na(Species))
  # %>% dplyr::filter(! stringr::str_detect(string = Species, pattern = "NA"))


#### -------------------------------- # 
dfs_list <- list(
  spp_turtles = df_turtles_spp,
  spp_seabirds = df_birds_spp,
  spp_mammals = df_mammals_spp
)

## Read spatial data ####

sf_world <- 
  sf::read_sf("./data-spatial/World.gpkg") %>% 
  sf::st_transform(4326)

sf_meow <- 
  sf::read_sf("./data-spatial/Brazil_MEOW.gpkg") %>% 
  dplyr::select(ECOREGION, PROVINCE) %>% 
  sf::st_transform(4326)

sf_brazil <- 
  sf::read_sf("./data-spatial/Brazil_Federal-states.gpkg") %>% 
  sf::st_transform(4326)

sf_blue_amazon <- 
  sf::read_sf("./data-spatial/Brazil_Blue-Amazon-extended.gpkg") %>% 
  sf::st_transform(4326)

# plot(sf_blue_amazon)

## [-- COME BACK HERE --] Number of studies and species recorded, by Ecoregion ####

## Get both maps for each megafauna group, patchwork them, save
for (df in 1:length(dfs_list)) {
  
  ## Get data.frame name
  df_name <- names(dfs_list)[df]
  
  ## Get data according to data.frame name
  df <- dfs_list[[df_name]]
  
  tmp <- 
    df %>%
    dplyr::group_by(ECOREGION) %>%
    dplyr::summarise(n_species = n_distinct(Species), 
                     n_studies = n_distinct(Number), 
                     .groups = "drop") %>% 
    dplyr::mutate(n_ratio = round(n_species/n_studies, digits = 2))
  
  meow_spp <- 
    merge(sf_meow, tmp, by = "ECOREGION") %>% 
    dplyr::filter(! ECOREGION %in% c("Uruguay-Buenos Aires Shelf", "Guianan")) %>% 
    dplyr::filter(! is.na(ECOREGION))
  
  plot_species <- 
    ggplot() + 
    geom_sf(data = sf_blue_amazon, fill = "lightblue", color = "lightblue") +
    geom_sf(data = meow_spp, aes(fill = n_species), color = "black") +
    geom_sf_text(data = meow_spp, aes(label = n_species), 
                 nudge_x = 2.1, size = 2.5, color = "black") +
    geom_sf(data = sf_world, fill = "grey80", colour = "grey70") +
    geom_sf(data = sf_brazil, fill = "grey65", colour = "grey20") +
    coord_sf(xlim = c(-60, -20), ylim = c(-37, 7)) +
    scale_fill_gradient2(
      low    = "#fff5eb",     # "Oranges"
      mid    = "#fee6ce",
      high   = "#a63603",
      name   = "No. species",
      breaks = round(seq(0, max(meow_spp$n_species), 
                         by = max(meow_spp$n_species)/5), 
                     digits = 0)
    ) +
    theme_linedraw() +
    theme(
      legend.position       = "top",
      legend.direction      = "horizontal", 
      legend.title          = element_text(size = 8),
      legend.text           = element_text(size = 8),
      legend.justification  = "center",
      axis.title.x          = element_blank(),
      axis.title.y          = element_blank()
    ) +
    guides(fill = guide_colorsteps(show.limits = TRUE)) +
    ggspatial::annotation_north_arrow(
      location = "br", which_north = "true",
      style = north_arrow_fancy_orienteering(),
      height = unit(1, "cm"),
      width = unit(1, "cm")
    )
  
  plot_studies <- 
    ggplot() + 
    geom_sf(data = sf_blue_amazon, fill = "lightblue", color = "lightblue") +
    geom_sf(data = meow_spp, aes(fill = n_studies), color = "black") +
    geom_sf_text(data = meow_spp, aes(label = n_studies), 
                 nudge_x = 2.1, size = 2.5, color = "black") +
    geom_sf(data = sf_world, fill = "grey80", colour = "grey70") +
    geom_sf(data = sf_brazil, fill = "grey65", colour = "grey20") +
    coord_sf(xlim = c(-60, -20), ylim = c(-37, 7)) +
    scale_fill_gradient2(
      low    = "#fcfbfd",     # "Purples"
      mid    = "#9e9ac8",
      high   = "#3f007d",
      name   = "No. studies",
      breaks = round(seq(0, max(meow_spp$n_studies), 
                         by = max(meow_spp$n_studies)/5), 
                     digits = 0)
    ) +
    theme_linedraw() +
    theme(
      legend.position       = "top",
      legend.direction      = "horizontal", 
      legend.title          = element_text(size = 8),
      legend.text           = element_text(size = 8),
      legend.justification  = "center",
      axis.title.x          = element_blank(),
      axis.title.y          = element_blank()
    ) +
    guides(fill = guide_colorsteps(show.limits = TRUE))
  
  plot_ratio <- 
    ggplot() + 
    geom_sf(data = sf_blue_amazon, fill = "lightblue", color = "lightblue") +
    geom_sf(data = meow_spp, aes(fill = n_ratio), color = "black") +
    geom_sf_text(data = meow_spp, aes(label = n_ratio), 
                 nudge_x = 2.1, size = 2.5, color = "black") +
    geom_sf(data = sf_world, fill = "grey80", colour = "grey70") +
    geom_sf(data = sf_brazil, fill = "grey65", colour = "grey20") +
    coord_sf(xlim = c(-60, -20), ylim = c(-37, 7)) +
    scale_fill_gradient2(
      low    = "#f7fcf5",     # "Greens"
      mid    = "#a1d99b",
      high   = "#00441b",
      name   = "Ratio",
      breaks = seq(0, max(meow_spp$n_ratio), 
                   by = max(meow_spp$n_ratio)/5)
    ) +
    theme_linedraw() +
    theme(
      legend.position       = "top",
      legend.direction      = "horizontal", 
      legend.title          = element_text(size = 8),
      legend.text           = element_text(size = 8),
      legend.justification  = "center",
      axis.title.x          = element_blank(),
      axis.title.y          = element_blank()
    ) +
    guides(fill = guide_colorsteps(show.limits = TRUE))
  
  plot <- plot_studies + plot_species + plot_ratio
  
  ggsave(plot, 
         filename = paste0("./results/EDA_spp_number-species-and-studies-by-ecoregion_", 
                           gsub(pattern = "spp_", replacement = "", x = df_name),
                           ".pdf"),
         height = 10, width = 20, units = "cm", dpi = 200)
  
  ## Clean environment
  rm(df, df_name, tmp, meow_spp, 
     plot_studies, plot_species, plot_ratio, plot)
}

## Species most recorded - barplot ####

for (df in 1:length(dfs_list)) {
  
  ## Get data.frame name
  df_name <- names(dfs_list)[df]
  
  ## Get data according to data.frame name
  df <- dfs_list[[df_name]]
  
  tmp <- 
    df %>% 
    dplyr::group_by(Species) %>% 
    dplyr::summarise(n = n_distinct(Number)) %>% 
    dplyr::arrange(desc(n)) %>% 
    head(15) %>% 
    dplyr::mutate(Species = forcats::fct_reorder(Species, n)) %>% 
    dplyr::filter(! is.na(Species))
  
  plot <- 
    ggplot(data = tmp,
           aes(y = n, x = Species)) + 
    geom_bar(stat = "identity", 
             fill = "lightblue", color = "black", width = 0.7) +
    geom_text(aes(label = n), 
              nudge_y = -15,
              # position = position_dodge(width = 0.7), 
              hjust = -0.2, color = "black", size = 3.5) +
    xlab("") + ylab("Number of publications") + 
    coord_flip() +
    theme_bw() +
    theme(axis.title = element_text(size = 10, face = "bold"),
          axis.text = element_text(size = 10),
          axis.text.y = element_text(face = "italic"))
  
  ggsave(plot, 
         filename = paste0("./results/EDA_spp_species-most-recorded-15top_", 
                           gsub(pattern = "spp_", replacement = "", x = df_name),
                           ".pdf"),
         height = 10, width = 15.5, units = "cm", dpi = 200)
  
  ## Clean environment
  rm(df, df_name, tmp, plot)
}

## Species most recorded, by 5-yr - linegraph ####

for (df in 1:length(dfs_list)) {
  
  ## Get data.frame name
  df_name <- names(dfs_list)[df]
  
  ## Get data according to data.frame name
  df <- dfs_list[[df_name]]
  
  top10_alltimes <- 
    df %>% 
    dplyr::distinct(Number, Species, .keep_all = TRUE) %>% 
    dplyr::mutate(Publication_date = 
                    as.numeric(stringr::str_sub(Citation, start= -4)),
                  .after = Citation) %>% 
    dplyr::group_by(Species, Publication_date) %>% 
    dplyr::filter(! is.na(Species)) %>% 
    group_by(Species) %>% 
    dplyr::summarise(n = n()) %>% 
    dplyr::arrange(desc(n)) %>% 
    head(10) %>% 
    dplyr::pull(Species)
  
  tmp <- 
    df %>% 
    dplyr::distinct(Number, Species, .keep_all = TRUE) %>% 
    dplyr::mutate(Publication_date = 
                    as.numeric(stringr::str_sub(Citation, start= -4)),
                  .after = Citation) %>% 
    dplyr::mutate(Year_5yr = case_when(
      Publication_date >= 1951 & Publication_date <= 1955 ~ "1951-1955",
      Publication_date >= 1956 & Publication_date <= 1960 ~ "1956-1960",
      Publication_date >= 1961 & Publication_date <= 1965 ~ "1961-1965",
      Publication_date >= 1966 & Publication_date <= 1970 ~ "1966-1970",
      Publication_date >= 1971 & Publication_date <= 1975 ~ "1971-1975",
      Publication_date >= 1976 & Publication_date <= 1980 ~ "1976-1980",
      Publication_date >= 1981 & Publication_date <= 1985 ~ "1981-1985",
      Publication_date >= 1986 & Publication_date <= 1990 ~ "1986-1990",
      Publication_date >= 1991 & Publication_date <= 1995 ~ "1991-1995",
      Publication_date >= 1996 & Publication_date <= 2000 ~ "1996-2000",
      Publication_date >= 2001 & Publication_date <= 2005 ~ "2001-2005",
      Publication_date >= 2006 & Publication_date <= 2010 ~ "2006-2010",
      Publication_date >= 2011 & Publication_date <= 2015 ~ "2011-2015",
      Publication_date >= 2016 & Publication_date <= 2020 ~ "2016-2020",
      Publication_date >= 2021 & Publication_date <= 2025 ~ "2021-2025*"),
      .after = Publication_date
    ) %>%
    dplyr::filter(! is.na(Species)) %>% 
    dplyr::filter(! is.na(Year_5yr)) %>% 
    group_by(Species, Year_5yr) %>% 
    dplyr::summarise(n = n()) %>% 
    dplyr::filter(Species %in% top10_alltimes)
  
  safe_rcartocolor_palette <- c("#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499", 
                                      "#44AA99", "#999933", "#882255", "#661100", "#6699CC", "#888888")
  
  plot <- 
    ggplot(data = tmp) +
    geom_line(aes(y = n, x = Year_5yr, 
                  group = Species, color = Species)) + 
    scale_color_manual(values = safe_rcartocolor_palette, name = "") + 
    xlab("") + ylab("Number of publications") + 
    guides(color = guide_legend(nrow = 3)) + 
    theme_bw() + 
    theme(axis.title = element_text(size = 8, face = "bold"),
          axis.text = element_text(size = 8),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
          legend.position = "bottom",
          legend.text = element_text(size = 7))
  
  ggsave(plot, 
         filename = paste0("./results/EDA_spp_species-most-recorded-10top-by-year_", 
                           gsub(pattern = "spp_", replacement = "", x = df_name),
                           ".pdf"),
         height = 10, width = 16, units = "cm", dpi = 200)
  
  ## Clean environment
  rm(df, df_name, top10_alltimes, tmp, plot, safe_rcartocolor_palette)
}

## Number of studies for the top-20 species, by 5-yr periods ####

for (df in 1:length(dfs_list)) {
  
  ## Get data.frame name
  df_name <- names(dfs_list)[df]
  
  ## Get data according to data.frame name
  df <- dfs_list[[df_name]]
  
  top20_alltimes <- 
    df %>% 
    dplyr::distinct(Number, Species, .keep_all = TRUE) %>% 
    dplyr::mutate(Publication_date = 
                    as.numeric(stringr::str_sub(Citation, start= -4)),
                  .after = Citation) %>% 
    dplyr::filter(! is.na(Species)) %>% 
    dplyr::group_by(Species) %>% 
    dplyr::summarise(n = n()) %>% 
    dplyr::arrange(desc(n)) %>% 
    head(20) %>% 
    dplyr::pull(Species)
  
  tmp <- 
    df %>% 
    dplyr::distinct(Number, Species, .keep_all = TRUE) %>% 
    dplyr::mutate(Publication_date = 
                    as.numeric(stringr::str_sub(Citation, start= -4)),
                  .after = Citation) %>% 
    dplyr::mutate(Year_5yr = case_when(
      Publication_date >= 1951 & Publication_date <= 1955 ~ "1951-1955",
      Publication_date >= 1956 & Publication_date <= 1960 ~ "1956-1960",
      Publication_date >= 1961 & Publication_date <= 1965 ~ "1961-1965",
      Publication_date >= 1966 & Publication_date <= 1970 ~ "1966-1970",
      Publication_date >= 1971 & Publication_date <= 1975 ~ "1971-1975",
      Publication_date >= 1976 & Publication_date <= 1980 ~ "1976-1980",
      Publication_date >= 1981 & Publication_date <= 1985 ~ "1981-1985",
      Publication_date >= 1986 & Publication_date <= 1990 ~ "1986-1990",
      Publication_date >= 1991 & Publication_date <= 1995 ~ "1991-1995",
      Publication_date >= 1996 & Publication_date <= 2000 ~ "1996-2000",
      Publication_date >= 2001 & Publication_date <= 2005 ~ "2001-2005",
      Publication_date >= 2006 & Publication_date <= 2010 ~ "2006-2010",
      Publication_date >= 2011 & Publication_date <= 2015 ~ "2011-2015",
      Publication_date >= 2016 & Publication_date <= 2020 ~ "2016-2020",
      Publication_date >= 2021 & Publication_date <= 2025 ~ "2021-2025*"),
      .after = Publication_date
    ) %>%
    dplyr::filter(! is.na(Species)) %>% 
    dplyr::filter(! is.na(Year_5yr)) %>% 
    dplyr::group_by(Species, Year_5yr) %>% 
    dplyr::summarise(n = n()) %>% 
    dplyr::filter(Species %in% top20_alltimes) %>% 
    dplyr::mutate(Species_ordered = factor(Species, 
                                           levels = rev(top20_alltimes)))
  
  plot <- 
    ggplot(data = tmp,
           aes(y = Species_ordered, x = Year_5yr, fill = n)) +
    geom_tile(color = "white") +
    scale_fill_viridis_c(name = "No. publications", option = "C") +
    xlab("") + ylab("") + 
    theme_bw() + 
    theme(axis.text = element_text(size = 8),
          axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
          axis.text.y = element_text(face = "italic"))
  
  ggsave(plot, 
         filename = paste0("./results/EDA_spp_species-most-studied-20top-by-5yr_", 
                           gsub(pattern = "spp_", replacement = "", x = df_name),
                           ".pdf"),
         height = 16, width = 16, units = "cm", dpi = 200)
  
  ## Clean environment
  rm(df, df_name, top20_alltimes, tmp, plot)
}

## Beta-diversity ####

for (df in 1:length(dfs_list)) {
  
  ## Get data.frame name
  df_name <- names(dfs_list)[df]
  
  ## Get data according to data.frame name
  df <- dfs_list[[df_name]]
  
  tmp <- 
    df %>% 
    dplyr::filter(! ECOREGION %in% c("NA", "Uruguay-Buenos Aires Shelf", 
                                     "Rio de la Plata", "Guianan")) %>% 
    dplyr::filter(! is.na(ECOREGION)) %>% 
    dplyr::distinct(ECOREGION, Species) %>% 
    dplyr::mutate(presence = 1) %>% 
    tidyr::pivot_wider(id_cols = ECOREGION,
                       names_from = Species,
                       values_from = presence,
                       values_fill = 0) %>% 
    tibble::column_to_rownames(var = "ECOREGION")
  
  ## Calculate betadiv
  beta_res <- betapart::beta.pair(tmp, index.family = "sorensen")
  
  ## Nestedness
  tmp_nestedness <- 
    data.frame(as.matrix(beta_res$beta.sne)) %>% 
    tibble::rownames_to_column("Var1") %>% 
    tidyr::pivot_longer(cols = c(2:ncol(.)),
                        names_to = "Var2") %>% 
    dplyr::mutate(Var2 = gsub(pattern = "\\.", replacement = " ", x = Var2))
  
  plot_nestedness <-
    ggplot(tmp_nestedness, aes(Var2, Var1, fill = value)) +
    geom_tile() +
    geom_text(aes(label = round(value, 2)), color = "black", size = 2) + 
    scale_fill_gradient(low = "white", 
                        high = "#B2182B",
                        name = "Nestedness") + # Sørensen dissimilarity
    xlab("") + ylab("") +
    theme_minimal() +
    theme(axis.text = element_text(size = 8),
          axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggsave(plot_nestedness, 
         filename = paste0("./results/EDA_spp_ecoregion-nestedness_", 
                           gsub(pattern = "spp_", replacement = "", x = df_name),
                           ".pdf"),
         height = 12, width = 16, units = "cm", dpi = 200)
  
  ## Sorensen dissimilarity
  pcoa_res <- cmdscale(beta_res$beta.sor, eig = TRUE, k = 2)
  
  tmp_pcoa <- data.frame(
    Ecoregion = rownames(pcoa_res$points),
    Axis1 = pcoa_res$points[, 1],
    Axis2 = pcoa_res$points[, 2]
  )
  
  plot_pcoa <- 
    ggplot(tmp_pcoa, 
         aes(x = Axis1, y = Axis2, label = Ecoregion)) +
    geom_point(size = 3, color = "blue") +
    ggrepel::geom_label_repel() +
    xlab("PCoA 1") + ylab("PCoA 2") +
    theme_bw()
  
  ggsave(plot_pcoa, 
         filename = paste0("./results/EDA_spp_ecoregion-Sorensen-PCoA_", 
                           gsub(pattern = "spp_", replacement = "", x = df_name),
                           ".pdf"),
         height = 12, width = 14, units = "cm", dpi = 200)
  
  ## Clean environment
  rm(df, df_name, tmp, beta_res,
     tmp_nestedness, plot_nestedness,
     pcoa_res, tmp_pcoa, plot_pcoa)
}


## Histogram "Data_type" ####

# facet_turtles -- Species
# facet_birds -- Order
# facet_mammals -- Family

for (df in 1:length(dfs_list)) {
  
  ## Get data.frame name
  df_name <- names(dfs_list)[df]
  
  ## Get data according to data.frame name
  df <- dfs_list[[df_name]]
  
  if(df_name == "spp_turtles"){
    tmp <- 
      df %>%
      # Remove records without any (geospatial) 'source' (hence, missing location...)
      dplyr::filter(! Coord_source == "NA") %>% 
      # Split 'Data_type' 
      tidyr::separate_rows(Data_type, sep = " \\| ") %>% 
      dplyr::group_by(Species, Data_type) %>% 
      dplyr::summarise(n = n())
    
    plot <- 
      ggplot(data = tmp,
             aes(y = n, x = Data_type)) + 
      geom_bar(stat = "identity", 
               fill = "lightblue", color = "black", width = 0.7) +
      xlab("") + ylab("Number of records") + 
      coord_flip() +
      facet_wrap(~ Species, ncol = length(unique(tmp$Species)),
                 scales = "free_x") +
      theme_bw() +
      theme(axis.title = element_text(size = 9.5, face = "bold"),
            axis.text = element_text(size = 9.5),
            axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
            strip.text = element_text(size = 7.5))
    
    ggsave(plot, 
           filename = paste0("./results/EDA_spp_barplot-data-type_", 
                             gsub(pattern = "spp_", replacement = "", x = df_name),
                             ".pdf"),
           height = 13, width = 20, units = "cm", dpi = 200)
  }
  
  if(df_name == "spp_seabirds"){
    tmp <- 
      df %>%
      # Remove records without any (geospatial) 'source' (hence, missing location...)
      dplyr::filter(! Coord_source == "NA") %>% 
      # Split 'Data_type' 
      tidyr::separate_rows(Data_type, sep = " \\| ") %>% 
      dplyr::group_by(Order, Data_type) %>% 
      dplyr::summarise(n = n())
    
    plot <- 
      ggplot(data = tmp,
             aes(y = n, x = Data_type)) + 
      geom_bar(stat = "identity", 
               fill = "lightblue", color = "black", width = 0.7) +
      xlab("") + ylab("Number of records") + 
      coord_flip() +
      facet_wrap(~ Order, ncol = length(unique(tmp$Order)),
                 scales = "free_x") +
      theme_bw() +
      theme(axis.title = element_text(size = 9.5, face = "bold"),
            axis.text = element_text(size = 9.5),
            axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
            strip.text = element_text(size = 7.5))
    
    ggsave(plot, 
           filename = paste0("./results/EDA_spp_barplot-data-type_", 
                             gsub(pattern = "spp_", replacement = "", x = df_name),
                             ".pdf"),
           height = 13, width = 22, units = "cm", dpi = 200)
  }
  
  if(df_name == "spp_mammals"){
    tmp <- 
      df %>%
      # Remove records without any (geospatial) 'source' (hence, missing location...)
      dplyr::filter(! Coord_source == "NA") %>% 
      # Split 'Data_type' 
      tidyr::separate_rows(Data_type, sep = " \\| ") %>% 
      dplyr::group_by(Family, Data_type) %>% 
      dplyr::summarise(n = n())
    
    plot <- 
      ggplot(data = tmp,
             aes(y = n, x = Data_type)) + 
      geom_bar(stat = "identity", 
               fill = "lightblue", color = "black", width = 0.7) +
      xlab("") + ylab("Number of records") + 
      coord_flip() +
      facet_wrap(~ Family, ncol = length(unique(tmp$Family)),
                 scales = "free_x") +
      theme_bw() +
      theme(axis.title = element_text(size = 9.5, face = "bold"),
            axis.text = element_text(size = 9.5),
            axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
            strip.text = element_text(size = 7.5))
    
    ggsave(plot, 
           filename = paste0("./results/EDA_spp_barplot-data-type_", 
                             gsub(pattern = "spp_", replacement = "", x = df_name),
                             ".pdf"),
           height = 13, width = 35, units = "cm", dpi = 200)
  }
  
  ## Clean environment
  rm(df, df_name, tmp, plot)
}

## Maps "Data_type" ####

# facet_turtles -- Species
# facet_birds -- Order
# facet_mammals -- Family

for (df in 1:length(dfs_list)) {
  
  ## Get data.frame name
  df_name <- names(dfs_list)[df]
  
  ## Get data according to data.frame name
  df <- dfs_list[[df_name]]
  
  tmp <- 
    df %>%
    # Remove records without any (geospatial) 'source' (hence, missing location...)
    dplyr::filter(! Coord_source == "NA") %>% 
    # Split 'Data_type' 
    tidyr::separate_rows(Data_type, sep = " \\| ") %>% 
    # Transform 'df' to 'sf' 
    sf::st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)
  
  ## Get the 5-most recorded 'Data_type' and transform other categories into 'Other'
  vec <- 
    tmp %>% 
    dplyr::group_by(Data_type) %>% 
    dplyr::summarise(n = n()) %>% 
    dplyr::arrange(desc(n)) %>% 
    dplyr::filter(! Data_type == "Stranding") %>% 
    head(5) %>% 
    dplyr::pull(Data_type)
  
  tmp <-
    tmp %>% 
    dplyr::mutate(Data_type2 = 
                    case_when(! (Data_type %in% vec) ~ "Other",
                              .default = Data_type)) %>% 
    dplyr::filter(! Data_type2 == "Other")
  
  if(df_name == "spp_turtles"){
    plot <- 
      ggplot() + 
      geom_sf(data = sf_blue_amazon, fill = "lightblue", color = "lightblue", alpha = 0.2) +
      geom_sf(data = sf_world, fill = "grey80", colour = "grey70") +
      geom_sf(data = sf_brazil, fill = "grey65", colour = "grey20") +
      geom_sf(data = tmp, 
              aes(colour = as.factor(Data_type2),
                  shape = as.factor(Coord_source)),
              size = 1, alpha = 0.7) +
      scale_color_brewer(palette = "Dark2") +
      facet_wrap(~ Species, nrow = 2) + 
      coord_sf(xlim = c(-60, -20), ylim = c(-37, 7)) +
      theme_bw() +
      theme(
        legend.position       = "bottom",
        legend.direction      = "horizontal", 
        legend.title          = element_text(size = 8),
        legend.text           = element_text(size = 8),
        legend.justification  = "center",
        axis.title = element_blank()
      ) + 
      guides(colour = guide_legend(title = "Data type", nrow = 2, byrow = TRUE),
             shape = guide_legend(title = "Coord. source", nrow = 2, byrow = TRUE))
    
    ggsave(plot, 
           filename = paste0(#"./results/EDA_spp_maps-data-type_", 
                              "./results/EDA_spp_maps-top5-data-type_", 
                             gsub(pattern = "spp_", replacement = "", x = df_name),
                             ".pdf"),
           height = 16, width = 16, units = "cm", dpi = 200)
  }
  
  if(df_name == "spp_seabirds"){
    plot <- 
      ggplot() + 
      geom_sf(data = sf_blue_amazon, fill = "lightblue", color = "lightblue", alpha = 0.2) +
      geom_sf(data = sf_world, fill = "grey80", colour = "grey70") +
      geom_sf(data = sf_brazil, fill = "grey65", colour = "grey20") +
      geom_sf(data = tmp, 
              aes(colour = as.factor(Data_type2),
                  shape = as.factor(Coord_source)),
              size = 1, alpha = 0.7) +
      scale_color_brewer(palette = "Dark2") +
      facet_wrap(~ Order, nrow = 2) + 
      coord_sf(xlim = c(-60, -20), ylim = c(-37, 7)) +
      theme_bw() +
      theme(
        legend.position       = "bottom",
        legend.direction      = "horizontal", 
        legend.title          = element_text(size = 8),
        legend.text           = element_text(size = 8),
        legend.justification  = "center",
        axis.title = element_blank()
      ) + 
      guides(colour = guide_legend(title = "Data type", nrow = 2, byrow = TRUE),
             shape = guide_legend(title = "Coord. source", nrow = 2, byrow = TRUE))
    
    ggsave(plot, 
           filename = paste0(#"./results/EDA_spp_maps-data-type_", 
                              "./results/EDA_spp_maps-top5-data-type_",  
                             gsub(pattern = "spp_", replacement = "", x = df_name),
                             ".pdf"),
           height = 14, width = 16, units = "cm", dpi = 200)
  }
  
  if(df_name == "spp_mammals"){
    plot <- 
      ggplot() + 
      geom_sf(data = sf_blue_amazon, fill = "lightblue", color = "lightblue", alpha = 0.2) +
      geom_sf(data = sf_world, fill = "grey80", colour = "grey70") +
      geom_sf(data = sf_brazil, fill = "grey65", colour = "grey20") +
      geom_sf(data = tmp, 
              aes(colour = as.factor(Data_type2),
                  shape = as.factor(Coord_source)),
              size = 1, alpha = 0.7) +
      scale_color_brewer(palette = "Dark2") +
      facet_wrap(~ Family, nrow = 3) + 
      coord_sf(xlim = c(-60, -20), ylim = c(-37, 7)) +
      theme_bw() +
      theme(
        legend.position       = "bottom",
        legend.direction      = "horizontal", 
        legend.title          = element_text(size = 8),
        legend.text           = element_text(size = 8),
        legend.justification  = "center",
        axis.title = element_blank()
      ) + 
      guides(colour = guide_legend(title = "Data type", nrow = 2, byrow = TRUE),
             shape = guide_legend(title = "Coord. source", nrow = 2, byrow = TRUE))
    
    ggsave(plot, 
           filename = paste0(#"./results/EDA_spp_maps-data-type_", 
                              "./results/EDA_spp_maps-top5-data-type_", 
                             gsub(pattern = "spp_", replacement = "", x = df_name),
                             ".pdf"),
           height = 23, width = 25, units = "cm", dpi = 200)
  }
  
  ## Clean environment
  rm(df, df_name, tmp, vec, plot)
}

## Refs that contributed the most for spatialised records #### 

### --> Importance of open-access data

head(dplyr::arrange(plyr::count(df_turtles_spp[df_turtles_spp$Coord_source=="Author", ]$Citation), dplyr::desc(freq)), n = 15)
head(dplyr::arrange(plyr::count(df_birds_spp[df_birds_spp$Coord_source=="Author", ]$Citation), dplyr::desc(freq)), n = 15)
head(dplyr::arrange(plyr::count(df_mammals_spp[df_mammals_spp$Coord_source=="Author", ]$Citation), dplyr::desc(freq)), n = 15)

## NEXT... ####