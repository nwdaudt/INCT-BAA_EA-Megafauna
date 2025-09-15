##
## Exploratory Data Analysis: Bibliographic info
## Plots and summaries to explore the datasets
##

## Libraries ####
library(plyr)
library(dplyr)
library(tidyr)
library(forcats)
library(ggplot2)
library(ggwordcloud)
library(snakecase)
library(fmsb) # radar plot
library(bibliometrix)
# library(sf)
# library(mapview)

## Read data ####

df_bib_turtles <- 
  read.csv2("./data-processed/bib_sea-turtles.csv") %>% 
  dplyr::mutate(Year_5yr = case_when(
    Year >= 1951 & Year <= 1955 ~ "1951-1955",
    Year >= 1956 & Year <= 1960 ~ "1956-1960",
    Year >= 1961 & Year <= 1965 ~ "1961-1965",
    Year >= 1966 & Year <= 1970 ~ "1966-1970",
    Year >= 1971 & Year <= 1975 ~ "1971-1975",
    Year >= 1976 & Year <= 1980 ~ "1976-1980",
    Year >= 1981 & Year <= 1985 ~ "1981-1985",
    Year >= 1986 & Year <= 1990 ~ "1986-1990",
    Year >= 1991 & Year <= 1995 ~ "1991-1995",
    Year >= 1996 & Year <= 2000 ~ "1996-2000",
    Year >= 2001 & Year <= 2005 ~ "2001-2005",
    Year >= 2006 & Year <= 2010 ~ "2006-2010",
    Year >= 2011 & Year <= 2015 ~ "2011-2015",
    Year >= 2016 & Year <= 2020 ~ "2016-2020",
    Year >= 2021 & Year <= 2025 ~ "2021-2025*"),
    .after = Year
  ) %>% 
  dplyr::mutate(Year_10yr = case_when(
    Year >= 1951 & Year <= 1960 ~ "1951-1960",
    Year >= 1961 & Year <= 1970 ~ "1961-1970",
    Year >= 1971 & Year <= 1980 ~ "1971-1980",
    Year >= 1981 & Year <= 1990 ~ "1981-1990",
    Year >= 1991 & Year <= 2000 ~ "1991-2000",
    Year >= 2001 & Year <= 2010 ~ "2001-2010",
    Year >= 2011 & Year <= 2020 ~ "2011-2020",
    Year >= 2021 & Year <= 2025 ~ "2021-2025*"),
    .after = Year_5yr
  )

df_bib_seabirds <- 
  read.csv2("./data-processed/bib_seabirds.csv") %>% 
  dplyr::mutate(Year_5yr = case_when(
    Year >= 1951 & Year <= 1955 ~ "1951-1955",
    Year >= 1956 & Year <= 1960 ~ "1956-1960",
    Year >= 1961 & Year <= 1965 ~ "1961-1965",
    Year >= 1966 & Year <= 1970 ~ "1966-1970",
    Year >= 1971 & Year <= 1975 ~ "1971-1975",
    Year >= 1976 & Year <= 1980 ~ "1976-1980",
    Year >= 1981 & Year <= 1985 ~ "1981-1985",
    Year >= 1986 & Year <= 1990 ~ "1986-1990",
    Year >= 1991 & Year <= 1995 ~ "1991-1995",
    Year >= 1996 & Year <= 2000 ~ "1996-2000",
    Year >= 2001 & Year <= 2005 ~ "2001-2005",
    Year >= 2006 & Year <= 2010 ~ "2006-2010",
    Year >= 2011 & Year <= 2015 ~ "2011-2015",
    Year >= 2016 & Year <= 2020 ~ "2016-2020",
    Year >= 2021 & Year <= 2025 ~ "2021-2025*"),
    .after = Year
  ) %>% 
  dplyr::mutate(Year_10yr = case_when(
    Year >= 1951 & Year <= 1960 ~ "1951-1960",
    Year >= 1961 & Year <= 1970 ~ "1961-1970",
    Year >= 1971 & Year <= 1980 ~ "1971-1980",
    Year >= 1981 & Year <= 1990 ~ "1981-1990",
    Year >= 1991 & Year <= 2000 ~ "1991-2000",
    Year >= 2001 & Year <= 2010 ~ "2001-2010",
    Year >= 2011 & Year <= 2020 ~ "2011-2020",
    Year >= 2021 & Year <= 2025 ~ "2021-2025*"),
    .after = Year_5yr
  )

df_bib_mammals <- 
  read.csv2("./data-processed/bib_marine-mammals.csv") %>% 
  dplyr::mutate(Year_5yr = case_when(
    Year >= 1951 & Year <= 1955 ~ "1951-1955",
    Year >= 1956 & Year <= 1960 ~ "1956-1960",
    Year >= 1961 & Year <= 1965 ~ "1961-1965",
    Year >= 1966 & Year <= 1970 ~ "1966-1970",
    Year >= 1971 & Year <= 1975 ~ "1971-1975",
    Year >= 1976 & Year <= 1980 ~ "1976-1980",
    Year >= 1981 & Year <= 1985 ~ "1981-1985",
    Year >= 1986 & Year <= 1990 ~ "1986-1990",
    Year >= 1991 & Year <= 1995 ~ "1991-1995",
    Year >= 1996 & Year <= 2000 ~ "1996-2000",
    Year >= 2001 & Year <= 2005 ~ "2001-2005",
    Year >= 2006 & Year <= 2010 ~ "2006-2010",
    Year >= 2011 & Year <= 2015 ~ "2011-2015",
    Year >= 2016 & Year <= 2020 ~ "2016-2020",
    Year >= 2021 & Year <= 2025 ~ "2021-2025*"),
    .after = Year
  ) %>% 
  dplyr::mutate(Year_10yr = case_when(
    Year >= 1951 & Year <= 1960 ~ "1951-1960",
    Year >= 1961 & Year <= 1970 ~ "1961-1970",
    Year >= 1971 & Year <= 1980 ~ "1971-1980",
    Year >= 1981 & Year <= 1990 ~ "1981-1990",
    Year >= 1991 & Year <= 2000 ~ "1991-2000",
    Year >= 2001 & Year <= 2010 ~ "2001-2010",
    Year >= 2011 & Year <= 2020 ~ "2011-2020",
    Year >= 2021 & Year <= 2025 ~ "2021-2025*"),
    .after = Year_5yr
  )

dfs_list <- list(
  bib_turtles = df_bib_turtles,
  bib_seabirds = df_bib_seabirds,
  bib_mammals = df_bib_mammals
)

dfs_merged <- 
  rbind(cbind(df_bib_turtles, Megafauna_group = "Sea turtles"),
        cbind(df_bib_seabirds, Megafauna_group = "Seabirds"),
        cbind(df_bib_mammals, Megafauna_group = "Marine mammals")
        )

dfs_merged$Megafauna_group <- factor(dfs_merged$Megafauna_group,
                                     levels = c("Sea turtles", "Seabirds", "Marine mammals"))

## ------------------------------------- ##
## -------------- bibTex --------------- ##
## ------------------------------------- ##

# bibtex_mammals <- bibliometrix::convert2df("./data-raw/marine-mammals.bib",
#                                            format = "bibtex")


## 1) General summary of the data ####

df_summary <- 
  data.frame(
    "Megafauna_group" = c("Sea turtles", 
                          "Seabirds", 
                          "Marine mammals"),
    "No_publications" = c(length(unique(df_bib_turtles$Number)),
                                 length(unique(df_bib_seabirds$Number)),
                                 length(unique(df_bib_mammals$Number))),
    "Oldest_publication" = c(min(df_bib_turtles$Year), 
                             min(df_bib_seabirds$Year), 
                             min(df_bib_mammals$Year)),
    "No_primary_themes" = c(length(unique(df_bib_turtles$Primary_theme)),
                                   length(unique(df_bib_seabirds$Primary_theme)),
                                   length(unique(df_bib_mammals$Primary_theme))),
    "Data_availability_Y" = c(nrow(df_bib_turtles[df_bib_turtles$Data_availability == "Y", ]),
                                nrow(df_bib_seabirds[df_bib_seabirds$Data_availability == "Y", ]),
                                nrow(df_bib_mammals[df_bib_mammals$Data_availability == "Y", ])),
    "Geo_data_availability_Y" = c(nrow(df_bib_turtles[df_bib_turtles$Georeferencing_data == "Y", ]),
                                              nrow(df_bib_seabirds[df_bib_seabirds$Georeferencing_data == "Y", ]),
                                              nrow(df_bib_mammals[df_bib_mammals$Georeferencing_data == "Y", ]))
  )

write.csv(df_summary, "./results/EDA_bib_summary_megafauna.csv",
          row.names = FALSE)

rm(df_summary)

## 2.1) Publications by Year #### 

## Plot all megafauna groups together

tmp <- 
  dfs_merged %>% 
  dplyr::group_by(Year, Megafauna_group) %>% 
  dplyr::summarise(n = n()) %>% 
  dplyr::arrange(Year) %>% 
  dplyr::filter(! Year == 2024)

plot <- 
  ggplot(data = tmp,
         aes(y = n, x = Year, colour = Megafauna_group)) + 
  geom_line(linewidth = 0.5) + 
  geom_point(size = 1) + 
  scale_color_viridis_d(option = "turbo", name = "") + 
  scale_x_continuous(
    limits = c(min(tmp$Year), 2025),
    # breaks = c(min(tmp$Year), 2025, by = 10),       ## Have to figure it out a way of 'ceiling()' to numbers ending on '0' or '5'...
    expand = c(0, 0)) +
  scale_y_continuous(
    limits = c(0, NA),
    breaks = seq(0, (max(tmp$n, na.rm = TRUE) + 2), by = 10),
    expand = c(0, 0)) +
  xlab("Year") + ylab("Number of publications") + 
  theme_bw() +
  theme(axis.title = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 12),
        legend.position = c(0.2, 0.8),
        legend.text = element_text(size = 12))

ggsave(plot, 
       filename = paste0("./results/EDA_bib_number-of-pubs-by-year_megafauna.pdf"),
       height = 12, width = 16, units = "cm", dpi = 200)

rm(tmp, plot)

## Plot each megafauna group individually

for (df in 1:length(dfs_list)) {
  
  ## Get data.frame name
  df_name <- names(dfs_list)[df]
  
  ## Get data according to data.frame name
  df <- dfs_list[[df_name]]
  
  tmp <- 
    df %>% 
    dplyr::group_by(Year) %>% 
    dplyr::summarise(n = n()) %>% 
    dplyr::arrange(Year) %>% 
    dplyr::filter(! Year == 2024)
  
  plot <- 
    ggplot(data = tmp,
           aes(y = n, x = Year)) + 
    geom_line(colour = "grey30", linewidth = 0.5) + 
    geom_point(colour = "black", size = 1) + 
    scale_x_continuous(
      limits = c(min(tmp$Year), 2025),
      # breaks = c(min(tmp$Year), 2025, by = 10),       ## Have to figure it out a way of 'ceiling()' to numbers ending on '0' or '5'...
      expand = c(0, 0)) +
    scale_y_continuous(
      limits = c(0, NA),
      breaks = seq(0, (max(tmp$n, na.rm = TRUE) + 2), by = 10),
      expand = c(0, 0)) +
    xlab("Year") + ylab("Number of publications") + 
    theme_bw() +
    theme(axis.title = element_text(size = 12, face = "bold"),
          axis.text = element_text(size = 12))
  
  ggsave(plot, 
         filename = paste0("./results/EDA_bib_number-of-pubs-by-year_", 
                           gsub(pattern = "bib_", replacement = "", x = df_name),
                           ".pdf"),
         height = 12, width = 16, units = "cm", dpi = 200)
  
  ## Clean environment
  rm(df, df_name, tmp, plot)
}

## 2.2) Publications by Year and Region #### 

tmp <- 
  dfs_merged %>% 
  tidyr::separate_rows(Region, sep = " \\| ") %>% 
  dplyr::filter(Region %in% c("NE", "SE", "S", "N")) %>% 
  dplyr::group_by(Year, Region, Megafauna_group) %>% 
  dplyr::summarise(n = n(), .groups = 'drop') %>% 
  dplyr::arrange(Year)


plot <- 
  ggplot(tmp, aes(x = Year, y = n, color = Region)) +
  geom_line(linewidth = 0.8) + 
  geom_point(data = subset(tmp, n > 0), # Pontos apenas onde quantidade > 0
             size = 1) + 
  scale_color_manual(values = c("NE" = "#F1A7C1", "SE" = "#A2C8D9", 
                                "S" = "#9FDAAD", "N" = "#F4D66A"),
                     name = "Regions") + 
  facet_wrap(~ Megafauna_group, nrow = 3, scales = "free_y") +
  xlab("Year") + ylab("Number of publications") + 
  theme_bw() + 
  theme(axis.title = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 12),
        strip.text = element_text(size = 12, face = "bold"),
        legend.position = c(0.12, 0.9),
        legend.text = element_text(size = 12))

ggsave(plot, 
       filename = paste0("./results/EDA_bib_number-of-pubs-by-year-and-region_megafauna.pdf"),
       height = 22, width = 16, units = "cm", dpi = 200)

rm(tmp, plot)

## 2.3) Publications by Year and Ecoregions (TBD) #### 

# tmp <- 
#   dfs_merged %>% 
#   tidyr::separate_rows(Federal_states, sep = " \\| ") %>% 
#   dplyr::filter(Region %in% c("NE", "SE", "S", "N")) %>% 
#   dplyr::group_by(Year, Region, Megafauna_group) %>% 
#   dplyr::summarise(n = n(), .groups = 'drop') %>% 
#   dplyr::arrange(Year)
# 
# 
# plot <- 
#   ggplot(tmp, aes(x = Year, y = n, color = Region)) +
#   geom_line(linewidth = 0.8) + 
#   geom_point(data = subset(tmp, n > 0), # Pontos apenas onde quantidade > 0
#              size = 1) + 
#   scale_color_manual(values = c("NE" = "#F1A7C1", "SE" = "#A2C8D9", 
#                                 "S" = "#9FDAAD", "N" = "#F4D66A"),
#                      name = "Regions") + 
#   facet_wrap(~ Megafauna_group, nrow = 3, scales = "free_y") +
#   xlab("Year") + ylab("Number of publications") + 
#   theme_bw() + 
#   theme(axis.title = element_text(size = 12, face = "bold"),
#         axis.text = element_text(size = 12),
#         strip.text = element_text(size = 12, face = "bold"),
#         legend.position = c(0.12, 0.9),
#         legend.text = element_text(size = 12))
# 
# ggsave(plot, 
#        filename = paste0("./results/EDA_bib_number-of-pubs-by-year-and-region_megafauna.pdf"),
#        height = 22, width = 16, units = "cm", dpi = 200)
# 
# rm(tmp, plot)

## 3) Top authors ####

## Top *first* author

for (df in 1:length(dfs_list)) {
  
  ## Get data.frame name
  df_name <- names(dfs_list)[df]
  
  ## Get data according to data.frame name
  df <- dfs_list[[df_name]]
  
  tmp <- 
    df %>% 
    dplyr::group_by(Author) %>% 
    dplyr::summarise(n = n()) %>% 
    dplyr::arrange(desc(n)) %>% 
    head(15) %>% 
    dplyr::mutate(Author = forcats::fct_reorder(Author, n))
  
  plot <- 
    ggplot(data = tmp,
           aes(y = n, x = Author)) + 
    geom_bar(stat = "identity", 
             fill = "lightblue", color = "black", width = 0.7) +
    geom_text(aes(label = n), 
              position = position_dodge(width = 0.7), 
              hjust = -0.2, color = "black", size = 3.5) +
    xlab("Author") + ylab("Number of publications") + 
    coord_flip() +
    theme_bw() +
    theme(axis.title = element_text(size = 10, face = "bold"),
          axis.text = element_text(size = 10))
  
  ggsave(plot, 
         filename = paste0("./results/EDA_bib_top-author-first_", 
                           gsub(pattern = "bib_", replacement = "", x = df_name),
                           ".pdf"),
         height = 10, width = 15.5, units = "cm", dpi = 200)
  
  ## Clean environment
  rm(df, df_name, tmp, plot)
}

## Top authors *total* (TBD)

# for (df in 1:length(dfs_list)) {
#   
#   ## Get data.frame name
#   df_name <- names(dfs_list)[df]
#   
#   ## Get data according to data.frame name
#   df <- 
#     dfs_list[[df_name]] %>% 
#     select(Reference) %>% 
#     dplyr::mutate(Authors = sub("\\(.*", "", Reference)) %>% 
#     dplyr::mutate(Authors = sub("& ", "", Authors)) #%>% 
#     # tidyr::separate_rows(Authors, sep = ", ") ###################### COME BACK HERE...
#   
#   tmp <- 
#     df %>% 
#     dplyr::group_by(Author) %>% 
#     dplyr::summarise(n = n()) %>% 
#     dplyr::arrange(desc(n)) %>% 
#     head(15) %>% 
#     dplyr::mutate(Author = forcats::fct_reorder(Author, n))
#   
#   plot <- 
#     ggplot(data = tmp,
#            aes(y = n, x = Author)) + 
#     geom_bar(stat = "identity", 
#              fill = "lightblue", color = "black", width = 0.7) +
#     geom_text(aes(label = n), 
#               position = position_dodge(width = 0.7), 
#               hjust = -0.2, color = "black", size = 3.5) +
#     xlab("Author") + ylab("Number of publications") + 
#     coord_flip() +
#     theme_bw() +
#     theme(axis.title = element_text(size = 10, face = "bold"),
#           axis.text = element_text(size = 10))
#   
#   ggsave(plot, 
#          filename = paste0("./results/EDA_bib_top-author-total_", 
#                            gsub(pattern = "bib_", replacement = "", x = df_name),
#                            ".pdf"),
#          height = 10, width = 15.5, units = "cm", dpi = 200)
#   
#   ## Clean environment
#   rm(df, df_name, tmp, plot)
# }

## 4) Top journals #### 

for (df in 1:length(dfs_list)) {
  
  ## Get data.frame name
  df_name <- names(dfs_list)[df]
  
  ## Get data according to data.frame name
  df <- dfs_list[[df_name]]
  
  tmp <- 
    df %>% 
    dplyr::group_by(Journal_Institution) %>% 
    dplyr::summarise(n = n()) %>% 
    dplyr::arrange(desc(n)) %>% 
    head(15) %>% 
    dplyr::mutate(Journal_Institution = forcats::fct_reorder(Journal_Institution, n))
  
  plot <- 
    ggplot(data = tmp,
           aes(y = n, x = Journal_Institution)) + 
    geom_bar(stat = "identity", 
             fill = "lightblue", color = "black", width = 0.7) +
    geom_text(aes(label = n), 
              position = position_dodge(width = 0.7), 
              hjust = -0.2, color = "black", size = 3) +
    xlab("Journal") + ylab("Number of publications") + 
    coord_flip() +
    theme_bw() +
    theme(axis.title = element_text(size = 9.5, face = "bold"),
          axis.text = element_text(size = 9.5))
  
  ggsave(plot, 
         filename = paste0("./results/EDA_bib_top-journals_", 
                           gsub(pattern = "bib_", replacement = "", x = df_name),
                           ".pdf"),
         height = 10, width = 22, units = "cm", dpi = 200)
  
  ## Clean environment
  rm(df, df_name, tmp, plot)
}

## 5) Language, Journal_scope ####

# Create list to save plots
pie_plots <- list()

for (df in 1:length(dfs_list)) {
  
  ## Get data.frame name
  df_name <- names(dfs_list)[df]
  
  ## Get data according to data.frame name
  df <- dfs_list[[df_name]]
  
  tmp_Language <- 
    df %>% 
    dplyr::group_by(Language) %>% 
    dplyr::summarise(n = n()) %>% 
    dplyr::mutate(Language = factor(Language, 
                                    levels = c("English", "Portuguese",
                                               "Spanish", "English | Spanish"))) %>% 
    dplyr::mutate(n_prop = round(n / sum(n) * 100, digits = 0))
  
  tmp_Journal_scope <- 
    df %>% 
    dplyr::group_by(Journal_scope) %>% 
    dplyr::summarise(n = n()) %>% 
    dplyr::mutate(Journal_scope = factor(Journal_scope, 
                                    levels = c("International", "National"))) %>% 
    dplyr::mutate(n_prop = round(n / sum(n) * 100, digits = 0))
  
  if(df_name == "bib_mammals"){
    plot_Language <- 
      ggplot(data = tmp_Language,
             aes(x = "", y = n_prop, fill = Language)) + 
      geom_bar(stat = "identity", width = 1) +
      scale_fill_viridis_d(option = "inferno", name = "") +
      coord_polar("y", start = 0) +
      xlab("") + ylab("") +
      guides(fill = guide_legend(nrow = 2)) +
      theme_minimal() +
      theme(axis.text = element_text(size = 12, face = "bold"),
            legend.text = element_text(size = 12),
            legend.position = "bottom")
    
    plot_Journal_scope <- 
      ggplot(data = tmp_Journal_scope,
             aes(x = "", y = n_prop, fill = Journal_scope)) + 
      geom_bar(stat = "identity", width = 1) +
      scale_fill_viridis_d(option = "cividis", name = "") +
      coord_polar("y", start = 0) +
      xlab("") + ylab("") +
      guides(fill = guide_legend(nrow = 2)) +
      theme_minimal() +
      theme(axis.text = element_text(size = 12, face = "bold"),
            legend.text = element_text(size = 12),
            legend.position = "bottom")
  } else {
    plot_Language <- 
      ggplot(data = tmp_Language,
             aes(x = "", y = n_prop, fill = Language)) + 
      geom_bar(stat = "identity", width = 1) +
      scale_fill_viridis_d(option = "inferno", name = "") +
      coord_polar("y", start = 0) +
      xlab("") + ylab("") +
      theme_minimal() +
      theme(axis.text = element_text(size = 12, face = "bold"),
            legend.position = "none")
    
    plot_Journal_scope <- 
      ggplot(data = tmp_Journal_scope,
             aes(x = "", y = n_prop, fill = Journal_scope)) + 
      geom_bar(stat = "identity", width = 1) +
      scale_fill_viridis_d(option = "cividis", name = "") +
      coord_polar("y", start = 0) +
      xlab("") + ylab("") +
      theme_minimal() +
      theme(axis.text = element_text(size = 12, face = "bold"),
            legend.position = "none")
  }
  
  patchwork <- plot_Language + plot_Journal_scope
  
  pie_plots[[gsub(pattern = "bib_", replacement = "", x = df_name)]] <- patchwork
  
  ## Clean environment
  rm(df, df_name, 
     tmp_Language, tmp_Journal_scope,
     plot_Language, plot_Journal_scope, 
     patchwork)
}

plot <- 
  pie_plots[["turtles"]] / 
  pie_plots[["seabirds"]] / 
  pie_plots[["mammals"]] + 
     patchwork::plot_annotation(tag_levels = list(c("Sea turtles", "",
                                                    "Seabirds", "",
                                                    "Marine Mammals", "")))

ggsave(plot, 
       filename = paste0("./results/EDA_bib_pie-patchwork-Language-Scope_megafauna.pdf"),
       height = 22, width = 16, units = "cm", dpi = 200)

rm(pie_plots, plot)


## 6.1) Primary_theme, Secondary_theme (overall) ####

## Primary
for (df in 1:length(dfs_list)) {
  
  ## Get data.frame name
  df_name <- names(dfs_list)[df]
  
  ## Get data according to data.frame name
  df <- dfs_list[[df_name]]
  
  tmp <- 
    df %>% 
    dplyr::group_by(Primary_theme) %>% 
    dplyr::summarise(n = n()) %>% 
    dplyr::arrange(desc(n)) %>% 
    head(15) %>% 
    dplyr::mutate(Primary_theme = forcats::fct_reorder(Primary_theme, n))
  
  plot <- 
    ggplot(data = tmp,
           aes(y = n, x = Primary_theme)) + 
    geom_bar(stat = "identity", 
             fill = "lightblue", color = "black", width = 0.7) +
    geom_text(aes(label = n), 
              position = position_dodge(width = 0.7), 
              hjust = -0.2, color = "black", size = 3) +
    xlab("") + ylab("Number of publications") + 
    ggtitle("Primary theme") +
    coord_flip() +
    theme_bw() +
    theme(axis.title = element_text(size = 9.5, face = "bold"),
          axis.text = element_text(size = 9.5))
  
  ggsave(plot, 
         filename = paste0("./results/EDA_bib_primary-themes-15top_", 
                           gsub(pattern = "bib_", replacement = "", x = df_name),
                           ".pdf"),
         height = 10, width = 22, units = "cm", dpi = 200)
  
  ## Clean environment
  rm(df, df_name, tmp, plot)
}

## Secondary
for (df in 1:length(dfs_list)) {
  
  ## Get data.frame name
  df_name <- names(dfs_list)[df]
  
  ## Get data according to data.frame name
  df <- dfs_list[[df_name]]
  
  tmp <- 
    df %>% 
    dplyr::group_by(Secondary_theme) %>% 
    dplyr::summarise(n = n()) %>% 
    dplyr::filter(! is.na(Secondary_theme)) %>% 
    dplyr::arrange(desc(n)) %>% 
    head(15) %>% 
    dplyr::mutate(Secondary_theme = forcats::fct_reorder(Secondary_theme, n))
  
  plot <- 
    ggplot(data = tmp,
           aes(y = n, x = Secondary_theme)) + 
    geom_bar(stat = "identity", 
             fill = "lightblue", color = "black", width = 0.7) +
    geom_text(aes(label = n), 
              position = position_dodge(width = 0.7), 
              hjust = -0.2, color = "black", size = 3) +
    xlab("") + ylab("Number of publications") + 
    ggtitle("Secondary theme") +
    coord_flip() +
    theme_bw() +
    theme(axis.title = element_text(size = 9.5, face = "bold"),
          axis.text = element_text(size = 9.5))
  
  ggsave(plot, 
         filename = paste0("./results/EDA_bib_secondary-themes-15top_", 
                           gsub(pattern = "bib_", replacement = "", x = df_name),
                           ".pdf"),
         height = 10, width = 22, units = "cm", dpi = 200)
  
  ## Clean environment
  rm(df, df_name, tmp, plot)
}

## 6.2) Primary_theme temporal ####

tmp <- 
  dfs_merged %>% 
  dplyr::group_by(Year_5yr, Primary_theme, Megafauna_group) %>% 
  dplyr::summarise(n = n()) %>% 
  dplyr::filter(! is.na(Year_5yr))

tmp2 <- 
  tmp %>% 
  dplyr::group_by(Year_5yr, Megafauna_group) %>% 
  dplyr::summarise(n_studies = sum(n)) %>% 
  dplyr::mutate(y = 1.05)

col_poly <- palette.colors(palette = "Polychrome 36")[1:31] %>% as.vector()

plot <- 
  ggplot(data = tmp,
         aes(y = n, x = Year_5yr)) + 
  geom_bar(aes(fill = Primary_theme),
           position = "fill", stat = "identity") +
  scale_fill_manual(values = col_poly, name = "") + 
  scale_y_continuous(labels = c("0%", "25%", "50%", "75%", "100%")) +
  geom_text(data = tmp2, 
            aes(x = Year_5yr, y = y, label = n_studies),
            size = 2) +
  facet_wrap(~ Megafauna_group, nrow = 3) + 
  guides(fill = guide_legend(ncol = 1)) +
  xlab("") + ylab("") + 
  theme_bw() +
  theme(axis.title = element_text(size = 8, face = "bold"),
        axis.text = element_text(size = 8),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        strip.text = element_text(size = 8, face = "bold"),
        legend.position = "right",
        legend.text = element_text(size = 6.5))

ggsave(plot, 
       filename = "./results/EDA_bib_primary-themes-temporal-5yr_megafauna.pdf",
       height = 22, width = 16, units = "cm", dpi = 200)

rm(col_poly, tmp, tmp2, plot)

## 7.1) Spatial_scale, Time_scale (Pie) [ignore] ####

# # Create list to save plots
# pie_plots <- list()
# 
# for (df in 1:length(dfs_list)) {
#   
#   ## Get data.frame name
#   df_name <- names(dfs_list)[df]
#   
#   ## Get data according to data.frame name
#   df <- dfs_list[[df_name]]
#   
#   tmp_Spatial_scale <- 
#     df %>% 
#     dplyr::group_by(Spatial_scale) %>% 
#     dplyr::summarise(n = n()) %>% 
#     dplyr::filter(! is.na(Spatial_scale)) %>% 
#     dplyr::mutate(Spatial_scale = factor(Spatial_scale, 
#                                     levels = c("Local", "National", 
#                                                "Regional", "Global"))) %>% 
#     dplyr::mutate(n_prop = round(n / sum(n) * 100, digits = 0))
#   
#   tmp_Time_scale <- 
#     df %>% 
#     dplyr::group_by(Time_scale) %>% 
#     dplyr::summarise(n = n()) %>% 
#     dplyr::filter(! is.na(Time_scale)) %>% 
#     dplyr::mutate(Time_scale = factor(Time_scale, 
#                                          levels = c("Short-term", "Mid-term", "Long-term"))) %>% 
#     dplyr::mutate(n_prop = round(n / sum(n) * 100, digits = 0))
#   
#   if(df_name == "bib_mammals"){
#     plot_Spatial_scale <- 
#       ggplot(data = tmp_Spatial_scale,
#              aes(x = "", y = n_prop, fill = Spatial_scale)) + 
#       geom_bar(stat = "identity", width = 1) +
#       scale_fill_viridis_d(option = "inferno", name = "") +
#       coord_polar("y", start = 0) +
#       xlab("") + ylab("") +
#       guides(fill = guide_legend(nrow = 2)) +
#       theme_minimal() +
#       theme(axis.text = element_text(size = 12, face = "bold"),
#             legend.text = element_text(size = 12),
#             legend.position = "bottom")
#     
#     plot_Time_scale <- 
#       ggplot(data = tmp_Time_scale,
#              aes(x = "", y = n_prop, fill = Time_scale)) + 
#       geom_bar(stat = "identity", width = 1) +
#       scale_fill_viridis_d(option = "rocket", name = "") +
#       coord_polar("y", start = 0) +
#       xlab("") + ylab("") +
#       guides(fill = guide_legend(nrow = 2)) +
#       theme_minimal() +
#       theme(axis.text = element_text(size = 12, face = "bold"),
#             legend.text = element_text(size = 12),
#             legend.position = "bottom")
#   } else {
#     plot_Spatial_scale <- 
#       ggplot(data = tmp_Spatial_scale,
#              aes(x = "", y = n_prop, fill = Spatial_scale)) + 
#       geom_bar(stat = "identity", width = 1) +
#       scale_fill_viridis_d(option = "inferno", name = "") +
#       coord_polar("y", start = 0) +
#       xlab("") + ylab("") +
#       theme_minimal() +
#       theme(axis.text = element_text(size = 12, face = "bold"),
#             legend.position = "none")
#     
#     plot_Time_scale <- 
#       ggplot(data = tmp_Time_scale,
#              aes(x = "", y = n_prop, fill = Time_scale)) + 
#       geom_bar(stat = "identity", width = 1) +
#       scale_fill_viridis_d(option = "rocket", name = "") +
#       coord_polar("y", start = 0) +
#       xlab("") + ylab("") +
#       theme_minimal() +
#       theme(axis.text = element_text(size = 12, face = "bold"),
#             legend.position = "none")
#   }
#   
#   patchwork <- plot_Spatial_scale + plot_Time_scale
#   
#   pie_plots[[gsub(pattern = "bib_", replacement = "", x = df_name)]] <- patchwork
#   
#   ## Clean environment
#   rm(df, df_name, 
#      tmp_Spatial_scale, tmp_Time_scale,
#      plot_Spatial_scale, plot_Time_scale, 
#      patchwork)
# }
# 
# plot <- 
#   pie_plots[["turtles"]] / 
#   pie_plots[["seabirds"]] / 
#   pie_plots[["mammals"]] + 
#   patchwork::plot_annotation(tag_levels = list(c("Sea turtles", "",
#                                                  "Seabirds", "",
#                                                  "Marine Mammals", "")))
# 
# ggsave(plot, 
#        filename = paste0("./results/EDA_bib_pie-patchwork-ST-scales_megafauna.pdf"),
#        height = 22, width = 18, units = "cm", dpi = 200)
# 
# rm(pie_plots, plot)

## 7.2) Spatial_scale, Time_scale (Barplots - %) ####

# Spatial - inferno
for (df in 1:length(dfs_list)) {
  
  ## Get data.frame name
  df_name <- names(dfs_list)[df]
  
  ## Get data according to data.frame name
  df <- dfs_list[[df_name]]
  
  tmp <- 
    df %>% 
    dplyr::group_by(Year_5yr, Spatial_scale) %>% 
    dplyr::summarise(n = n()) %>% 
    dplyr::filter(! is.na(Spatial_scale)) %>% 
    dplyr::filter(! is.na(Year_5yr)) %>% 
    dplyr::mutate(Spatial_scale = factor(Spatial_scale,
                                         levels = c("Local", "National", 
                                                    "Regional", "Global")))
  
  tmp2 <- 
    tmp %>% 
    dplyr::group_by(Year_5yr) %>% 
    dplyr::summarise(n_studies = sum(n)) %>% 
    dplyr::mutate(y = 1.05)
  
  plot <- 
    ggplot(data = tmp,
           aes(y = n, x = Year_5yr)) + 
    geom_bar(aes(fill = Spatial_scale),
             position = "fill", stat = "identity") +
    scale_fill_viridis_d(option = "inferno", name = "") +
    scale_y_continuous(labels = c("0%", "25%", "50%", "75%", "100%")) +
    geom_text(data = tmp2, 
              aes(x = Year_5yr, y = y, label = n_studies),
              size = 3) +
    xlab("") + ylab("") + 
    theme_bw() +
    theme(axis.title = element_text(size = 9.5, face = "bold"),
          axis.text = element_text(size = 9.5),
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
  
  ggsave(plot, 
         filename = paste0("./results/EDA_bib_spatial-scale-5yr_", 
                           gsub(pattern = "bib_", replacement = "", x = df_name),
                           ".pdf"),
         height = 10, width = 22, units = "cm", dpi = 200)
  
  ## Clean environment
  rm(df, df_name, tmp, tmp2, plot)
}

# Time - rocket
for (df in 1:length(dfs_list)) {
  
  ## Get data.frame name
  df_name <- names(dfs_list)[df]
  
  ## Get data according to data.frame name
  df <- dfs_list[[df_name]]
  
  tmp <- 
    df %>% 
    dplyr::group_by(Year_5yr, Time_scale) %>% 
    dplyr::summarise(n = n()) %>% 
    dplyr::filter(! is.na(Time_scale)) %>% 
    dplyr::filter(! is.na(Year_5yr)) %>% 
    dplyr::mutate(Time_scale = factor(Time_scale,
                                         levels = c("Short-term", 
                                                    "Mid-term", 
                                                    "Long-term")))
  
  tmp2 <- 
    tmp %>% 
    dplyr::group_by(Year_5yr) %>% 
    dplyr::summarise(n_studies = sum(n)) %>% 
    dplyr::mutate(y = 1.05)
  
  plot <- 
    ggplot(data = tmp,
           aes(y = n, x = Year_5yr)) + 
    geom_bar(aes(fill = Time_scale),
             position = "fill", stat = "identity") +
    scale_fill_viridis_d(option = "rocket", name = "") +
    scale_y_continuous(labels = c("0%", "25%", "50%", "75%", "100%")) +
    geom_text(data = tmp2, 
              aes(x = Year_5yr, y = y, label = n_studies),
              size = 3) +
    xlab("") + ylab("") + 
    theme_bw() +
    theme(axis.title = element_text(size = 9.5, face = "bold"),
          axis.text = element_text(size = 9.5),
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
  
  ggsave(plot, 
         filename = paste0("./results/EDA_bib_time-scale-5yr_", 
                           gsub(pattern = "bib_", replacement = "", x = df_name),
                           ".pdf"),
         height = 10, width = 22, units = "cm", dpi = 200)
  
  ## Clean environment
  rm(df, df_name, tmp, tmp2, plot)
}

## 7.2) Spatial_scale, Time_scale (Barplots - stack [N]) ####

# Spatial - inferno
for (df in 1:length(dfs_list)) {
  
  ## Get data.frame name
  df_name <- names(dfs_list)[df]
  
  ## Get data according to data.frame name
  df <- dfs_list[[df_name]]
  
  tmp <- 
    df %>% 
    dplyr::group_by(Year_5yr, Spatial_scale) %>% 
    dplyr::summarise(n = n()) %>% 
    dplyr::filter(! is.na(Spatial_scale)) %>% 
    dplyr::filter(! is.na(Year_5yr)) %>% 
    dplyr::mutate(Spatial_scale = factor(Spatial_scale,
                                         levels = c("Local", "National", 
                                                    "Regional", "Global")))
  
  plot <- 
    ggplot(data = tmp,
           aes(y = n, x = Year_5yr)) + 
    geom_bar(aes(fill = Spatial_scale),
             position = "stack", stat = "identity") +
    scale_fill_viridis_d(option = "inferno", name = "") +
    xlab("") + ylab("") + 
    theme_bw() +
    theme(axis.title = element_text(size = 9.5, face = "bold"),
          axis.text = element_text(size = 9.5),
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
          legend.position = c(0.2, 0.7))
  
  ggsave(plot, 
         filename = paste0("./results/EDA_bib_spatial-scale-5yr-stack_", 
                           gsub(pattern = "bib_", replacement = "", x = df_name),
                           ".pdf"),
         height = 10, width = 22, units = "cm", dpi = 200)
  
  ## Clean environment
  rm(df, df_name, tmp, plot)
}

# Time - rocket
for (df in 1:length(dfs_list)) {
  
  ## Get data.frame name
  df_name <- names(dfs_list)[df]
  
  ## Get data according to data.frame name
  df <- dfs_list[[df_name]]
  
  tmp <- 
    df %>% 
    dplyr::group_by(Year_5yr, Time_scale) %>% 
    dplyr::summarise(n = n()) %>% 
    dplyr::filter(! is.na(Time_scale)) %>% 
    dplyr::filter(! is.na(Year_5yr)) %>% 
    dplyr::mutate(Time_scale = factor(Time_scale,
                                      levels = c("Short-term", 
                                                 "Mid-term", 
                                                 "Long-term")))
  
  
  plot <- 
    ggplot(data = tmp,
           aes(y = n, x = Year_5yr)) + 
    geom_bar(aes(fill = Time_scale),
             position = "stack", stat = "identity") +
    scale_fill_viridis_d(option = "rocket", name = "") +
    xlab("") + ylab("") + 
    theme_bw() +
    theme(axis.title = element_text(size = 9.5, face = "bold"),
          axis.text = element_text(size = 9.5),
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
          legend.position = c(0.2, 0.8))
  
  ggsave(plot, 
         filename = paste0("./results/EDA_bib_time-scale-5yr-stack_", 
                           gsub(pattern = "bib_", replacement = "", x = df_name),
                           ".pdf"),
         height = 10, width = 22, units = "cm", dpi = 200)
  
  ## Clean environment
  rm(df, df_name, tmp, plot)
}

## 8.1) Habitat ####

# tmp <- 
#   dfs_merged %>% 
#   tidyr::separate_rows(Habitat, sep = " \\| ") %>% 
#   dplyr::filter(! is.na(Habitat)) %>% 
#   dplyr::group_by(Year, Habitat, Megafauna_group) %>% 
#   dplyr::summarise(n = n(), .groups = 'drop') %>% 
#   dplyr::arrange(Year)
# 
# View(tmp %>% 
#   dplyr::group_by(Megafauna_group, Habitat) %>% 
#   dplyr::summarise(max = max(n)))

# scales::show_col(viridisLite::turbo(3))
# Sea turtles: "#30123BFF"
# Seabirds:"#A2FC3CFF"
# Mar. Mammals: "#7A0403FF"

## Plot each group individually
for (df in 1:length(dfs_list)) {
  
  ## Get data.frame name
  df_name <- names(dfs_list)[df]
  
  ## Get data according to data.frame name
  df <- dfs_list[[df_name]]
  
  tmp <-
    df %>%
    tidyr::separate_rows(Habitat, sep = " \\| ") %>%
    dplyr::filter(! is.na(Habitat)) %>%
    dplyr::group_by(Habitat) %>%
    dplyr::summarise(n = n(), .groups = 'drop') %>% 
    t() %>% 
    as.data.frame()
  
  colnames(tmp) <- as.vector(tmp[1,1:ncol(tmp)])
  
  tmp <- tmp[2,]
  tmp <- apply(tmp, c(1, 2), as.numeric) %>% as.data.frame()
  
  max_n <- max(tmp)
  
  tmp <- rbind(rep(max_n, ncol(tmp)),
               rep(0, ncol(tmp)),
               tmp)
  
  if(df_name == "bib_turtles"){
    pdf("./results/EDA_bib_habitat_turtles.pdf", height = 7, width = 7)
    
    fmsb::radarchart(
      tmp,
      # Customise polygon
      pcol = "#30123BFF", pfcol = scales::alpha("#30123BFF", 0.5), plwd = 2,
      # Customise grid
      cglcol = "grey50", cglty = 1, 
      axistype = 1,
      axislabcol = "grey30", 
      caxislabels = as.character(seq(0, max_n, round((max_n/5), digits = 0))), 
      cglwd = 0.8
      )
    
    dev.off()
  }
  
  if(df_name == "bib_seabirds"){
    pdf("./results/EDA_bib_habitat_seabirds.pdf", height = 7, width = 7)
    
    fmsb::radarchart(
      tmp,
      # Customise polygon
      pcol = "#A2FC3CFF", pfcol = scales::alpha("#A2FC3CFF", 0.5), plwd = 2,
      # Customise grid
      cglcol = "grey50", cglty = 1, 
      axistype = 1,
      axislabcol = "grey30", 
      caxislabels = as.character(seq(0, max_n, round((max_n/5), digits = 0))), 
      cglwd = 0.8
    )
    
    dev.off()
  }
  
  if(df_name == "bib_mammals"){
    pdf("./results/EDA_bib_habitat_mammals.pdf", height = 7, width = 7)
    
    fmsb::radarchart(
      tmp,
      # Customise polygon
      pcol = "#7A0403FF", pfcol = scales::alpha("#7A0403FF", 0.5), plwd = 2,
      # Customise grid
      cglcol = "grey50", cglty = 1, 
      axistype = 1,
      axislabcol = "grey30", 
      caxislabels = as.character(seq(0, max_n, round((max_n/5), digits = 0))), 
      cglwd = 0.8
    )
    
    dev.off()
  }
  
  ## Clean environment
  rm(df_name, df, tmp, max_n)
}

## 8.2) Habitat_sub (TBD) ####

## 9.1) Anthropogenic_threats - wordcloud ####

for (df in 1:length(dfs_list)) {
  
  ## Get data.frame name
  df_name <- names(dfs_list)[df]
  
  ## Get data according to data.frame name
  df <- dfs_list[[df_name]]
  
  tmp <- 
    df %>% 
    dplyr::select(Anthropogenic_threats) %>% 
    dplyr::filter(! is.na(Anthropogenic_threats)) %>% 
    tidyr::separate_rows(Anthropogenic_threats, sep = " \\| ") %>%
    dplyr::mutate(Anthropogenic_threats = gsub(pattern = "Bycacth",
                                               replacement = "Bycatch",
                                               x = Anthropogenic_threats)) %>% 
    dplyr::group_by(Anthropogenic_threats) %>% 
    dplyr::summarise(n = n()) 
  
  set.seed(1) # For reproducibility
  if(df_name == "bib_turtles"){
    plot <- 
      ggplot(data = tmp, 
             aes(label = Anthropogenic_threats, size = n, color = scale(n))) +
      geom_text_wordcloud() +
      scale_color_gradient2(low = "#fec44f",
                            mid = "#cc4c02",
                            high = "#662506") +
      labs(title = "Sea turtles",
           caption = "Colours are scaled") +
      theme_minimal() +
      theme(legend.position = "none")
  }
  
  if(df_name == "bib_seabirds"){
    plot <- 
      ggplot(data = tmp, 
             aes(label = Anthropogenic_threats, size = n, color = scale(n))) +
      geom_text_wordcloud() +
      scale_color_gradient2(low = "#fec44f",
                            mid = "#cc4c02",
                            high = "#662506") +
      labs(title = "Seabirds",
           caption = "Colours are scaled") + 
      theme_minimal() +
      theme(legend.position = "none")
  }
  
  if(df_name == "bib_mammals"){
    plot <- 
      ggplot(data = tmp, 
             aes(label = Anthropogenic_threats, size = n, color = scale(n))) +
      geom_text_wordcloud() +
      # scale_color_gradient2(low = "#edf8b1",
      #                       mid = "#41b6c4",
      #                       high = "#081d58") +
      scale_color_gradient2(low = "#fec44f",
                           mid = "#cc4c02",
                           high = "#662506") +
      labs(title = "Marine mammals",
           caption = "Colours are scaled") +
      theme_minimal() +
      theme(legend.position = "none")
  }
  
  ggsave(plot, 
         filename = paste0("./results/EDA_bib_anthropogenic-threats_", 
                           gsub(pattern = "bib_", replacement = "", x = df_name),
                           ".pdf"),
         height = 10, width = 10, units = "cm", dpi = 200)
  
  ## Clean environment
  rm(df, df_name, tmp, plot)
}

## 9.2) Anthropogenic_threats, by Year - linegraph ####

for (df in 1:length(dfs_list)) {
  
  ## Get data.frame name
  df_name <- names(dfs_list)[df]
  
  ## Get data according to data.frame name
  df <- dfs_list[[df_name]]
  
  top10_alltimes <- 
    df %>% 
    dplyr::select(Anthropogenic_threats) %>% 
    dplyr::filter(! is.na(Anthropogenic_threats)) %>% 
    tidyr::separate_rows(Anthropogenic_threats, sep = " \\| ") %>%
    dplyr::mutate(Anthropogenic_threats = gsub(pattern = "Bycacth",
                                               replacement = "Bycatch",
                                               x = Anthropogenic_threats)) %>% 
    dplyr::group_by(Anthropogenic_threats) %>% 
    dplyr::summarise(n = n()) %>% 
    dplyr::arrange(desc(n)) %>% 
    head(10) %>% 
    dplyr::pull(Anthropogenic_threats)
  
  tmp <- 
    df %>% 
    dplyr::select(Year, Anthropogenic_threats) %>% 
    dplyr::filter(! is.na(Anthropogenic_threats)) %>% 
    tidyr::separate_rows(Anthropogenic_threats, sep = " \\| ") %>%
    dplyr::mutate(Anthropogenic_threats = gsub(pattern = "Bycacth",
                                               replacement = "Bycatch",
                                               x = Anthropogenic_threats)) %>% 
    dplyr::group_by(Year, Anthropogenic_threats) %>% 
    dplyr::summarise(n = n()) %>% 
    dplyr::filter(Anthropogenic_threats %in% top10_alltimes)
  
  plot <-
    ggplot(data = tmp) + 
    geom_line(aes(x = Year, y = n, color = Anthropogenic_threats)) + 
    xlab("") + ylab("Number of studies") +
    guides(color = guide_legend(nrow = 3)) + 
    theme_bw() + 
    theme(legend.position = "bottom",
          legend.title = element_blank())
  
  ggsave(plot, 
         filename = paste0("./results/EDA_bib_anthropogenic-threats-by-year_", 
                           gsub(pattern = "bib_", replacement = "", x = df_name),
                           ".pdf"),
         height = 14, width = 16, units = "cm", dpi = 200)
  
  ## Clean environment
  rm(df, df_name, tmp, top10_alltimes, plot)
}


## 10) Natural_forcers - wordcloud ####

for (df in 1:length(dfs_list)) {
  
  ## Get data.frame name
  df_name <- names(dfs_list)[df]
  
  ## Get data according to data.frame name
  df <- dfs_list[[df_name]]
  
  tmp <- 
    df %>% 
    dplyr::select(Natural_forcers) %>% 
    dplyr::filter(! is.na(Natural_forcers)) %>% 
    tidyr::separate_rows(Natural_forcers, sep = " \\| ") %>%
    dplyr::group_by(Natural_forcers) %>% 
    dplyr::summarise(n = n()) 
  
  set.seed(1) # For reproducibility
  if(df_name == "bib_turtles"){
    plot <- 
      ggplot(data = tmp, 
             aes(label = Natural_forcers, size = n, color = scale(n))) +
      geom_text_wordcloud() +
      scale_color_gradient2(low = "#d9d9d9",
                            mid = "#737373",
                            high = "#000000") +
      labs(title = "Sea turtles",
           caption = "Colours are scaled") +
      theme_minimal() +
      theme(legend.position = "none")
  }
  
  if(df_name == "bib_seabirds"){
    plot <- 
      ggplot(data = tmp, 
             aes(label = Natural_forcers, size = n, color = scale(n))) +
      geom_text_wordcloud() +
      scale_color_gradient2(low = "#d9d9d9",
                            mid = "#737373",
                            high = "#000000") +
      labs(title = "Seabirds",
           caption = "Colours are scaled") + 
      theme_minimal() +
      theme(legend.position = "none")
  }
  
  if(df_name == "bib_mammals"){
    plot <- 
      ggplot(data = tmp, 
             aes(label = Natural_forcers, size = n, color = scale(n))) +
      geom_text_wordcloud() +
      scale_color_gradient2(low = "#d9d9d9",
                            mid = "#737373",
                            high = "#000000") +
      labs(title = "Marine mammals",
           caption = "Colours are scaled") +
      theme_minimal() +
      theme(legend.position = "none")
  }
  
  ggsave(plot, 
         filename = paste0("./results/EDA_bib_natural-forcers_", 
                           gsub(pattern = "bib_", replacement = "", x = df_name),
                           ".pdf"),
         height = 10, width = 10, units = "cm", dpi = 200)
  
  ## Clean environment
  rm(df, df_name, tmp, plot)
}

## 11.1) {bibliometrix} ... worth it? ####

# results <- biblioAnalysis(bibtex_mammals, sep = ";")
# summary_bibtex <- summary(results, k = 10, pause = FALSE)
# print(summary_bibtex)

## Keywords (rethink the need of this one) [done, but ignore for now] ####

# for (df in 1:length(dfs_list)) {
# 
#   ## Get data.frame name
#   df_name <- names(dfs_list)[df]
# 
#   ## Get data according to data.frame name
#   df <- dfs_list[[df_name]]
# 
#   tmp <-
#     df %>%
#     dplyr::select(Key.word_1, Key.word_2, Key.word_3, Key.word_4) %>%
#     tidyr::pivot_longer(cols = everything(),
#                         names_to = "Keyword_N",
#                         values_to = "Keyword") %>%
#     dplyr::filter(! is.na(Keyword)) %>%
#     dplyr::mutate(Keyword = tolower(Keyword)) %>%
#     dplyr::mutate(Keyword = snakecase::to_sentence_case(Keyword)) %>%
#     dplyr::group_by(Keyword) %>%
#     dplyr::summarise(n = n())
# 
#   if(df_name == "bib_turtles"){
#     tmp <- tmp %>% dplyr::filter(n > 3)
#   }
#   if(df_name == "bib_seabirds"){
#     tmp <- tmp %>% dplyr::filter(n > 4)
#   }
#   if(df_name == "bib_mammals"){
#     tmp <- tmp %>% dplyr::filter(n > 9)
#   }
# 
#   set.seed(1) # For reproducibility
#   if(df_name == "bib_turtles"){
#     plot <-
#       ggplot(data = tmp,
#              aes(label = Keyword, size = n)) +
#       geom_text_wordcloud() +
#       labs(caption = "NOTE: n > 3")
#     theme_minimal()
#   }
# 
#   if(df_name == "bib_seabirds"){
#     plot <-
#       ggplot(data = tmp,
#              aes(label = Keyword, size = n)) +
#       geom_text_wordcloud() +
#       labs(caption = "NOTE: n > 4")
#     theme_minimal()
#   }
# 
#   if(df_name == "bib_mammals"){
#     plot <-
#       ggplot(data = tmp,
#              aes(label = Keyword, size = n)) +
#       geom_text_wordcloud() +
#       labs(caption = "NOTE: n > 9")
#     theme_minimal()
#   }
# 
#   ggsave(plot,
#          filename = paste0("./results/EDA_bib_keywords_",
#                            gsub(pattern = "bib_", replacement = "", x = df_name),
#                            ".pdf"),
#          height = 10, width = 10, units = "cm", dpi = 200)
# 
#   ## Clean environment
#   rm(df, df_name, tmp, plot)
# }

#### ...NEXT? ####