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
# library(sf)
# library(mapview)

## Read data ####

df_bib_turtles <- read.csv2("./data-processed/bib_sea-turtles.csv")
df_bib_seabirds <- read.csv2("./data-processed/bib_seabirds.csv")
df_bib_mammals <- read.csv2("./data-processed/bib_marine-mammals.csv")

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

for (df in 1:length(dfs_list)) {
  
  ## Get data.frame name
  df_name <- names(dfs_list)[df]
  
  ## Get data according to data.frame name
  df <- 
    dfs_list[[df_name]] %>% 
    select(Reference) %>% 
    dplyr::mutate(Authors = sub("\\(.*", "", Reference)) %>% 
    dplyr::mutate(Authors = sub("& ", "", Authors)) #%>% 
    # tidyr::separate_rows(Authors, sep = ", ") ###################### COME BACK HERE...
  
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
         filename = paste0("./results/EDA_bib_top-author-total_", 
                           gsub(pattern = "bib_", replacement = "", x = df_name),
                           ".pdf"),
         height = 10, width = 15.5, units = "cm", dpi = 200)
  
  ## Clean environment
  rm(df, df_name, tmp, plot)
}

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

## 5) Keywords
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

## 6) Keywords (rethink the need of this one) ####

for (df in 1:length(dfs_list)) {
  
  ## Get data.frame name
  df_name <- names(dfs_list)[df]
  
  ## Get data according to data.frame name
  df <- dfs_list[[df_name]]
  
  tmp <- 
    df %>% 
    dplyr::select(Key.word_1, Key.word_2, Key.word_3, Key.word_4) %>% 
    tidyr::pivot_longer(cols = everything(),
                        names_to = "Keyword_N",
                        values_to = "Keyword") %>% 
    dplyr::filter(! is.na(Keyword)) %>% 
    dplyr::mutate(Keyword = tolower(Keyword)) %>% 
    dplyr::mutate(Keyword = snakecase::to_sentence_case(Keyword)) %>% 
    dplyr::group_by(Keyword) %>% 
    dplyr::summarise(n = n()) 
  
  if(df_name == "bib_turtles"){
    tmp <- tmp %>% dplyr::filter(n > 3)
  }
  if(df_name == "bib_seabirds"){
    tmp <- tmp %>% dplyr::filter(n > 4)
  }
  if(df_name == "bib_mammals"){
    tmp <- tmp %>% dplyr::filter(n > 9)
  }
  
  set.seed(1) # For reproducibility
  if(df_name == "bib_turtles"){
    plot <- 
      ggplot(data = tmp, 
             aes(label = Keyword, size = n)) +
      geom_text_wordcloud() +
      labs(caption = "NOTE: n > 3")
      theme_minimal()
  }
  
  if(df_name == "bib_seabirds"){
    plot <- 
      ggplot(data = tmp, 
             aes(label = Keyword, size = n)) +
      geom_text_wordcloud() +
      labs(caption = "NOTE: n > 4")
      theme_minimal()
  }
  
  if(df_name == "bib_mammals"){
    plot <- 
      ggplot(data = tmp, 
             aes(label = Keyword, size = n)) +
      geom_text_wordcloud() +
      labs(caption = "NOTE: n > 9")
      theme_minimal()
  }
  
  ggsave(plot, 
         filename = paste0("./results/EDA_bib_keywords_", 
                           gsub(pattern = "bib_", replacement = "", x = df_name),
                           ".pdf"),
         height = 10, width = 10, units = "cm", dpi = 200)
  
  ## Clean environment
  rm(df, df_name, tmp, plot)
}

## 7) Spatial_scale, Time_scale ####

# Create list to save plots
pie_plots <- list()

for (df in 1:length(dfs_list)) {
  
  ## Get data.frame name
  df_name <- names(dfs_list)[df]
  
  ## Get data according to data.frame name
  df <- dfs_list[[df_name]]
  
  tmp_Spatial_scale <- 
    df %>% 
    dplyr::group_by(Spatial_scale) %>% 
    dplyr::summarise(n = n()) %>% 
    dplyr::filter(! is.na(Spatial_scale)) %>% 
    dplyr::mutate(Spatial_scale = factor(Spatial_scale, 
                                    levels = c("Local", "National", 
                                               "Regional", "Global"))) %>% 
    dplyr::mutate(n_prop = round(n / sum(n) * 100, digits = 0))
  
  tmp_Time_scale <- 
    df %>% 
    dplyr::group_by(Time_scale) %>% 
    dplyr::summarise(n = n()) %>% 
    dplyr::filter(! is.na(Time_scale)) %>% 
    dplyr::mutate(Time_scale = factor(Time_scale, 
                                         levels = c("Short-term", "Mid-term", "Long-term"))) %>% 
    dplyr::mutate(n_prop = round(n / sum(n) * 100, digits = 0))
  
  if(df_name == "bib_mammals"){
    plot_Spatial_scale <- 
      ggplot(data = tmp_Spatial_scale,
             aes(x = "", y = n_prop, fill = Spatial_scale)) + 
      geom_bar(stat = "identity", width = 1) +
      scale_fill_viridis_d(option = "inferno", name = "") +
      coord_polar("y", start = 0) +
      xlab("") + ylab("") +
      guides(fill = guide_legend(nrow = 2)) +
      theme_minimal() +
      theme(axis.text = element_text(size = 12, face = "bold"),
            legend.text = element_text(size = 12),
            legend.position = "bottom")
    
    plot_Time_scale <- 
      ggplot(data = tmp_Time_scale,
             aes(x = "", y = n_prop, fill = Time_scale)) + 
      geom_bar(stat = "identity", width = 1) +
      scale_fill_viridis_d(option = "rocket", name = "") +
      coord_polar("y", start = 0) +
      xlab("") + ylab("") +
      guides(fill = guide_legend(nrow = 2)) +
      theme_minimal() +
      theme(axis.text = element_text(size = 12, face = "bold"),
            legend.text = element_text(size = 12),
            legend.position = "bottom")
  } else {
    plot_Spatial_scale <- 
      ggplot(data = tmp_Spatial_scale,
             aes(x = "", y = n_prop, fill = Spatial_scale)) + 
      geom_bar(stat = "identity", width = 1) +
      scale_fill_viridis_d(option = "inferno", name = "") +
      coord_polar("y", start = 0) +
      xlab("") + ylab("") +
      theme_minimal() +
      theme(axis.text = element_text(size = 12, face = "bold"),
            legend.position = "none")
    
    plot_Time_scale <- 
      ggplot(data = tmp_Time_scale,
             aes(x = "", y = n_prop, fill = Time_scale)) + 
      geom_bar(stat = "identity", width = 1) +
      scale_fill_viridis_d(option = "rocket", name = "") +
      coord_polar("y", start = 0) +
      xlab("") + ylab("") +
      theme_minimal() +
      theme(axis.text = element_text(size = 12, face = "bold"),
            legend.position = "none")
  }
  
  patchwork <- plot_Spatial_scale + plot_Time_scale
  
  pie_plots[[gsub(pattern = "bib_", replacement = "", x = df_name)]] <- patchwork
  
  ## Clean environment
  rm(df, df_name, 
     tmp_Spatial_scale, tmp_Time_scale,
     plot_Spatial_scale, plot_Time_scale, 
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
       filename = paste0("./results/EDA_bib_pie-patchwork-ST-scales_megafauna.pdf"),
       height = 22, width = 18, units = "cm", dpi = 200)

rm(pie_plots, plot)

#### ...NEXT? ####