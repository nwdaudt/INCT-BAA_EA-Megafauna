##
## Data grooming (2)
## Adjusting some columns (post-hoc), according to the final protocol
## Farias et al. (2025) MethodsX 15: 103561. <https://doi.org/10.1016/j.mex.2025.103561>
##

##
### Script originally written by G. B. Farias, with few amendments
### (Note: comments are all in Portuguese)
##

### ----- [NO NEED TO RUN THIS SCRIPT AGAIN]

## Libraries ####
library(dplyr)
library(stringr)
library(purrr)
# library(readxl)
# library(writexl)

## Read data  ####

## Note: read data for ONE group, then run the script, then repeat for the next group

# spp <- read.csv2("./data-processed/spp_sea-turtles.csv")
# ref <- read.csv2("./data-processed/bib_sea-turtles.csv")

# spp <- read.csv2("./data-processed/spp_seabirds.csv")
# ref <- read.csv2("./data-processed/bib_seabirds.csv")

# spp <- read.csv2("./data-processed/spp_marine-mammals.csv")
# ref <- read.csv2("./data-processed/bib_marine-mammals.csv")

#### 1) Concatenar Palavras chave em uma única coluna ####

ref <- ref %>%
  rowwise() %>%
  mutate(
    keyword = {
      kws <- c_across(starts_with("Key.word_"))
      kws <- kws[!is.na(kws) & kws != "" & kws != "NA"]  # Caso tenha NA em uma ou todas as colunas ele ignora, também inseri o texto "NA", se vocês não usaram como texto apenas deixaram em branco, podem excluir essa parte
      if (length(kws) == 0) NA_character_ else paste(kws, collapse = " | ")
    }
  ) %>%
  ungroup() %>%
  select(-starts_with("Key.word_")) %>% # Exclui as colunas originais, caso não queiram excluir, podem remover essa parte
  relocate(keyword, .after = 4) # Apenas reordena a coluna nova para o lugar das antigas


#### 2a) Atualização de Nomenclatura - Habitat e Habitat sub ####
# i) Shallow e Deep reef para REF
ref_temp <- ref %>%
  mutate(
    # Passo 1: Identificar e extrair os Habitats, considerando variações no nome
    terms_to_move = str_extract_all(
      Habitat,
      regex("shallow reef(s)?|deep reef(s)?", ignore_case = TRUE)
    ) %>%
      lapply(function(x) {
        if (length(x) == 0) {
          NA_character_
        } else {
          # Padroniza o formato: primeira letra maiúscula e plural
          x %>% 
            str_to_sentence() %>% 
            str_replace("reef$", "reefs") %>% 
            unique()
        }
      }),
    
    # Passo 2: Processar Habitat_sub 
    Habitat_sub = sapply(1:n(), function(i) {
      # Termos existentes - trata NA/"NA" como vazio (não inclui na saída), caso você não tenha inserido NA pode ignorar
      existing <- if (is.na(Habitat_sub[i]) | Habitat_sub[i] == "NA") {
        character(0)
      } else {
        str_split(Habitat_sub[i], " \\| ")[[1]] %>% str_trim()
      }
      
      # Termos a adicionar (apenas os que não existem)
      to_add <- if (all(is.na(terms_to_move[[i]]))) {
        character(0)
      } else {
        setdiff(terms_to_move[[i]], existing)
      }
      
      # Combina os termos (ignora completamente NA/"NA" existentes)
      if (length(to_add) == 0) {
        if (length(existing) == 0) NA_character_ else Habitat_sub[i]
      } else {
        new_terms <- paste(c(existing, to_add), collapse = " | ")
        if (new_terms == "") NA_character_ else new_terms
      }
    }),
    
    # Passo 3: Modificar Habitat substituindo os termos
    Habitat = str_replace_all(
      Habitat,
      regex("shallow reef(s)?|deep reef(s)?", ignore_case = TRUE),
      "Reef"
    ) %>% 
      str_replace_all("\\s*\\|\\s*", " | ") %>%  # Padroniza separadores
      str_replace_all("^\\s+|\\s+$", "") %>%     # Remove espaços extras
      {ifelse(. == "", NA_character_, .)}        # Converte vazio para NA
  ) %>%
  select(-terms_to_move)  # Remove coluna auxiliar

#Cheque ref_temp e caso esteja tudo ok substitua

ref <- ref_temp


# i) Shallow e Deep reef para SPP
spp_temp <- spp %>%
  mutate(
    # Passo 1: Identificar e extrair os Habitats, considerando variações no nome
    terms_to_move = str_extract_all(
      Habitat,
      regex("shallow reef(s)?|deep reef(s)?", ignore_case = TRUE)
    ) %>%
      lapply(function(x) {
        if (length(x) == 0) {
          NA_character_
        } else {
          # Padroniza o formato: primeira letra maiúscula e plural
          x %>% 
            str_to_sentence() %>% 
            str_replace("reef$", "reefs") %>% 
            unique()
        }
      }),
    
    # Passo 2: Processar Habitat_sub 
    Habitat_sub = sapply(1:n(), function(i) {
      # Termos existentes - trata NA/"NA" como vazio (não inclui na saída), caso você não tenha inserido NA pode ignorar
      existing <- if (is.na(Habitat_sub[i]) | Habitat_sub[i] == "NA") {
        character(0)
      } else {
        str_split(Habitat_sub[i], " \\| ")[[1]] %>% str_trim()
      }
      
      # Termos a adicionar (apenas os que não existem)
      to_add <- if (all(is.na(terms_to_move[[i]]))) {
        character(0)
      } else {
        setdiff(terms_to_move[[i]], existing)
      }
      
      # Combina os termos (ignora completamente NA/"NA" existentes)
      if (length(to_add) == 0) {
        if (length(existing) == 0) NA_character_ else Habitat_sub[i]
      } else {
        new_terms <- paste(c(existing, to_add), collapse = " | ")
        if (new_terms == "") NA_character_ else new_terms
      }
    }),
    
    # Passo 3: Modificar Habitat substituindo os termos
    Habitat = str_replace_all(
      Habitat,
      regex("shallow reef(s)?|deep reef(s)?", ignore_case = TRUE),
      "Reef"
    ) %>% 
      str_replace_all("\\s*\\|\\s*", " | ") %>%  # Padroniza separadores
      str_replace_all("^\\s+|\\s+$", "") %>%     # Remove espaços extras
      {ifelse(. == "", NA_character_, .)}        # Converte vazio para NA
  ) %>%
  select(-terms_to_move)  # Remove coluna auxiliar


#Cheque spp_temp e caso esteja tudo ok substitua

spp <- spp_temp


# ii) Plastisphere
# Criar novo ref_temp a partir do ref atualizado
ref_temp <- ref %>%
  mutate(
    # PASSO 1: Mover "Plastisphere" de Primary_theme e Secondary_theme para Habitat
    # Identificar células com "Plastisphere"
    plastisphere_in_primary = str_detect(Primary_theme, regex("Plastisphere", ignore_case = TRUE)),
    plastisphere_in_secondary = str_detect(Secondary_theme, regex("Plastisphere", ignore_case = TRUE)),
    
    # Extrair os termos "Plastisphere" para adicionar a Habitat
    plastisphere_to_add = case_when(
      plastisphere_in_primary & plastisphere_in_secondary ~ "Plastisphere",
      plastisphere_in_primary ~ "Plastisphere",
      plastisphere_in_secondary ~ "Plastisphere",
      TRUE ~ NA_character_
    ),
    
    # Atualizar Habitat (adicionar "Plastisphere" se necessário)
    Habitat = case_when(
      !is.na(plastisphere_to_add) & (is.na(Habitat) | Habitat == "NA") ~ plastisphere_to_add,
      !is.na(plastisphere_to_add) ~ paste(Habitat, plastisphere_to_add, sep = " | "),
      TRUE ~ Habitat
    ),
    
    # Remover "Plastisphere" das colunas de theme (substituir por NA)
    Primary_theme = ifelse(plastisphere_in_primary, NA_character_, Primary_theme),
    Secondary_theme = ifelse(plastisphere_in_secondary, NA_character_, Secondary_theme),
    
    # PASSO 2: Garantir que NAs não fiquem com " | " (manutenção do código anterior)
    Habitat = str_replace_all(Habitat, regex("NA \\| | \\| NA", ignore_case = TRUE), "") %>%
      str_replace_all("^\\s*|\\s*$", "") %>% # Remove espaços extras
      na_if(""), # Converte vazio para NA
    
    Habitat_sub = str_replace_all(Habitat_sub, regex("NA \\| | \\| NA", ignore_case = TRUE), "") %>%
      str_replace_all("^\\s*|\\s*$", "") %>% # Remove espaços extras
      na_if("") # Converte vazio para NA
  ) %>%
  # Remover colunas auxiliares
  select(-plastisphere_in_primary, -plastisphere_in_secondary, -plastisphere_to_add)

#Cheque ref_temp e caso esteja tudo ok substitua

ref <-ref_temp

# iii) Atualização de Marsh e SaltMarsh

ref <- ref %>%  
  mutate(Habitat = str_replace_all(Habitat, "\\bMarsh\\b", "Salt Marsh"))  

#### 2b) Atualização de Nomenclatura - Natural forcers ####
ref_temp <- ref %>%
  mutate(
    Natural_forcers = case_when(
      # i) River plume → Hydrological_River plume
      str_detect(Natural_forcers, regex("river\\s?plume", ignore_case = TRUE)) ~
        str_replace(Natural_forcers, regex("river\\s?plume", ignore_case = TRUE), 
                    "Hydrological_River plume"),
      
      # ii) Water mass intrusions → Hydrological_Water mass intrusions
      str_detect(Natural_forcers, regex("water\\s?mass\\s?(intrusions|intrusion)", ignore_case = TRUE)) ~
        str_replace(Natural_forcers, regex("water\\s?mass\\s?(intrusions|intrusion)", ignore_case = TRUE), 
                    "Hydrological_Water mass intrusions"),
      
      # iii) Parent separation → Calf-parent separation
      str_detect(Natural_forcers, regex("Parental separation", ignore_case = TRUE)) ~
        str_replace(Natural_forcers, regex("Parental separation", ignore_case = TRUE), 
                    "Calf-parent separation"),
      
      # iv) UV Radiation → Physical-chemical_UV Radiation
      str_detect(Natural_forcers, regex("uv\\s?radiation", ignore_case = TRUE)) ~
        str_replace(Natural_forcers, regex("uv\\s?radiation", ignore_case = TRUE), 
                    "Physical-chemical_UV Radiation"),
      
      # Mantém o original se não encontrar nenhum dos padrões
      TRUE ~ Natural_forcers
    )
  )
#Checar se está tudo ok e atualizar
ref <-ref_temp

#### 2c) Atualização de Nomenclatura - Data_type (spp) ####
spp_temp <- spp %>%
  mutate(
    Data_type = map_chr(Data_type, ~ {
      if (is.na(.x)) return(NA_character_)  # Mantém NAs
      
      # Separa os valores, remove NAs e espaços
      values <- str_split(.x, "\\s*\\|\\s*")[[1]] %>%
        discard(~ .x == "NA" | is.na(.x) | .x == "") %>%
        str_trim()
      
      # Aplica transformações em cada valor
      transformed_values <- map_chr(values, function(val) {
        case_when(
          str_detect(val, regex("^collection$", ignore_case = TRUE)) ~ "Scientific collection",
          str_detect(val, regex("^cultivation$", ignore_case = TRUE)) ~ "Cell and Microbiology Culture",
          str_detect(val, regex("^bycatch$", ignore_case = TRUE)) ~ "Fisheries_Bycatch",
          str_detect(val, regex("^ghost\\s*fishing$", ignore_case = TRUE)) ~ "Fisheries_Ghost Fishing",
          str_detect(val, regex("^fishing$", ignore_case = TRUE)) ~ "Fisheries_Fishing",
          str_detect(val, regex("^landing$", ignore_case = TRUE)) ~ "Fisheries_Landing",
          str_detect(val, regex("^herbarium$", ignore_case = TRUE)) ~ "Scientific Collection_Herbarium",
          str_detect(val, regex("^tracking$", ignore_case = TRUE)) ~ "Remote Sensing_Biologging",
          str_detect(val, regex("^satellite$", ignore_case = TRUE)) ~ "Remote Sensing_Imagery",
          TRUE ~ val
        )
      })
      
      # Combina os valores transformados, omitindo vazios
      if (length(transformed_values) == 0) NA_character_ else
        paste(transformed_values, collapse = " | ")
    })
  )
#Conferir se está tudo ok, caso estiver, substituir
spp <-spp_temp

#### 2d) Atualização de Nomenclatura - Primary e Secondary themes ####
ref_temp <- ref %>%
  mutate(
    across(c(Primary_theme, Secondary_theme), ~ {
      map_chr(.x, function(x) {
        if (is.na(x) || x == "NA") return(NA_character_)
        
        # Separa valores múltiplos
        values <- str_split(x, "\\s*\\|\\s*")[[1]] %>%
          discard(~ .x == "NA" | is.na(.x) | .x == "") %>%
          str_trim()
        
        # Aplica transformações
        transformed_values <- map_chr(values, function(val) {
          case_when(
            str_detect(val, regex("^Population Dynamics$", ignore_case = TRUE)) ~ 
              "Abundance and diversity_Population Dynamics",
            str_detect(val, regex("^Ecological Modeling$", ignore_case = TRUE)) ~ 
              "Modeling",
            str_detect(val, regex("^Evolutionary genetics$", ignore_case = TRUE)) ~ 
              "Molecular Ecology_Evolutionary genetics",
            str_detect(val, regex("^Metagenomics$", ignore_case = TRUE)) ~ 
              "Molecular Ecology_Metagenomics",
            str_detect(val, regex("^Population genetics$", ignore_case = TRUE)) ~ 
              "Molecular Ecology_Population genetics",
            str_detect(val, regex("^Feeding$", ignore_case = TRUE)) ~ 
              "Trophic Ecology_Feeding",
            str_detect(val, regex("^Stable isotopes$", ignore_case = TRUE)) ~  
              "Trophic Ecology_Stable isotopes",                              
            str_detect(val, regex("^Ecotoxicology$", ignore_case = TRUE)) ~ 
              "Pollution_Ecotoxicology",
            TRUE ~ val
          )
        })
        
        # Recombina os valores
        if (length(transformed_values) == 0) NA_character_ else
          paste(transformed_values, collapse = " | ")
      })
    })
  )

#Checar no temp se está tudo ok, se tiver, atualizar
ref <-ref_temp

#### 2e) Atualização de Nomenclatura - Anthropogenic threats ####
ref_temp <- ref %>%
  mutate(
    Anthropogenic_threats = map_chr(Anthropogenic_threats, ~ {
      if (is.na(.x) || .x == "NA") return(NA_character_)
      
      # Separa valores múltiplos e limpa
      values <- str_split(.x, "\\s*\\|\\s*")[[1]] %>%
        discard(~ .x == "NA" | is.na(.x) | .x == "") %>%
        str_trim()
      
      # Aplica transformações
      transformed_values <- map_chr(values, function(val) {
        case_when(
          # Correção para o typo Bycacth
          str_detect(val, regex("bycacth", ignore_case = TRUE)) ~ "Fisheries_Bycatch",
          
          # Animal disturbance
          str_detect(val, regex("boat\\s*collisions?", ignore_case = TRUE)) ~ "Animal disturbance_Boat collision",
          str_detect(val, regex("harvest", ignore_case = TRUE)) ~ "Animal disturbance_Harvest",
          str_detect(val, regex("injure", ignore_case = TRUE)) ~ "Animal disturbance_Injure",
          str_detect(val, regex("domestication", ignore_case = TRUE)) ~ "Animal disturbance_Domestication",
          
          # Fisheries (incluindo a versão correta Bycatch)
          str_detect(val, regex("aquaculture", ignore_case = TRUE)) ~ "Fisheries_Aquaculture",
          str_detect(val, regex("bycatch", ignore_case = TRUE)) ~ "Fisheries_Bycatch",
          str_detect(val, regex("fishery\\s*interactions?", ignore_case = TRUE)) ~ "Fisheries_Fishery Interaction",
          str_detect(val, regex("fishing\\s*pressure", ignore_case = TRUE)) ~ "Fisheries_Fishing Pressure",
          str_detect(val, regex("ghost\\s*gear", ignore_case = TRUE)) ~ "Fisheries_Ghost Gear",
          
          # Habitat disturbance
          str_detect(val, regex("dredge", ignore_case = TRUE)) ~ "Habitat disturbance_Dredge",
          str_detect(val, regex("portuary\\s*activit(y|ies)", ignore_case = TRUE)) ~ "Habitat disturbance_Portuary activity",
          str_detect(val, regex("urbanization", ignore_case = TRUE)) ~ "Habitat disturbance_Urbanization",
          
          # Pollution
          str_detect(val, regex("agroindustrial\\s*waste\\s*pollution", ignore_case = TRUE)) ~ "Pollution_Physico-chemical_AWP",
          str_detect(val, regex("domestic\\s*waste\\s*pollution", ignore_case = TRUE)) ~ "Pollution_Physico-chemical_DWP",
          str_detect(val, regex("heavy\\s*metals?", ignore_case = TRUE)) ~ "Pollution_Chemical_Heavy Metals",
          str_detect(val, regex("microplastics?", ignore_case = TRUE)) ~ "Pollution_Physico-chemical_Microplastics",
          str_detect(val, regex("mining", ignore_case = TRUE)) ~ "Pollution_Physico-chemical_Mining",
          str_detect(val, regex("oil\\s*fields?", ignore_case = TRUE)) ~ "Pollution_Physico-chemical_Oil Field",
          str_detect(val, regex("oil\\s*spills?", ignore_case = TRUE)) ~ "Pollution_Physico-chemical_Oil Spill",
          str_detect(val, regex("plastic\\s*pollution", ignore_case = TRUE)) ~ "Pollution_Physico-chemical_Plastic",
          str_detect(val, regex("pahs|polycyclic\\s*aromatic\\s*hydrocarbons", ignore_case = TRUE)) ~ "Pollution_Chemical_PAH",
          str_detect(val, regex("pops|persistent\\s*organic\\s*pollutants", ignore_case = TRUE)) ~ "Pollution_Chemical_POPs",
          str_detect(val, regex("light\\s*pollution", ignore_case = TRUE)) ~ "Pollution_Physical_Light",
          str_detect(val, regex("noise\\s*pollution", ignore_case = TRUE)) ~ "Pollution_Physical_Noise",
          str_detect(val, regex("thermal\\s*pollution", ignore_case = TRUE)) ~ "Pollution_Physical_Thermal",
          str_detect(val, regex("debris\\s*ingestion", ignore_case = TRUE)) ~ "Pollution_Debris ingestion",
          
          # Mantém original se não encontrar padrão
          TRUE ~ val
        )
      })
      
      # Recombina os valores
      if (length(transformed_values) == 0) NA_character_ else
        paste(transformed_values, collapse = " | ")
    })
  )

#Checar no temp se está tudo ok, se tiver, atualizar
ref <-ref_temp

#### 3) Criação da coluna Global_filling ####

# Criar Global_filling como cópia de Habitat, posicionada após Habitat_sub
ref_temp <- ref %>%
  mutate(Global_filling = Habitat, .after = Habitat_sub)

#Caso 1. Continental shelf
# Lista de subcategorias para Continental shelf
continental_subcategories <- c(
  "Bay", "Intertidal area", "Subtidal area", "Basin", "Bight", 
  "Cove", "Channel", "Gulf", "Artificial structure", "Peninsula",
  "Continental island", "Submarine canyons"
)

# Função para mover subcategorias erroneamente de Habitat para Habitat_sub
move_subcategories_to_sub <- function(Habitat, Habitat_sub) { 
  # Extrai valores de Habitat
  habitat_values <- if (is.na(Habitat)) {
    character(0)
  } else {
    str_split(Habitat, "\\s*\\|\\s*")[[1]] %>%
      str_trim()
  }
  
  # Identifica subcategorias que estão erroneamente em Habitat
  subs_in_habitat <- habitat_values[habitat_values %in% continental_subcategories]
  
  if (length(subs_in_habitat) > 0) {
    # Remove as subcategorias de Habitat
    new_habitat <- habitat_values[!habitat_values %in% continental_subcategories]
    new_habitat <- if (length(new_habitat) == 0) NA_character_ else paste(new_habitat, collapse = " | ")
    
    # Adiciona as subcategorias a Habitat_sub
    existing_subs <- if (is.na(Habitat_sub)) {
      character(0)
    } else {
      str_split(Habitat_sub, "\\s*\\|\\s*")[[1]] %>%
        str_trim()
    }
    
    new_subs <- unique(c(existing_subs, subs_in_habitat))
    new_habitat_sub <- if (length(new_subs) == 0) NA_character_ else paste(new_subs, collapse = " | ")
    
    return(list(habitat = new_habitat, habitat_sub = new_habitat_sub))
  }
  
  return(list(habitat = Habitat, habitat_sub = Habitat_sub))
}
# Função atualizada para processar múltiplos habitats
process_continental_cases <- function(habitat, global_filling, habitat_sub) {
  # Primeiro corrige casos onde subcategorias estão em Habitat em vez de Habitat_sub
  corrected <- move_subcategories_to_sub(habitat, habitat_sub)
  habitat <- corrected$habitat
  habitat_sub <- corrected$habitat_sub
  
  # Verifica se há subcategorias relevantes (exceto casos especiais)
  if (is.na(habitat_sub) || habitat_sub == "NA") return(global_filling)
  
  subs <- str_split(habitat_sub, "\\s*\\|\\s*")[[1]] %>%
    str_trim() %>%
    .[. %in% continental_subcategories]
  
  if (length(subs) == 0) return(global_filling)
  
  # Processa cada subcategoria
  new_entries <- c()
  for (sub in subs) {
    if (sub %in% c("Intertidal area", "Subtidal area")) {
      # Para estes casos, só adiciona se "Continental shelf" estiver no habitat
      if (str_detect(habitat, "Continental shelf")) {
        new_entries <- c(new_entries, paste0("Continental shelf_", sub))
      }
    } else {
      # Para outras subcategorias, adiciona diretamente
      new_entries <- c(new_entries, paste0("Continental shelf_", sub))
    }
  }
  
  if (length(new_entries) == 0) return(global_filling)
  
  # Mantém outros valores não relacionados
  current_values <- if (is.na(global_filling)) {
    character(0)
  } else {
    str_split(global_filling, "\\s*\\|\\s*")[[1]] %>%
      str_trim() %>%
      .[. != "Continental shelf"]
  }
  
  all_values <- unique(c(current_values, new_entries))
  if (length(all_values) == 0) NA_character_ else paste(all_values, collapse = " | ")
}

# Aplicar a transformação em ref_temp
ref_temp <- ref_temp %>%
  mutate(
    # Primeiro corrigimos Habitat e Habitat_sub
    corrected_data = pmap(list(Habitat, Habitat_sub), move_subcategories_to_sub),
    Habitat = map_chr(corrected_data, "habitat"),
    Habitat_sub = map_chr(corrected_data, "habitat_sub"),
    # Depois processamos o Global_filling
    Global_filling = pmap_chr(list(Habitat, Global_filling, Habitat_sub), process_continental_cases),
    # Remove a coluna temporária
    corrected_data = NULL
  )

#Checar se está ok, caso esteja atualizar ref

ref <- ref_temp

#Caso 2 Deep Ocean
# Função final completa
update_deep_ocean <- function(habitat, habitat_sub, global_filling) {
  # 1. Primeiro REMOVE subcategorias mal formatadas
  if (!is.na(global_filling)) {
    global_filling <- global_filling %>%
      str_split("\\s*\\|\\s*") %>%
      pluck(1) %>%
      # Remove qualquer Seamounts/Submarine canyons sem prefixo correto
      discard(~ str_detect(.x, "(?<!Deep ocean_)(Seamounts|Submarine canyons)")) %>%
      {if (length(.) == 0) NA else paste(., collapse = " | ")}
  }
  
  # 2. Processa novas entradas (mesma lógica anterior)
  new_entries <- character(0)
  remove_entries <- c("Deep ocean", "Seamounts")
  
  # Processa Habitat_sub
  if (!is.na(habitat_sub)) {
    subs <- str_split(habitat_sub, "\\s*\\|\\s*")[[1]] %>% 
      str_trim() %>%
      .[. %in% c("Seamounts", "Submarine canyons")]
    
    if (length(subs) > 0) {
      new_entries <- paste0("Deep ocean_", subs)
    }
  }
  
  # Processa Habitat
  if (!is.na(habitat) && str_detect(habitat, "Seamounts")) { ## Added "!is.na(habitat) && "
    new_entries <- c(new_entries, "Deep ocean_Seamounts")
  }
  
  # 3. Atualiza Global_filling
  if (length(new_entries) > 0) {
    current_values <- if (is.na(global_filling)) {
      character(0)
    } else {
      str_split(global_filling, "\\s*\\|\\s*")[[1]] %>%
        str_trim() %>%
        .[!. %in% remove_entries]
    }
    
    global_filling <- unique(c(current_values, new_entries)) %>%
      {if (length(.) == 0) NA else paste(., collapse = " | ")}
  }
  
  global_filling
}

# Aplicação final
ref_temp <- ref_temp %>%
  mutate(
    Global_filling = pmap_chr(list(Habitat, Habitat_sub, Global_filling), update_deep_ocean)
  )

#Checar se está ok, caso esteja atualizar ref

ref <- ref_temp

#Caso 3. Estuary

# Função para processar casos de Estuary
process_estuary <- function(habitat, habitat_sub, global_filling) {
  # Verifica se é um caso Estuary (considerando múltiplos valores)
  is_estuary_case <- str_detect(habitat, "Estuary")
  
  # Subcategorias específicas
  estuary_subs <- c("Creek", "Intertidal area", "Subtidal area")
  
  # Processa apenas se for caso Estuary e tiver Habitat_sub
  if (is_estuary_case && !is.na(habitat_sub)) {
    # Extrai subcategorias relevantes
    subs <- str_split(habitat_sub, "\\s*\\|\\s*")[[1]] %>% 
      str_trim() %>%
      .[. %in% estuary_subs]
    
    if (length(subs) > 0) {
      # Cria as novas entradas com prefixo
      new_entries <- ifelse(
        subs == "Creek",
        "Estuary_Creek",
        ifelse(subs %in% c("Intertidal area", "Subtidal area"),
               paste0("Estuary_", subs),
               subs)
      )
      
      # Remove "Estuary" solto se existir
      current_values <- if (is.na(global_filling)) {
        character(0)
      } else {
        str_split(global_filling, "\\s*\\|\\s*")[[1]] %>%
          str_trim() %>%
          .[. != "Estuary"]  # Remove "Estuary" solto
      }
      
      # Combina tudo
      global_filling <- unique(c(current_values, new_entries)) %>%
        {if (length(.) == 0) NA else paste(., collapse = " | ")}
    }
  }
  
  # Caso especial para Creek (mesmo sem Estuary em Habitat)
  if (!is_estuary_case && !is.na(habitat_sub) && str_detect(habitat_sub, "Creek")) {
    new_entry <- "Estuary_Creek"
    current_values <- if (is.na(global_filling)) {
      character(0)
    } else {
      str_split(global_filling, "\\s*\\|\\s*")[[1]] %>%
        str_trim()
    }
    global_filling <- unique(c(current_values, new_entry)) %>%
      {if (length(.) == 0) NA else paste(., collapse = " | ")}
  }
  
  global_filling
}

# Aplicar a transformação em ref_temp
ref_temp <- ref_temp %>%
  mutate(
    Global_filling = pmap_chr(list(Habitat, Habitat_sub, Global_filling), process_estuary)
  )

#Checar se está ok, caso esteja atualizar ref

ref <- ref_temp

# Caso 4. Oceanic island

# Função para processar casos de Oceanic Island
process_oceanic_island <- function(habitat, habitat_sub, global_filling) {
  # Subcategorias específicas
  oceanic_subs <- c("Atoll", "Archipelago")
  
  # Verifica se há subcategorias relevantes em Habitat_sub
  if (!is.na(habitat_sub)) {
    subs <- str_split(habitat_sub, "\\s*\\|\\s*")[[1]] %>% 
      str_trim() %>%
      .[. %in% oceanic_subs]
    
    if (length(subs) > 0) {
      # Cria as novas entradas com prefixo
      new_entries <- paste0("Oceanic island_", subs)
      
      # Remove "Oceanic island" solto se existir
      current_values <- if (is.na(global_filling)) {
        character(0)
      } else {
        str_split(global_filling, "\\s*\\|\\s*")[[1]] %>%
          str_trim() %>%
          .[. != "Oceanic island"]  # Remove "Oceanic island" solto
      }
      
      # Combina tudo
      global_filling <- unique(c(current_values, new_entries)) %>%
        {if (length(.) == 0) NA else paste(., collapse = " | ")}
    }
  }
  
  global_filling
}

# Aplicar a transformação em ref_temp
ref_temp <- ref_temp %>%
  mutate(
    Global_filling = pmap_chr(list(Habitat, Habitat_sub, Global_filling), process_oceanic_island)
  )

#Checar se está ok, caso esteja atualizar ref

ref <- ref_temp

#Caso 5. Reef

# Função para processar casos de Reef
process_reefs <- function(habitat, habitat_sub, global_filling) {
  # Subcategorias específicas
  reef_subs <- c("Deep reefs", "Shallow reefs")
  
  # Verifica se há subcategorias relevantes em Habitat_sub
  if (!is.na(habitat_sub)) {
    subs <- str_split(habitat_sub, "\\s*\\|\\s*")[[1]] %>% 
      str_trim() %>%
      .[. %in% reef_subs]
    
    if (length(subs) > 0) {
      # Cria as novas entradas com prefixo
      new_entries <- paste0("Reef_", subs)
      
      # Remove "Reef" solto se existir
      current_values <- if (is.na(global_filling)) {
        character(0)
      } else {
        str_split(global_filling, "\\s*\\|\\s*")[[1]] %>%
          str_trim() %>%
          .[. != "Reef"]  # Remove "Reef" solto
      }
      
      # Combina tudo
      global_filling <- unique(c(current_values, new_entries)) %>%
        {if (length(.) == 0) NA else paste(., collapse = " | ")}
    }
  }
  
  global_filling
}

# Aplicar a transformação em ref_temp
ref_temp <- ref_temp %>%
  mutate(
    Global_filling = pmap_chr(list(Habitat, Habitat_sub, Global_filling), process_reefs)
  )

#Checar se está ok, caso esteja atualizar ref

ref <- ref_temp

#Caso 6. Sandy beach
# Função para processar casos de Sandy beach
process_sandy_beach <- function(habitat, habitat_sub, global_filling) {
  # Verifica se é um caso Sandy beach (considerando múltiplos valores)
  is_sandy_beach_case <- str_detect(habitat, "Sandy beach")
  
  # Subcategorias específicas
  sandy_subs_all <- c("Sandy dunes", "Intertidal area", "Subtidal area")
  sandy_subs_conditional <- c("Intertidal area", "Subtidal area") # Exigem Sandy beach em Habitat
  
  # Processa apenas se tiver Habitat_sub relevante
  if (!is.na(habitat_sub)) {
    # Extrai subcategorias
    subs <- str_split(habitat_sub, "\\s*\\|\\s*")[[1]] %>% 
      str_trim() %>%
      .[. %in% sandy_subs_all]
    
    # Filtra as condicionais
    if (!is_sandy_beach_case) {
      subs <- subs[!subs %in% sandy_subs_conditional]
    }
    
    if (length(subs) > 0) {
      # Cria as novas entradas com prefixo
      new_entries <- paste0("Sandy beach_", subs)
      
      # Remove "Sandy beach" solto se existir
      current_values <- if (is.na(global_filling)) {
        character(0)
      } else {
        str_split(global_filling, "\\s*\\|\\s*")[[1]] %>%
          str_trim() %>%
          .[. != "Sandy beach"]  # Remove "Sandy beach" solto
      }
      
      # Combina tudo
      global_filling <- unique(c(current_values, new_entries)) %>%
        {if (length(.) == 0) NA else paste(., collapse = " | ")}
    }
  }
  
  global_filling
}

# Aplicar a transformação em ref_temp
ref_temp <- ref_temp %>%
  mutate(
    Global_filling = pmap_chr(list(Habitat, Habitat_sub, Global_filling), process_sandy_beach)
  )

#Checar se está ok, caso esteja atualizar ref

ref <- ref_temp

#Caso 7. Plastisphere
# Função para adicionar prefixo Plastisphere
add_plastisphere_prefix <- function(habitat, global_filling) {
  if (!is.na(habitat) && str_detect(habitat, "Plastisphere")) { ## Added "!is.na(habitat) && "
    if (is.na(global_filling)) {
      "Plastisphere"
    } else {
      # Adiciona prefixo, exceto ao próprio "Plastisphere"
      items <- str_split(global_filling, "\\s*\\|\\s*")[[1]] %>%
        map_chr(~ {
          if (.x == "Plastisphere") {
            "Plastisphere"
          } else if (!str_detect(.x, "^Plastisphere_")) {
            paste0("Plastisphere_", .x)
          } else {
            .x
          }
        })
      
      paste(items, collapse = " | ")
    }
  } else {
    global_filling
  }
}

# Função para limpar Plastisphere redundante
clean_plastisphere_duplicates <- function(global_filling) {
  if (is.na(global_filling)) return(NA_character_)
  
  items <- str_split(global_filling, "\\s*\\|\\s*")[[1]] %>%
    str_trim()
  
  # Se tiver "Plastisphere" junto com outros mais específicos
  if ("Plastisphere" %in% items && any(str_detect(items, "^Plastisphere_"))) {
    items <- items[items != "Plastisphere"]  # Remove o genérico
  }
  
  if (length(items) == 0) {
    NA_character_
  } else {
    paste(items, collapse = " | ")
  }
}

# Aplicação final
ref_temp <- ref_temp %>%
  mutate(
    # Primeiro aplica os prefixos
    Global_filling = map2_chr(Habitat, Global_filling, add_plastisphere_prefix),
    # Depois limpa os redundantes
    Global_filling = map_chr(Global_filling, clean_plastisphere_duplicates)
  )


#Checar ref_temp e se estiver ok salvar

ref <- ref_temp


#### 4) Atualizando spp ####

# 1. Selecionar as colunas atualizadas de ref_temp
ref_updates <- ref_temp %>%
  select(Number, Global_filling)

# 2. Atualizar spp com posicionamento correto
spp_updated <- spp %>%
  left_join(ref_updates, by = "Number") %>%
  # Reposiciona as colunas após Data_type
  relocate(any_of(c("Global_filling")), 
           .after = "Data_type")



spp <- spp_updated


#### 5) Remover duplicadas #####
# Aqui é para o caso de nas modificações ter ficado nos habitats mais de um, por exemplo Reef | Reef
# Função para remover duplicatas dentro de uma célula
clean_duplicates_within_cell <- function(column) {
  map_chr(column, ~ {
    if (is.na(.x) || .x == "NA") return(NA_character_)
    
    # Divide os valores
    values <- str_split(.x, "\\s*\\|\\s*")[[1]] %>% 
      str_trim()
    
    # Remove duplicatas mantendo a ordem
    unique_values <- unique(values)
    
    # Recombina (ou retorna NA se vazio)
    if (length(unique_values) == 0) NA_character_ else paste(unique_values, collapse = " | ")
  })
}

# Aplicar às colunas
ref <- ref %>%
  mutate(
    Habitat = clean_duplicates_within_cell(Habitat),
    Habitat_sub = clean_duplicates_within_cell(Habitat_sub)
  )

spp <- spp %>%
  mutate(
    Habitat = clean_duplicates_within_cell(Habitat),
    Habitat_sub = clean_duplicates_within_cell(Habitat_sub)
  )

#### Exportar banco de dados atualizado ####


# Salvar o dataframe 'ref'

# write.csv2(ref, "./data-processed/bib_sea-turtles.csv", row.names = FALSE)
# write.csv2(ref, "./data-processed/bib_seabirds.csv", row.names = FALSE)
# write.csv2(ref, "./data-processed/bib_marine-mammals.csv", row.names = FALSE)

# Salvar o dataframe 'spp'

# write.csv(spp, "./data-processed/spp_sea-turtles.csv", row.names = FALSE)
# write.csv(spp, "./data-processed/spp_seabirds.csv", row.names = FALSE)
# write.csv(spp, "./data-processed/spp_marine-mammals.csv", row.names = FALSE)
