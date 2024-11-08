library(SWATtunR)
library(sf)
library(tidyverse)
library(stringr)
library(treemapify)
library(RColorBrewer)
library(tidytext)
# library(devtools)
# install_github("wilkox/treemapify")
# library(treemapify)
# install.packages("tidytext")

## Settings
## =============================================================================
## Paths to the shapes and csvs
path_shapes <- "Data/Shapes_CVSs"
## Paths to the models
paths_models <- "Data/Models"
## Crop selection
crops_sel <- c("wwht", "wbar", "swht", "fesc", "corn", "canp", "barl", "alfa")
## Lookup to the CS info in the figure
lookup <- data.frame(cs_name = c("CS1", "CS2", "CS3", "CS4", "CS7", "CS8",
                                 "CS9", "CS10", "CS11", "CS12"),
                     cs_label = c("DE", "CH", "HU1", "PL",
                                  "BE", "LT", "IT", "NO",
                                  "HU2", "CZ"))
## =============================================================================

##Reading all the paths
folders <- list.dirs(path = path_shapes, full.names = TRUE, recursive = TRUE)[-1]
folders_plants <- list.dirs(paths_models, full.names = TRUE, recursive = FALSE)

##Read all the shapefiles and extract the data
df <- NULL
for(f in folders){
  # Read the shapefile
  print(f)
  shp <- read_sf(list.files(path = f, pattern = ".shp$", full.names = TRUE))
  # if(basename(f) == "CS2") shp$type <- shp$lu_2
  if(!"type" %in% names(shp)) shp$type <- shp$lu
  shp$area <- as.vector(st_area(shp))
  shp <- shp %>%
    select(type, area, starts_with("y_")) %>%
    st_drop_geometry
  ##Removing "NA" from all columns if exists in shp dataframe
  shp[shp == "NA"] <- NA
  shp[] <- lapply(seq_len(ncol(shp)), function(i) {
    if (i == which(names(shp) == "type")) {
      return(shp[[i]])  # Keep the 'type' column unchanged
    } else {
      return(ifelse(is.na(shp[[i]]), shp$type, shp[[i]]))}})
  shp <- shp %>%
    pivot_longer(cols = -c(type, area), names_to = "year", values_to = "crop") %>%
    mutate(year = as.numeric(gsub("[^0-9]", "", year))) %>%
    select(-type) %>%
    mutate(cs_name = basename(f))
  if(is.null(df)) df <- shp else df <- bind_rows(df, shp)
  # if(basename(f) == "CS2") stop()
  print("done!")
}

##Extract management data
df_mgt <- NULL
for(f in folders){
  gen <- list.files(path = f, pattern = "generic", full.names = TRUE)
  crp <- list.files(path = f, pattern = "mgt_crop", full.names = TRUE)
  if(length(crp) == 0)  crp <- list.files(path = f, pattern = "crop", full.names = TRUE)
  gen <- read.csv(gen) %>%
    rename(lu = 1) %>%
    mutate(cs_name = basename(f),
           mgt_type = "generic",
           op_data3 = as.character(op_data3))
  crp <- read.csv(crp) %>%
    rename(lu = 1) %>%
    mutate(cs_name = basename(f),
           mgt_type = "crop",
           op_data3 = as.character(op_data3))
  if(is.null(df_mgt)) df_mgt <- gen else df_mgt <- bind_rows(df_mgt, gen)
  df_mgt <- bind_rows(df_mgt, crp)
}

## Clean the management data
df_mgt_link <- df_mgt %>%
  #filter by operation column start with "harvest"
  filter(str_detect(operation, '^harvest')|str_detect(operation, '^initial_plant')) %>%
  select(lu, op_data1, cs_name) %>%
  filter(!(grepl("^cov",lu) & grepl("^radi",op_data1))) %>% ##Removing cover crops with radishes
  filter(!(grepl("CS7",cs_name) & grepl("^radi|^ryeg",op_data1))) %>% ##Removing cover crops with radishes))
  filter(!(grepl("CS2",cs_name) & grepl("^shrb",op_data1))) %>%
  filter(!(grepl("CS9",cs_name) & grepl("^mustard",op_data1))) %>%
  mutate(lu = str_replace_all(lu, "_[0-9]+\\.[0-9]+yr", "")) %>% ##Removing the pastures with different harvests patterns
  mutate(lu = str_replace_all(lu, "_.*$", "")) %>%
  unique() %>%
  group_by(cs_name, lu)%>%
  slice(1) %>%
  ungroup()

df_mgt_link[df_mgt_link$cs_name %in% c("CS3b", "CS11") & df_mgt_link$lu== "meadow", "lu"] <- "pasture"

## Reading base plant.plt
plants_base <- read_tbl("Data/Models/plants.plt") %>%
  select(-description) %>%
  filter(name %in% unique(df_mgt_link$op_data1)) %>%
  select(name, tmp_base, days_mat, lai_pot, harv_idx, bm_e) %>%
  pivot_longer(!c("name"), names_to = "parameter", values_to = "value") %>%
  mutate(cs_name = "base") %>%
  filter(name %in% crops_sel)
plants_base$name <- factor(plants_base$name, levels = crops_sel, ordered = TRUE)

## Reading calibrated plants.plt for all the case studies
plants_df <- NULL
for(f in folders_plants){
  plants <- read_tbl(paste0(f, "/plants.plt.bkp99"))
  plants$cs_name <- basename(f)
  if("wnd_live" %in% names(plants)){
    plants <- dplyr::rename(plants, aeration = wnd_live)
  }
  if(is.null(plants_df)){
    plants_df <- plants
  } else {
    plants_add <- select(plants, any_of(names(plants_df))) %>%
      map2_df(map(select(plants_df, any_of(names(plants))), class), ~{class(.x) <- .y;.x})
    plants_df <- bind_rows(plants_df, plants_add)
  }
}

## Cleaning up the data frame
plants <- plants_df %>%
  inner_join(df_mgt_link %>% mutate(cs_name = ifelse(cs_name == "CS3b", "CS3", cs_name)), by = c("cs_name", "name" = "op_data1")) %>%
  select(cs_name, name, tmp_base, days_mat, lai_pot, harv_idx, bm_e) %>%
  pivot_longer(!c("cs_name", "name"), names_to = "parameter", values_to = "value") %>%
  mutate(name = str_replace_all(name, "[:digit:]", "")) %>%
  mutate(name = str_replace_all(name, "_.*$", "")) %>%
  filter(name %in% crops_sel) %>%
  left_join(lookup, by = "cs_name")

## Factors to adjust the order of the case studies codes
plants$cs_label  <- factor(plants$cs_label, levels = lookup$cs_label, ordered = TRUE)
plants$name <- factor(plants$name, levels = crops_sel, ordered = TRUE)


##Make a plot of parameter ranges
ggplot() +
  geom_point(data = plants_base, aes(x = value, y = name), color = "black", size = 12, shape = 124)+
  geom_point(data = plants, aes(x = value, y = name, color = factor(cs_label, level = c("BE", "CH", "CZ", "DE", "HU1", "HU2", "IT", "LT", "NO", "PL"))), size = 3, alpha = 0.9, position = position_dodge(width = 0.3)) +

  scale_color_manual(values = c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "grey20" , "cyan3"))+
  #facet_grid(~parameter, scales = "free") +
  facet_grid(~factor(parameter, levels = c("days_mat", "bm_e", "harv_idx","lai_pot","tmp_base")), scales = "free") +
  theme_bw() +
  theme(axis.text.y = element_text(face = "bold"),
        strip.text = element_text(face = "bold")) +
  labs(x = "Parameter values",
       y = "Crops",
       color='Catchments')

