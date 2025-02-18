##==============================================================================
## 1) Loading shape files and extracting the data
##==============================================================================

##Read all the shapefiles and extract the data
df <- NULL
f_shp <- gsub(path_to_mod, path_shapes, folders)
for(f in f_shp){
  # Read the shapefile
  print(f)
  shp <- read_sf(list.files(path = f, pattern = ".shp$", full.names = TRUE))
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
  print("done!")
}

##==============================================================================
## 2) Loading management files and extracting the data
##==============================================================================

##Extract management data
df_mgt <- NULL
for(f in f_shp){
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

##==============================================================================
## 3) Cleaning management data
##==============================================================================

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

## Correcting the land use for CS3 and CS11
df_mgt_link[df_mgt_link$cs_name %in% c("CS3", "CS11") &
              df_mgt_link$lu== "meadow", "lu"] <- "pasture"

##==============================================================================
## 4) Cleaning management data
##==============================================================================

## Reading base plant.plt
plants_base <- plants_plt_base %>%
  select(-description) %>%
  filter(name %in% unique(df_mgt_link$op_data1)) %>%
  select(name, tmp_base, days_mat, lai_pot, harv_idx, bm_e) %>%
  pivot_longer(!c("name"), names_to = "parameter", values_to = "value") %>%
  mutate(cs_name = "base") %>%
  filter(name %in% crops_sel)
plants_base$name <- factor(plants_base$name, levels = crops_sel, ordered = TRUE)

##==============================================================================
## 5) Combining calibrated plants.plt for all the case studies
##==============================================================================

## Reading calibrated plants.plt for all the case studies
plants_df <- NULL
for(f in f_ini){
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

##==============================================================================
## 6) Preparing the data frame for the plot
##==============================================================================

## Cleaning up the data frame
plants <- plants_df %>%
  inner_join(df_mgt_link, by = c("cs_name", "name" = "op_data1")) %>%
  select(cs_name, name, tmp_base, days_mat, lai_pot, harv_idx, bm_e) %>%
  pivot_longer(!c("cs_name", "name"), names_to = "parameter", values_to = "value") %>%
  ##Removing the numbers from the crop names
  mutate(name = str_replace_all(name, "[:digit:]", "")) %>%
  ##Removing additional things from the crop names
  mutate(name = str_replace_all(name, "_.*$", "")) %>%
  filter(name %in% crops_sel) %>%
  left_join(lookup, by = "cs_name")

## Factors to adjust the order of the case studies codes
plants$cs_label  <- factor(plants$cs_label, levels = order_cs, ordered = TRUE)
plants$name <- factor(plants$name, levels = crops_sel, ordered = TRUE)

## Defining the color palette
my_pal <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2",
            "#D55E00", "#CC79A7", "grey20", "cyan3", "#FFD700", "#8B4513",
            "#4682B4", "#FF4500", "#6A5ACD")

##==============================================================================
## 7) Make a plot of parameter ranges and saving
##==============================================================================

##Make a plot of parameter ranges
fig3 <- ggplot() +
  geom_point(data = plants_base, aes(x = value, y = name), color = "black",
             size = 12, shape = 124)+
  geom_point(data = plants, aes(x = value, y = name, color = cs_label), size = 3,
             alpha = 0.9, position = position_dodge(width = 0.3)) +

  scale_color_manual(values =  my_pal[c(1:length(folders))])+
  facet_grid(~factor(parameter, levels = c("days_mat", "bm_e", "harv_idx",
                                           "lai_pot","tmp_base")), scales = "free") +
  theme_bw() +
  theme(axis.text.y = element_text(face = "bold"),
        strip.text = element_text(face = "bold")) +
  labs(x = "Parameter values",
       y = "Crops",
       color='Catchments')

fig3

##==============================================================================
## 8) Save the plots
##==============================================================================

fig_path <- paste(results_folder, "figs", sep = "/")
if (!file.exists(fig_path)) dir.create(fig_path)

ggsave(filename = paste(fig_path, "fg2_par_change.png", sep = "/"), plot = fig3,
       width = 1000, height = 700, units = "px", dpi = 100)

