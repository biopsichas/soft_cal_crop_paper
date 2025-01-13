##==============================================================================
## 1) Loading shape files and extracting the data
##==============================================================================

f_shp <- gsub(path_to_mod, path_shapes, folders)
df <- NULL
##Read all the shapefiles and extract the data
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

## Cleaning crop labels
df <- mutate(df, crop = str_replace_all(crop, "_.*$", "")) %>%
  group_by(cs_name, crop, year) %>%
  summarise(area = sum(area))

##==============================================================================
## 2) Loading management files and extracting data
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

df_mgt_link[df_mgt_link$cs_name %in% c("CS3", "CS11") & df_mgt_link$lu== "meadow", "lu"] <- "pasture"

##==============================================================================
## 4) Get the number of years for each case study
##==============================================================================
df_years <- df %>%
  ungroup() %>%
  select(cs_name, year) %>%
  group_by(cs_name) %>%
  summarise(year_min = min(year), year_max = max(year)) %>%
  mutate(years = year_max - year_min + 1) %>%
  select(cs_name, years) %>%
  left_join(lookup, by = "cs_name")

##==============================================================================
## 5) Testing are the areas right
##==============================================================================
df %>% ungroup %>%
  select(-crop) %>%
  group_by(cs_name) %>%
  summarise(area = round(sum(area)/1000000, 2)) %>%
  left_join(df_years, by = "cs_name") %>%
  mutate(area = area/years)

##==============================================================================
## 6) Clean the crop rotation data and join with the management data (to get crop codes)
##==============================================================================

df_clean <- df %>%
  select(-year) %>%
  group_by(cs_name, crop) %>%
  summarise(area = sum(area)) %>%
  ungroup() %>%
  left_join(df_mgt_link, by = c("crop" = "lu", "cs_name")) %>%
  mutate(op_data1 = str_replace_all(op_data1, "_.*$", "")) %>%
  mutate(op_data1= str_replace_all(op_data1, "[0-9]", "")) %>%
  filter(!is.na(op_data1) & !op_data1 %in% c("rnge", "frst", "rnge_test", "orcd", "rngb",
                                             "wetl", "frst_test", "wetn", "frst_comm",
                                             "orcd_comm", "wetl_comm", "frsd", "frse", "oak",
                                             "pine", "popl", "will", "frsdit", "hayd")) %>%
  left_join(lookup, by = "cs_name")


##==============================================================================
## 7) Testing are agricultral areas right
##==============================================================================

df_clean %>%
  ungroup() %>%
  select(-op_data1) %>%
  group_by(cs_name, cs_label) %>%
  summarise(area = sum(area)) %>%
  ungroup() %>%
  left_join(df_years, by = "cs_name") %>%
  mutate(area = round(area/years/1000000, 2))

## Check if there are any missing or not needed crop codes
unique(df_clean$op_data1)

##==============================================================================
## 8) Calculate the percentage of each crop in the rotation
##==============================================================================

df_final <- left_join(df_clean, select(df_clean, cs_name, area) %>%
                        group_by(cs_name) %>%
                        summarise_all(sum) %>%
                        rename(area_sum = area), by = "cs_name") %>%
  mutate(proc = round(100*area/area_sum, 1)) %>%
  select(-starts_with(c("area", "crop"))) %>%
  group_by(cs_label, op_data1) %>%
  summarise(proc = sum(proc)) %>%
  ungroup() %>%
  rename(crop = op_data1)

df_final_area <- df_clean %>%
  select(-crop) %>%
  group_by(cs_label, op_data1) %>%
  summarise(area = sum(area)) %>%
  left_join(select(df_clean, cs_label, area) %>%
              group_by(cs_label) %>%
              summarise_all(sum) %>%
              rename(area_sum = area), by = "cs_label") %>%
  left_join(df_years, by = "cs_label") %>%
  mutate(area = round(0.000001* area / years, 2),
         area_sum = round(0.000001* area_sum / years, 2)) %>%
  select(-years)

##==============================================================================
## 9) Check the sums
##==============================================================================

df_final[c("cs_label","proc")] %>% group_by(cs_label) %>% summarise_all(sum)

##==============================================================================
## 10) Prepare excel tables
##==============================================================================
table <- df_final %>%
  pivot_wider(names_from = cs_label, values_from = proc)
# order_cs <- order_cs[order_cs != lookup[!lookup$cs_name %in% unique(res$cs), "cs_label"]]
table <- table[, c("crop", order_cs)]

##==============================================================================
## 11) Plot the data
##==============================================================================
## Order of the case studies in the plots
df_final$cs_label  <- factor(df_final$cs_label, levels = order_cs, ordered = TRUE)

## Plot the area data
fig6 <- ggplot(df_final, aes(reorder_within(crop, proc, within= tolower(cs_label), sep = ">"), proc)) +
  geom_bar(stat="identity",fill = "blue") +  # Adjust binwidth as needed
  facet_wrap(~cs_label, scales = "free") +
  labs(title = "Precentage of crop areas during all rotations",
       x = "Codes",
       y = "% by area") +
  theme_minimal()+
  theme(strip.text.x = element_text(face = "bold", size = 12))+
  coord_flip()

## Plot the data in a treemap
## Prepare color schema
# n <- length(unique(df_final$crop))
# qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
# color_schema <-  unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))

## Count the number of unique crops in plot, which should be equal to the number of colors
unique(df_final %>% filter(proc >= 1) %>% select(crop)) %>% count
## Color schema
color_schema <- c(
  "akgs" = "#00BFFF",   # Akgs (Alkali Grass)
  "alfa" = "#FF5733",   # Alfa (Alfalfa)
  "barl" = "#A1FF66",   # Barley
  "berm" = "#228B22",   # Bermuda Grass (Green color)
  "canp" = "#33A1FF",   # Canola
  "cana" = "#8B0000",   # Canola (Redder variant)
  "cngr" = "#32CD32",   # Conger (type of grass)
  "clvr" = "#6699FF",   # Clover
  "corn" = "#FF5733",   # Corn
  "crrt" = "#B22222",   # Carrot
  "csil" = "#33FF57",   # Silage (Corn Silage)
  "csi"  = "#FF6347",   # Corn Silage (Tomato Red for distinction)
  "fesc" = "#3357FF",   # Fescue
  "fpea" = "#66FF99",   # Field Pea
  "grbn" = "#A52A2A",   # Green Beans (Brownish)
  "hay"  = "gray80",    # Hay
  "lmix" = "#FFB6C1",   # Legume Mix
  "mint" = "#800080",   # Mint (Purple)
  "oats" = "#FF99FF",   # Oats
  "onio" = "#FF4500",   # Onion (Orange-Red)
  "ryeg" = "#A1FF99",    # Ryegrass (Light Green)
  "peas" = "#9933FF",   # Peas
  "popy" = "#990000",   # Poppy (Red)
  "pota" = "#FFD700",   # Potato (Golden)
  "sgbt" = "#B28DFF",   # Sugar Beet
  "soy"  = "#006400",   # Soybean (Dark Green)
  "sunf" = "#FFCC33",   # Sunflower (Yellow)
  "swht" = "#FF66CC",   # Summer Wheat
  "tobc" = "#FF9966",   # Tobacco
  "trit" = "#FF33A1",   # Triticum (Wheat)
  "wbar" = "#A1FF33",   # Barley Wheat
  "wira" = "#FFD700",   # Wira (a type of wheat)
  "wiry" = "#FF6347",   # Wiry (Tomato Red)
  "wpas" = "#FF99FF",   # Winter Pastures (Pink)
  "wtrc" = "#0000FF",   # Winter tricale (Blue)
  "wwht" = "#FF9933"    # Winter Wheat (Light Orange)
)

## Plot the treemap
fig7 <- ggplot(df_final, aes(area = proc, fill = crop, label = crop)) +
  geom_treemap() +
  geom_treemap_text(colour = "black",
                    place = "centre",
                    min.size = 1,
                    size = 12) +
  scale_fill_manual(values = color_schema) + ##Colors for inside box
  scale_color_manual(values = color_schema) +
  facet_wrap(~ cs_label, 3)+
  theme_bw()+
  theme(strip.background = element_blank(),
        strip.text.x = element_text(face = "bold", size = 12),
        legend.position="none",
        plot.margin = unit(c(0,5,5,37), "points"))

##==============================================================================
## 12) Save the plots
##==============================================================================

fig_path <- paste(results_folder, "figs", sep = "/")
if (!file.exists(fig_path)) dir.create(fig_path)

ggsave(paste(fig_path, "fg6_crop_areas_km2_pr.jpeg", sep = "/"), plot = fig6,
       width = 1000, height = 700, units = "px", dpi = 100)

ggsave(paste(fig_path, "fg7_crop_areas_pr.png", sep = "/"), plot = fig7,
       width = 1000, height = 700, units = "px", dpi = 100)

openxlsx::write.xlsx(table, paste(fig_path, "areas_previous.xlsx", sep = "/"),  rowNames = F)
openxlsx::write.xlsx(df_final_area, paste(fig_path, "areas_km2.xlsx", sep = "/"),  rowNames = F)
