##==============================================================================
## 1) Get the crop area
##==============================================================================

nb_harvest <- read.csv(paste("Data", "number_of_harvests.csv", sep = "/"))

crop_area <- read_yields(f_ini) %>%
  select(plant_name, `harv_area(ha)`, cs_name) %>%
  filter(`harv_area(ha)` > 0) %>%
  left_join(nb_harvest, by = c("cs_name", "plant_name" = "crop"))%>%
  mutate(harvest = ifelse(is.na(harvest),  1, harvest)) %>%
  mutate(harv_area_ha = `harv_area(ha)` / harvest) %>%
  left_join(lookup, by = "cs_name") %>%
  select(plant_name, harv_area_ha, cs_label)

##==============================================================================
## 2) Check the sums of the crop area for each case study
##==============================================================================

crop_sums <- crop_area %>% group_by(cs_label) %>%
  summarise(sum_harv_area_ha = round(sum(harv_area_ha)/100,0))
crop_sums

##==============================================================================
## 3) Calculate the percentage of the crop area for each crop
##==============================================================================

crop_area_prec <- crop_area %>%
  left_join(crop_sums, by = "cs_label") %>%
  mutate(proc = harv_area_ha / sum_harv_area_ha)

##==============================================================================
## 4) Plot the treemap
##==============================================================================

## Order of the case studies in the plots
crop_area_prec$cs_label  <- factor(crop_area_prec$cs_label, levels = order_cs, ordered = TRUE)

## Prepare color schema
n <- length(unique(crop_area_prec$plant_name))
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
color_schema <-  unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))

## Plot the treemap
fig5 <- ggplot(crop_area_prec, aes(area = proc, fill = plant_name, label =  plant_name)) +
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

fig5

##==============================================================================
## 5) Save the plots
##==============================================================================

fig_path <- paste(results_folder, "figs", sep = "/")
if (!file.exists(fig_path)) dir.create(fig_path)

ggsave(paste(fig_path, "fg5_crop_areas.png", sep = "/"), plot = fig5,
       width = 1000, height = 700, units = "px", dpi = 100)

table <- crop_area_prec %>%
  select(-c(sum_harv_area_ha, proc)) %>%
  mutate(harv_area_ha = round(harv_area_ha/100, 1)) %>%
  pivot_wider(names_from = cs_label, values_from = harv_area_ha)

openxlsx::write.xlsx(table, paste(fig_path, "areas.xlsx", sep = "/"),  rowNames = F)
