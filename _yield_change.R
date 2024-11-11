##==============================================================================
## 1) Read the observed yield data for all case studies
##==============================================================================

##Read csv files
df <- NULL
for(f in basename(folders)){
  yield <- read.csv(paste(path_yield_csvs, paste0(f, ".csv"), sep = "/"), sep = ";") %>%
    mutate_at(vars(starts_with("yield")), ~suppressWarnings(as.numeric(.)))
  yield$cs_name <- f
  if(is.null(df)) df <- yield else  df <- bind_rows(df, yield)
}

##==============================================================================
## 2) Read model yield results and calculate difference from observations
##==============================================================================

yld <- bind_rows(read_yields(f_ini), read_yields(f_sc)) %>%
  left_join(df[c("plant_name", "yield_mean", "cs_name")], by = c("cs_name", "plant_name")) %>%
  filter(yield_mod > 0 & !is.na(yield_mean)) %>%
  unique

##==============================================================================
## 3) Correct yields for number of harvests
##==============================================================================

nb_harvest <- read.csv(paste("Data", "number_of_harvests.csv", sep = "/"))

yld_corrected <-select(yld, plant_name, cs_name, type, yield_mod, yield_mean) %>%
  filter(plant_name %in% crops_sel) %>%
  left_join(nb_harvest, by = c("cs_name", "plant_name" = "crop")) %>%
  mutate(yield_mod_adjusted = ifelse(harvest > 1,  yield_mod * harvest, yield_mod)) %>%
  mutate(proc = round(100* (yield_mod_adjusted/yield_mean - 1), 2)) %>%
  left_join(lookup, by = "cs_name")

##==============================================================================
## 4) Plot the results
##==============================================================================

## Factors to adjust the order of the case studies codes
yld_corrected$cs_label  <- factor(yld_corrected$cs_label, levels = rev(order_cs), ordered = TRUE)
yld_corrected$plant_name <- factor(yld_corrected$plant_name, levels = crops_sel, ordered = TRUE)

fig4 <- ggplot(yld_corrected, aes(x = cs_label, y = proc, color = type))+
  geom_point()+
  geom_hline(yintercept=0, linetype="dashed", color = "red")+
  facet_wrap(~plant_name, nrow = 2) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  coord_flip()+
  labs(x = "Case studies", y = "Difference in % from observed mean yield", color = "Setup state")

fig4

##==============================================================================
## 5) Save the plots
##==============================================================================

fig_path <- paste(results_folder, "figs", sep = "/")
if (!file.exists(fig_path)) dir.create(fig_path)

ggsave(paste(fig_path, "fg4_yield_change.png", sep = "/"), plot = fig4,
       width = 1000, height = 700, units = "px", dpi = 100)


