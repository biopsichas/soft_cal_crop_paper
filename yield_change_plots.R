library(stringr)
library(tidyverse)
library(SWATtunR)
library(doParallel)
library(foreach)
library(R.utils)
library(purrr)
source('functions.R')
## Paths to the shapes and csvs
path_yield_csvs<- "Data/Obs_yields"
yield_csvs <- list.files(path_yield_csvs, full.names = TRUE, recursive = FALSE)
##Read all the shapefiles and extract the data
df <- NULL
for(f in yield_csvs){
  yield <- read.csv(f, sep = ";") %>%
    mutate_at(vars(starts_with("yield")), as.numeric)
  yield$cs_name <- str_replace(basename(f), ".csv", "")
  if(is.null(df)) df <- yield else  df <- bind_rows(df, yield)
}

mod_yld <- NULL
folders <- list.dirs("Data/Models", full.names = TRUE, recursive = FALSE)
folders <- folders[!folders %in% c("Data/Models/CS8")]
for(f in folders){
  crop <- SWATtunR:::read_tbl(paste0(f, "/basin_crop_yld_aa.txt")) %>%
    select(plant_name,`yld(t/ha)`) %>%
    rename(yield_mod= `yld(t/ha)`)
  crop$cs_name <- basename(f)
  if(is.null(mod_yld)) mod_yld <- crop else mod_yld <- bind_rows(mod_yld, crop)
}

y <- mod_yld %>% left_join(df[c("plant_name", "yield_mean", "cs_name")], by = c("cs_name", "plant_name")) %>%
  filter(yield_mod>0 & !is.na(yield_mean)) %>%
  mutate(proc = round(100* (yield_mod/yield_mean - 1), 2)) %>%
  unique

y %>% unique %>%
  select(plant_name, cs_name) %>%
  group_by(cs_name) %>%
  count

swat_exe <- "Rev_61_0_64rel.exe"
plants_plt_base <- read_tbl(paste0("Data/Models/plants.plt"))

## 1) Prepare setups for plants.plt base run
walk("Models/CS8", ~prepare_plants_base(.x))
## 2) Run models for the base plants.plt
run_models(folders, swat_exe)

## 1) Prepare setups for plants.plt base run
walk(folders, ~prepare_plants_base(.x))
## 2) Run models for the base plants.plt
run_models(folders, swat_exe)

mod_yld2 <- NULL
for(f in folders){
  crop <- SWATtunR:::read_tbl(paste0(f, "/basin_crop_yld_aa.txt")) %>%
    select(plant_name,`yld(t/ha)`) %>%
    rename(yield_mod= `yld(t/ha)`)
  print(dim(crop))
  crop$cs_name <- basename(f)
  if(is.null(mod_yld2)) mod_yld2 <- crop else mod_yld2 <- bind_rows(mod_yld2, crop)
}

y2 <- mod_yld2 %>% left_join(df[c("plant_name", "yield_mean", "cs_name")], by = c("cs_name", "plant_name")) %>%
  filter(yield_mod>0 & !is.na(yield_mean))


crops_sel <- c("wwht", "wbar", "swht", "fesc", "corn", "canp", "barl", "alfa")
xxx <- bind_rows(y %>% mutate(type = "Soft-cal"), y2 %>% mutate(type = "Initial")) %>%
  select(plant_name, cs_name, proc, type, yield_mod, yield_mean) %>%
  filter(plant_name %in% crops_sel) %>%
  left_join(x, by = c("cs_name", "plant_name" = "crop")) %>%
  mutate(yield_mod_adjusted = ifelse(harvest > 1,  yield_mod * harvest, yield_mod)) %>%
  filter(!plant_name %in% c("radi", "ryeg", "shrb", "must")) %>%
  mutate(proc = round(100* (yield_mod_adjusted/yield_mean - 1), 2))

xxx$cs_name <- factor(xxx$cs_name, levels = c("CS12", "CS11", "CS10", "CS9", "CS7", "CS4", "CS3", "CS2", "CS1"), ordered = TRUE)


ggplot(xxx, aes(x = cs_name, y = proc, color = type))+
  geom_point()+
  geom_hline(yintercept=0, linetype="dashed", color = "red")+
  facet_wrap(~plant_name, nrow = 2) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  coord_flip()+
  labs(x = "Case studies", y = "Difference in % from observed mean yield", color = "Setup state")


df_yld_aa <- NULL

for(cs in c("CS1", "CS2", "CS3", "CS4", "CS7", "CS8",
            "CS9", "CS10", "CS11", "CS12")){
  crop <- SWATtunR::read_tbl(paste0("Models/", cs, "/basin_crop_yld_yr.txt"))
  crop <- crop[c("plant_name", "yld(t/ha)")] %>%
    group_by(plant_name) %>%
    summarise_all(mean)
  crop$cs_name <- cs

  if(is.null(df_yld_aa)) df_yld_aa <- crop else df_yld_aa <- bind_rows(df_yld_aa, crop)
}

mydf <- readRDS("test/back_up_data/mydf_final.rds")
x <- mydf %>%
  select(-mgt, -rot_year) %>%
  group_by(cs_name, crop) %>%
  summarise_all(max)
df_yld_aa_adjusted <- df_yld_aa %>%
  left_join(x, by = c("cs_name", "plant_name" = "crop")) %>%
  mutate(harvest = ifelse (is.na(harvest), 1, harvest)) %>%
  mutate(harvest_adjusted = ifelse(harvest > 1,  `yld(t/ha)` * harvest,  `yld(t/ha)`)) %>%
  filter(!plant_name %in% c("radi", "ryeg", "shrb", "must"))
library(openxlsx)
openxlsx::write.xlsx(df_yld_aa_adjusted, "areas.xlsx",  rowNames = F)


select(df_yld_aa_adjusted, cs_name, harvest_adjusted) %>%
  group_by(cs_name) %>%
  summarize_all(sum) %>%
  mutate(area_km2 = harvest_adjusted/100)



