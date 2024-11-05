library(SWATtunR)
library(doParallel)
library(foreach)
library(R.utils)
library(tidyverse)
library(stringr)
library(purrr)
source('functions.R')

##Settings
swat_exe <- "Rev_61_0_64rel.exe"
folders <- list.dirs("Data/Models", full.names = TRUE, recursive = FALSE)
folders <- folders[!folders %in% c("Data/Models/CS8")]
plants_plt_base <- read_tbl(paste0("Data/Models/plants.plt"))


## 1) Prepare setups for plants.plt base run
walk(folders, ~prepare_plants_base(.x))

## 2) Run models for the base plants.plt
run_models(folders, swat_exe)

## 3) Extract results
res <- get_results(folders, "initial")

## 4) Update models for soft-cal runs
walk(folders, ~file.copy(from = paste0(.x,"/plants.plt.bkp99"), to = paste0(.x, "/plants.plt"), overwrite = TRUE))

## 5) Run models for the soft-cal plants.plt
run_models(folders, swat_exe)

## 6) Extract results and combine with base run results
res_final <- bind_rows(res, get_results(folders, "soft-cal"))

## 7) Plot the results
res_final$cs <- factor(res_final$cs, levels = c("CS1", "CS2", "CS3", "CS4", "CS7", #"CS8",
                                                     "CS9", "CS10", "CS11", "CS12"), ordered = TRUE)

res_wb <- res_final %>% filter (variables %in% c("cn", "ecanopy","eplant","esoil", "et", "latq", "perc", "qtile", "surq_gen", "sw", "sw_300", "wateryld"))
res_wq <- res_final %>% filter (variables %in% c("N_loss", "N_loss_ratio", "P_loss", "P_loss_ratio", "Sed_loss"))

ggplot(res_wb, aes(x = cs, y = values, color = status, shape = status))+
  geom_point()+
  facet_wrap(~variables, nrow = 4, scales = "free") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(res_wq, aes(x = cs, y = values, color = status, shape = status))+
  geom_point()+
  facet_wrap(~variables, nrow = 4, scales = "free") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
