##==============================================================================
## Load libraries and functions
##==============================================================================
## Load libraries
library(SWATtunR)
library(doParallel)
library(foreach)
library(R.utils)
library(tidyverse)
library(stringr)
library(purrr)
library(RColorBrewer)
library(tidytext)
library(sf)
library(treemapify)
library(openxlsx)
## install_github("wilkox/treemapify")
## Load custom functions
source('functions.R')

##==============================================================================
## Define your settings (user input)
##==============================================================================

## SWAT+ executable to be used. Should be in the path_to_mod folder
swat_exe <- "Rev_61_0_64rel.exe"

## Path to the folder containing the models of case studies, plants.plt base file
## and and swat executable
path_to_mod <- "Data/Models"

## Path to the folder where the results will be stored
results_folder <- "Temp"
## Names of soft-cal and base folders and how results will be named.
base_pth <- "initial"
sc_path <- "soft-cal"

## Case studies to exclude from the analysis
cs_to_exclude <- "CS8" #Multiple CS could be excluded as well "CS8|CS4".
##  To include all case studies, set to NULL

## Paths to the shapes and csvs (with case studies management info)
path_shapes <- "Data/Shapes_CVSs"
## Paths to observed yield data
path_yield_csvs<- "Data/Obs_yields"

## Settings to be changed by the user (applied to all the plots)
## Crop selection
crops_sel <- c("wwht", "wbar", "swht", "fesc", "corn", "canp", "barl", "alfa")

## Lookup to the CS info in the figure
lookup <- data.frame(cs_name = c("CS1", "CS2", "CS3", "CS4", "CS7", "CS8",
                                 "CS9", "CS10", "CS11", "CS12"),
                     cs_label = c("DE", "CH", "HU1", "PL",
                                  "BE", "LT", "IT", "NO",
                                  "HU2", "CZ"))

## Order of the case studies in the plots
order_cs <- c("DE", "CH", "HU1", "PL", "BE", "LT", "IT", "NO", "HU2", "CZ")

## NO USER INPUT BELOW THIS LINE!!!

##==============================================================================
## 1) Prepare base directory to run analysis
##==============================================================================

## Reading folders, excluding case studies (if needed)
folders <- list.dirs(path_to_mod, full.names = TRUE, recursive = FALSE)
if(!is.null(cs_to_exclude)) folders <-  folders[!grepl(cs_to_exclude, folders)]

## Delete existing results folder (if you need!!!)
if (file.exists(results_folder)) unlink(results_folder, recursive = TRUE)

## Coping base files to the results folder
dir.create(paste(results_folder, base_pth, sep = "/"), recursive = TRUE)
file.copy(folders, paste(results_folder, base_pth, sep = "/"), recursive=TRUE)

##==============================================================================
## 2) Prepare plants.plt for base (initial) run analysis
##==============================================================================

## Read plants.plt base file (one for all case studies)
plants_plt_base <- read_tbl("Data/plants.plt")

## List all directories in the base folder
f_ini <- list.dirs(paste(results_folder, base_pth, sep = "/"),
                   full.names = TRUE, recursive = FALSE)

## Prepare setups for plants.plt base run. This will create plants.plt files for
## each case study. Base plants.plt file will be merged with plants.plt.bkp0 files
## in the case study folders. Only plants missing in the base file will be added.
walk(f_ini, ~prepare_plants_base(.x))

##==============================================================================
## 3) Prepare models for soft-calibrated runs
##==============================================================================

## Create directory and copy folders
R.utils::copyDirectory(paste(results_folder, base_pth, sep = "/"),
                       paste(results_folder, sc_path, sep = "/"))

## Overwrite plants.plt files in the case study folders with the soft-calibrated
## plants.plt files, which are saved as with the extension .bkp99
f_sc <- gsub(base_pth, sc_path, f_ini)
walk(f_sc, ~file.copy(from = paste0(.x,"/plants.plt.bkp99"),
                         to = paste0(.x, "/plants.plt"), overwrite = TRUE))

##==============================================================================
## 4) Run all the models
##==============================================================================

c(f_ini, f_sc) %>% run_models(swat_exe)
## Save environment (just in case ;)
save.image("Temp/Env.RData")

##==============================================================================
## 5) Generate the figures (you can run this with each separate script)
##==============================================================================

## Results will be saved in
print(paste("Please find the results in", paste(results_folder, "figs", sep = "/")))

## a) Plot the difference between the initial and soft-calibrated models
source('_output_diff.R')

## b) Plot the parameter changes
source('_par_move.R')

## c) Plot the yield changes
source('_yield_change.R')

## d) Plot the crop area
source('_crop_area.R')


