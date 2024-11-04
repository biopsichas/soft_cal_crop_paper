# R packages -------------------------------------------------------
library(tidyverse)
# library(data.table)
library(vroom)

# Project path -----------------------------------------------------
proj_path <- 'D:/SWAT/FamR/farmR_misure/simulazione'
# ------------------------------------------------------------------

# Functions --------------------------------------------------------
read_tbl <- function(tbl_name, proj_path, row_data_start, row_col_names) {
  tbl_path <- paste(proj_path, tbl_name, sep = '/')
  col_names <- vroom_lines(tbl_path, skip = row_col_names - 1, n_max = 1) %>%
    str_trim(.) %>%
    str_split(., '[:space:]+') %>%
    unlist()

  tbl <- vroom_lines(tbl_path, skip = row_data_start - 1) %>%
    str_trim(.) %>%
    str_split(., '\t[:space:]+|[:space:]+')

  is_num <- tbl[[1]] %>% as.numeric() %>% suppressWarnings() %>% map_lgl(., ~!is.na(.x)) %>%  which()

  tbl <- tbl %>%
    map(., ~ set_names(.x, col_names)) %>%
    map_df(., bind_rows) %>%
    mutate(across(all_of(is_num), ~ as.numeric(.x)))

  return(tbl)
}

# Read landuse.lum --------------------------------------------------
lum <- read_tbl('landuse.lum', proj_path, 3, 2)
lum_head <- vroom_lines(paste(proj_path, 'landuse.lum', sep = '/'), n_max = 1) %>%
  paste0(., ', edited manually on ', Sys.time())

# Define pointers in landuse.lum ------------------------------------

## cn2
lum$cn2[which(substr(lum$name,1,5)=='field')] <- 'rc_strow_g'
lum$cn2[which(substr(lum$name,1,4)=='frst')] <- 'wood_g'
lum$cn2[which(substr(lum$name,1,4)=='orcd')] <- 'wood_f'
lum$cn2[which(substr(lum$name,1,4)=='rnge')] <- 'pastg_g'
lum$cn2[which(substr(lum$name,1,4)=='wetl')] <- 'null'
lum$cn2[which(substr(lum$name,1,4)=='urld')] <- 'urban_uc'
lum$cn2[which(substr(lum$name,1,4)=='urml')] <- 'urban'
lum$cn2[which(substr(lum$name,1,4)=='ucom')] <- 'urban'
lum$cn2[which(substr(lum$name,1,4)=='past')] <- 'pastg_g'


## cons_prac
lum$cons_prac[which(substr(lum$name,1,5)=='field')] <- 'up_down_slope'
lum$cons_prac[which(substr(lum$name,1,4)=='frst')] <- 'greening'
lum$cons_prac[which(substr(lum$name,1,4)=='orcd')] <- 'greening'
lum$cons_prac[which(substr(lum$name,1,4)=='rnge')] <- 'greening'
lum$cons_prac[which(substr(lum$name,1,4)=='wetl')] <- 'greening'
lum$cons_prac[which(substr(lum$name,1,4)=='urld')] <- 'null'
lum$cons_prac[which(substr(lum$name,1,4)=='urml')] <- 'greening'
lum$cons_prac[which(substr(lum$name,1,4)=='ucom')] <- 'up_down_slope'
lum$cons_prac[which(substr(lum$name,1,4)=='past')] <- 'greening'

## ov_mann
lum$ov_mann[which(substr(lum$name,1,5)=='field')] <- 'convtill_res'
lum$ov_mann[which(substr(lum$name,1,4)=='frst')] <- 'forest_med'
lum$ov_mann[which(substr(lum$name,1,4)=='orcd')] <- 'forest_light'
lum$ov_mann[which(substr(lum$name,1,4)=='rnge')] <- 'densegrass'
lum$ov_mann[which(substr(lum$name,1,4)=='wetl')] <- 'null'
lum$ov_mann[which(substr(lum$name,1,4)=='urld')] <- 'urban_asphalt'
lum$ov_mann[which(substr(lum$name,1,4)=='urml')] <- 'shortgrass'
lum$ov_mann[which(substr(lum$name,1,4)=='ucom')] <- 'shortgrass'
lum$ov_mann[which(substr(lum$name,1,4)=='past')] <- 'densegrass'



## urban
lum$urban[which(substr(lum$name,1,4)=='urld')] <- 'urld'
lum$urban[which(substr(lum$name,1,4)=='urml')] <- 'urml'
lum$urban[which(substr(lum$name,1,4)=='ucom')] <- 'ucom'


# Write new landuse.lum ---------------------------------------------
fmt_nam <- c('%-28s', '%-9s', rep('%17s', 12))
fmt_val <- c('%-33s', '%-4s', rep('%17s', 12))

lum_names <- colnames(lum) %>%
  map2_chr(., fmt_nam, ~sprintf(.y, .x)) %>%
  paste(., collapse = ' ')

lum_lines <- lum %>%
  map2_df(., fmt_val, ~sprintf(.y, .x)) %>%
  apply(., 1, paste, collapse = ' ')

lum_lines <- c(lum_head, lum_names, lum_lines)

write_lines(lum_lines, paste(proj_path, 'landuse2.lum', sep = '/'))
