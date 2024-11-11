## Run parallelization
run_models <- function(paths, swat_exe){

  #' Function to copy exe between destinations and run it
  #'
  #' @param path_from character path of directory from which file should be taken
  #' @param path_to character path to which file should be put
  #' @param file_name character names of file to copy and run

  exe_copy_run <- function(path_from, path_to, file_name){
    # Copy into the destination directory
    file.copy(from = paste0(path_from, "/", file_name),
              to = paste0(path_to, "/", file_name), overwrite = TRUE)

    ##Reset working directory to setup location
    wd_base <- getwd()
    if (str_sub(getwd(), -nchar(path_to), -1) != path_to) setwd(path_to)

    ##Write files
    system(file_name)

    ##Reset back working directory
    setwd(wd_base)
  }

  cores <- ifelse(detectCores() > length(paths)+1, length(paths), detectCores())
  cl <- makeCluster(cores,  outfile="")
  registerDoParallel(cl)
  ## Run model runs
  foreach(f = paths, .packages = c("stringr")) %dopar% {exe_copy_run("Data", f, swat_exe)}
  ##Clean clusters after
  stopCluster(cl)
  return(cat("Done!"))
}

## Assemble results
get_results <- function(paths, run_name, basin = TRUE){
  wb <- map(paths, ~hru_aa_wb(.x, basin)) %>%
    enframe(value = "data") %>%
    unnest(cols = c(data)) %>%
    select(-name) %>%
    pivot_longer(-scen_name, names_to = "variables", values_to = "values")

  nb <- map(paths, ~hru_aa_nb(.x, basin)) %>%
    enframe(value = "data") %>%
    unnest(cols = c(data)) %>%
    select(-name) %>%
    pivot_longer(-scen_name, names_to = "variables", values_to = "values")

  res <- bind_rows(wb, nb) %>%
    mutate(status = run_name) %>%
    rename(cs = scen_name)

  return(res)
}

## Preparation of plants.plt base run
prepare_plants_base <- function(path){
  plants_plt_fmt <- c('%-12s', '%-18s', '%-12s', rep('%12.5f', ncol(plants_plt_base) - 4), '%-40s')
  if (!file.exists(paste0(path,"/plants.plt.bkp99"))) copy_file_version(path, '/plants.plt', file_version = 99)
  if (file.exists(paste0(path,"/plants.plt.bkp0"))) {
    plants_plt_base_org <- read_tbl(paste0(path,"/plants.plt.bkp0"))
    colnames(plants_plt_base_org) <- tolower(names(plants_plt_base_org)) ##In case somebody has Upper cases
    missing_plants <- plants_plt_base_org[!plants_plt_base_org$name %in% plants_plt_base$name,]
    plants_plt_base_add <- bind_rows(plants_plt_base, select(missing_plants, any_of(names(plants_plt_base))))
  } else {
    plants_plt_base_add <- plants_plt_base
  }
  if (file.exists(paste0(path,"/plants.plt"))) file.remove(paste0(path,"/plants.plt"))
  write_tbl(plants_plt_base_add, paste0(path, '/plants.plt'), fmt = plants_plt_fmt)
  ## Updating file.cio file
  file_cio <- readLines(paste0(path, "/file.cio"))
  if(!grepl("calibration.cal", file_cio[22], fixed = TRUE)){
    copy_file_version(path, '/file.cio', file_version = 99)
    file_cio[22] <- "chg               cal_parms.cal     null              null              null              null              null              null              null              null              "
    writeLines(file_cio, paste0(path, "/file.cio"))
  }
  if (!file.exists(paste0(path,"/print.plt.bkp99"))) copy_file_version(path, '/print.prt', file_version = 99)
  unlink(paste0(path, "Models/print.prt"))
  file.copy(from = "Models/print.prt", to = paste0(path, "/print.prt"), overwrite = TRUE)
  #
  del_f <- list.files(path = path, pattern = ".*.exe|.*.measr|.*.txt|.*.zip|.*success.fin|.*co2.out|.*simulation.out|.*.bak|.*.mgts|.*.farm|.*area_calc.out|.*checker.out|.*sqlite|.*diagnostics.out|.*erosion.out|.*files_out.out", full.names = TRUE)
  unlink(del_f)

  hru_agr <- read_tbl(paste0(path,'/hru-data.hru')) %>%
    select(id, lu_mgt) %>%
    mutate(n = grepl("[0-9]", lu_mgt)) %>%
    filter(n) %>%
    select(id) %>%
    rename(hru_id = id)
  write.table(hru_agr, file = paste0(path, '/hru_agr.txt'), sep = "\t", row.names = FALSE, quote = FALSE)
  return(cat("Done!"))
}


# water balance related indicators based on annual average hru output
hru_aa_wb <- function(path, basin = TRUE){
  df_out <- data.frame(scen_name=sapply(strsplit(path, split ="/"),tail,1),
                       sw=NA,
                       perc=NA,
                       precip=NA,
                       snofall=NA,
                       snomlt=NA,
                       surq_gen=NA,
                       latq=NA,
                       wateryld=NA,
                       et=NA,
                       ecanopy=NA,
                       eplant=NA,
                       esoil=NA,
                       surq_cont=NA,
                       cn=NA,
                       sw_300=NA,
                       pet=NA,
                       qtile=NA,
                       surq_cha=NA,
                       latq_cha=NA)

  # Read file
  hru_wb <- read_tbl(paste0(path,'/hru_wb_aa.txt'))
  hru_area <- read_tbl(paste0(path,'/hru.con'))

  # Specify the columns you want to keep
  # Keep all hru_ls columns

  columns_to_keep <- c("jday", "mon", "day", "yr", "unit", "gis_id", "name",
                       "sw_ave", "perc", "precip", "snofall", "snomlt", "surq_gen",
                       "latq", "wateryld", "et", "ecanopy", "eplant", "esoil",
                       "surq_cont", "cn", "sw_300",
                       "pet", "qtile", "surq_cha", "latq_cha")

  # Create a new data frame with only the selected columns
    # Read in vector for agricultural area
  hru_agr <- read.table(paste0(path,'/hru_agr.txt'), h=T)
  idx <- hru_agr$hru_id
  df_selected_hru_wb <- hru_wb[idx, columns_to_keep]

  # Add the HRU area to each HRU
  hru_wb <- hru_wb[idx,] %>% left_join(hru_area[idx,] %>% select(id, area), by = c("unit" = "id"))

  # Calculate the weighted values
  df_selected_hru_wb  <- df_selected_hru_wb  %>%
    mutate(weighted_sw = sw_ave * hru_wb$area,
           weighted_perc = perc * hru_wb$area,
           weighted_precip = precip * hru_wb$area,
           weighted_snofall = snofall * hru_wb$area,
           weighted_snomlt = snomlt * hru_wb$area,
           weighted_surqgen = surq_gen * hru_wb$area,
           weighted_latq = latq * hru_wb$area,
           weighted_wateryld = wateryld * hru_wb$area,
           weighted_perc = perc * hru_wb$area,
           weighted_et = et * hru_wb$area,
           weighted_ecanopy = ecanopy * hru_wb$area,
           weighted_eplant = eplant * hru_wb$area,
           weighted_esoil = esoil * hru_wb$area,
           weighted_surqcont = surq_cont * hru_wb$area,
           weighted_cn = cn * hru_wb$area,
           weighted_sw300 = sw_300 * hru_wb$area,
           weighted_pet = pet * hru_wb$area,
           weighted_qtile = qtile * hru_wb$area,
           weighted_surqcha = surq_cha * hru_wb$area,
           weighted_latqcha = latq_cha * hru_wb$area,)

  if(basin){

    # Calculate the total weighted sum
    total_weighted_sw <- sum(df_selected_hru_wb$weighted_sw, na.rm = TRUE)
    total_weighted_perc <- sum(df_selected_hru_wb$weighted_perc, na.rm = TRUE)
    total_weighted_precip <- sum(df_selected_hru_wb$weighted_precip, na.rm = TRUE)
    total_weighted_snofall <- sum(df_selected_hru_wb$weighted_snofall, na.rm = TRUE)
    total_weighted_snomlt <- sum(df_selected_hru_wb$weighted_snomlt, na.rm = TRUE)
    total_weighted_surqgen <- sum(df_selected_hru_wb$weighted_surqgen, na.rm = TRUE)
    total_weighted_latq <- sum(df_selected_hru_wb$weighted_latq, na.rm = TRUE)
    total_weighted_wateryld <- sum(df_selected_hru_wb$weighted_wateryld, na.rm = TRUE)
    total_weighted_et <- sum(df_selected_hru_wb$weighted_et, na.rm = TRUE)
    total_weighted_ecanopy <- sum(df_selected_hru_wb$weighted_ecanopy, na.rm = TRUE)
    total_weighted_eplant <- sum(df_selected_hru_wb$weighted_eplant, na.rm = TRUE)
    total_weighted_esoil <- sum(df_selected_hru_wb$weighted_esoil, na.rm = TRUE)
    total_weighted_surqcont <- sum(df_selected_hru_wb$weighted_surqcont, na.rm = TRUE)
    total_weighted_cn <- sum(df_selected_hru_wb$weighted_cn, na.rm = TRUE)
    total_weighted_sw300 <- sum(df_selected_hru_wb$weighted_sw300, na.rm = TRUE)
    total_weighted_pet <- sum(df_selected_hru_wb$weighted_pet, na.rm = TRUE)
    total_weighted_qtile <- sum(df_selected_hru_wb$weighted_qtile, na.rm = TRUE)
    total_weighted_surqcha <- sum(df_selected_hru_wb$weighted_surqcha, na.rm = TRUE)
    total_weighted_latqcha <- sum(df_selected_hru_wb$weighted_latqcha, na.rm = TRUE)

    # Calculate the total area across all HRUs
    total_area <- sum(hru_wb$area, na.rm = TRUE)

    # Calculate the area-weighted averages
    df_out[1,2] <- round(total_weighted_sw / total_area,3)
    df_out[1,3] <- round(total_weighted_perc / total_area,3)
    df_out[1,4] <- round(total_weighted_precip / total_area,3)
    df_out[1,5] <- round(total_weighted_snofall / total_area,3)
    df_out[1,6] <- round(total_weighted_snomlt / total_area,3)
    df_out[1,7] <- round(total_weighted_surqgen / total_area,3)
    df_out[1,8] <- round(total_weighted_latq / total_area,3)
    df_out[1,9] <- round(total_weighted_wateryld / total_area,3)
    df_out[1,10] <- round(total_weighted_et / total_area,3)
    df_out[1,11] <- round(total_weighted_ecanopy / total_area,3)
    df_out[1,12] <- round(total_weighted_eplant / total_area,3)
    df_out[1,13] <- round(total_weighted_esoil / total_area,3)
    df_out[1,14] <- round(total_weighted_surqcont / total_area,3)
    df_out[1,15] <- round(total_weighted_cn / total_area,3)
    df_out[1,16] <- round(total_weighted_sw300 / total_area,3)
    df_out[1,17] <- round(total_weighted_pet / total_area,3)
    df_out[1,18] <- round(total_weighted_qtile / total_area,3)
    df_out[1,19] <- round(total_weighted_surqcha / total_area,3)
    df_out[1,20] <- round(total_weighted_latqcha / total_area,3)
    return(df_out)
  } else {
    return(df_selected_hru_wb)
  }

}

# nutrient and sediment related indicators based on annual average hru output
hru_aa_nb <- function(path, basin = TRUE){
  df_out <- data.frame(scen_name=sapply(strsplit(path, split ="/"),tail,1),
                       N_loss=NA,
                       P_loss=NA,
                       Sed_loss=NA,
                       N_loss_ratio=NA,
                       P_loss_ratio=NA)

  # Read file
  hru_ls <- read_tbl(paste0(path,'/hru_ls_aa.txt'))
  hru_nb <- read_tbl(paste0(path,'/hru_nb_aa.txt'))
  hru_pw<- read_tbl(paste0(path,'/hru_pw_aa.txt'))
  hru_area <- read_tbl(paste0(path,'/hru.con'))

  # Specify the columns you want to keep
  # Keep all hru_ls columns

  columns_to_keep <- c("jday", "mon", "day", "yr", "unit", "gis_id", "name",
                       "fertn", "fixn", "no3atmo", "nh4atmo", "fertp", "denit")

  # Read in vector for agricultural area
  hru_agr <- read.table(paste0(path,'/hru_agr.txt'), h=T)
  idx <- hru_agr$hru_id
  df_selected_hru_nb <- hru_nb[idx, columns_to_keep]

  # Calculate the sum of N inputs
  df_selected_hru_nb <- df_selected_hru_nb %>%
    mutate(N_inputs = fertn + fixn + no3atmo + nh4atmo)

  # Calculate N losses
  # Add N_losses as a new column to df_selected_hru_nb

  hru_ls <- hru_ls[idx,] %>%
    mutate(N_losses = sedorgn + surqno3 + lat3no3 + tileno3 + hru_pw$percn[idx])

  # The P input is only fertp from hru_nb
  # Calculate the sum of P losses
  hru_ls <- hru_ls %>%
    mutate(P_losses = sedorgp + surqsolp + sedminp + tilelabp + lchlabp)

  # Add the HRU area to each HRU
  hru_ls <- hru_ls %>% left_join(hru_area[idx,] %>% select(id, area), by = c("unit" = "id"))

  # Calculate the weighted values for N and P inputs
  df_selected_hru_nb  <- df_selected_hru_nb  %>%
    mutate(weighted_N_inputs = N_inputs * hru_ls$area,
           weighted_P_inputs = fertp * hru_ls$area)
  # Calculate the weighted values for N, P and sediment losses
  hru_ls <- hru_ls %>%
    mutate(weighted_N_losses = N_losses * area,
           weighted_P_losses = P_losses * area,
           weighted_Sed_losses = sedyld * area)
  if(basin){
    # Calculate the total weighted sum for N and P
    total_weighted_N_inputs <- sum(df_selected_hru_nb$weighted_N_inputs, na.rm = TRUE)
    total_weighted_N_losses <- sum(hru_ls$weighted_N_losses, na.rm = TRUE)
    total_weighted_P_inputs <- sum(df_selected_hru_nb$weighted_P_inputs, na.rm = TRUE)
    total_weighted_P_losses <- sum(hru_ls$weighted_P_losses, na.rm = TRUE)
    total_weighted_Sed_losses <- sum(hru_ls$weighted_Sed_losses, na.rm = TRUE)

    # Calculate the total area across all HRUs
    total_area <- sum(hru_ls$area, na.rm = TRUE)

    # Calculate the area-weighted averages for N and P
    area_weighted_average_N_inputs <- total_weighted_N_inputs / total_area
    area_weighted_average_P_inputs <- total_weighted_P_inputs / total_area
    area_weighted_average_N_losses <- total_weighted_N_losses / total_area
    area_weighted_average_P_losses <- total_weighted_P_losses / total_area
    area_weighted_average_Sed_losses <- total_weighted_Sed_losses / total_area

    df_out[1,2] <- round(area_weighted_average_N_losses,3)
    df_out[1,3] <- round(area_weighted_average_P_losses,3)
    df_out[1,4] <- round(area_weighted_average_Sed_losses,3)
    df_out[1,5] <- round(area_weighted_average_N_losses/area_weighted_average_N_inputs,3)
    df_out[1,6] <- round(area_weighted_average_P_losses/area_weighted_average_P_inputs,3)
    return(df_out)
  } else{
    return(hru_ls)
  }
}

## Read yield results from model setups
read_yields <- function(folders){
  yld <- NULL
  for(f in folders){
    crop <- SWATtunR:::read_tbl(paste0(f, "/basin_crop_yld_aa.txt")) %>%
      select(plant_name,`harv_area(ha)`, `yld(t/ha)`) %>%
      rename(yield_mod = `yld(t/ha)`)
    crop$cs_name <- basename(f)
    crop$type <- str_extract(f, paste0('(?<=', results_folder, '/)[^/]+'))
    if(is.null(yld)) yld <- crop else yld <- bind_rows(yld, crop)
  }
  return(yld)
}
