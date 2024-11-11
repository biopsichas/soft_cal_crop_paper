## For this script to work, you need to have run the run_analysis.R script first!
## This script will compare the results of the base and soft-calibrated model runs

##==============================================================================
## 1) Collect results into one data frame
##==============================================================================

res <- bind_rows(get_results(f_ini, base_pth), get_results(f_sc, sc_path))

##==============================================================================
## 2) Add labels and arrangement of them
##==============================================================================

res <- left_join(res, lookup, by = c("cs" = "cs_name"))
order_cs <- order_cs[order_cs != lookup[!lookup$cs_name %in% unique(res$cs), "cs_label"]]
res$cs_label <- factor(res$cs_label, levels = order_cs)

##==============================================================================
## 3) Filter into two data frames: one with water balance and water quality
##==============================================================================

res_wb <- filter (res, variables %in%
                    c("cn", "ecanopy","eplant","esoil", "et", "latq", "perc",
                      "qtile", "surq_gen", "sw", "sw_300", "wateryld"))
res_wq <- filter (res, variables %in% c("N_loss", "N_loss_ratio", "P_loss",
                                        "P_loss_ratio", "Sed_loss"))

##==============================================================================
## 4) Plot results
##==============================================================================

## Water balance
fig1 <- ggplot(res_wb, aes(x = cs_label, y = values, color = status, shape = status))+
  geom_point()+
  facet_wrap(~variables, nrow = 4, scales = "free") +
  labs(title = "Water balance components",
       x = "Case study",
       y = "Values",
       color = "Status",
       shape = "Status" ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

fig1

## Water quality
fig2 <- ggplot(res_wq, aes(x = cs_label, y = values, color = status, shape = status))+
  geom_point()+
  facet_wrap(~variables, nrow = 4, scales = "free") +
  labs(title = "Water quality components",
       x = "Case study",
       y = "Values",
       color = "Status",
       shape = "Status" ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

fig2

##==============================================================================
## 5) Save the plots
##==============================================================================

fig_path <- paste(results_folder, "figs", sep = "/")
if (!file.exists(fig_path)) dir.create(fig_path)

ggsave(paste(fig_path, "fg1_water_balance.png", sep = "/"), plot = fig1,
       width = 1000, height = 700, units = "px", dpi = 100)
ggsave(paste(fig_path, "fg1_water_quality.png", sep = "/"), plot = fig2,
       width = 1000, height = 700, units = "px", dpi = 100)

