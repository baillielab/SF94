# perform sensitivity analyses

# calculate the number of patients that were transferred to another hospital from subset 1
transfer<-table(subset1$dsterm)
write.csv(transfer, paste0("/home/skerr/Git/SF94/Outputs/", time_stamp, "/Sensitivity", "/transfer.csv"))

# create a subset that does not include transferred patients from the main analysis (subset 1)
subset4 <- subset(subset1, discharge_transfer == "NO" ) 

# repeat main analysis without the transferred patients

# Mortality
sample_size_mort <- rbind(
  calculate_sample_size_mort(mean(subset1$mortality_28, na.rm = TRUE), 0.85),
  calculate_sample_size_mort(mean(subset4$mortality_28, na.rm = TRUE), 0.85)
) %>%
  data.frame()

rownames(sample_size_mort) <- c("subset1", "subset4")

write.csv(sample_size_mort, paste0("/home/skerr/Git/SF94/Outputs/", time_stamp, "/Sensitivity", "/sample_size_mortality.csv"))

# Mortality - logrank

df_mort_lr_sample_size <- rbind(
  calculate_sample_size_lr(subset1, "mortality_28", 0.85, 0.05, 0.8, 0, 28),
  calculate_sample_size_lr(subset4, "mortality_28", 0.85, 0.05, 0.8, 0, 28)
) %>%
  data.frame()

rownames(df_mort_lr_sample_size) <- c("subset1", "subset4")

write.csv(df_mort_lr_sample_size, paste0("/home/skerr/Git/SF94/Outputs/", time_stamp, "/Sensitivity", "/mort_logrank_sample_size.csv"))

# Opportunistic SF94

df_sf94_mean <- rbind(
  calculate_mean_sf94(subset1, 1),
  calculate_mean_sf94(subset4, 4)
) %>%
  data.frame()

names(df_sf94_mean) <- c("day5_P", "day8_P")
rownames(df_sf94_mean) <- c("subset1", "subset4")

df_sf94_sd <- rbind(
  calculate_sd_sf94(subset1, 1),
  calculate_sd_sf94(subset4, 4)
) %>%
  data.frame()

names(df_sf94_sd) <- c("day5_P", "day8_P")
rownames(df_sf94_sd) <- c("subset1", "subset4")

df_sf94_corr <- cbind(
  calculate_correlation_sf94(subset1),
  calculate_correlation_sf94(subset4)
) %>%
  t() %>%
  data.frame()

names(df_sf94_corr) <- c("day5_P", "day8_P")
rownames(df_sf94_corr) <- c("subset1", "subset4")
