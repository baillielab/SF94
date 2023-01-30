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

df_sf94_effect_size <-
  rbind(
    c(effect_size_boot_sf94(subset1, 1:nrow(subset1), 5, 0.85), effect_size_boot_sf94(subset1, 1:nrow(subset1), 8, 0.85)),
    c(effect_size_boot_sf94(subset4, 1:nrow(subset4), 5, 0.85), effect_size_boot_sf94(subset4, 1:nrow(subset4), 8, 0.85))
  ) %>%
  data.frame()

names(df_sf94_effect_size) <- c("day5_P", "day8_P")
rownames(df_sf94_effect_size) <- c("subset1", "subset4")

df_sf94_sample_size <- rbind(
  # subset1
  c(
    calculate_sample_size_sf94(
      0.05, 0.8, df_sf94_effect_size["subset1", "day5_P"],
      df_sf94_sd["subset1", "day5_P"],
      df_sf94_corr["subset1", "day5_P"]
    ),
    calculate_sample_size_sf94(
      0.05, 0.8, df_sf94_effect_size["subset1", "day8_P"],
      df_sf94_sd["subset1", "day8_P"],
      df_sf94_corr["subset1", "day8_P"]
    )
  ),
  # subset4
  c(
    calculate_sample_size_sf94(
      0.05, 0.8, df_sf94_effect_size["subset4", "day5_P"],
      df_sf94_sd["subset4", "day5_P"],
      df_sf94_corr["subset4", "day5_P"]
    ),
    calculate_sample_size_sf94(
      0.05, 0.8, df_sf94_effect_size["subset4", "day8_P"],
      df_sf94_sd["subset4", "day8_P"],
      df_sf94_corr["subset4", "day8_P"]
    )
  )) %>%
  data.frame()

names(df_sf94_sample_size) <- c("day5_P", "day8_P")
rownames(df_sf94_sample_size) <- c("subset1", "subset4")

# Write out
write.csv(df_sf94_mean, paste0("/home/skerr/Git/SF94/Outputs/", time_stamp, "/Sensitivity", "/sf94_means.csv"))
write.csv(df_sf94_sd, paste0("/home/skerr/Git/SF94/Outputs/", time_stamp, "/Sensitivity", "/sf94_sd.csv"))
write.csv(df_sf94_corr, paste0("/home/skerr/Git/SF94/Outputs/", time_stamp, "/Sensitivity", "/sf94_correlation.csv"))
write.csv(df_sf94_effect_size, paste0("/home/skerr/Git/SF94/Outputs/", time_stamp, "/Sensitivity", "/sf94_effect_size.csv"))
write.csv(df_sf94_sample_size, paste0("/home/skerr/Git/SF94/Outputs/", time_stamp, "/Sensitivity", "/sf94_sample_size.csv"))

# WHO
df_who_effect_size <-
  rbind(
    c(effect_size_boot_who(subset1, 1:nrow(subset1), 5, 0.85), effect_size_boot_who(subset1, 1:nrow(subset1), 8, 0.85)),
    c(effect_size_boot_who(subset4, 1:nrow(subset4), 5, 0.85), effect_size_boot_who(subset4, 1:nrow(subset4), 8, 0.85))
  ) %>%
  data.frame()

names(df_who_effect_size) <- c("day5_P", "day8_P")
rownames(df_who_effect_size) <- c("subset1", "subset4")

who_table <- rbind(create_who_table(subset1), create_who_table(subset4)) %>%
  data.frame()

names(who_table) <- 4:10
rownames(who_table) <- c(
  "subset1_day5_P", "subset1_day8_P",
  "subset4_day5_P", "subset4_day8_P"
)

who_prop_table <- rbind(
  create_who_prop_table(subset1),
  create_who_prop_table(subset4)
) %>%
  data.frame()

names(who_prop_table) <- 4:10
rownames(who_prop_table) <- c(
  "subset1_day5_P", "subset1_day8_P",
  "subset4_day5_P", "subset4_day8_P"
)

df_who_sample_size <- rbind(
  # subset1
  c(
    calculate_sample_size_who(0.05, 0.8, df_who_effect_size["subset1", "day5_P"], who_prop_table["subset1_day5_P", ]),
    calculate_sample_size_who(0.05, 0.8, df_who_effect_size["subset1", "day8_P"], who_prop_table["subset1_day8_P", ])
  ),
  
  # subset4
  c(
    calculate_sample_size_who(0.05, 0.8, df_who_effect_size["subset4", "day5_P"], who_prop_table["subset4_day5_P", ]),
    calculate_sample_size_who(0.05, 0.8, df_who_effect_size["subset4", "day8_P"], who_prop_table["subset4_day8_P", ])
  )
) %>%
  data.frame()

names(df_who_sample_size) <- c("day5_P", "day8_P")
rownames(df_who_sample_size) <- c("subset1", "subset4")

# Write out
write.csv(who_table, paste0("/home/skerr/Git/SF94/Outputs/", time_stamp, "/Sensitivity", "/who_table_output.csv"))
write.csv(who_prop_table, paste0("/home/skerr/Git/SF94/Outputs/", time_stamp, "/Sensitivity", "/who_prop_output.csv"))
write.csv(df_who_effect_size, paste0("/home/skerr/Git/SF94/Outputs/", time_stamp, "/Sensitivity", "/who_effect_size.csv"))
write.csv(df_who_sample_size, paste0("/home/skerr/Git/SF94/Outputs/", time_stamp, "/Sensitivity", "/who_sample_size.csv"))

# 1/2 level sustained improvement

# Dataframe of effect sizes for different days and subsets
df_susimp_effect_size <-
  rbind(
    c(effect_size_boot_susimp(subset1, 1:nrow(subset1), 1, 0.85), effect_size_boot_susimp(subset1, 1:nrow(subset1), 2, 0.85)),
    c(effect_size_boot_susimp(subset4, 1:nrow(subset4), 1, 0.85), effect_size_boot_susimp(subset4, 1:nrow(subset4), 2, 0.85))
  ) %>%
  data.frame()

names(df_susimp_effect_size) <- c("sustained_1L_improvement", "sustained_2L_improvement")
rownames(df_susimp_effect_size) <- c("subset1", "subset4")

sus_imp_table <- rbind(
  create_sus_imp_table(subset1),
  create_sus_imp_table(subset4)
) %>%
  data.frame()

names(sus_imp_table) <- c("sustained_1L_improvement", "sustained_2L_improvement")
rownames(sus_imp_table) <- c(
  "subset1_FALSE", "subset1_TRUE",
  "subset2_FALSE", "subset2_TRUE"
)

susimp_prop_table <- rbind(
  create_susimp_prop_table(subset1),
  create_susimp_prop_table(subset4)
) %>%
  data.frame()

names(susimp_prop_table) <- c("sustained_1L_improvement", "sustained_2L_improvement")
rownames(susimp_prop_table) <- c("subset1", "subset4")

df_susimp_sample_size <- rbind(
  # subset1
  c(
    calculate_sample_size_susimp(
      0.8, df_susimp_effect_size["subset1", "sustained_1L_improvement"],
      susimp_prop_table["subset1", "sustained_1L_improvement"]
    ),
    calculate_sample_size_susimp(
      0.8, df_susimp_effect_size["subset1", "sustained_2L_improvement"],
      susimp_prop_table["subset1", "sustained_2L_improvement"]
    )
  ),
  
  # subset2
  c(
    calculate_sample_size_susimp(
      0.8, df_susimp_effect_size["subset4", "sustained_1L_improvement"],
      susimp_prop_table["subset4", "sustained_1L_improvement"]
    ),
    calculate_sample_size_susimp(
      0.8, df_susimp_effect_size["subset4", "sustained_2L_improvement"],
      susimp_prop_table["subset4", "sustained_2L_improvement"]
    )
  )
) %>%
  data.frame()

names(df_susimp_sample_size) <- c("sustained_1L_improvement", "sustained_2L_improvement")
rownames(df_susimp_sample_size) <- c("subset1", "subset4")


# Write out
write.csv(sus_imp_table, paste0("/home/skerr/Git/SF94/Outputs/", time_stamp, "/Sensitivity", "/sus_imp_output.csv"))
write.csv(susimp_prop_table, paste0("/home/skerr/Git/SF94/Outputs/", time_stamp, "/Sensitivity", "/susimp_prop_output.csv"))
write.csv(df_susimp_effect_size, paste0("/home/skerr/Git/SF94/Outputs/", time_stamp, "/Sensitivity", "/susimp_effect_size.csv"))
write.csv(df_susimp_sample_size, paste0("/home/skerr/Git/SF94/Outputs/", time_stamp, "/Sensitivity", "/susimp_sample_size.csv"))

# 1/2 level improvement logrank
# 1 level sustained improvement
df_susimp1_lr_sample_size <- rbind(
  calculate_sample_size_lr(subset1, "sustained_1L_improvement", 0.85, 0.05, 0.8, 0, 28),
  calculate_sample_size_lr(subset4, "sustained_1L_improvement", 0.85, 0.05, 0.8, 0, 28)
) %>%
  data.frame()

rownames(df_susimp1_lr_sample_size) <- c("subset1", "subset4")

write.csv(df_susimp1_lr_sample_size, paste0("/home/skerr/Git/SF94/Outputs/", time_stamp, "/Sensitivity", "/susimp1_logrank_sample_size.csv"))

# 2 level sustained improvement
df_susimp2_lr_sample_size <- rbind(
  calculate_sample_size_lr(subset1, "sustained_2L_improvement", 0.85, 0.05, 0.8, 0, 28),
  calculate_sample_size_lr(subset4, "sustained_2L_improvement", 0.85, 0.05, 0.8, 0, 28)
) %>%
  data.frame()

rownames(df_susimp2_lr_sample_size) <- c("subset1", "subset4")

write.csv(df_susimp2_lr_sample_size, paste0("/home/skerr/Git/SF94/Outputs/", time_stamp, "/Sensitivity", "/susimp2_logrank_sample_size.csv"))

# protocolised sf94

# Estimate model to get coefficients for opportunistic measurement
model <- glm(mortality_28 ~ sf94_day5_P + sf94_day0 + age_estimateyears + sex, data = subset4, family = binomial)

# Mean and standard deviation of opportunistic measurements
mean_opp <- df_sf94_mean["subset4", "day5_P"]
sd_opp <- df_sf94_sd["subset4", "day5_P"]

# Calculate correlation between sf94 day 5 and sf94 day 0.
rho <- cor(subset4$sf94_day5_P, subset4$sf94_day0, use = "complete.obs")

# Assume that protocolised measurements have 0.8* standard deviation of opportunistic measurements,
# and there is 0.7 correlation between them

# This value for mean of protocolised day 5 sf4 measurements comes from the RECOVERY trial
# mean_prot = 3.41345
# Alternatively, set mean_prot and mean_opp equal
mean_prot <- mean_opp

# sd_prot = 0.8 * sd_opp

# Previously we assumed sd_opp = 0.8 * sd_prot.
# Now we have data from recovery indicating sd_prot = 1.25
sd_prot <- 1.25

# 0.56767 is the correlation between protocolised sf94 measurements on day 5 and day 0.
# This comes from the RECOVERY trial
# The error measurement model is assumed to be unchanging over time, and we can use
# it to derive this formula for the correlation between opportunistic and protocolised
# measurements
rho_opp_prot <- sqrt(rho / 0.56767)



# Make dataframe of protocolised coefficients
df_coef_prot <- rbind(
  create_coef_prot(mean_prot, sd_prot, 0.5, model$coef),
  create_coef_prot(mean_prot, sd_prot, 0.6, model$coef),
  create_coef_prot(mean_prot, sd_prot, 0.7, model$coef),
  create_coef_prot(mean_prot, sd_prot, 0.8, model$coef),
  create_coef_prot(mean_prot, sd_prot, 0.9, model$coef)
) %>%
  data.frame() %>%
  dplyr::rename(intercept = X.Intercept., sex = sexFemale)

df_coef_prot["rho"] <- seq(0.5, 0.9, 0.1)

# Calculate effect sizes
sf94_prot_effect_size <-
  c(
    effect_size_boot_sf94_prot(subset4, 1:nrow(subset4), treatment = 0.85, sd_mult = 1, rho_opp_prot = 0.5),
    effect_size_boot_sf94_prot(subset4, 1:nrow(subset4), treatment = 0.85, sd_mult = 1, rho_opp_prot = 0.6),
    effect_size_boot_sf94_prot(subset4, 1:nrow(subset4), treatment = 0.85, sd_mult = 1, rho_opp_prot = 0.7),
    effect_size_boot_sf94_prot(subset4, 1:nrow(subset4), treatment = 0.85, sd_mult = 1, rho_opp_prot = 0.8),
    effect_size_boot_sf94_prot(subset4, 1:nrow(subset4), treatment = 0.85, sd_mult = 1, rho_opp_prot = 0.9)
  )

# rho_prot is the correlation between the protocolised sf94 measurements on day 0 and day 5
# The proof of this follows from the assumptions that have been made, along with the
# measurement error model.
# This is explained with mathematical detail in the paper
rho_prot <- rho / seq(0.5, 0.9, 0.1)**2

calculate_sample_size_sf94_vec <- Vectorize(calculate_sample_size_sf94)

# Calculate sample sizes
sf94_prot_sample_size <- calculate_sample_size_sf94_vec(0.05, 0.8, sf94_prot_effect_size, sd_prot, rho_prot)

# Dataframe of results
df_sf94_prot_effect_sample_size <- data.frame(
  rho = seq(0.5, 0.9, 0.1),
  rho_prot = rho_prot,
  effect_size = sf94_prot_effect_size,
  sample_size = sf94_prot_sample_size
)


# Write out
write.csv(df_coef_prot, paste0("/home/skerr/Git/SF94/Outputs/", time_stamp, "/Sensitivity", "/sf94_prot_coef.csv"))
write.csv(df_sf94_prot_effect_sample_size, paste0("/home/skerr/Git/SF94/Outputs/", time_stamp, "/Sensitivity", "/sf94_prot_effect_sample_size.csv"))


