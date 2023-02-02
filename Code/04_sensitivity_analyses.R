# perform sensitivity analyses

# calculate the number of patients that were transferred to another hospital from subset 1
transfer<-table(subset1$dsterm)
write.csv(transfer, paste0("/home/skerr/Git/SF94/Outputs/", time_stamp, "/sensitivity", "/transfer.csv"))

# create a subset that does not include transferred patients from the main analysis (subset 1)
subset4 <- filter(subset1, dsterm != "Transfer to other facility") 

# repeat main analysis without the transferred patients

# Mortality
sample_size_mort <- rbind(
  calculate_sample_size_mort(mean(subset1$mortality_28, na.rm = TRUE), 0.85),
  calculate_sample_size_mort(mean(subset4$mortality_28, na.rm = TRUE), 0.85)
) %>%
  data.frame()

rownames(sample_size_mort) <- c("subset1", "subset4")

write.csv(sample_size_mort, paste0("/home/skerr/Git/SF94/Outputs/", time_stamp, "/sensitivity", "/sample_size_mortality.csv"))

# Mortality - logrank

df_mort_lr_sample_size <- rbind(
  calculate_sample_size_lr(subset1, "mortality_28", 0.85, 0.05, 0.8, 0, 28),
  calculate_sample_size_lr(subset4, "mortality_28", 0.85, 0.05, 0.8, 0, 28)
) %>%
  data.frame()

rownames(df_mort_lr_sample_size) <- c("subset1", "subset4")

write.csv(df_mort_lr_sample_size, paste0("/home/skerr/Git/SF94/Outputs/", time_stamp, "/sensitivity", "/mort_logrank_sample_size.csv"))

# Opportunistic SF94

df_sf94_mean <- rbind(
  calculate_mean_sf94(subset1),
  calculate_mean_sf94(subset4)
) %>%
  data.frame()

names(df_sf94_mean) <- c("day5_P", "day8_P")
rownames(df_sf94_mean) <- c("subset1", "subset4")

df_sf94_sd <- rbind(
  calculate_sd_sf94(subset1),
  calculate_sd_sf94(subset4)
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
write.csv(df_sf94_mean, paste0("/home/skerr/Git/SF94/Outputs/", time_stamp, "/sensitivity", "/sf94_means.csv"))
write.csv(df_sf94_sd, paste0("/home/skerr/Git/SF94/Outputs/", time_stamp, "/sensitivity", "/sf94_sd.csv"))
write.csv(df_sf94_corr, paste0("/home/skerr/Git/SF94/Outputs/", time_stamp, "/sensitivity", "/sf94_correlation.csv"))
write.csv(df_sf94_effect_size, paste0("/home/skerr/Git/SF94/Outputs/", time_stamp, "/sensitivity", "/sf94_effect_size.csv"))
write.csv(df_sf94_sample_size, paste0("/home/skerr/Git/SF94/Outputs/", time_stamp, "/sensitivity", "/sf94_sample_size.csv"))

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
write.csv(who_table, paste0("/home/skerr/Git/SF94/Outputs/", time_stamp, "/sensitivity", "/who_table_output.csv"))
write.csv(who_prop_table, paste0("/home/skerr/Git/SF94/Outputs/", time_stamp, "/sensitivity", "/who_prop_output.csv"))
write.csv(df_who_effect_size, paste0("/home/skerr/Git/SF94/Outputs/", time_stamp, "/sensitivity", "/who_effect_size.csv"))
write.csv(df_who_sample_size, paste0("/home/skerr/Git/SF94/Outputs/", time_stamp, "/sensitivity", "/who_sample_size.csv"))

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
  "subset4_FALSE", "subset4_TRUE"
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
  
  # subset4
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
write.csv(sus_imp_table, paste0("/home/skerr/Git/SF94/Outputs/", time_stamp, "/sensitivity", "/sus_imp_output.csv"))
write.csv(susimp_prop_table, paste0("/home/skerr/Git/SF94/Outputs/", time_stamp, "/sensitivity", "/susimp_prop_output.csv"))
write.csv(df_susimp_effect_size, paste0("/home/skerr/Git/SF94/Outputs/", time_stamp, "/sensitivity", "/susimp_effect_size.csv"))
write.csv(df_susimp_sample_size, paste0("/home/skerr/Git/SF94/Outputs/", time_stamp, "/sensitivity", "/susimp_sample_size.csv"))

# 1/2 level improvement logrank
# 1 level sustained improvement
df_susimp1_lr_sample_size <- rbind(
  calculate_sample_size_lr(subset1, "sustained_1L_improvement", 0.85, 0.05, 0.8, 0, 28),
  calculate_sample_size_lr(subset4, "sustained_1L_improvement", 0.85, 0.05, 0.8, 0, 28)
) %>%
  data.frame()

rownames(df_susimp1_lr_sample_size) <- c("subset1", "subset4")

write.csv(df_susimp1_lr_sample_size, paste0("/home/skerr/Git/SF94/Outputs/", time_stamp, "/sensitivity", "/susimp1_logrank_sample_size.csv"))

# 2 level sustained improvement
df_susimp2_lr_sample_size <- rbind(
  calculate_sample_size_lr(subset1, "sustained_2L_improvement", 0.85, 0.05, 0.8, 0, 28),
  calculate_sample_size_lr(subset4, "sustained_2L_improvement", 0.85, 0.05, 0.8, 0, 28)
) %>%
  data.frame()

rownames(df_susimp2_lr_sample_size) <- c("subset1", "subset4")

write.csv(df_susimp2_lr_sample_size, paste0("/home/skerr/Git/SF94/Outputs/", time_stamp, "/sensitivity", "/susimp2_logrank_sample_size.csv"))

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
write.csv(df_coef_prot, paste0("/home/skerr/Git/SF94/Outputs/", time_stamp, "/sensitivity", "/sf94_prot_coef.csv"))
write.csv(df_sf94_prot_effect_sample_size, paste0("/home/skerr/Git/SF94/Outputs/", time_stamp, "/sensitivity", "/sf94_prot_effect_sample_size.csv"))


############################################################################################################################################################
# S/F94 without imputed data
# use S/F94_day5 instead of s/f94_day5_P (and the same for day 8)
######################### Sample size calculation with opportunistic sf94 as endpoint ###############################
## INPUTS REQUIRED ##
# alpha - significance level =0.05
# power - specified power =0.8
# delta - change in mean sf94
# sd - standard deviation of the outcome measure
# rho - correlation between the outcome measured at baseline and at follow-up

## Summary stats. This is required for some corrections to the sample size calculation
# Mean and SD
calculate_mean_sf94 <- function(df) {
  mean <- as.data.frame(rbind(sapply(df[c("sf94_day5", "sf94_day8")], mean, na.rm = T)))
  
  names(mean) <- c("day5", "day8")
  
  return(mean)
}

calculate_sd_sf94 <- function(df) {
  sd <- as.data.frame(rbind(sapply(df[c("sf94_day5", "sf94_day8")], sd, na.rm = T)))
  
  names(sd) <- c("day5", "day8")
  
  return(sd)
}

df_sf94_mean <- rbind(
  calculate_mean_sf94(subset1),
  calculate_mean_sf94(subset2),
  calculate_mean_sf94(subset3)
) %>%
  data.frame()

names(df_sf94_mean) <- c("day5", "day8")
rownames(df_sf94_mean) <- c("subset1", "subset2", "subset3")

df_sf94_sd <- rbind(
  calculate_sd_sf94(subset1),
  calculate_sd_sf94(subset2),
  calculate_sd_sf94(subset3)
) %>%
  data.frame()

names(df_sf94_sd) <- c("day5", "day8")
rownames(df_sf94_sd) <- c("subset1", "subset2", "subset3")

# Correlation
calculate_correlation_sf94 <- function(df) {
  correlation_subset_05 <- subset(df, (!is.na(df[, "sf94_day0"]) & (!is.na(df[, "sf94_day5"]))))
  correlation_subset_08 <- subset(df, (!is.na(df[, "sf94_day0"]) & (!is.na(df[, "sf94_day8"]))))
  # DAY 0/5
  
  w <- correlation_subset_05[, "sf94_day0"]
  x <- correlation_subset_05[, "sf94_day5"]
  day05_cor <- cor(w, x)
  
  # DAY 0/8
  y <- correlation_subset_08[, "sf94_day0"]
  z <- correlation_subset_08[, "sf94_day8"]
  day08_cor <- cor(y, z)
  
  cor_output <- rbind(day05_cor, day08_cor)
  return(cor_output)
}

df_sf94_corr <- cbind(
  calculate_correlation_sf94(subset1),
  calculate_correlation_sf94(subset2),
  calculate_correlation_sf94(subset3)
) %>%
  t() %>%
  data.frame()

names(df_sf94_corr) <- c("day5", "day8")
rownames(df_sf94_corr) <- c("subset1", "subset2", "subset3")


# Effect size calculator for logistic regression
calculate_effect_size <- function(prob_pred, treatment, coef) {
  return(mean(log((treatment * (1 - prob_pred)) / (1 - treatment * prob_pred)), na.rm = TRUE) / coef)
}

effect_size_boot_sf94 <- function(data, indices, day, treatment) {
  formula <- as.formula(paste0("mortality_28 ~ sf94_day", day, "_P + sf94_day0 + age_estimateyears + sex"))
  
  model <- glm(formula,
               data = data[indices, ],
               family = binomial
  )
  
  prob_pred <- predict(model, type = "response")
  
  coef <- model$coef[2] # is coefficient on sf94 on day of interest
  
  effect_size <- calculate_effect_size(prob_pred, treatment, coef)
  return(effect_size)
}


# Dataframe of effect sizes for different days and subsets
df_sf94_effect_size <-
  rbind(
    c(effect_size_boot_sf94(subset1, 1:nrow(subset1), 5, 0.85), effect_size_boot_sf94(subset1, 1:nrow(subset1), 8, 0.85)),
    c(effect_size_boot_sf94(subset2, 1:nrow(subset2), 5, 0.85), effect_size_boot_sf94(subset2, 1:nrow(subset2), 8, 0.85)),
    c(effect_size_boot_sf94(subset3, 1:nrow(subset3), 5, 0.85), effect_size_boot_sf94(subset3, 1:nrow(subset3), 8, 0.85))
  ) %>%
  data.frame()

names(df_sf94_effect_size) <- c("day5", "day8")
rownames(df_sf94_effect_size) <- c("subset1", "subset2", "subset3")

calculate_sample_size_sf94 <- function(alpha, power, delta, sd, rho) {
  # Calculate sample size for a t test
  power <- power.t.test(n = NULL, delta = delta, sd = sd, power = power, sig.level = alpha)
  
  # Apply ANCOVA correction
  sample_size <- 2 * round(((1 - (rho^2)) * power$n))
  return(sample_size)
}

df_sf94_sample_size <- rbind(
  # subset1
  c(
    calculate_sample_size_sf94(
      0.05, 0.8, df_sf94_effect_size["subset1", "day5"],
      df_sf94_sd["subset1", "day5"],
      df_sf94_corr["subset1", "day5"]
    ),
    calculate_sample_size_sf94(
      0.05, 0.8, df_sf94_effect_size["subset1", "day8"],
      df_sf94_sd["subset1", "day8"],
      df_sf94_corr["subset1", "day8"]
    )
  ),
  # subset2
  c(
    calculate_sample_size_sf94(
      0.05, 0.8, df_sf94_effect_size["subset2", "day5"],
      df_sf94_sd["subset2", "day5"],
      df_sf94_corr["subset2", "day5"]
    ),
    calculate_sample_size_sf94(
      0.05, 0.8, df_sf94_effect_size["subset2", "day8"],
      df_sf94_sd["subset2", "day8"],
      df_sf94_corr["subset2", "day8"]
    )
  ),
  # subset3
  c(
    calculate_sample_size_sf94(
      0.05, 0.8, df_sf94_effect_size["subset3", "day5"],
      df_sf94_sd["subset3", "day5"],
      df_sf94_corr["subset3", "day5"]
    ),
    calculate_sample_size_sf94(
      0.05, 0.8, df_sf94_effect_size["subset3", "day8"],
      df_sf94_sd["subset3", "day8"],
      df_sf94_corr["subset3", "day8"]
    )
  )
) %>%
  data.frame()

names(df_sf94_sample_size) <- c("day5", "day8")
rownames(df_sf94_sample_size) <- c("subset1", "subset2", "subset3")


# Write out
write.csv(df_sf94_mean, paste0("/home/skerr/Git/SF94/Outputs/", time_stamp, "/sensitivity/non_imputed", "/sf94_means.csv"))
write.csv(df_sf94_sd, paste0("/home/skerr/Git/SF94/Outputs/", time_stamp, "/sensitivity/non_imputed", "/sf94_sd.csv"))
write.csv(df_sf94_corr, paste0("/home/skerr/Git/SF94/Outputs/", time_stamp, "/sensitivity/non_imputed", "/sf94_correlation.csv"))
write.csv(df_sf94_effect_size, paste0("/home/skerr/Git/SF94/Outputs/", time_stamp, "/sensitivity/non_imputed", "/sf94_effect_size.csv"))
write.csv(df_sf94_sample_size, paste0("/home/skerr/Git/SF94/Outputs/", time_stamp, "/sensitivity/non_imputed", "/sf94_sample_size.csv"))


# Graph regression analysis

# tbl_regression appears only to work with glm and not lrm
multivariate_model <- glm(mortality_28 ~ sf94_day0 + sf94_day5, subset1, family = "binomial")

results <- tidy(multivariate_model) %>%
  mutate(
    ucl = estimate + 1.96 * std.error,
    lcl = estimate - 1.96 * std.error,
    term = gsub("period", "", term)
  ) %>%
  select(term, estimate, lcl, ucl) %>%
  mutate_if(is.numeric, ~ formatC(round(exp(.), 2), format = "f", big.mark = ",", drop0trailing = TRUE)) %>%
  mutate(estimate = paste0(estimate, " (", lcl, " - ", ucl, ")")) %>%
  select(-lcl, -ucl)

write.csv(results, paste0("/home/skerr/Git/SF94/Outputs/", time_stamp, "/sensitivity/non_imputed", "/day0_day5_P_multivariate_model_summary.csv"))

# First need to set data distribution for rms functions
attach(subset1)
ddist <- datadist(sf94_day0, sf94_day5, mortality_28)
options(datadist = "ddist")
detach(subset1)

### Predicted mortality risk plots

## Multivariate
multivariate_model <- lrm(mortality_28 ~ sf94_day0 + sf94_day5, subset1, x = TRUE, y = TRUE)
not_na <- sum(!is.na(subset1$mortality_28) & !is.na(subset1$sf94_day0) & !is.na(subset1$sf94_day5))

# Day 0
predictions <- Predict(multivariate_model, fun = plogis) %>%
  data.frame()

plot_d0_multi <- ggplot(filter(predictions, .predictor. == "sf94_day0"), aes(x = sf94_day0, y = yhat)) +
  geom_line() +
  theme_bw() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1) +
  xlab("S/F94 day 0") +
  ggtitle(paste0("N=", not_na)) +
  ylab("Risk of 28-day mortality") +
  ylim(0, 0.8) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 12),
    axis.text.y = element_text(size = 9),
    axis.text.x = element_text(size = 9),
    axis.title.y = element_text(size = 9),
    axis.title.x = element_text(size = 9)
  )

plot_d5_multi <- ggplot(filter(predictions, .predictor. == "sf94_day5"), aes(x = sf94_day5, y = yhat)) +
  geom_line() +
  theme_bw() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1) +
  xlab("S/F94 day 5") +
  ggtitle(paste0("N=", not_na)) +
  ylab("Risk of 28-day mortality") +
  ylim(0, 0.8) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 12),
    axis.text.y = element_text(size = 9),
    axis.text.x = element_text(size = 9),
    axis.title.y = element_text(size = 9),
    axis.title.x = element_text(size = 9)
  )

ggarrange(plot_d0_multi, plot_d5_multi,
          ncol = 2, nrow = 1
)


## Univariate
univariate_model <- lrm(mortality_28 ~ sf94_day0, subset1, x = TRUE, y = TRUE)

not_na <- sum(!is.na(subset1$mortality_28) & !is.na(subset1$sf94_day0))

predictions <- Predict(univariate_model, fun = plogis) %>%
  data.frame()

plot_d0_uni <- ggplot(predictions, aes(x = sf94_day0, y = yhat)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.1) +
  theme_bw() +
  xlab("S/F94 day 0") +
  ggtitle(paste0("N=", not_na)) +
  ylab("Risk of 28-day mortality") +
  ylim(0, 0.8) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 12),
    axis.text.y = element_text(size = 9),
    axis.text.x = element_text(size = 9),
    axis.title.y = element_text(size = 9),
    axis.title.x = element_text(size = 9)
  )

day_0_plots <- ggarrange(plot_d0_multi, plot_d0_uni,
                         ncol = 2, nrow = 1
)


ggsave(plot = day_0_plots, dpi = 300, path = paste0("/home/skerr/Git/SF94/Outputs/", time_stamp, "/sensitivity/non_imputed"), filename = "day_0_predicted_mortality_plots.pdf")
ggsave(
  plot = plot_d5_multi, dpi = 300, path = paste0("/home/skerr/Git/SF94/Outputs/", time_stamp, "/sensitivity/non_imputed"), filename = "day_5_predicted_mortality_multivariate_model_plot.pdf",
  width = 4, height = 7, units = "cm"
)
ggsave(plot = plot_d0_uni, dpi = 300, path = paste0("/home/skerr/Git/SF94/Outputs/", time_stamp, "/sensitivity/non_imputed"), filename = "day_0_predicted_mortality_univariate_model_plot.pdf")
ggsave(plot = plot_d0_multi, dpi = 300, path = paste0("/home/skerr/Git/SF94/Outputs/", time_stamp, "/sensitivity/non_imputed"), filename = "day_0_predicted_mortality_multivariate_model_plot.pdf")


############################################################################################################################################################
# WHO graph with imputed data

# sf94 day 5 violin plot for everyone in subset 1
# who_day5 <- filter(data_1, days_since_start == 5, !is.na(severity_scale_ordinal)) %>%
#   mutate(severity_scale_ordinal = paste0("WHO level ", severity_scale_ordinal)) %>%
#   mutate(severity_scale_ordinal = factor(severity_scale_ordinal,
#                                          levels = c(
#                                            "WHO level 4", "WHO level 5",
#                                            "WHO level 6", "WHO level 7",
#                                            "WHO level 8", "WHO level 9", "WHO level 10"
#                                          )
#   ))

n_day5 <- length(unique(subset1$subjid[!is.na(subset1$sf94_day5_P)]))
title <- paste0("N=", n_day5)



ggplot(
  subset1 %>% 
    filter(!is.na(who_day5_P)) %>%
    mutate(who_day5_P = factor(who_day5_P)),
  aes(x = who_day5_P, y = sf94_day5_P, fill = who_day5_P)
) +
  geom_violin() + # remove outliers
  theme_bw() +
  ggtitle(title) +
  ylab("S/F94 day5 (with imputed values)") +
  scale_fill_manual(values = c(
    "#ff0000", "#d7001b", "#a5002b", "#7e0055",
    "#530073", "#350087", "#0000aa"
  )) +
  xlab("WHO ordinal severity scale") +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5)
  ) + # remove legend + center title
  scale_x_discrete(labels = c("4 Hosp", "5 Ox", "6 CPAP/HFNO", "7 IMV", "8 IMV S/F<2", "9 MOF", "10 Dead"))

ggsave(dpi = 300, path = paste0("/home/skerr/Git/SF94/Outputs/", time_stamp, "/sensitivity"), filename = "who_sf94_day5_violin_plot_imputed.pdf")
