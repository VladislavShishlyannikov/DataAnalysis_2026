# 1. Load data and libraries

library(MASS)
library(dplyr)
library(tidyr)

data <- read.csv("data_for_analysis.csv")
str(data)

# 2. Identify continuous variables (exclude lipids5 initially)

continuous_vars <- data %>%
  select(-record_id, -outcome, -factor_eth, -factor_h, -factor_pcos, -factor_prl, -lipids5) %>%
  names()

continuous_vars

# 3. Function to estimate best distribution by group

estimate_distribution_by_group <- function(df, var_name, group_col = "outcome") {
  groups <- unique(df[[group_col]])
  groups <- groups[!is.na(groups)]
  
  results <- list()
  
  for (g in groups) {
    vals <- df[df[[group_col]] == g, var_name]
    vals <- vals[!is.na(vals)]
    
    if (length(vals) < 5) next
    
    # Fit distributions
    fit_norm <- tryCatch(fitdistr(vals, "normal"), error = function(e) NULL)
    fit_lognorm <- tryCatch(fitdistr(vals, "lognormal"), error = function(e) NULL)
    fit_exp <- tryCatch(fitdistr(vals, "exponential"), error = function(e) NULL)
    
    # Calculate BIC
    bic_values <- c()
    dist_names <- c()
    
    if (!is.null(fit_norm)) {
      bic_values <- c(bic_values, BIC(fit_norm))
      dist_names <- c(dist_names, "normal")
    }
    if (!is.null(fit_lognorm)) {
      bic_values <- c(bic_values, BIC(fit_lognorm))
      dist_names <- c(dist_names, "lognormal")
    }
    if (!is.null(fit_exp)) {
      bic_values <- c(bic_values, BIC(fit_exp))
      dist_names <- c(dist_names, "exponential")
    }
    
    if (length(bic_values) == 0) next
    
    best_idx <- which.min(bic_values)
    best_dist <- dist_names[best_idx]
    best_bic <- bic_values[best_idx]
    
    # Extract parameters
    params <- list()
    if (best_dist == "normal") {
      params$mean <- fit_norm$estimate[1]
      params$sd <- fit_norm$estimate[2]
    } else if (best_dist == "lognormal") {
      params$meanlog <- fit_lognorm$estimate[1]
      params$sdlog <- fit_lognorm$estimate[2]
    } else if (best_dist == "exponential") {
      params$rate <- fit_exp$estimate[1]
    }
    
    results[[paste0(var_name, "_group_", g)]] <- list(
      variable = var_name,
      group = g,
      n = length(vals),
      mean_empirical = mean(vals),
      sd_empirical = sd(vals),
      best_distribution = best_dist,
      BIC = best_bic,
      parameters = params
    )
  }
  
  return(results)
}

# 4. Apply to all continuous variables (without lipids5)

all_results <- list()

for (var in continuous_vars) {
  cat("\nProcessing:", var, "\n")
  res <- estimate_distribution_by_group(data, var)
  all_results <- c(all_results, res)
}

# 5. Create summary table (obligatory part)

summary_table <- data.frame()

for (res in all_results) {
  row <- data.frame(
    Variable = res$variable,
    Group = res$group,
    N = res$n,
    Mean = round(res$mean_empirical, 3),
    SD = round(res$sd_empirical, 3),
    Best_Distribution = res$best_distribution,
    BIC = round(res$BIC, 2),
    Parameters = paste(names(res$parameters), round(unlist(res$parameters), 3), collapse = "; ")
  )
  summary_table <- bind_rows(summary_table, row)
}

# View table
print(summary_table)

# Save table
write.csv(summary_table, "distribution_by_group_summary.csv", row.names = FALSE)

# 6. Fix missing data in lipids5

# Find error: lipids5 has NAs
sum(is.na(data$lipids5))

# Impute missing lipids5 using group median (or mean)
data_fixed <- data %>%
  group_by(outcome) %>%
  mutate(lipids5 = ifelse(is.na(lipids5), median(lipids5, na.rm = TRUE), lipids5)) %>%
  ungroup()

# Verify
sum(is.na(data_fixed$lipids5))

# 7. Include lipids5 in continuous variables

continuous_vars_fixed <- names(data_fixed)[!names(data_fixed) %in% 
                                             c("record_id", "outcome", "factor_eth", "factor_h", "factor_pcos", "factor_prl")]

# 8. Re-run distribution estimation with fixed data

all_results_fixed <- list()

for (var in continuous_vars_fixed) {
  cat("\nProcessing (fixed):", var, "\n")
  res <- estimate_distribution_by_group(data_fixed, var)
  all_results_fixed <- c(all_results_fixed, res)
}

# 9. Create final summary table (with fixed data)

summary_table_fixed <- data.frame()

for (res in all_results_fixed) {
  row <- data.frame(
    Variable = res$variable,
    Group = res$group,
    N = res$n,
    Mean = round(res$mean_empirical, 3),
    SD = round(res$sd_empirical, 3),
    Best_Distribution = res$best_distribution,
    BIC = round(res$BIC, 2),
    Parameters = paste(names(res$parameters), round(unlist(res$parameters), 3), collapse = "; ")
  )
  summary_table_fixed <- bind_rows(summary_table_fixed, row)
}

# View final table
print(summary_table_fixed)

# Save final table
write.csv(summary_table_fixed, "distribution_by_group_final.csv", row.names = FALSE)

# 10. Visual comparison (example: lipids1 by group)

library(ggplot2)

# Original vs imputed lipids5
ggplot(data_fixed, aes(x = factor(outcome), y = lipids5, fill = factor(outcome))) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Lipids5 Distribution by Outcome Group (after imputation)",
       x = "Outcome", y = "Lipids5") +
  theme_minimal()

# Density plots for selected variables by group
selected_vars <- c("lipids1", "lipids2", "lipids3", "lipids4", "lipids5")

for (var in selected_vars) {
  p <- ggplot(data_fixed, aes(x = .data[[var]], fill = factor(outcome))) +
    geom_density(alpha = 0.5) +
    labs(title = paste("Distribution of", var, "by Outcome"),
         x = var, y = "Density") +
    theme_minimal()
  print(p)
  ggsave(paste0(var, "_by_outcome.png"), plot = p, width = 8, height = 5)
}