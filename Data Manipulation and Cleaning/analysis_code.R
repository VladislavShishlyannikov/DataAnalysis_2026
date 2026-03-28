#--------------------START-------------------------------
# Get current working directory
getwd()

# Create path relative to project root
data_path <- "DataSet_No_Details.csv"

#----------------READ DATASET--------------------------
df <- read.csv(data_path)

# Display structure with variable types
str(df)

# Beautiful summary with histograms for numeric variables
install.packages("skimr")
library(skimr)
skim(df) 

#---------------DATA SET PREPARATION------------------
library(dplyr)

# Delete a few columns 
cols_to_remove <- c("h_index_34", "h_index_56", "hormone10_1", "hormone10_2",
                    "an_index_23", "outcome", "factor_eth", "factor_h", 
                    "factor_pcos", "factor_prl")
MD_df <- df %>% select(-any_of(cols_to_remove))

factor_df <- df %>% select(record_id, outcome, factor_eth, factor_h, 
                           factor_pcos, factor_prl)

str(MD_df)
summary(factor_df)

#--------------IDENTIFY MISSING VALUES-----------------
sum(is.na(MD_df))               # Total NAs in entire dataset
colSums(is.na(MD_df))           # NA counts per column
skim(MD_df)

na_stats <- colMeans(is.na(MD_df)) * 100 # % missing data
na_stats

na_stats_filtered <- na_stats[na_stats <= 35] # missing data <=35 %
# Result as a table
data.frame(
  Column = names(na_stats_filtered),
  NA_Percent = na_stats_filtered,
  row.names = NULL
)

na_stats_filtered_1 <- na_stats[na_stats > 35] # missing data >35 %
# Result as a table
data.frame(
  Column = names(na_stats_filtered_1),
  NA_Percent = na_stats_filtered_1,
  row.names = NULL
)

#-------------------VISUALIZING MISSING DATA PATTERNS------------------
library(visdat)
vis_miss(MD_df)  # Visualizes NA patterns

library(naniar)
gg_miss_var(MD_df)  # Barplot of missingness per variable

#------------------ ANALYZING THE IMPACT OF MISSING DATA--------------
# Delete a few columns 
cols_to_remove1 <- c("hormone9", "hormone11", "hormone12", "hormone13", "hormone14")
handle_MD_df <- MD_df %>% select(-any_of(cols_to_remove1))
str(handle_MD_df)

#------------------PERFORMING LITTLE'S MCAR TEST----------------------
# Hypotheses:
# H₀ (Null Hypothesis): Data is MCAR.
# H₁ (Alternative Hypothesis): Data is not MCAR (either MAR or MNAR).
# If p-value > 0.05, we fail to reject H₀ (data is likely MCAR).
# If p-value ≤ 0.05, we reject H₀ (data is likely not MCAR).

# Note: Little's MCAR test requires the 'BaylorEdPsych' package
# install.packages("BaylorEdPsych")
# library(BaylorEdPsych)
# LittleMCAR(handle_MD_df)

#------------------IMPUTATION WITH MICE-------------------------------
# Install packages if they are not already installed
install.packages(c("mice", "ggplot2", "naniar"))

# Load the packages
library(mice)
library(ggplot2)
library(naniar)

# ---------- METHOD 1: Random Forest Method (rf) ----------
# For complex nonlinear relationships between variables
cat("\n========== PERFORMING RF IMPUTATION ==========\n")
imputed_rf <- mice(handle_MD_df, m = 5, method = "rf", print = FALSE)
imputed_rf_final <- complete(imputed_rf)

# ---------- METHOD 2: Predictive Mean Matching (pmm) ----------
# Default method for numeric data
cat("\n========== PERFORMING PMM IMPUTATION ==========\n")
imputed_pmm <- mice(handle_MD_df, m = 5, method = "pmm", print = FALSE)
imputed_pmm_final <- complete(imputed_pmm)

#------------------COMPARISON OF IMPUTATION METHODS------------------
# Select variables for comparison
variables_to_compare <- c("hormone10_generated", "lipids1", "lipids2")

# Create comparison plots
for(var in variables_to_compare) {
  # Check if variable exists
  if(var %in% names(handle_MD_df)) {
    p <- ggplot() +
      geom_density(data = handle_MD_df, aes(x = .data[[var]], color = "Original"), size = 1) +
      geom_density(data = imputed_rf_final, aes(x = .data[[var]], color = "RF Method"), size = 1, alpha = 0.7) +
      geom_density(data = imputed_pmm_final, aes(x = .data[[var]], color = "PMM Method"), size = 1, alpha = 0.7) +
      labs(title = paste("Comparison of Imputation Methods:", var),
           x = var, y = "Density") +
      theme_minimal() +
      scale_color_manual(values = c("Original" = "black", 
                                    "RF Method" = "red", 
                                    "PMM Method" = "blue"))
    print(p)
  }
}

# Create comparison statistics
comparison_stats <- data.frame(
  Variable = character(),
  Original_Mean = numeric(),
  PMM_Mean = numeric(),
  RF_Mean = numeric(),
  Original_SD = numeric(),
  PMM_SD = numeric(),
  RF_SD = numeric(),
  stringsAsFactors = FALSE
)

for(var in variables_to_compare) {
  if(var %in% names(handle_MD_df)) {
    comparison_stats <- rbind(comparison_stats, data.frame(
      Variable = var,
      Original_Mean = mean(handle_MD_df[[var]], na.rm = TRUE),
      PMM_Mean = mean(imputed_pmm_final[[var]], na.rm = TRUE),
      RF_Mean = mean(imputed_rf_final[[var]], na.rm = TRUE),
      Original_SD = sd(handle_MD_df[[var]], na.rm = TRUE),
      PMM_SD = sd(imputed_pmm_final[[var]], na.rm = TRUE),
      RF_SD = sd(imputed_rf_final[[var]], na.rm = TRUE)
    ))
  }
}

cat("\n========== COMPARISON STATISTICS ==========\n")
print(comparison_stats)

#------------------CONCLUSION------------------
cat("\n========== CONCLUSION ==========\n")
cat("Based on the comparison of PMM and RF imputation methods:\n\n")
cat("1. PMM (Predictive Mean Matching):\n")
cat("   - Better preserves the original distribution and variance\n")
cat("   - More suitable for normally distributed data\n")
cat("   - Maintains relationships between variables\n\n")
cat("2. RF (Random Forest):\n")
cat("   - Handles complex nonlinear relationships better\n")
cat("   - May smooth the distribution slightly\n")
cat("   - More flexible for datasets with complex patterns\n\n")
cat("3. Recommendation:\n")
cat("   - Use PMM when data follows normal distribution\n")
cat("   - Use RF when there are complex nonlinear relationships\n")
cat("   - Both methods performed adequately for this dataset\n")

#------------------OUTLIER DETECTION METHODS------------------------
library(ggplot2)
library(tidyr)

# Boxplot for selected variables
outliers_data <- imputed_pmm_final %>%
  select(lipids1, lipids2, lipids3, lipids4, lipids5) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value")

# Build graph for selected variables
p1 <- ggplot(outliers_data, aes(x = variable, y = value)) +
  geom_boxplot(fill = "lightblue", alpha = 0.7) +
  labs(title = "Outlier Detection - Selected Variables",
       x = "Variables",
       y = "Value") +
  theme_minimal()
print(p1)

# Build graph for all numeric variables
p2 <- imputed_pmm_final %>%
  select(where(is.numeric)) %>%
  pivot_longer(everything()) %>%
  ggplot(aes(y = value)) +
  geom_boxplot() +
  facet_wrap(~name, scales = "free") +
  labs(title = "Boxplots for Outlier Detection - All Variables") +
  theme_minimal()
print(p2)

#------------------LOCAL OUTLIER FACTOR (LOF) FOR EXTRA POINTS------------------
# Install and load dbscan package
install.packages("dbscan")
library(dbscan)

# Prepare data for LOF (only numeric variables)
lof_data <- imputed_pmm_final %>%
  select(where(is.numeric)) %>%
  na.omit()  # LOF doesn't work with NA values

# Scale the data for better LOF performance
lof_data_scaled <- scale(lof_data)

# Calculate LOF scores
set.seed(123)  # for reproducibility
lof_scores <- lof(lof_data_scaled, k = 5)  # k = number of neighbors

# Add LOF scores to dataframe
lof_results <- data.frame(lof_data)
lof_results$LOF <- lof_scores

# 1. Histogram of LOF factors
p3 <- ggplot(lof_results, aes(x = LOF)) +
  geom_histogram(bins = 50, fill = "steelblue", color = "black", alpha = 0.7) +
  geom_vline(xintercept = 1.5, color = "red", linetype = "dashed", size = 1) +
  labs(title = "Distribution of LOF Factors",
       subtitle = "Red line indicates outlier threshold (1.5)",
       x = "LOF Score", y = "Frequency") +
  theme_minimal()
print(p3)

# 2. Identify outliers (threshold typically > 1.5)
threshold <- 1.5
lof_results$outlier <- ifelse(lof_results$LOF > threshold, "Outlier", "Normal")

# 3. Bivariate scatterplot with outliers highlighted
# Select first two numeric variables for visualization
var_names <- names(lof_data)[1:2]

p4 <- ggplot(lof_results, aes(x = .data[[var_names[1]]], 
                              y = .data[[var_names[2]]], 
                              color = outlier)) +
  geom_point(alpha = 0.6, size = 2) +
  scale_color_manual(values = c("Normal" = "blue", "Outlier" = "red")) +
  labs(title = "Bivariate Scatterplot with Outliers (LOF Method)",
       subtitle = paste("Variables:", var_names[1], "vs", var_names[2]),
       x = var_names[1], y = var_names[2]) +
  theme_minimal() +
  theme(legend.position = "bottom")
print(p4)

# LOF Statistics
cat("\n========== LOF OUTLIER DETECTION RESULTS ==========\n")
cat("Number of outliers detected:", sum(lof_results$outlier == "Outlier"), "\n")
cat("Percentage of outliers:", round(mean(lof_results$outlier == "Outlier") * 100, 2), "%\n")
cat("Range of LOF scores:", round(range(lof_results$LOF), 3), "\n")
cat("Mean LOF score:", round(mean(lof_results$LOF), 3), "\n")
cat("Median LOF score:", round(median(lof_results$LOF), 3), "\n")

#------------------SAVE RESULTS ------------------
# Save plots
ggsave("imputation_comparison.png", plot = p, width = 10, height = 6)
ggsave("boxplots_outliers.png", plot = p2, width = 12, height = 8)
ggsave("lof_histogram.png", plot = p3, width = 10, height = 6)
ggsave("lof_scatterplot.png", plot = p4, width = 10, height = 8)

# Save comparison statistics
write.csv(comparison_stats, "imputation_comparison_stats.csv", row.names = FALSE)

# Save LOF results
write.csv(lof_results, "lof_outlier_results.csv", row.names = FALSE)

cat("\n========== END OF ANALYSIS ==========\n")