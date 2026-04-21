library(gtsummary)
library(car)
library(lawstat)
library(corrplot)
library(ggplot2)
library(reshape2)

data_for_analysis <- read.csv("data_for_analysis.csv")
data_for_analysis$outcome <- as.factor(data_for_analysis$outcome)

hormones <- c("hormone1", "hormone2", "hormone3", "hormone4", "hormone5",
              "hormone6", "hormone7", "hormone8", "hormone10_generated")

tbl_summary(data_for_analysis, by = outcome, include = all_of(hormones)) %>%
  add_p() %>%
  add_overall()

results <- data.frame(
  Hormone = hormones,
  p_Levene = NA,
  p_Shapiro_0 = NA,
  p_Shapiro_1 = NA,
  p_Brunner = NA,
  p_t_test = NA,
  p_Wilcox = NA,
  Normality_0 = NA,
  Normality_1 = NA,
  Equal_Variance = NA,
  Recommended_Test = NA,
  stringsAsFactors = FALSE
)

for (h in hormones) { # циклический костыль
  cat("Processing:", h, "\n") 
  
  group0 <- data_for_analysis[[h]][data_for_analysis$outcome == 0]
  group1 <- data_for_analysis[[h]][data_for_analysis$outcome == 1]
  
  group0 <- group0[!is.na(group0)]
  group1 <- group1[!is.na(group1)]
  
  par(mfrow = c(2, 2))
  
  # Histogram group 0
  hist(group0, main = paste(h, "- Outcome 0"), col = "lightgreen", xlab = h)
  # Q-Q plot group 0
  qqnorm(group0, main = paste("Q-Q Plot:", h, "(Outcome 0)"))
  qqline(group0, col = "red", lwd = 2)
  
  # Histogram group 1
  hist(group1, main = paste(h, "- Outcome 1"), col = "lightblue", xlab = h)
  # Q-Q plot group 1
  qqnorm(group1, main = paste("Q-Q Plot:", h, "(Outcome 1)"))
  qqline(group1, col = "red", lwd = 2)
  # dev.copy(png, paste0(h, "_qq_hist.png")) # too much png
  # dev.off()
  
  # Shapiro-Wilk test
  shapiro0 <- shapiro.test(group0)
  shapiro1 <- shapiro.test(group1)
  
  results[results$Hormone == h, "p_Shapiro_0"] <- shapiro0$p.value
  results[results$Hormone == h, "p_Shapiro_1"] <- shapiro1$p.value
  results[results$Hormone == h, "Normality_0"] <- ifelse(shapiro0$p.value > 0.05, "Yes", "No")
  results[results$Hormone == h, "Normality_1"] <- ifelse(shapiro1$p.value > 0.05, "Yes", "No")
  
  cat("\nShapiro-Wilk test (Outcome 0): p =", shapiro0$p.value, "->", results[results$Hormone == h, "Normality_0"], "\n")
  cat("Shapiro-Wilk test (Outcome 1): p =", shapiro1$p.value, "->", results[results$Hormone == h, "Normality_1"], "\n")
  
  # Levene's test
  temp_df <- data.frame(
    value = data_for_analysis[[h]],
    outcome = data_for_analysis$outcome
  )
  temp_df <- temp_df[!is.na(temp_df$value), ]
  
  levene_result <- leveneTest(value ~ outcome, data = temp_df)
  p_levene <- levene_result$`Pr(>F)`[1]
  results[results$Hormone == h, "p_Levene"] <- p_levene
  results[results$Hormone == h, "Equal_Variance"] <- ifelse(p_levene > 0.05, "Yes", "No")
  
  cat("Levene's test: p =", p_levene, "-> Equal variance:", results[results$Hormone == h, "Equal_Variance"], "\n")
  
  # Brunner-Munzel test
  bm_result <- brunner.munzel.test(group0, group1)
  results[results$Hormone == h, "p_Brunner"] <- bm_result$p.value
  cat("Brunner-Munzel test: p =", bm_result$p.value, "\n")
  
  #t-test
  t_result <- t.test(group0, group1)
  results[results$Hormone == h, "p_t_test"] <- t_result$p.value
  cat("t-test (Welch): p =", t_result$p.value, "\n")
  
  # Wilcoxon test 
  wilcox_result <- wilcox.test(group0, group1)
  results[results$Hormone == h, "p_Wilcox"] <- wilcox_result$p.value
  cat("Wilcoxon test: p =", wilcox_result$p.value, "\n")
  
  # Recommendation: which test is applicable 
  norm0 <- shapiro0$p.value > 0.05
  norm1 <- shapiro1$p.value > 0.05
  equal_var <- p_levene > 0.05
  
  if (norm0 && norm1 && equal_var) {
    rec <- "Student's t-test"
  } else if (norm0 && norm1 && !equal_var) {
    rec <- "Welch t-test"
  } else {
    rec <- "Brunner-Munzel test"
  }
  
  results[results$Hormone == h, "Recommended_Test"] <- rec
  cat("Recommended test:", rec, "\n")
}

cat("\n========== FINAL RESULTS TABLE ==========\n")
print(results)

write.csv(results, "hormone_tests_results.csv", row.names = FALSE)

# Correlation heatmaps
normality_summary <- data.frame( # Determine correlation method based on normality of most variables
  Hormone = hormones,
  Normal_0 = results$Normality_0,
  Normal_1 = results$Normality_1
)

# If >50% of variables are normal in a group -> use Pearson, else Spearman
pct_normal_0 <- sum(normality_summary$Normal_0 == "Yes") / nrow(normality_summary)
pct_normal_1 <- sum(normality_summary$Normal_1 == "Yes") / nrow(normality_summary)

method_0 <- ifelse(pct_normal_0 > 0.5, "pearson", "spearman")
method_1 <- ifelse(pct_normal_1 > 0.5, "pearson", "spearman")

cat("CORRELATION METHOD SELECTION\n")
cat("Outcome 0: ", pct_normal_0 * 100, "% variables normal -> using", toupper(method_0), "\n")
cat("Outcome 1: ", pct_normal_1 * 100, "% variables normal -> using", toupper(method_1), "\n")

data_0 <- data_for_analysis[data_for_analysis$outcome == 0, hormones]
data_1 <- data_for_analysis[data_for_analysis$outcome == 1, hormones]

data_0 <- data_0[complete.cases(data_0), ]
data_1 <- data_1[complete.cases(data_1), ]

# Correlation matrices
cor_0 <- cor(data_0, method = method_0)
cor_1 <- cor(data_1, method = method_1)

par(mfrow = c(1, 2))

# Heatmap for outcome = 0
corrplot(cor_0, method = "color", type = "upper", order = "hclust",
         title = paste("Correlation Matrix (Outcome = 0)\nMethod:", toupper(method_0)),
         mar = c(0, 0, 2, 0), tl.cex = 0.8)
# Heatmap for outcome = 1
corrplot(cor_1, method = "color", type = "upper", order = "hclust",
         title = paste("Correlation Matrix (Outcome = 1)\nMethod:", toupper(method_1)),
         mar = c(0, 0, 2, 0), tl.cex = 0.8)
# dev.copy(png, "correlation_heatmaps.png", width = 1200, height = 600)
# dev.off()

write.csv(cor_0, "correlation_matrix_outcome_0.csv")
write.csv(cor_1, "correlation_matrix_outcome_1.csv")