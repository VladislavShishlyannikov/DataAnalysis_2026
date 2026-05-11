library(pROC)

data <- read.csv("data_for_analysis.csv")
data$outcome <- as.factor(data$outcome)
data <- data[!is.na(data$outcome), ]

# Correlation analysis with permutation test
perm_spearman <- function(x, y, R = 500) {
  complete_idx <- complete.cases(x, y)
  x <- x[complete_idx]
  y <- y[complete_idx]
  
  observed <- cor(x, y, method = "spearman")
  perm_cors <- replicate(R, cor(x, sample(y), method = "spearman"))
  p_value <- mean(abs(perm_cors) >= abs(observed))
  
  return(list(observed = observed, p_value = p_value))
}

numeric_vars <- names(data)[sapply(data, is.numeric)]
numeric_vars <- numeric_vars[!numeric_vars %in% c("record_id")]

corr_results <- data.frame(
  var1 = character(),
  var2 = character(),
  spearman_corr = numeric(),
  p_value = numeric(),
  stringsAsFactors = FALSE
)

total_pairs <- 0
total_combinations <- choose(length(numeric_vars), 2)
cat("Total pairs to process:", total_combinations, "\n")

for (i in 1:(length(numeric_vars) - 1)) {
  for (j in (i + 1):length(numeric_vars)) {
    total_pairs <- total_pairs + 1
    
    if (total_pairs %% 50 == 0) {
      cat("Progress:", total_pairs, "/", total_combinations, "pairs completed\n")
    }
    
    perm_result <- perm_spearman(data[[numeric_vars[i]]], data[[numeric_vars[j]]], R = 500)
    
    corr_results <- rbind(corr_results, data.frame(
      var1 = numeric_vars[i],
      var2 = numeric_vars[j],
      spearman_corr = perm_result$observed,
      p_value = perm_result$p_value,
      stringsAsFactors = FALSE
    ))
  }
}

corr_results <- corr_results[order(-abs(corr_results$spearman_corr)), ]
write.csv(corr_results, "correlation_permutation_results.csv", row.names = FALSE)

cat("\nTotal pairs processed:", nrow(corr_results), "\n")
cat("Significant correlations (p < 0.05):", sum(corr_results$p_value < 0.05), "\n")
print(head(corr_results, 20))

# Regression analysis
cat("\n========== REGRESSION ANALYSIS ==========\n")

df <- data[complete.cases(data$lipids1, data$lipids2), ]
df <- df[order(df$lipids2), ]

model_linear <- lm(lipids1 ~ lipids2, data = df)
model_quadratic <- lm(lipids1 ~ poly(lipids2, 2), data = df)
model_cubic <- lm(lipids1 ~ poly(lipids2, 3), data = df)
model_exp <- lm(log(lipids1) ~ lipids2, data = df)
model_log <- lm(lipids1 ~ log(lipids2), data = df)

model_comparison <- data.frame(
  model = c("Linear", "Quadratic", "Cubic", "Exponential", "Logarithmic"),
  BIC = c(BIC(model_linear), BIC(model_quadratic), BIC(model_cubic), 
          BIC(model_exp), BIC(model_log))
)
model_comparison <- model_comparison[order(model_comparison$BIC), ]
print(model_comparison)
write.csv(model_comparison, "regression_model_comparison.csv", row.names = FALSE)

# -------------------- Logistic regression --------------------
cat("\n========== LOGISTIC REGRESSION ==========\n")

all_predictors <- c("lipids1", "lipids2", "lipids3", "lipids4", "lipids5",
                    "hormone1", "hormone2", "hormone3", "hormone4", "hormone5",
                    "hormone6", "hormone7", "hormone8", "hormone10_generated")

logit_data <- data[complete.cases(data[, all_predictors]), ]
logit_data$outcome <- as.factor(logit_data$outcome)
cat("Complete cases:", nrow(logit_data), "\n")

hormone_vars <- c("hormone1", "hormone2", "hormone3", "hormone4", "hormone5",
                  "hormone6", "hormone7", "hormone8", "hormone10_generated")
lipid_vars <- c("lipids1", "lipids2", "lipids3", "lipids4", "lipids5")

logit_model_1 <- glm(outcome ~ lipids1, data = logit_data, family = binomial)
logit_model_2 <- glm(outcome ~ lipids1 + lipids2, data = logit_data, family = binomial)
logit_model_3 <- glm(as.formula(paste("outcome ~", paste(hormone_vars, collapse = " + "))),
                     data = logit_data, family = binomial)
logit_model_4 <- glm(as.formula(paste("outcome ~", paste(lipid_vars, collapse = " + "))),
                     data = logit_data, family = binomial)

full_model <- glm(outcome ~ lipids1 + lipids2 + lipids3 + lipids4 + lipids5 +
                    hormone1 + hormone2 + hormone3 + hormone4 + hormone5 +
                    hormone6 + hormone7 + hormone8 + hormone10_generated,
                  data = logit_data, family = binomial)

step_model <- step(full_model, direction = "both", trace = FALSE)

logit_comparison <- data.frame(
  model = c("lipids1 only", "lipids1 + lipids2", "All hormones", "All lipids", "Stepwise AIC"),
  AIC = c(AIC(logit_model_1), AIC(logit_model_2), AIC(logit_model_3), 
          AIC(logit_model_4), AIC(step_model)),
  BIC = c(BIC(logit_model_1), BIC(logit_model_2), BIC(logit_model_3),
          BIC(logit_model_4), BIC(step_model))
)
logit_comparison <- logit_comparison[order(logit_comparison$AIC), ]
print(logit_comparison)

best_logit_name <- logit_comparison$model[1]
cat("\nBest model:", best_logit_name, "\n")

if (best_logit_name == "lipids1 only") {
  best_fit <- logit_model_1
} else if (best_logit_name == "lipids1 + lipids2") {
  best_fit <- logit_model_2
} else if (best_logit_name == "All hormones") {
  best_fit <- logit_model_3
} else if (best_logit_name == "All lipids") {
  best_fit <- logit_model_4
} else {
  best_fit <- step_model
}

odds_ratios <- exp(cbind(OR = coef(best_fit), confint(best_fit)))
print(odds_ratios)
write.csv(odds_ratios, "logistic_odds_ratios.csv", row.names = TRUE)

logit_data$pred_prob <- predict(best_fit, type = "response")
roc_curve <- roc(logit_data$outcome, logit_data$pred_prob)
auc_value <- auc(roc_curve)

cat("AUC =", round(auc_value, 4), "\n")
plot(roc_curve, main = paste("ROC Curve - AUC =", round(auc_value, 4)))

logit_data$pred_class <- ifelse(logit_data$pred_prob > 0.5, 1, 0)
conf_matrix <- table(Actual = logit_data$outcome, Predicted = logit_data$pred_class)
print(conf_matrix)
cat("Accuracy:", round(sum(diag(conf_matrix)) / sum(conf_matrix), 4), "\n")

# Summary
cat("\n========== SUMMARY ==========\n")
cat("1. Correlation permutation test completed for", nrow(corr_results), "pairs\n")
cat("2. Significant correlations (p < 0.05):", sum(corr_results$p_value < 0.05), "\n")
cat("3. Best regression model:", model_comparison$model[1], "(BIC =", min(model_comparison$BIC), ")\n")
cat("4. Best logistic model:", best_logit_name, "(AIC =", min(logit_comparison$AIC), ")\n")
cat("5. AUC =", round(auc_value, 4), "\n")