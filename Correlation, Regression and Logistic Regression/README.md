# Task 4: Correlation, Regression and Logistic Regression

## Assignment Description
This assignment focuses on statistical analysis of hormone and lipid data:

1. Correlation analysis between all numeric variables with significance assessment (permutation method)
2. Regression analysis between variables with model selection by BIC
3. Logistic regression using hormone variables to predict binary outcome with model comparison (AIC/BIC), odds ratios calculation, and results interpretation

## Student Information
- **Name:** Shishlyannikov Vladislav
- **Date:** 12.05.2026
- **Course:** Data Analysis 2026

## Data Description
- **Dataset:** `data_for_analysis.csv`
- **Number of observations (after removing NA in outcome):** 1147
- **Number of numeric variables analyzed:** 29 (excluding `record_id`)
- **Outcome groups:** 0 (n = 987), 1 (n = 160)
- **Complete cases for logistic regression:** 872

## R Environment
- **R Version:** 4.5.3
- **Platform:** Windows 10

## Procedures Used

### 1. Correlation Analysis (Permutation Method)
- **Method:** Spearman correlation (non-parametric, appropriate for non-normal data)
- **Permutation test:** 500 permutations per pair
- **Variables:** All 29 numeric variables (hormones, lipids, antioxidants, etc.)
- **Total pairs analyzed:** 406
- **Significance level:** p < 0.05

### 2. Regression Analysis
- **Dependent variable:** `lipids1`
- **Independent variable:** `lipids2`
- **Models tested:** Linear, Quadratic, Cubic, Exponential, Logarithmic
- **Model selection criterion:** BIC (lower is better)

### 3. Logistic Regression
- **Dependent variable:** `outcome` (binary: 0/1)
- **Predictors:** Hormone variables (`hormone1`–`hormone8`, `hormone10_generated`) and lipid variables
- **Models compared:** lipids1 only, lipids1+lipids2, All hormones, All lipids, Stepwise AIC
- **Selection criterion:** AIC and BIC
- **Performance metrics:** ROC-AUC, accuracy, confusion matrix

## Results

### Correlation Analysis

| Metric | Value |
|--------|-------|
| Total pairs processed | 406 |
| Significant correlations (p < 0.05) | 113 (27.8%) |

**Top 10 strongest correlations:**

| Variable 1 | Variable 2 | Spearman r | p-value |
|------------|------------|------------|---------|
| lipids1 | lipids5 | 0.999 | < 0.001 |
| lipids2 | lipids4 | 0.884 | < 0.001 |
| lipid_pero2 | lipid_pero3 | 0.770 | < 0.001 |
| hormone3 | hormone4 | 0.584 | < 0.001 |
| factor_h | factor_pcos | -0.523 | < 0.001 |
| lipid_pero1 | lipid_pero2 | 0.444 | < 0.001 |
| factor_h | hormone5 | 0.433 | < 0.001 |
| factor_prl | hormone2 | 0.433 | < 0.001 |
| lipid_pero1 | lipid_pero3 | 0.382 | < 0.001 |
| antioxidant4 | antioxidant5 | 0.334 | < 0.001 |

### Regression Analysis (lipids1 ~ lipids2)

| Model | BIC | Rank |
|-------|-----|------|
| Logarithmic | 997.54 | **1 (best)** |
| Linear | 999.04 | 2 |
| Quadratic | 1005.17 | 3 |
| Cubic | 1010.70 | 4 |
| Exponential | 1147.28 | 5 |

**Best model:** Logarithmic (`lipids1 ~ log(lipids2)`) with BIC = 997.54

### Logistic Regression Models Comparison

| Model | AIC | BIC |
|-------|-----|-----|
| **Stepwise AIC** | **718.75** | **747.38** |
| lipids1 + lipids2 | 734.65 | 748.96 |
| All hormones | 734.71 | 782.42 |
| All lipids | 738.77 | 767.39 |
| lipids1 only | 744.40 | 753.94 |

**Best model:** Stepwise AIC (lowest AIC = 718.75, lowest BIC = 747.38)

### Stepwise Model Odds Ratios

| Predictor | OR | 2.5% CI | 97.5% CI | Interpretation |
|-----------|-----|---------|----------|-----------------|
| (Intercept) | 0.112 | 0.038 | 0.325 | Baseline odds |
| lipids2 | **1.381** | 1.147 | 1.661 | 38% increase in odds per unit |
| hormone1 | **0.825** | 0.671 | 0.979 | 17.5% decrease in odds per unit |
| hormone2 | 1.000 | 1.000 | 1.001 | No significant effect |
| hormone5 | 0.999 | 0.999 | 1.000 | No significant effect |
| hormone8 | **0.996** | 0.993 | 0.998 | Small protective effect |

### Model Performance (Stepwise AIC)

| Metric | Value |
|--------|-------|
| AUC (ROC) | **0.6515** |
| Accuracy | **84.86%** |
| True Negatives | 740 |
| True Positives | 0 |
| False Negatives | 132 |

## Files in this Folder
- `data_for_analysis.csv` - Original dataset
- `correlation_permutation_results.csv` - Complete correlation matrix (406 pairs) with Spearman r and p-values
- `regression_model_comparison.csv` - BIC comparison of 5 regression models
- `logistic_odds_ratios.csv` - Odds ratios with 95% confidence intervals for the best model
- `analysis_code.R` - R script for correlation, regression and logistic regression analysis
- `ROC_curve.png` - ROC curve plot (AUC = 0.6515)

## Conclusion

1. **Correlation analysis:** 113 out of 406 variable pairs showed statistically significant correlations (p < 0.05). The strongest correlations were observed between lipid variables (`lipids1` ↔ `lipids5`, r = 0.999) and between hormone variables (`hormone3` ↔ `hormone4`, r = 0.584).

2. **Regression analysis:** The logarithmic model (`lipids1 ~ log(lipids2)`) performed best (BIC = 997.54), suggesting a non-linear relationship between `lipids1` and `lipids2`.

3. **Logistic regression:** The stepwise AIC model was selected as the best (AIC = 718.75, BIC = 747.38). Significant predictors included:
   - `lipids2` (positive association, OR = 1.38)
   - `hormone1` (negative association, OR = 0.83)
   - `hormone8` (weak negative association, OR = 0.996)

4. **Model performance:** The AUC of 0.6515 indicates modest discriminative ability. The high accuracy (84.86%) is driven by the class imbalance (majority class 0).
