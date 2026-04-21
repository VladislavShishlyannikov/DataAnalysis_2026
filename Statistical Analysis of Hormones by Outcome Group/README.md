# Statistical Analysis of Hormones by Outcome Group

## Assignment Description
This assignment focuses on comparing hormone levels between two outcome groups (0 and 1) using various statistical tests. The analysis includes:

1. Descriptive statistics table by outcome group for all hormones
2. Normality testing (Shapiro-Wilk test)
3. Homogeneity of variance testing (Levene's test)
4. Group comparison tests: Brunner-Munzel, t-test (Welch), Wilcoxon test
5. Q-Q plots and histograms for each hormone by group
6. Recommendation of the most appropriate test for each hormone
7. Correlation heatmaps for each outcome group (using Spearman correlation due to non-normal data)

## Student Information
- **Name:** Shishlyannikov Vladislav
- **Date:** 22.04.2026
- **Course:** Data Analysis 2026

## Data Description
- **Dataset:** `data_for_analysis.csv`
- **Number of observations:** 1148
- **Number of variables:** 31
- **Outcome groups:** 0 (n = 987), 1 (n = 160)
- **Hormones analyzed:** hormone1, hormone2, hormone3, hormone4, hormone5, hormone6, hormone7, hormone8, hormone10_generated

## R Environment
- **R Version:** 4.5.3
- **Platform:** Windows 10

## Procedures Used

### 1. Descriptive Statistics
- Generated summary table using `gtsummary::tbl_summary()`
- Stratified by outcome group (0 and 1)
- Included all 9 hormone variables

### 2. Normality Assessment
- **Test:** Shapiro-Wilk test (appropriate for n < 5000)
- **Performed separately for:** each hormone × each outcome group
- **Decision rule:** p > 0.05 indicates normal distribution

### 3. Homogeneity of Variance
- **Test:** Levene's test
- **Purpose:** Check if variances are equal between groups
- **Decision rule:** p > 0.05 indicates equal variances

### 4. Group Comparison Tests
For each hormone, three tests were performed:

| Test | Type | Best for |
|------|------|----------|
| Brunner-Munzel | Non-parametric | Non-normal data with possible unequal variances |
| t-test (Welch) | Parametric | Normal data (unequal variances allowed) |
| Wilcoxon | Non-parametric | Non-normal data (assumes similar distribution shapes) |

### 5. Test Selection Logic
- **Student's t-test:** Both groups normal AND equal variances
- **Welch t-test:** Both groups normal BUT unequal variances
- **Brunner-Munzel test:** Non-normal data (recommended for this dataset)

### 6. Visualization
- **Histograms:** Distribution shape for each hormone by group
- **Q-Q plots:** Normality assessment
- **Correlation heatmaps:** Spearman correlations between all hormones

## Results

### Normality Assessment
**All 9 hormones showed non-normal distribution in both outcome groups (p < 0.05 for all Shapiro-Wilk tests).**

| Hormone | Outcome 0 (p-value) | Normal? | Outcome 1 (p-value) | Normal? |
|---------|--------------------|---------|---------------------|---------|
| hormone1 | 6.23e-44 | No | 1.40e-09 | No |
| hormone2 | 8.29e-37 | No | 6.29e-17 | No |
| hormone3 | 6.06e-44 | No | 1.23e-18 | No |
| hormone4 | 7.42e-50 | No | 9.17e-22 | No |
| hormone5 | 4.15e-44 | No | 2.32e-18 | No |
| hormone6 | 1.77e-28 | No | 3.12e-09 | No |
| hormone7 | 1.21e-31 | No | 7.72e-13 | No |
| hormone8 | 6.21e-21 | No | 6.40e-05 | No |
| hormone10_generated | 4.74e-57 | No | 2.20e-15 | No |

### Homogeneity of Variance (Levene's Test)
**All hormones showed equal variances between groups (p > 0.05 for all).**

| Hormone | p-value | Equal Variance? |
|---------|---------|-----------------|
| hormone1 | 0.087 | Yes |
| hormone2 | 0.979 | Yes |
| hormone3 | 0.577 | Yes |
| hormone4 | 0.063 | Yes |
| hormone5 | 0.165 | Yes |
| hormone6 | 0.534 | Yes |
| hormone7 | 0.799 | Yes |
| hormone8 | 0.071 | Yes |
| hormone10_generated | 0.691 | Yes |

### Group Comparison Tests (p-values)

| Hormone | Brunner-Munzel | t-test (Welch) | Wilcoxon | Significant (p < 0.05) |
|---------|----------------|----------------|----------|------------------------|
| hormone1 | 0.750 | 0.051 | 0.759 | No |
| hormone2 | **0.0036** | 0.110 | **0.0054** | **Yes** |
| hormone3 | 0.131 | 0.360 | 0.135 | No |
| hormone4 | 0.164 | 0.092 | 0.148 | No |
| hormone5 | **0.0030** | **0.0061** | **0.0037** | **Yes** |
| hormone6 | 0.817 | 0.458 | 0.816 | No |
| hormone7 | 0.922 | 0.942 | 0.921 | No |
| hormone8 | **0.00038** | **3.83e-05** | **0.00038** | **Yes** |
| hormone10_generated | 0.789 | 0.340 | 0.791 | No |

**Significant differences between outcome groups were found for:**
- **hormone2** (p = 0.0036, Brunner-Munzel)
- **hormone5** (p = 0.0030, Brunner-Munzel)
- **hormone8** (p = 0.00038, Brunner-Munzel)

### Recommended Test
**For all hormones, the Brunner-Munzel test is recommended** because:
1. All hormones showed non-normal distribution (Shapiro-Wilk p < 0.05)
2. The Brunner-Munzel test is robust to non-normality and does not assume equal variances
3. It is more reliable than Wilcoxon when distribution shapes differ between groups

### Correlation Analysis

**Method selected:** Spearman correlation (non-parametric)

**Reasoning:** 0% of variables were normally distributed in both groups → Spearman is appropriate.

#### Key correlations (Outcome = 0):
| Pair | Correlation | Strength |
|------|-------------|----------|
| hormone3 ↔ hormone4 | 0.587 | Moderate positive |
| hormone2 ↔ hormone10_generated | 0.174 | Weak positive |
| hormone5 ↔ hormone8 | 0.254 | Weak positive |

#### Key correlations (Outcome = 1):
| Pair | Correlation | Strength |
|------|-------------|----------|
| hormone3 ↔ hormone4 | 0.564 | Moderate positive |
| hormone7 ↔ hormone10_generated | 0.269 | Weak positive |
| hormone5 ↔ hormone8 | 0.250 | Weak positive |

**Pattern interpretation:** The strongest correlation in both groups is between hormone3 and hormone4, suggesting these two hormones are consistently related regardless of outcome status.

## Files in this Folder
- `data_for_analysis.csv` - Original dataset
- `hormone_tests_results.csv` - Complete table of all test results
- `correlation_matrix_outcome_0.csv` - Spearman correlation matrix (outcome = 0)
- `correlation_matrix_outcome_1.csv` - Spearman correlation matrix (outcome = 1)
- `Practice 3.R` - R script for statistical analysis and visualization
- Q-Q plots and histograms for each hormone
- Correlation heatmaps for both outcome groups

## Conclusion

1. **Normality:** None of the 9 hormones followed a normal distribution in either outcome group.
2. **Variance:** All hormones had equal variances between outcome groups.
3. **Group differences:** Statistically significant differences (p < 0.05) were found for **hormone2, hormone5, and hormone8**, indicating these hormones differ between outcome groups.
4. **Recommended test:** **Brunner-Munzel test** is the most appropriate for all hormones due to non-normal data distribution.
5. **Correlations:** Spearman correlation was used (0% normal data). The strongest relationship was between hormone3 and hormone4 in both groups.
