# Distribution Estimation by Outcome Group

## Assignment Description
This assignment focuses on estimating probability distributions of continuous variables grouped by the binary outcome variable. The analysis includes:

1. Identifying the best-fitting distribution (normal, lognormal, exponential) for each continuous variable within each outcome group (0 and 1) using the Bayesian Information Criterion (BIC).
2. Creating a summary table with descriptive statistics and distribution parameters.
3. Fixing missing data in the `lipids5` variable (extra points).
4. Visualizing density distributions by outcome group.

## Student Information
- **Name:** Shishlyannikov Vladislav
- **Date:** 11.04.2026
- **Course:** Data Analysis 2026

## Data Description
- **Dataset:** `data_for_analysis.csv`
- **Number of observations:** 1148
- **Number of variables:** 31
- **Missing values in lipids5 (before imputation):** 276
- **Outcome groups:** 0 (n = 987), 1 (n = 160)
- **Variables analyzed:** All continuous variables (hormones, lipids, antioxidants, etc.)

## R Environment
- **R Version:** 4.5.3
- **Platform:** Windows 10

## Procedures Used

### 1. Data Preparation
- Merged `factor_df` and `imputed_df` by `record_id` (inner join)
- Identified continuous variables (excluded categorical factors and identifiers)

### 2. Distribution Estimation
For each continuous variable and each outcome group (0 and 1):
- Fitted three distributions: normal, lognormal, exponential
- Calculated BIC for each fit
- Selected the distribution with the lowest BIC value
- Extracted distribution parameters (mean/sd for normal, meanlog/sdlog for lognormal, rate for exponential)

### 3. Missing Data Handling (Extra Points)
- **Problem:** `lipids5` contained 276 missing values
- **Solution:** Imputed missing values using group median (by outcome group)
- **Verification:** No remaining NAs after imputation

### 4. Visualization
- Created density plots for `lipids1` through `lipids5` by outcome group

## Results
Best distributions by variable and outcome group can be seen in file `distribution_by_group_final.csv`

### Key Findings
- **Lognormal distribution** was the most common best fit across variables (majority of cases)
- **Exponential distribution** was selected for some variables (hormone3, hormone5, hormone7 in group 0)
- **Normal distribution** appeared only for a few cases (antioxidant1 in both groups, lipids5 in group 1, etc.)
- Group differences were generally small, with slightly higher values in group 1 for several variables

## Files in this Folder
- `data_for_analysis.csv` - Original merged dataset
- `distribution_by_group_summary.csv` - Summary table (obligatory part, before fixing lipids5)
- `distribution_by_group_final.csv` - Complete summary table with distribution parameters
- `analysis_code.R` - R script for distribution estimation and visualization
- `lipids1_by_outcome.png` - Density plot of lipids1 by outcome group
- `lipids2_by_outcome.png` - Density plot of lipids2 by outcome group
- `lipids3_by_outcome.png` - Density plot of lipids3 by outcome group
- `lipids4_by_outcome.png` - Density plot of lipids4 by outcome group
- `lipids5_by_outcome.png` - Density plot of lipids5 by outcome group

## Conclusion
The analysis successfully identified the best-fitting distributions for all continuous variables stratified by outcome groups. The lognormal distribution proved to be the preferred model, indicating that most variables are best described on a logarithmic scale. The exponential distribution was appropriate for some highly skewed variables without negative values. Missing data in the `lipids5` variable were successfully imputed using group medians, allowing for a full analysis of all variables. Density plots visually confirm the differences in distributions between outcome groups.
