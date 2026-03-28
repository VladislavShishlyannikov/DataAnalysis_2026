# Data Manipulation and Cleaning - Assignment

## Assignment Description
This assignment focuses on data cleaning, missing value imputation, and outlier detection techniques in R.

## Student Information
- **Name:** Shishlyannikov Vladislav
- **Date:** 29.03.2026
- **Course:** Data Analysis 2026

## Data Description
- **Dataset:** DataSet_No_Details.csv
- **Number of observations:** 1148
- **Number of variables:** 41
- **Variables:** Clinical measurements including hormones, lipids, and indices

## R Environment
- **R Version:** 4.5.3 (2026-03-11)
- **Platform:** Windows 10

## Procedures Used

### 1. Data Preparation
- Removal of irrelevant columns
- Factor variable extraction

### 2. Missing Value Analysis
- Missing value visualization (visdat, naniar)
- Missing percentage calculation
- Little's MCAR test

### 3. Imputation Methods
- **PMM (Predictive Mean Matching):** Non-parametric approach preserving distribution
- **RF (Random Forest):** Handles complex nonlinear relationships

### 4. Comparison of Methods
- Density plots comparing original vs imputed distributions
- Statistical comparison (means and standard deviations)

### 5. Outlier Detection
- Boxplot visualization
- **LOF (Local Outlier Factor)** algorithm for multivariate outlier detection

## Results

### Imputation Comparison
- PMM method better preserves original distribution
- RF method handles nonlinear relationships effectively

### Outlier Detection
- Number of outliers identified: 7
- LOF scores distribution visualized in histogram

## Files in this Folder
- `DataSet_No_Details.csv` - Original dataset
- `analysis_code.R` - R script with complete analysis
- `imputation_comparison.png` - Density plots comparing methods
- `boxplots_outliers.png` - Boxplots for outlier detection
- `lof_histogram.png` - LOF factor distribution
- `lof_scatterplot.png` - Bivariate scatterplot with outliers
- `imputation_comparison_stats.csv` - Statistical comparison table
- `lof_outlier_results.csv` - LOF results with outlier labels

## Conclusion
Both imputation methods performed adequately. PMM is recommended for normally distributed data, while RF is better for complex patterns. LOF successfully identified outliers in the multivariate dataset.
