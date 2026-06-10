# Task 6: Principal Component Analysis of Morphometric Data

## Assignment Description

This assignment focuses on the analysis of morphometric traits using correlation analysis and Principal Component Analysis (PCA):

1. Range standardization of morphometric variables
2. Spearman correlation analysis between all morphometric traits with significance assessment (p < 0.05)
3. Principal Component Analysis (PCA) of standardized morphometric measurements
4. Visualization of PCA results using:

   * PCA biplot with points coloured by group
   * PCA biplot with 95% confidence ellipses
   * Interactive 3D PCA biplot (PC1, PC2, PC3) with loading vectors

---

## Student Information

* **Name:** Shishlyannikov Vladislav
* **Date:** 11.06.26
* **Course:** Data Analysis 2026

---

## Data Description

* **Dataset:** `data_morphometry.txt`
* **Grouping variable:** first column of the dataset
* **Morphometric traits analyzed:** 11
* **Data preprocessing:** min–max normalization (range standardization to [0,1])

The dataset contains quantitative measurements of vegetative and reproductive plant structures, including shoot height, leaf dimensions, perianth dimensions, stamen height and pistil height.

---

## R Environment

* **R Version:** 4.5.3
* **Platform:** Windows 10

---

## Procedures Used

### 1. Data Standardization

All morphometric variables were standardized using range normalization:

[
x' = \frac{x - \min(x)}{\max(x)-\min(x)}
]

This transformation scales all variables to the interval [0,1], ensuring that variables measured in different units contribute equally to the analysis.

### 2. Correlation Analysis

* **Method:** Spearman rank correlation
* **Variables analyzed:** all morphometric traits
* **Significance level:** p < 0.05
* **Output:** table of statistically significant correlation coefficients

### 3. Principal Component Analysis (PCA)

* **Input data:** standardized morphometric traits
* **Method:** PCA using `prcomp()`
* **Purpose:** dimensionality reduction and identification of major patterns of variation among individuals

### 4. PCA Visualization

Three PCA visualizations were produced:

1. PCA biplot with observations coloured according to group membership
2. PCA biplot with observations coloured by group and 95% confidence ellipses
3. Interactive 3D PCA biplot using the first three principal components

---

## Results

### Correlation Analysis

A total of **49 statistically significant Spearman correlations** (p < 0.05) were identified among the morphometric traits.

All significant correlations were positive, indicating coordinated variation among morphological characteristics.

### Strongest Correlations

| Trait 1                    | Trait 2                  | Spearman r |
| -------------------------- | ------------------------ | ---------- |
| Length of first leaf       | Length of second leaf    | 0.944      |
| Length of outer perianth   | Length of inner perianth | 0.924      |
| Width of outer perianth    | Width of inner perianth  | 0.879      |
| Width of first leaf        | Width of second leaf     | 0.797      |
| Height of generative shoot | Length of second leaf    | 0.689      |
| Height of generative shoot | Length of first leaf     | 0.662      |

The strongest relationships were observed between measurements describing similar organs. Leaf dimensions and perianth dimensions exhibited particularly high correlations, suggesting strong morphological integration and coordinated growth patterns.

---

### Principal Component Analysis

#### Variance Explained

| Component | Variance Explained (%) | Cumulative (%) |
| --------- | ---------------------- | -------------- |
| PC1       | 51.01                  | 51.01          |
| PC2       | 17.20                  | 68.22          |
| PC3       | 10.32                  | 78.54          |
| PC4       | 8.16                   | 86.70          |

The first principal component explained more than half of the total variation in the dataset (51.01%), indicating that a single dominant gradient of morphological variation is present.

The first two principal components accounted for 68.22% of the total variance, while the first three components explained 78.54%, providing a reliable low-dimensional representation of the data.

---

### Interpretation of Principal Components

#### PC1 (51.01% of variance)

The first principal component received positive contributions from nearly all morphometric variables. The largest loadings were associated with:

| Trait                    | Loading |
| ------------------------ | ------- |
| Length of outer perianth | 0.407   |
| Length of inner perianth | 0.391   |
| Width of second leaf     | 0.356   |
| Width of first leaf      | 0.316   |
| Length of first leaf     | 0.313   |
| Length of second leaf    | 0.312   |

Because all major loadings are positive, PC1 primarily represents an overall size gradient. Individuals with high PC1 scores tend to have larger leaves, larger perianth structures, and generally larger morphological dimensions.

#### PC2 (17.20% of variance)

The strongest positive loadings were associated with leaf widths and leaf lengths:

| Trait                 | Loading |
| --------------------- | ------- |
| Width of first leaf   | 0.393   |
| Width of second leaf  | 0.354   |
| Length of first leaf  | 0.285   |
| Length of second leaf | 0.258   |

The strongest negative loadings were associated with perianth dimensions:

| Trait                    | Loading |
| ------------------------ | ------- |
| Length of inner perianth | -0.411  |
| Length of outer perianth | -0.348  |
| Width of outer perianth  | -0.325  |
| Width of inner perianth  | -0.322  |

Therefore, PC2 reflects a contrast between vegetative traits (leaf dimensions) and reproductive traits (perianth dimensions).

#### PC3 (10.32% of variance)

The largest positive loading was observed for:

| Trait                      | Loading |
| -------------------------- | ------- |
| Height of generative shoot | 0.523   |

The strongest negative loadings were:

| Trait                   | Loading |
| ----------------------- | ------- |
| Width of outer perianth | -0.446  |
| Width of inner perianth | -0.391  |
| Width of second leaf    | -0.367  |
| Width of first leaf     | -0.345  |

This component appears to separate individuals with taller generative shoots from those with relatively broader leaves and perianth structures.

---

### PCA Visualization

#### PCA Biplot (Groups)

The PCA biplot allowed visualization of individuals in the reduced multivariate space defined by the first two principal components. Group colouring facilitated assessment of similarities and differences among groups.

#### PCA Biplot with 95% Confidence Ellipses

Confidence ellipses provided an estimate of within-group variation and group overlap. Groups with strongly overlapping ellipses exhibit similar morphometric characteristics, whereas separated ellipses indicate morphological differentiation.

#### Interactive 3D PCA Biplot

The interactive 3D PCA visualization incorporated PC1, PC2 and PC3, together explaining 78.54% of total variance.

Loading vectors showed how individual traits contributed to the principal components. Variables associated with leaf dimensions and perianth dimensions had the strongest influence on the overall PCA structure, whereas floral height measurements contributed more strongly to PC3.


---

## Files in this Folder

* `data_morphometry.txt` – original morphometric dataset
* `significant_spearman_correlations.txt` – significant Spearman correlation coefficients (p < 0.05)
* `Practice 6.R` – R script used for analysis
* `PCA_biplot1.png` – PCA biplot coloured by group
* `PCA_biplot2.png` – PCA biplot with 95% confidence ellipses
* `PCA_3D_biplot.html` – interactive 3D PCA visualization

---

## Conclusion

1. **Correlation analysis:** 49 statistically significant positive correlations were identified among the morphometric traits. The strongest relationships were observed between dimensions of the same organs, particularly leaf lengths and perianth measurements.

2. **Principal Component Analysis:** PCA effectively reduced the dimensionality of the dataset. The first principal component explained 51.01% of the total variation, while the first three components together explained 78.54%.

3. **Morphological gradients:** PC1 represented a general size gradient, PC2 contrasted vegetative and reproductive structures, and PC3 reflected variation between shoot height and organ width measurements.

4. **Trait relationships:** The analysis revealed strong coordination among morphometric traits, indicating that many structures vary together during plant development.

5. **Group visualization:** PCA biplots and confidence ellipses enabled visual assessment of group structure and morphological similarity, while the interactive 3D PCA plot provided additional insight into multivariate relationships.

6. **Overall conclusion:** PCA successfully summarized complex morphometric variation and identified the major axes of morphological differentiation within the dataset, demonstrating strong covariation among plant traits and highlighting the most important contributors to morphological diversity.
