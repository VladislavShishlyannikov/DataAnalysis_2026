# Task 5: Multivariate Analysis of Arctic Chrysophyte Communities

## Assignment Description
This assignment focuses on multivariate analysis of Arctic chrysophyte abundance data:

1. Compute NMDS ordination using Bray–Curtis distance
2. Fit significant species vectors
3. Perform UPGMA clustering
4. Cut dendrogram into 2–3 clusters
5. Plot NMDS with points coloured by cluster, confidence ellipses, significant species arrows, and non‑overlapping site labels
6. Test differences between clusters using PERMANOVA
7. Report R² and p‑value, conclude whether clusters differ significantly in species composition

## Student Information
- **Name:** Shishlyannikov Vladislav
- **Date:** 23.05.2026
- **Course:** Data Analysis 2026

## Data Description
- **Dataset:** `data.txt`
- **Number of observations (sites):** 6
- **Number of species:** 34

### Sites
- Ledyanaya Gora
- Karaul village
- Ladyginskie Yary
- Sopochnaya Karga
- Sibiryakov Island
- Chernyi Bay

## R Environment
- **R Version:** 4.5.3
- **Platform:** Windows 10

## Procedures Used

### 1. NMDS Ordination (Bray–Curtis)
- **Distance measure:** Bray–Curtis (standard for ecological community data)
- **Method:** Non‑metric Multidimensional Scaling
- **Random starts:** 100 tries
- **Resulting stress:** 0 (near‑perfect representation)

### 2. Significant Species Vectors
- **Method:** Fits species vectors onto NMDS ordination
- **Significance threshold:** p ≤ 0.05
- **Number of permutations:** 999

### 3. UPGMA Clustering
- **Distance matrix:** Bray–Curtis
- **Linkage method:** Average

### 4. Cluster Assignment
- Dendrogram cut into 2 clusters (selected based on visual inspection)

### 5. NMDS Visualization
- Points coloured by cluster
- Confidence ellipses
- Significant species arrows
- Site labels without overlap

### 6. PERMANOVA
- **Formula:** d ~ clusters
- **Permutations:** 999
- **Distance:** Bray–Curtis

## Results

### NMDS Ordination
| Parameter | Value |
|-----------|-------|
| Distance | Bray–Curtis |
| Dimensions | 2 |
| Stress | 0 |
| Note | Near‑perfect representation of dissimilarities |

### Significant Species (envfit, p ≤ 0.05)

| Species | NMDS1 | NMDS2 | r² | p-value |
|---------|-------|-------|-----|---------|
| **S. cornuta** | -0.664 | -0.748 | 0.840 | **0.033** |

Only one species showed statistically significant fit to the ordination at α = 0.05.

### UPGMA Clustering

**Cluster assignment (k = 2):**

| Site | Cluster |
|------|---------|
| Ledyanaya Gora | 1 |
| Karaul village | 2 |
| Ladyginskie Yary | 1 |
| Sopochnaya Karga | 1 |
| Sibiryakov Island, Srednee Lake | 2 |
| Chernyi Bay | 1 |

**Cluster composition:**
- **Cluster 1 (4 sites):** Ledyanaya Gora, Ladyginskie Yary, Sopochnaya Karga, Chernyi Bay
- **Cluster 2 (2 sites):** Karaul village, Sibiryakov Island

### PERMANOVA Results

| Metric | Value |
|--------|-------|
| R² | 0.4088 |
| p-value | 0.0667 |
| F-statistic | 2.766 |
| Degrees of freedom (Model) | 1 |
| Degrees of freedom (Residual) | 4 |

**Interpretation:**
- R² = 0.409 means that **40.9%** of the variation in species composition is explained by cluster membership
- p = 0.0667 > 0.05 → **NO statistically significant difference** between clusters at the conventional α = 0.05 level

## Files in this Folder
- `data.txt` - Original dataset
- `Practice 5.R` - R script for multivariate analysis
- `NMDS_plot.png` - NMDS ordination plot with clusters, ellipses, and species arrows
- `dendrogram.png` - UPGMA dendrogram

## Conclusion

1. **NMDS ordination** successfully reduced the 34‑dimensional species data to 2 dimensions with zero stress, indicating excellent representation of the original dissimilarities.

2. **Significant species:** Only `S. cornuta` showed a statistically significant fit to the ordination (p = 0.033), suggesting it is the primary species driving the observed patterns.

3. **UPGMA clustering** separated the 6 sites into 2 distinct groups:
   - Cluster 1: 4 sites (Ledyanaya Gora, Ladyginskie Yary, Sopochnaya Karga, Chernyi Bay)
   - Cluster 2: 2 sites (Karaul village, Sibiryakov Island)

4. **PERMANOVA** revealed that the difference between clusters is **not statistically significant** (p = 0.0667 > 0.05). Although cluster membership explains 40.9% of the variation in species composition, the small sample size (n = 6) limits statistical power.

5. **Final conclusion:** The cluster groups don't represent distinct ecological communities at the conventional significance level (α = 0.05). However, the marginal p‑value (0.0667) suggests a potential trend that could be confirmed with additional sampling.

