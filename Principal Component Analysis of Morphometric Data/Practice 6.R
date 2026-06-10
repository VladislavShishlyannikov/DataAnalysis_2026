library(vegan)      
library(factoextra) 
library(plotly)   

# Set locale to Russian (optional, for correct text encoding)
Sys.setlocale("LC_CTYPE", "russian")

# Read morphometry data (tab-separated file with header)
data <- read.table(
  "data_morphometry.txt",
  header = TRUE,
  sep = "\t",
  fileEncoding = "CP1251"
)

# Save the first column (group labels)
p_names <- data[, 1]

data <- data[, -1]

# Standardize columns to range [0,1] (min-max normalization)
data <- decostand(data, method = "range", MARGIN = 2)

# -------------------------------
# Correlation analysis (Spearman)
# -------------------------------

# Initialize matrices for correlation coefficients and p-values
DD <- matrix(nrow = ncol(data), ncol = ncol(data))
rownames(DD) <- colnames(data)
colnames(DD) <- colnames(data)

DP <- DD

# Calculate Spearman correlations and p-values
for (i in 1:ncol(data)) {
  for (j in 1:ncol(data)) {
    R <- cor.test(data[, i], data[, j], method = "spearman")
    DD[i, j] <- R$estimate
    DP[i, j] <- R$p.value
    
    if (i == j)
      DD[i, j] <- 1
  }
}

# Keep only significant correlations
DD[DP > 0.05] <- 0

sig_corr <- data.frame()

for (i in 1:(ncol(data) - 1)) {
  for (j in (i + 1):ncol(data)) {
    if (DP[i, j] < 0.05) {
      sig_corr <- rbind(
        sig_corr,
        data.frame(
          Trait1 = colnames(data)[i],
          Trait2 = colnames(data)[j],
          Spearman_r = round(DD[i, j], 3),
          P_value = round(DP[i, j], 5)
        )
      )
    }
  }
}

cat("\nSignificant Spearman correlations (p < 0.05):\n")
print(sig_corr)

write.table(
  sig_corr,
  "significant_spearman_correlations.txt",
  sep = "\t",
  row.names = FALSE
)

# -------------------------------
# Principal Component Analysis (PCA)
# -------------------------------

fit <- prcomp(data)

# Variance explained
summary(fit)

# PCA biplot with points coloured by group
fviz_pca_biplot(
  fit,
  habillage = p_names
)

# PCA biplot with 95% confidence ellipses
fviz_pca_biplot(
  fit,
  habillage = p_names,
  addEllipses = TRUE,
  ellipse.level = 0.95
)

# -------------------------------
# Interactive 3D PCA biplot
# -------------------------------

# Observation scores
scores <- fit$x

x <- scores[, 1]
y <- scores[, 2]
z <- scores[, 3]

loads <- fit$rotation

group_factor <- as.factor(p_names)

# Create 3D scatter plot
p <- plot_ly() %>%
  add_trace(
    x = x,
    y = y,
    z = z,
    type = "scatter3d",
    mode = "markers",
    color = group_factor,
    text = rownames(scores),
    hoverinfo = "text",
    marker = list(size = 4)
  )

scale.loads <- 5

for (k in 1:nrow(loads)) {
  
  x_line <- c(0, loads[k, 1]) * scale.loads
  y_line <- c(0, loads[k, 2]) * scale.loads
  z_line <- c(0, loads[k, 3]) * scale.loads
  
  p <- p %>%
    add_trace(
      x = x_line,
      y = y_line,
      z = z_line,
      type = "scatter3d",
      mode = "lines",
      line = list(width = 8),
      opacity = 1,
      showlegend = FALSE
    )
}

for (k in 1:nrow(loads)) {
  
  p <- p %>%
    add_trace(
      x = loads[k, 1] * scale.loads,
      y = loads[k, 2] * scale.loads,
      z = loads[k, 3] * scale.loads,
      type = "scatter3d",
      mode = "text",
      text = rownames(loads)[k],
      showlegend = FALSE
    )
}

p <- p %>%
  layout(
    scene = list(
      xaxis = list(title = "PC1"),
      yaxis = list(title = "PC2"),
      zaxis = list(title = "PC3")
    )
  )

# Display interactive 3D PCA biplot
print(p)
library(htmlwidgets)

saveWidget(
  p,
  file = "PCA_3D_biplot.html",
  selfcontained = TRUE
)