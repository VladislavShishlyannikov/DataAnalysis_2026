library(vegan)

data <- read.table("data.txt", header = TRUE, sep = "\t", check.names = FALSE)
rownames(data) <- data[, 1]
data <- data[, -1]
summary(data)
dim(data)

cat("\n========== NMDS WITH BRAY-CURTIS ==========\n")
set.seed(123)
ord <- metaMDS(data, distance = "bray", trymax = 100)
ord

cat("\n========== ENVFIT: SIGNIFICANT SPECIES (p ≤ 0.05) ==========\n")
fit_sp <- envfit(ord, data, permutations = 999)
fit_sp

sig_species <- which(fit_sp$vectors$pvals <= 0.05)
cat("\nSignificant species (p ≤ 0.05):", length(sig_species), "\n")
print(names(sig_species))

cat("\n========== UPGMA CLUSTERING ==========\n")
d <- vegdist(data, method = "bray")
fit <- hclust(d, method = "average")

# Plot dendrogram
par(mfrow = c(1, 1))
plot(fit, hang = -1, main = "UPGMA Dendrogram (Bray–Curtis)", 
     xlab = "Sites", ylab = "Bray–Curtis Dissimilarity")

cat("\n========== CUTTING DENDROGRAM ==========\n")

# Try 2 clusters
clusters_2 <- cutree(fit, k = 2)
cat("\nCluster assignment (k = 2):\n")
print(clusters_2)

# Try 3 clusters
clusters_3 <- cutree(fit, k = 3)
cat("\nCluster assignment (k = 3):\n")
print(clusters_3)

selected_clusters <- clusters_2
k_selected <- 2 # ____Cмена кластеров___
cat("\nSelected number of clusters:", k_selected, "\n")

cat("\n========== NMDS VISUALIZATION ==========\n")

cluster_colors <- c("red", "blue", "green")[1:k_selected]

plot(ord, type = "n", main = paste("NMDS Ordination (Bray–Curtis)\nClusters (k =", k_selected, ")"))

points(ord, disp = "sites", pch = 21, cex = 2, 
       bg = cluster_colors[selected_clusters], col = "black", lwd = 1.5)

ordiellipse(ord, groups = as.factor(selected_clusters), 
            col = cluster_colors[1:k_selected], 
            lwd = 2, label = FALSE)

plot(fit_sp, p.max = 0.05, col = "darkgreen", cex = 0.8, add = TRUE)

orditorp(ord, display = "sites", labels = rownames(data), 
         col = "black", cex = 0.7, air = 0.5)

legend("topright", legend = paste("Cluster", 1:k_selected), 
       fill = cluster_colors[1:k_selected], title = "Clusters")

cat("\n========== PERMANOVA (adonis2) ==========\n")

set.seed(123)
permanova_result <- adonis2(d ~ as.factor(selected_clusters), 
                            permutations = 999, 
                            method = "bray")

print(permanova_result)

# Extract R² and p-value
r_squared <- permanova_result$R2[1]
p_value <- permanova_result$`Pr(>F)`[1]

cat("\n========== PERMANOVA RESULTS ==========\n")
cat("R² =", round(r_squared, 4), "\n")
cat("p-value =", p_value, "\n")

cat("\n========== CONCLUSION ==========\n")

if (p_value < 0.05) {
  cat("The PERMANOVA test shows a statistically significant difference",
      "(p =", p_value, ") in species composition between clusters.\n")
  cat("This indicates that the cluster groups represent distinct",
      "ecological communities.\n")
} else {
  cat("The PERMANOVA test shows NO statistically significant difference",
      "(p =", p_value, ") in species composition between clusters.\n")
  cat("This suggests that the cluster groups do not represent distinct",
      "ecological communities.\n")
}

cat("\nR² =", round(r_squared, 4), 
    "means that", round(r_squared * 100, 1), 
    "% of the variation in species composition is explained by cluster membership.\n")