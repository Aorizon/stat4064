library(ggplot2)

# Scale the variables
scaled_data <- scale(Auto76[, c("mpg", "cylinders", "displacement", "horsepower", "weight", "acceleration", "year")])

# Perform hierarchical clustering using single linkage and Euclidean distance
hclust_single <- hclust(dist(scaled_data), method = "single")

# Perform hierarchical clustering using centroid linkage and Euclidean distance
hclust_centroid <- hclust(dist(scaled_data), method = "centroid")

# Plot the dendrograms separately
par(mfrow = c(2, 1))  # Set the plotting layout to 2 rows and 1 column

# Single linkage dendrogram
plot(hclust_single, main = "Hierarchical Clustering (Single Linkage)", xlab = "Observations", ylab = "Distance", labels = FALSE)

# Centroid linkage dendrogram
plot(hclust_centroid, main = "Hierarchical Clustering (Centroid Linkage)", xlab = "Observations", ylab = "Distance", labels = FALSE)
