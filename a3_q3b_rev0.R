library(ggplot2)

# Scale the variables
scaled_data <- scale(Auto76[, c("mpg", "cylinders", "displacement", "horsepower", "weight", "acceleration", "year")])

# Perform hierarchical clustering using complete linkage and Euclidean distance
hclust_result <- hclust(dist(scaled_data), method = "complete")

# Plot the dendrogram without numbers
plot(hclust_result, main = "Hierarchical Clustering (Complete Linkage)", xlab = "Observations", ylab = "Distance", labels = FALSE)
