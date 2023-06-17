library(ggplot2)

# Scale the variables
scaled_data <- scale(Auto76[, c("mpg", "cylinders", "displacement", "horsepower", "weight", "acceleration", "year")])

# Set the number of runs
num_runs <- 20

# Initialize empty vectors to store the variability values
between_variability <- vector()
total_within_variability <- vector()

# Perform k-means clustering for k = 2 to 6
for (k in 2:6) {
  best_within_variability <- Inf
  
  # Run k-means clustering multiple times and select the best run based on total within-cluster sum of squares
  for (i in 1:num_runs) {
    set.seed(i)  # Set a seed for reproducibility
    
    # Perform k-means clustering
    kmeans_result <- kmeans(scaled_data, centers = k)
    
    # Calculate the total within-cluster sum of squares
    within_variability <- kmeans_result$tot.withinss
    
    # Update the best within-cluster sum of squares if a lower value is found
    if (within_variability < best_within_variability) {
      best_within_variability <- within_variability
    }
  }
  
  # Calculate the between-cluster variability
  between_variability[k - 1] <- sum(kmeans_result$betweenss)
  
  # Store the best total within-cluster sum of squares
  total_within_variability[k - 1] <- best_within_variability
}

# Create a data frame to store the variability values
variability_data <- data.frame(k = 2:6, between_variability, total_within_variability)

# Plot the between-cluster and total within-cluster variabilities
ggplot(variability_data, aes(x = k)) +
  geom_line(aes(y = between_variability, color = "Between-Cluster Variability"), size = 1) +
  geom_line(aes(y = total_within_variability, color = "Total Within-Cluster Variability"), size = 1) +
  labs(x = "Number of Clusters (k)", y = "Variability", color = "Variability Type") +
  ggtitle("BC and TWC Variabilities") +
  theme_bw()
