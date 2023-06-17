library(ggplot2)

# Convert scaled_data to a dataframe
scaled_data_df <- as.data.frame(scaled_data)

# Reshape the data to long format
scaled_data_long <- tidyr::gather(scaled_data_df, variable, value)

# Create a histogram plot
histogram_plot <- ggplot(scaled_data_long, aes(x = value)) +
  geom_histogram(fill = "steelblue", color = "white") +
  facet_wrap(~ variable, scales = "free") +
  labs(x = "Scaled Value", y = "Frequency") +
  ggtitle("Histograms of Scaled Variables") +
  theme_bw()

# Display the plot
print(histogram_plot)

#normalized_data <- scale(scaled_data, center = FALSE, scale = FALSE)
#combined_data <- rowMeans(normalized_data)
#hist(combined_data, breaks = 20, col = "blue", main = "Histogram of Combined Data", xlab = "Combined Variable")





