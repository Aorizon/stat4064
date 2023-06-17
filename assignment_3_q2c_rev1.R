library(ggplot2)
library(ISLR2)

# Load data and remove NAs
data(Auto)
auto_clean <- na.omit(Auto)

# Filter for cars year >= 76
Auto76 <- subset(auto_clean, year >= 76)

# Define grid of horsepower values
grid_horsepower <- seq(4, 192, 4)

# Define df-values
df_values <- c(4, 6, 8, 16, 32)

# Create an empty dataframe to store the results
plot_data <- data.frame()

# Fit smoothing splines for each df-value
for (df in df_values) {
  spline_fit <- smooth.spline(Auto76$horsepower, Auto76$mpg, df = df)
  
  # Predict mpg values on grid of horsepower values
  grid_mpg_estimates <- predict(spline_fit, x = grid_horsepower)$y
  
  # Create a dataframe with horsepower and mpg estimates
  df_results <- data.frame(horsepower = grid_horsepower, mpg_estimates = grid_mpg_estimates, df = df)
  
  # Append the results to the plot_data dataframe
  plot_data <- rbind(plot_data, df_results)
}

# Convert df to numeric
plot_data$df <- as.numeric(plot_data$df)

# Plot the results
ggplot(data = plot_data, aes(x = horsepower, y = mpg_estimates, color = factor(df))) +
  geom_point(data = Auto76, aes(x = horsepower, y = mpg), color = "black") +
  geom_line(aes(group = df), size = 1) +
  labs(x = "Horsepower", y = "Mpg", color = "df") +
  ggtitle("Smoothing Splines with Different df-values") +
  theme_bw()

