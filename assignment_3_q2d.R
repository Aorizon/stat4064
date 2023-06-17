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

# Create an empty vector to store CV MSE values
cv_mse <- vector("numeric", length = length(df_values))

# Perform cross-validation for each df-value
for (i in 1:length(df_values)) {
  # Fit smoothing spline
  spline_fit <- smooth.spline(Auto76$horsepower, Auto76$mpg, df = df_values[i])
  
  # Predict mpg values using cross-validation
  cv_predictions <- predict(spline_fit, x = Auto76$horsepower, cv = TRUE)$y
  
  # Calculate CV MSE
  cv_mse[i] <- mean((Auto76$mpg - cv_predictions)^2)
}

# Find the index of the df-value with the lowest CV MSE
best_df_index <- which.min(cv_mse)
best_df <- df_values[best_df_index]

# Fit the smoothing spline with the best df-value
best_spline_fit <- smooth.spline(Auto76$horsepower, Auto76$mpg, df = best_df)

# Predict mpg values on the grid of horsepower values
grid_mpg_estimates <- predict(best_spline_fit, x = grid_horsepower)$y

# Create a data frame with horsepower and mpg estimates
df_results <- data.frame(horsepower = grid_horsepower, mpg_estimates = grid_mpg_estimates)

# Plot the results
ggplot(data = df_results, aes(x = horsepower, y = mpg_estimates)) +
  geom_point(data = Auto76, aes(x = horsepower, y = mpg), color = "black") +
  geom_line(size = 1) +
  labs(x = "Horsepower", y = "Mpg") +
  ggtitle(paste("Fitted Function (df =", best_df, ")")) +
  theme_bw()
