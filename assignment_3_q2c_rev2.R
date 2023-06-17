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

# Plot the results for each df-value
plots <- list()

for (df in df_values) {
  # Fit smoothing spline
  spline_fit <- smooth.spline(Auto76$horsepower, Auto76$mpg, df = df)
  
  # Predict mpg values on grid of horsepower values
  grid_mpg_estimates <- predict(spline_fit, x = grid_horsepower)$y
  
  # Create a data frame with horsepower and mpg estimates
  df_results <- data.frame(horsepower = grid_horsepower, mpg_estimates = grid_mpg_estimates)
  
  # Create the plot for the current df-value
  plot <- ggplot(df_results, aes(x = horsepower, y = mpg_estimates)) +
    geom_point(data = Auto76, aes(x = horsepower, y = mpg)) +
    geom_line(size = 1) +
    labs(x = "Horsepower", y = "Mpg") +
    ggtitle(paste("Smoothing Spline (df =", df, ")")) +
    theme_bw()
  
  plots[[as.character(df)]] <- plot
}

# Arrange the plots in a grid
library(gridExtra)
grid.arrange(grobs = plots, ncol = 2)
