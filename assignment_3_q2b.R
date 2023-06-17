library(ISLR2)
library(ggplot2)
library(splines)

# Load data and remove NAs
data(Auto)
auto_clean <- na.omit(Auto)

# Filter for cars year >= 76
Auto76 <- subset(auto_clean, year >= 76)

# Define knot values
knots <- c(60, 92, 116, 140, 164)

# Fit regression splines with degree 2 and knots
spline_fit_deg2 <- lm(mpg ~ bs(horsepower, degree = 2, knots = knots), data = Auto76)

# Fit regression splines with degree 3 and knots
spline_fit_deg3 <- lm(mpg ~ bs(horsepower, degree = 3, knots = knots), data = Auto76)

# Predict mpg values on the grid of horsepower values
grid_df <- data.frame(horsepower = grid_horsepower)

# Predictions for degree 2 spline
grid_df$mpg_estimates_deg2 <- predict(spline_fit_deg2, newdata = grid_df)

# Predictions for degree 3 spline
grid_df$mpg_estimates_deg3 <- predict(spline_fit_deg3, newdata = grid_df)

# Plot the results
ggplot() +
  geom_point(data = Auto76, aes(x = horsepower, y = mpg)) +
  geom_line(data = grid_df, aes(x = horsepower, y = mpg_estimates_deg2), color = "red", size = 1, linetype = "dashed") +
  geom_line(data = grid_df, aes(x = horsepower, y = mpg_estimates_deg3), color = "blue", size = 1, linetype = "dotted") +
  labs(x = "Horsepower", y = "Mpg") +
  ggtitle("Comparison of Spline Fits (Degrees 2 and 3)") +
  theme_bw()
