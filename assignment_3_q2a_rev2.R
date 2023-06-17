library(ggplot2)
library(splines)
library(ISLR2)

# Load data and remove NAs
data(Auto)
auto_clean <- na.omit(Auto)

# Filter for cars year >= 76
Auto76 <- subset(auto_clean, year >= 76)

# Define new grid of horsepower values
grid_horsepower <- seq(4, 192, 4)

# Define knot values
knots <- c(60, 92, 116, 140, 164)

# Fit regression splines
spline_fit <- lm(mpg ~ bs(horsepower, knots = knots), data = Auto76)

# Predict mpg values for the new grid of horsepower values
grid_mpg_estimates <- predict(spline_fit, newdata = data.frame(horsepower = grid_horsepower), type = "response")

# Create a data frame with horsepower and mpg estimates
df_results <- data.frame(horsepower = grid_horsepower, mpg_estimates = grid_mpg_estimates)

# Plot the results
ggplot() +
  geom_line(data = df_results, aes(x = horsepower, y = mpg_estimates), size = 1) +
  geom_point(data = Auto76, aes(x = horsepower, y = mpg), color = "black") +
  xlim(4, 192) +  # Set the x-axis limits from 4 to 192
  labs(x = "Horsepower", y = "Mpg") +
  ggtitle("Regression Splines with Knots") +
  theme_bw() 