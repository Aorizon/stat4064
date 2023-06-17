library(ISLR2)
library(ggplot2)
library(splines)

# Load data and remove NAs
data(Auto)
auto_clean <- na.omit(Auto)

# Filter for cars year >= 76
Auto76 <- subset(auto_clean, year >= 76)

# Convert horsepower to an atomic vector
horsepower <- as.numeric(Auto76$horsepower)

# Define grid of horsepower values
grid_horsepower <- seq(4, 192, 4)

# Define knot values
knots <- c(60, 92, 116, 140, 164)

# Restrict grid of horsepower values within knot range
grid_horsepower <- grid_horsepower[grid_horsepower >= min(knots) & grid_horsepower <= max(knots)]

# Fit regression splines
spline_fit <- lm(mpg ~ bs(horsepower, knots = knots), data = Auto76)

# Predict mpg values on the grid of horsepower values
grid_df <- data.frame(horsepower = grid_horsepower)
grid_df$mpg_estimates <- predict(spline_fit, newdata = grid_df)

# Plot the results
ggplot() +
  geom_point(data = Auto76, aes(x = horsepower, y = mpg)) +
  geom_line(data = grid_df, aes(x = horsepower, y = mpg_estimates), color = "red", size = 1) +
  labs(x = "Horsepower", y = "Mpg") +
  ggtitle("Regression Splines: Estimating Mpg from Horsepower") +
  theme_bw()