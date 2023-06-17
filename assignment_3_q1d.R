library(ISLR2)
library(ggplot2)

# Load data and remove NAs
data(Auto)
auto_clean <- na.omit(Auto)

# Filter for cars year >= 76
Auto76 <- subset(auto_clean, year >= 76)

# Fit polynomial models of degree 5
degree <- 5
poly_terms <- poly(Auto76$horsepower, degree, raw = TRUE)
fit <- lm(mpg ~ poly_terms, data = Auto76)

# Define grid of horsepower values
grid_horsepower <- seq(4, 192, 4)

# Predict mpg values on grid of horsepower values
grid_terms <- poly(grid_horsepower, degree = 5, raw = TRUE)
grid_mpg_estimates <- predict(fit, newdata = data.frame(pred_hp = grid_terms[,1]))
grid_df <- data.frame(
  horsepower = seq(4, 192, 4),
  mpg_estimates = grid_mpg_estimates[seq(4, 192, 4)]
)

# Plot the results
ggplot(data = grid_df, aes(x = horsepower, y = mpg_estimates)) +
  geom_point() +
  geom_line(aes(x = horsepower, y = mpg_estimates), color = "red", size = 1) +
  labs(x = "Horsepower", y = "Mpg") +
  ggtitle("Mpg response vs. Horsepower predictor") +
  theme_bw()