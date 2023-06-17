library(ggplot2)
library(splines)
library(ISLR2)

# Load data and remove NAs
data(Auto)
auto_clean <- na.omit(Auto)

# Filter for cars year >= 76
Auto76 <- subset(auto_clean, year >= 76)

# Define knot values
knots <- c(60, 92, 116, 140, 164)

# Fit regression splines of degree 2 and 3
spline_fit_degree2 <- lm(mpg ~ bs(horsepower, knots = knots, degree = 2), data = Auto76)
spline_fit_degree3 <- lm(mpg ~ bs(horsepower, knots = knots, degree = 3), data = Auto76)

# Predict mpg values for the new grid of horsepower values
grid_horsepower <- seq(4, 192, 4)
grid_mpg_estimates_degree2 <- predict(spline_fit_degree2, newdata = data.frame(horsepower = grid_horsepower), type = "response")
grid_mpg_estimates_degree3 <- predict(spline_fit_degree3, newdata = data.frame(horsepower = grid_horsepower), type = "response")

# Create a data frame with horsepower and mpg estimates
df_results <- data.frame(horsepower = grid_horsepower, 
                         mpg_estimates_degree2 = grid_mpg_estimates_degree2,
                         mpg_estimates_degree3 = grid_mpg_estimates_degree3)

# Plot the results
ggplot(data = df_results) +
  geom_line(aes(x = horsepower, y = mpg_estimates_degree2, color = "Degree 2"), size = 1) +
  geom_line(aes(x = horsepower, y = mpg_estimates_degree3, color = "Degree 3"), size = 1) +
  geom_point(data = Auto76, aes(x = horsepower, y = mpg), color = "black") +
  labs(x = "Horsepower", y = "Mpg", color = "Degree") +
  ggtitle("Regression Splines with Knots and Degrees") +
  theme_bw() +
  scale_color_manual(values = c("blue", "red")) #+
  #ylim(0, 120)  # Set the y-axis limits from 0 to 120
