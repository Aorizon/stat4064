library(ISLR2)
library(ggplot2)
library(boot)

# Load the Auto data
data(Auto)

# Remove observations with missing values
auto_clean <- na.omit(Auto)

# Subset of cars with Year >= 76
Auto76 <- subset(auto_clean, year >= 76)

# (a) Scatterplot of mpg vs horsepower for Auto76
ggplot(data = Auto76, aes(x = horsepower, y = mpg)) +
  geom_point() +
  labs(x = "Horsepower", y = "Mpg") +
  ggtitle("Scatterplot of Horsepower vs MPG for Auto76")

# (b) Fit a linear model to horsepower and mpg
lm_model <- lm(mpg ~ horsepower, data = Auto76)
slope <- coef(lm_model)[2]
intercept <- coef(lm_model)[1]
equation <- paste("mpg =", slope, "* horsepower +", intercept)
mse <- mean(lm_model$residuals^2)

# Add fitted line to the scatterplot
scatterplot <- ggplot(data = Auto76, aes(x = horsepower, y = mpg)) +
  geom_point() +
  geom_abline(intercept = intercept, slope = slope, color = "red") +
  labs(x = "Horsepower", y = "Mpg") +
  ggtitle("Scatterplot of Horsepower vs MPG with Fitted Line") 

print(scatterplot)

# (c) Fit polynomials of degree 2, 3, 4, and 5 with LOOCV
degrees <- c(2, 3, 4, 5)
cv_mse <- rep(0, length(degrees))

for (deg in degrees) {
  poly_model <- glm(mpg ~ poly(horsepower, deg), data = Auto76)
  cv_mse[deg] <- cv.glm(Auto76, poly_model, K = nrow(Auto76))$delta[1]
}

best_degree <- which.min(cv_mse) + 1
best_poly_model <- glm(mpg ~ poly(horsepower, best_degree), data = Auto76)
best_poly_model <- glm(mpg ~ poly(horsepower, 3), data = Auto76)

# (d) Predict estimates for mpg using the selected model on grid G
grid_horsepower <- seq(4, 192, 4)
grid_data <- data.frame(horsepower = grid_horsepower)
grid_data$mpg_estimates <- predict(best_poly_model, newdata = grid_data)

# Scatterplot with predicted estimates
scatterplot <- ggplot() +
  geom_point(data = Auto76, aes(x = horsepower, y = mpg), color = "black") +
  geom_line(data = grid_data, aes(x = horsepower, y = mpg_estimates), color = "red") +
  labs(x = "Horsepower", y = "Mpg") +
  ggtitle("Predicted Mpg values using Horsepower grid test data") +
  theme_bw()

print(scatterplot)

