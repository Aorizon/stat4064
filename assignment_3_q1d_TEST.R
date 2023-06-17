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

# Predict mpg values using the fitted model
mpg_estimates <- predict(fit, newdata = Auto76)

# Create a dataframe with original data and estimated mpg values
data_df <- data.frame(Horsepower = Auto76$horsepower, MPG = Auto76$mpg, EstimatedMPG = mpg_estimates)

# Plot the original data and the fitted curve
ggplot(data_df, aes(x = Horsepower, y = MPG)) +
  geom_point() +
  geom_line(aes(y = EstimatedMPG), color = "red", size = 1) +
  labs(x = "Horsepower", y = "Mpg") +
  ggtitle("Original Data and Fitted Polynomial Curve") +
  theme_bw()
