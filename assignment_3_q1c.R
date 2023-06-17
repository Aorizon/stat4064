library(ISLR2)
library(ggplot2)

# Load data and remove NAs
data(Auto)
auto_clean <- na.omit(Auto)

# Filter for cars year >= 76
Auto76 <- subset(auto_clean, year >= 76)

# Fit polynomial models of degree 2 to 5
degrees <- 2:5
MSE <- numeric(length = length(degrees))
for (i in 1:length(degrees)) {
  degree <- degrees[i]
  poly_terms <- poly(Auto76$horsepower, degree, raw = TRUE)
  fit <- lm(mpg ~ poly_terms, data = Auto76)
  MSE[i] <- sum((Auto76$mpg - predict(fit, newdata = Auto76, type = "response"))^2) / nrow(Auto76)
}

# Find the index of the best degree
best_degree <- which.min(MSE)

# Print the results
cat("The best polynomial degree is:", degrees[best_degree], "\n")
cat("The MSE of the selected model is:", MSE[best_degree], "\n")

# Plot the results
ggplot(Auto76, aes(x = horsepower, y = mpg)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x, degrees[best_degree], raw = TRUE), 
              se = FALSE, colour = "red") +
  labs(x = "Horsepower", y = "Mpg") +
  ggtitle("Polynomial model of Horsepower predicting Mpg") +
  theme_bw()
