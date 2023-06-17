library(ISLR2)
library(ggplot2)
library(MASS)
library(boot)
data(Auto)

# Remove NAs
auto_clean <- na.omit(Auto)

# Cars year >= 76
Auto76 <- subset(auto_clean, year >= 76)

# create scatterplot
ggplot(Auto76, aes(x = horsepower, y = mpg)) +
  geom_point() +

  # add axis titles
  labs(x = "Horsepower", y = "Mpg") +

  # add plot title
  ggtitle("Mpg response vs. Horsepower predictor")

# Fit linear model
model <- lm(mpg ~ horsepower, data = Auto76)

# print model summary
summary(model)

# calculate MSE
mse <- mean(model$residuals^2)
cat("MSE:", mse)

# create scatterplot with linear model
ggplot(Auto76, aes(x = horsepower, y = mpg)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +

  # add axis titles
  labs(x = "Horsepower", y = "Mpg") +

  # add plot title
  ggtitle("Linear model of Horsepower predicting Mpg")

