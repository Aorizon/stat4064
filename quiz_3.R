library(MASS)
library(ISLR)
library(ISLR2)
library(graphics)
library(ggplot2)
library(FNN)
library(DAAG)

# #Import data
# data(Smarket)
# print(Smarket)
# Smarket <- Smarket[, -1]
# print(Smarket)
# #print(Smarket)
# 
# # Fit the logistic regression model with all predictors
# model <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = Smarket, family = binomial())
# 
# # Make predictions on the same dataset
# pred <- predict(model, type = "response")
# 
# # Count the number of observations with label Up that were assigned to Down
# n <- sum(Smarket$Direction == "Up" & pred < 0.5)
# 
# n

# Load the Default dataset from ISLR2 library
data(Default)

# Fit a logistic regression model to the Default dataset
model <- glm(default ~ student, data = Default, family = "binomial")

# Print the summary of the model
summary(model)

            
