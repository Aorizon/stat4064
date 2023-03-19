library(MASS)
library(ISLR)
library(ISLR2)
library(graphics)
library(ggplot2)
library(FNN)
library(DAAG)

# #Load Carseats dataset
# data(Carseats)

# #head(Carseats)
# str(Carseats)
# 
# # Perform linear regression on Sales using all other variables
# model <- lm(Carseats$Sales ~ ., data = Carseats[, -1])
# 
# # View the model summary
# summary(model)
# 
# # Perform linear regression on Sales using all other variables
# model2 <- lm(Carseats$Sales ~ ., data = Carseats[, c('CompPrice','Price','ShelveLoc')])
# 
# # View the model summary
# summary(model2)

# #Define number of iterations of k
# iter = 21
# 
# #Setup vector to store errors
# errors <- numeric(iter)
# 
# #k-NN regression 
# for (k in 1:21){
#   Carseats.knn <- knn.reg(train = Carseats$Price, y = Carseats$Sales, k = k)
#   
#   #Calculate PRESS error at position 'k' in 'errors'
#   errors[k] <- sum((Carseats.knn$pred - Carseats$Sales)^2)
# }
# 
# #Finding lowest PRESS error k value (locates index of value in list using which.min)
# best_k <- which.min(errors)
# 
# #Perform kNN regression using optimal k hyperparameter
# Carseats.knn <- knn.reg(train = Carseats$Price, y = Carseats$Sales, k = best_k)
# 
# # Create a scatter plot of the predicted vs. actual values
# plot(Carseats$Sales, Carseats.knn$pred, main = "k-NN Regression: Predicted vs. Actual Sales", 
#      xlab = "Actual Sales", ylab = "Predicted Sales")
# 
# #Summary
# summary_msg <- paste(best_k,"is the best k value with a PRESS value of", errors[best_k])
# cat(summary_msg)

# #Load in 'hills' dataset
# data(hills)

# #Plot response 'time' against predictors
# plot(hills)

# # Perform linear regression on Sales using all other variables
# linear_model_dist <- lm(time ~ dist, data = hills)
# linear_model_climb <- lm(time ~ climb, data = hills)
# linear_model_combined <- lm(time ~ climb + dist, data = hills)
# 
# # model_dist$residuals
# # model_climb$residuals
# 
# #Quadratic regression
# # quad_model_dist <- lm(time ~ dist +I(dist^2), data = hills)
# # quad_model_climb <- lm(time ~ climb + I(climb^2), data = hills)
# quad_model_combined <- lm(time ~ climb + dist + I(climb^2) +I(dist^2), data = hills)
# 
# # summary(quad_model_dist)
# # summary(quad_model_climb)
# # summary(quad_model_combined)
# summary(linear_model_combined)
# 
# #MSE
# MSE_linear <- mean((linear_model_combined$residuals^2))
# MSE_quad <- mean((quad_model_combined$residuals^2))
# c(MSE_linear,MSE_quad)

# #Polynomial
# poly_4 <- lm(time ~ poly(dist,4), data = hills)
# summary(poly_4)

# #Optimised poly
# poly_selector <- function(x) {
#   errors <- numeric(x)
#   for (p in 1:x) {
#     poly <- lm(time ~ poly(dist,p), data = hills)
#     errors[p] <- mean(poly$residuals^2)
#   }
#   best_x <- which.min(errors)
#   return(list(best_x=best_x,errors=errors))
# }
# 
# poly_no <- 6
# poly_selector_results <- poly_selector(poly_no)
# 
# poly <- lm(time ~ poly(dist,poly_selector_results$best_x), data = hills)
# 
# #Plot poly MSE values
# x_values <- c(1:poly_no)
# y_values <- poly_selector_results$errors
# plot(x=x_values, y=y_values)






