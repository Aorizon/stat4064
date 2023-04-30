# Import libraries
library(ISLR2)
library(MASS)

# Add Weekly dataset
data(Weekly)

# Create a data frame with only the predictors and response variable
weekly_df <- data.frame(Weekly$Lag1, Weekly$Lag2, Weekly$Lag4, Direction = as.factor(Weekly$Direction))
names(weekly_df) <- c("Lag1", "Lag2","Lag4","Direction")

# Remove the Direction variable before computing column means
means <- colMeans(weekly_df[, -4], na.rm = TRUE)

# Replace missing values with column means
weekly_df[is.na(weekly_df)] <- means[rep(1, length(weekly_df))]

# Initialize the error counter
error_count <- 0

##### LOGISTIC REGRESSION #####

# Fit the LG model and calculate LOOCV error
for (i in 1:nrow(weekly_df)) {
  # Fit the model using all observations except the i-th observation
  fit <- glm(Direction ~ Lag1 + Lag2 + Lag4, data = weekly_df[-i,], family = binomial)
  
  # Make a prediction for the i-th observation
  pred <- predict(fit, newdata = weekly_df[i, -4], type = "response")
  
  # Compute the prediction error
  error_count <- error_count + (pred - ifelse(weekly_df[i, 4] == "Up", 1, 0))^2
}

# Compute the LOOCV error as the average of the prediction errors
glm_loocv_error <- error_count / nrow(weekly_df)

cat("Logistic Regression LOOCV Error:", round(glm_loocv_error, 4), "\n")

##### LDA #####

# Fit the LDA model and calculate LOOCV error
error_count <- 0
for (i in 1:nrow(weekly_df)) {
  # Fit the model using all observations except the i-th observation
  fit <- lda(Direction ~ Lag1 + Lag2 + Lag4, data = weekly_df[-i,])
  
  # Make a prediction for the i-th observation
  pred <- predict(fit, newdata = weekly_df[i, -4])
  
  # Compute the prediction error
  error_count <- error_count + (pred$class != weekly_df[i, 4])
}

# Compute the LOOCV error as the average of the prediction errors
lda_loocv_error <- error_count / nrow(weekly_df)

cat("LDA LOOCV Error:", round(lda_loocv_error, 4), "\n")

##### QDA #####

# Fit the QDA model and calculate LOOCV error
error_count <- 0
for (i in 1:nrow(weekly_df)) {
  # Fit the model using all observations except the i-th observation
  fit <- qda(Direction ~ Lag1 + Lag2 + Lag4, data = weekly_df[-i,])
  
  # Make a prediction for the i-th observation
  pred <- predict(fit, newdata = weekly_df[i, -4])
  
  # Compute the prediction error
  error_count <- error_count + (pred$class != weekly_df[i, 4])
}

# Compute the LOOCV error as the average of the prediction errors
qda_loocv_error <- error_count / nrow(weekly_df)

cat("QDA LOOCV Error:", round(qda_loocv_error, 4), "\n")

