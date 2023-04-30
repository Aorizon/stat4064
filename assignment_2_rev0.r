# Import libraries
library(ISLR2)
library(MASS)

# Add Weekly dataset
data(Weekly)

# Create a data frame with only the predictors and response variable
weekly_df <- data.frame(Weekly$Lag1, Weekly$Lag2, Weekly$Lag4, Direction = as.factor(Weekly$Direction))
names(weekly_df) <- c("Lag1", "Lag2","Lag4","Direction")

# Remove rows with missing values
weekly_df <- na.omit(weekly_df)

# Define the LOOCV function
loocv <- function(model) {
  n <- nrow(weekly_df)
  errors <- rep(0, n)
  
  for (i in 1:n) {
    # Fit the model using all observations except the i-th observation
    fit <- model(-i, data = weekly_df, na.action = na.exclude)
    
    # Make a prediction for the i-th observation
    pred <- predict(fit, newdata = weekly_df[i, -4], type = "response")
    
    # Compute the prediction error
    errors[i] <- (pred - ifelse(weekly_df[i, 4] == "Up", 1, 0))^2
  }
  
  # Compute the LOOCV error as the average of the prediction errors
  loocv_error <- mean(errors)
  
  return(loocv_error)
}

# Calculate the LOOCV error for logistic regression
glm_loocv_error <- loocv(function(i, data,na.action) glm(Direction ~ Lag1 + Lag2 + Lag4, data = data[-i, ], na.action = na.exclude, family = "binomial"))
cat("Logistic Regression LOOCV Error:", round(glm_loocv_error, 4), "\n")

# Calculate the LOOCV error for LDA
lda_loocv_error <- loocv(function(i, data,na.action) lda(Direction ~ Lag1 + Lag2 + Lag4, data = data[-i, ], na.action = na.exclude))
cat("LDA LOOCV Error:", round(lda_loocv_error, 4), "\n")

# Calculate the LOOCV error for QDA
qda_loocv_error <- loocv(function(i, data,na.action) qda(Direction ~ Lag1 + Lag2 + Lag4, data = data[-i, ], na.action = na.exclude))
cat("QDA LOOCV Error:", round(qda_loocv_error, 4), "\n")
